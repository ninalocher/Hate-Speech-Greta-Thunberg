
### create final dataset for descriptive analysis

#load packages
  list.of.packages <- c("tidytext","dplyr","tidyr", "formattable", "stringr", "quanteda")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = TRUE)


#load predicted values and dfm 
  load("Data/outcome_hate.RData")
  load("Data/outcome_cat.RData")
  outcome_inter <- readRDS("Data/outcome_inter.RData")
  dfm <- readRDS("Data/dfmnostop_all.RDS")
  
  #function
  num_conversion <- function(x){
    as.numeric(x)-1
  }

#empty dataframes for merging results
  classified <- data.frame(tweet__id = docnames(dfm))
  performance <- data.frame(metric = c("Accuracy", 
                                       "Precision_1", "Precision_2", "Precision_3", "Precision_4", "Precision_5", 
                                       "Recall_1", "Recall_2", "Recall_3", "Recall_4", "Recall_5", 
                                       "F1_1" ,"F1_2" ,"F1_3" ,"F1_4" ,"F1_5", "RMSE"))

#left join all predids and metrics together 
  for(i in 1:3){
    predids <- outcome_hate[i][[1]]$predids
    metric <- outcome_hate[i][[1]]$metric
    classified <- left_join(classified, predids, by="tweet__id")
    performance <- left_join(performance, metric, by="metric")
  }
  classified$hatescaleM <- round(classified$hatescaleM, digits = 0)

#left join all predids and metrics together for categories
  for(i in 1:19){
    predids <- outcome_cat[i][[1]]$predids
    metric <- outcome_cat[i][[1]]$metric
    classified <- left_join(classified, predids, by="tweet__id")
    performance <- left_join(performance, metric, by="metric")
  }

#left join all predids and metrics together for intersectional categories
  for(i in 1:13){
    predids <- outcome_inter[i][[1]]$predids
    metric <- outcome_inter[i][[1]]$metric
    classified <- left_join(classified, predids, by="tweet__id")
    performance <- left_join(performance, metric, by="metric")
  }
  
#save performance metrics  
  write.csv(performance, "Data/performance_predicted_postclassifier.csv", row.names = FALSE, quote = TRUE)
  write.csv(classified, "Data/tweets_predicted_postclassifier.csv", row.names = FALSE, quote = TRUE)
  
#clean performance metrics  
  performance <- performance %>% dplyr::select(-X4_class_age, -X4_gender_manip_age, -X4_gender_manip, -X4_mental_cc, -X4_manip_mental_age, 
                                     -X4_manip_cc_age, -X4_manip_mental, -X4_cc_age, -X4_manip_class, -X4_dontknow, -X3_dontknow)  
  performance[,-1] <- round(performance[,-1], digits =2)
  rownames(performance) <- performance$metric
  col_order <- c("metric","hatenomNA", "hatescale", "hatescaleM","X3_disparagement",
                 "X3_insult", "X3_mocking", "X3_prejudice", "X3_violence", "X3_other", 
                 "X4_age", "X4_manipulation", "X4_climatechange","X4_mentalphysicalcapacity", 
                 "X4_gender", "X4_class", "X4_race", "X4_manip_age",
                 "X4_mental_age", "X4_gender_age", "X4_manip_cc","X4_other", "X5_dontknow", 
                 "X5_implicitlanguage", "X5_explicitlanguage")
  performance <- performance[, col_order]
  
  performance_gen <- performance %>% 
    dplyr::select(-hatescale, -hatescaleM) %>%
    filter(metric == "Precision_2" | metric == "Accuracy" | metric == "Recall_2" | metric == "F1_2")
  performance_gen <- as.data.frame(t(performance_gen))
  colnames(performance_gen) <- c("Accuracy", "Precision", "Recall", "F1")
  performance_gen <- performance_gen[c(-1,-21),]

  performance_scale <- performance %>%
    dplyr::select(hatescale, hatescaleM)
  colnames(performance_scale) <- c("Hatefulness (Factor)", "Hatefulness (Metric)")

#visualize performance metrics
  colnames(performance_gen) <- c("Accuracy", "Precision", "   Recall   ", "     F1     ")
  customGreen0 = "#DeF7E9"
  customGreen = "#71CA97"
  formattable(performance_gen, 
              align =c("c","c","c","c"), 
              list(`rownames` = formatter(
                "span", style = ~ style(color = "black",font.weight = "bold")),
                `Accuracy`= color_tile(customGreen0, customGreen),
                `Precision`= color_tile(customGreen0, customGreen),
                `   Recall   `= color_tile(customGreen0, customGreen),
                `     F1     `= color_tile(customGreen0, customGreen)))
  
  formattable(performance_scale, 
              align =c("c","c"), 
              list(`rownames()` = formatter(
                "span", style = ~ style(color = "black",font.weight = "bold")),
                `Hatefulness (Factor)`= color_tile(customGreen0, customGreen),
                `Hatefulness (Metric)`= color_tile(customGreen, customGreen0)))
  
  
  
  
#load rest of data for merging
  tweets_predicted_postclassifier <- read.csv("Data/tweets_predicted_postclassifier.csv", #predicted output
                                              stringsAsFactors = FALSE, numerals = "no.loss")
  tweets_all_preclassifier <- readRDS("Data/tweets_all_merged_preprocessed.RData")   #full dataset with manually labelled tweets
  
#clean data  
  tweets_predicted_postclassifier <- tweets_predicted_postclassifier %>%  #predicted tweets
    mutate_at(names(tweets_predicted_postclassifier)[str_detect(names(tweets_predicted_postclassifier),"^X")],as.numeric) %>%
    mutate(hatenomNA = as.numeric(hatenomNA))

  tweets_labelled_preclassifier <- tweets_all_preclassifier %>% dplyr::filter(!is.na(tag__title))    #manually labelled tweets


#merge labelled pre and post classified data  
  final <- tweets_all_preclassifier %>% 
    dplyr::select(!starts_with("X")) %>% 
    left_join(tweets_predicted_postclassifier, by="tweet__id")%>% 
    dplyr::select(-ends_with(".x")) %>% 
    dplyr::rename(c(hatescale = hatescale.y,hatenomNA = hatenomNA.y,hatescaleM = hatescaleM.y))%>% 
    dplyr::filter(!tweet__id %in% (tweets_labelled_preclassifier$tweet__id)) %>%
    mutate(hatescaleM = ifelse(hatescaleM > 5, 5, 
                               ifelse(hatescaleM < 1, 1, hatescaleM)))
  
  final <- final %>% mutate_at(names(final)[str_detect(names(final),"^X")],num_conversion) %>%
    mutate(hatenomNA = num_conversion(hatenomNA)) %>% 
    bind_rows(tweets_labelled_preclassifier   %>% 
      dplyr::mutate(hatescale = as.numeric(hatescale)) %>%
      dplyr::select(-starts_with(c("X1", "X2"))) %>%
      mutate_if(is.factor,num_conversion)) %>%  
    mutate_at(names(final)[str_detect(names(final),"^X")],as.factor) %>%
    mutate_at(c("hatenomNA","hatescale"),as.factor)

  
#save final dataset  
write.csv(final, "Data/tweets_final_postclassifier.csv", row.names = FALSE, quote = TRUE)
