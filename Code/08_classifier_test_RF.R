
###classification test RF & evaluation

#load packages
list.of.packages <- c("tidytext","dplyr","tidyr", "readtext", "quanteda", "quanteda.corpora", "kernlab",
                      "caret", "vecsets", "doParallel", "vip", "rsample", "foreach", "klaR", "e1071", 
                      "quanteda.textmodels", "naivebayes", "stringr", "ranger", "formattable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


#load data
dfmhashtag <- readRDS("Data/dfmhashtag.RDS")
dfmnostop <- readRDS("Data/dfmnostop.RDS")
dfmstem <- readRDS("Data/dfmstem.RDS")
dfmtfidf <- readRDS("Data/dfmtfidf.RDS")
tweets_sample <- readRDS("Data/tweets_sample.RData")


#empty dataframes for merging results
performance_checks_rf <- data.frame(metric = c("Accuracy", 
                                            "Precision_1", "Precision_2", "Precision_3", "Precision_4", "Precision_5", 
                                            "Recall_1", "Recall_2", "Recall_3", "Recall_4", "Recall_5", 
                                            "F1_1" ,"F1_2" ,"F1_3" ,"F1_4" ,"F1_5", "RMSE"))


#programme function for classification for all data
cat_classifier_rf <- function(category,var,df){
  
  #split in labelled (for CV) and unlabelled (for prediction)
  docvars(category, var) <- df %>% dplyr::select(var)  
  labelled <- category %>% 
    dfm_subset((docvars(category, var)) == "0"| (docvars(category, var)) == "1" | (docvars(category, var)) == "2" | (docvars(category, var)) == "3" | 
                 (docvars(category, var)) == "4" | (docvars(category, var)) == "5")
  split <- sample(c("train", "test"), size=ndoc(labelled), prob=c(0.80, 0.20), replace=TRUE)
  train <- which(split=="train") 
  test <- which(split=="test")
  train <- labelled[train,]
  test <- labelled[test,]
  
  #upsample training data
  if(var != "hatescaleM"){
    unbalanced <- data.frame(id = docnames(train), 
                             var = docvars(train, var),
                             stringsAsFactors = FALSE)
    balanced_dfm <- caret::upSample(unbalanced %>% dplyr::select(-var), unbalanced$var,
                                    list=FALSE, yname = var)
    ids <- balanced_dfm %>% subset(duplicated(balanced_dfm) == TRUE)
    balanced_dfm <- rbind(train, train[ids$id,])
    docvars(balanced_dfm, var) <-  as.factor(c((docvars(train, var)), (docvars(train[ids$id,], var))))
  } else {
    balanced_dfm <- train  
  }
  train <- dfm_trim(balanced_dfm, min_termfreq = 1)
  feat <- featnames(train)
  test <- dfm_match(test, feat)
  if(var != "hatescale" & var != "hatescaleM"){
    levels(docvars(test, var)) <- c("1", "2")
  }
  
  #tune
  set.seed(124)
  model_tune <- ranger(
    x = train, 
    y = docvars(train, var))
  pred <- predict(model_tune, data = test)
  
  
  if(var != "hatescaleM"){
    cv <- confusionMatrix(pred$predictions, docvars(test, var), positive = "2", mode = "everything")
    print(cv)
  }else{
    rmse <- data.frame(var = docvars(test, var), pred = pred)
    rmse <- rmse %>%
      mutate(residual = var - pred)  %>% 
      summarize(rmse  = sqrt(mean(residual^2)))
    print(rmse)
  }
  
  #dataframe
  if(var!= "hatescaleM" & var!= "hatescale"){
    metric <- data.frame(Accuracy = cv$overall[1], 
                         Precision_1 = NA,
                         Precision_2 = cv$byClass[5],
                         Precision_3 = NA,
                         Precision_4 = NA, 
                         Precision_5 = NA,
                         Recall_1 = NA, 
                         Recall_2 = cv$byClass[6], 
                         Recall_3 = NA, 
                         Recall_4 = NA, 
                         Recall_5 = NA, 
                         F1_1 = NA, 
                         F1_2 = cv$byClass[7],
                         F1_3 = NA, 
                         F1_4 = NA, 
                         F1_5 = NA, 
                         RMSE = NA,
                         row.names = var)
  }else if(var == "hatescale"){
    metric <- data.frame(Accuracy = cv$overall[1], 
                         Precision_1 = cv$byClass[21],
                         Precision_2 = cv$byClass[22],
                         Precision_3 = cv$byClass[23],
                         Precision_4 = cv$byClass[24], 
                         Precision_5 = cv$byClass[25],
                         Recall_1 = cv$byClass[26], 
                         Recall_2 = cv$byClass[27], 
                         Recall_3 = cv$byClass[28], 
                         Recall_4 = cv$byClass[29], 
                         Recall_5 = cv$byClass[30], 
                         F1_1 = cv$byClass[31], 
                         F1_2 = cv$byClass[32],
                         F1_3 = cv$byClass[33], 
                         F1_4 = cv$byClass[34], 
                         F1_5 = cv$byClass[35], 
                         RMSE = NA,
                         row.names = var)
  }else if(var == "hatescaleM"){
    metric <- data.frame(Accuracy = NA, 
                         Precision_1 = NA,
                         Precision_2 = NA,
                         Precision_3 = NA, 
                         Precision_4 = NA, 
                         Precision_5 = NA,
                         Recall_1 = NA, 
                         Recall_2 = NA, 
                         Recall_3 = NA,  
                         Recall_4 = NA, 
                         Recall_5 = NA, 
                         F1_1 = NA, 
                         F1_2 = NA,
                         F1_3 = NA, 
                         F1_4 = NA, 
                         F1_5 = NA, 
                         RMSE = rmse$rmse,
                         row.names = var)
  }
  
  metric <- metric %>% pivot_longer(
    cols = c("Accuracy", 
             "Precision_1", "Precision_2", "Precision_3", "Precision_4", "Precision_5", 
             "Recall_1", "Recall_2", "Recall_3", "Recall_4", "Recall_5", 
             "F1_1" ,"F1_2" ,"F1_3" ,"F1_4" ,"F1_5", "RMSE"), 
    names_to = "metric", 
    values_to = var)
  
  return(metric)
}

##A: dfmhashtag
#prediction on hatenom and hatescale on all data
    cluster <- makeCluster(detectCores()-1)
    registerDoParallel(cluster)
    start.time <- Sys.time()
    
    outcome_hate <- foreach (i = c(42,43,45), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "ranger")) %dopar%  {
      result <- cat_classifier_sample(dfmhashtag,names(tweets_sample)[i], tweets_sample)
    }
               
    print(Sys.time() - start.time)
    stopCluster(cluster)
  
  #left join metrics together 
    for(i in 1:3){
      metric <- outcome_hate[i][[1]]
      performance_checks_rf <- left_join(performance_checks_rf, metric, by="metric")
    }
    colnames(performance_checks_rf)[2:4] <- paste(colnames(performance_checks_rf[2:4]), "dfmhashtag", sep = "_")
  
  
##B: dfmnostop
  #prediction on hatenom and hatescale on all data
    cluster <- makeCluster(detectCores()-1)
    registerDoParallel(cluster)
    start.time <- Sys.time()
  
  #1: CLASSIFIER FOR HATE   
    outcome_hate <- foreach (i = c(42,43,45), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar%  {
      result <- cat_classifier_sample(dfmnostop,names(tweets_sample)[i], tweets_sample)
    }
    
    print(Sys.time() - start.time)
    stopCluster(cluster)

  #left join metrics together 
    for(i in 1:3){
      metric <- outcome_hate[i][[1]]
      performance_checks_rf <- left_join(performance_checks_rf, metric, by="metric")
    }
    colnames(performance_checks_rf)[5:7] <- paste(colnames(performance_checks_rf[5:7]), "dfmnostop", sep = "_")
  
  
##C: dfmstem
  #prediction on hatenom and hatescale on all data
    cluster <- makeCluster(detectCores()-1)
    registerDoParallel(cluster)
    start.time <- Sys.time()
  
  #1: CLASSIFIER FOR HATE   
    outcome_hate <- foreach (i = c(42,43,45), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar%  {
      result <- cat_classifier_sample(dfmstem,names(tweets_sample[i]), tweets_sample)
    }
    
    print(Sys.time() - start.time)
    stopCluster(cluster)
  
  #left join metrics together 
    for(i in 1:3){
      metric <- outcome_hate[i][[1]]
      performance_checks_rf <- left_join(performance_checks_rf, metric, by="metric")
    }
    colnames(performance_checks_rf)[8:10] <- paste(colnames(performance_checks_rf[8:10]), "dfmstem", sep = "_")
  
  
##D: dfmtfidf
  #prediction on hatenom and hatescale on all data
    cluster <- makeCluster(detectCores()-1)
    registerDoParallel(cluster)
    start.time <- Sys.time()
  
  #1: CLASSIFIER FOR HATE   
    outcome_hate <- foreach (i = c(42,43,45), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar%  {
      result <- cat_classifier_sample(dfmtfidf,names(tweets_sample)[i], tweets_sample)
    }
    
    print(Sys.time() - start.time)
    stopCluster(cluster)
  
  #left join metrics together 
    for(i in 1:3){
      metric <- outcome_hate[i][[1]]
      performance_checks_rf <- left_join(performance_checks_rf, metric, by="metric")
    }
    colnames(performance_checks_rf)[11:13] <- paste(colnames(performance_checks_rf[11:13]), "nostop_tfidf", sep = "_")
  
  
#save data  
  saveRDS(performance_checks_rf, "Data/performance_test_RF.RData")
  
  
#evaluation performance scores
  performance_checks_SVM <- readRDS("Data/performance_test_SVM.RData")
  performance_checks_NB <- readRDS("Data/performance_test_nb.RData")
  performance_checks_RF <- readRDS("Data/performance_test_RF.RData")
  
  
  colnames(performance_checks_SVM)[2:13] <- paste(colnames(performance_checks_SVM)[2:13], "svm", sep = "_")
  colnames(performance_checks_NB)[2:9] <- paste(colnames(performance_checks_NB)[2:9], "nb", sep = "_")
  colnames(performance_checks_RF)[2:13] <- paste(colnames(performance_checks_RF)[2:13], "rf", sep = "_")
  table_classiftest <- left_join(performance_checks_SVM, performance_checks_NB, by = "metric")
  table_classiftest <- left_join(table_classiftest, performance_checks_RF, by = "metric")
  table_classiftest <- table_classiftest %>% dplyr::select(metric, starts_with("hatenomNA"))
  table_classiftest <- na.omit(table_classiftest)
  table_classiftest$metric <- c("Accuracy", "Precision", "Recall", "F1")
  rownames(table_classiftest) <- c("Accuracy", "Precision", "Recall", "F1")
  table_classiftest <- table_classiftest %>% dplyr::select(-metric)
  table_classiftest <- round(table_classiftest, digits = 2)
  colnames(table_classiftest) <- c("SVM w/o urls", "SVM w/o stopwords", "SVM stemmed", "SVM w/o stopwords tf-idf", 
                                    "NB w/o urls", "NB w/o stopwords", "NB stemmed", "NB w/o stopwords tf-idf", 
                                    "RF w/o urls", "RF w/o stopwords", "RF stemmed", "RF w/o stopwords tf-idf")
  table_classiftest <- as.data.frame(t(table_classiftest))
  customGreen0 = "#DeF7E9"
  customGreen = "#71CA97"
  formattable(table_classiftest, 
              align =c("c","c","c","c"), 
              list(`Metric` = formatter(
                "span", style = ~ style(color = "black",font.weight = "bold")),
                `Accuracy`= color_tile(customGreen0, customGreen),
                `Precision`= color_tile(customGreen0, customGreen),
                `Recall`= color_tile(customGreen0, customGreen),
                `F1`= color_tile(customGreen0, customGreen)))
  