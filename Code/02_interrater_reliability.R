###03: Inter-Rater-Reliability Assessment

#load packages
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  list.of.packages <- c("irr", "dplyr","tidyr","tidyverse","epiR","psych", "formattable")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  invisible(lapply(list.of.packages, require, character.only = TRUE))


#Round 1: IRR with first labelling round categories


#create two datasets - one with multiple coders and one with one coder only
labelledunisample1 <- read.csv("Data/screened_tweets_firstround.csv", stringsAsFactors = FALSE, numerals = "no.loss")
labelledunisample1 <- labelledunisample1 %>%
  filter(tag__title == "Universal sample")
labelledunisample1 <- labelledunisample1 %>%
  filter(user__username %in% (labelledunisample1 %>% 
                                group_by(user__username) %>% 
                                summarise(count = n()) %>% 
                                filter(count==100))$user__username)
#results relevant: 1 is relevant, 2 is not relevant, 3 is maybe
 
#function for interrater reliability
  irr_nom <- function(unisample_irr, var){
  
    unisample_irr[,3] = as.factor(unisample_irr[,3])
    unisample_wide <- unisample_irr %>% 
      spread(user__username, var)
    unisample_wide <- unisample_wide %>%
    dplyr::select(-tweet__id)

  ##1 SIMPLE PERCENTAGE AGREEMENT
    perc <- unisample_wide %>%
      agree()
    
  ##2 EXACT KAPPAM FLEISS
    fleiss <- unisample_wide %>%
      kappam.fleiss(exact = TRUE, detail = FALSE)
    
  ##3 KAPPAM LIGHT
    klight <- unisample_wide %>%
      kappam.light()
    
  ##4 CATEGORYKAPPAM FLEISS
    cat <- unisample_wide %>%
      kappam.fleiss(exact = FALSE, detail = TRUE)
    
  table <- data.frame(Measurement = c("Agreement", "Total Fleiss Kappa", "Average Cohen's Kappa", 
                                      "ICC", "Kendall", "Cronbarch's alpha"),
                      var = c(perc$value, fleiss$value, klight$value, NA, NA, NA))
  table$var <- round(table$var, digits = 2)
  return(table)
}

#1 Relevance with three categories
  unisample_relevance <- labelledunisample1 %>%
    dplyr::select(tweet__id, user__username, relevant)
  irr1 <- irr_nom(unisample_relevance, names(unisample_relevance[3]))

#2 Relevance with two categories
  unisample_relevance <-  labelledunisample1 %>% 
    mutate(relevance2 = ifelse(relevant ==1,1,ifelse(relevant==2,2,ifelse(relevant==3,NA,NA)))) %>%
    dplyr::select(tweet__id, user__username, relevance2)
  irr2 <- irr_nom(unisample_relevance, names(unisample_relevance[3]))

#3 Category hateful tweet (2), offensive tweet (1) or neither (0) with NA set as neither
  #because many raters did not use the category "neither" as they valued the observation as "not      
      #relevant" in the other category
  unisample_relevance <- labelledunisample1 %>%
    mutate(hatecat = ifelse(hateful.tweet ==1,2,ifelse(offensive.tweet==1,1,ifelse(neither==1,0,0)))) %>% 
    dplyr::select(tweet__id, user__username, hatecat)
  irr3 <- irr_nom(unisample_relevance, names(unisample_relevance[3]))
  
#4 Binary Hate variable Hate/Offense and Neither
  unisample_relevance <-  labelledunisample1 %>% 
    mutate(hateoff = ifelse(hateful.tweet ==1,1,ifelse(offensive.tweet==1,1,ifelse(neither==1,0,0)))) %>%
    dplyr::select(tweet__id, user__username, hateoff)
  irr4 <- irr_nom(unisample_relevance, names(unisample_relevance[3]))
  
irr_batch1 <- left_join(irr3, irr4, by = "Measurement")  
irr_batch1 <- left_join(irr_batch1, irr3, by = "Measurement")
irr_batch1 <- left_join(irr_batch1, irr4, by = "Measurement") 
names(irr_batch1) <- c("Measurement", "Hate (2) / Off (1) / Neither (0)", "Hate/Off (1) / Neither (0)", "rel1", "rel2")



#Round2: IRR with categoriesf rom second labelling round

#1 Load data
  labelled_round2 <- read.csv("Data/screened_tweets_all_030520.csv", header = TRUE,
  stringsAsFactors = FALSE, numerals = "no.loss")
  labelled_round2 <- labelled_round2 %>%
    rename(X1_hate5 = X2...5...extremely.hateful.offensive, 
           X1_hate4 = X2...4...very.hateful.offensive, 
           X1_hate3 = X2...3...somewhat.hateful.offensive, 
           X1_hate2 = X2...2...not.very.hateful.offensive, 
           X1_hate1 = X2...1...not.hateful.offensive.at.all, 
           X1_dontknow = X2...y_don.t.know, 
           X2_environmentalgroup = X3...environmental.group,
           X2_gretathunberg = X3...Greta.Thunberg,
           X2_other = X3...x_other,
           X2_dontknow = X3...y_don.t.know,
           X3_disparagement = X4...disparagement,
           X3_mocking = X4...mocking,
           X3_violence = X4...violence,
           X3_prejudice = X4...prejudice,
           X3_insult = X4...insult,
           X3_other = X4...x_other,
           X3_dontknow = X4...y_don.t.know,
           X4_gender = X5...gender,
           X4_age = X5...age,
           X4_class = X5...class,
           X4_manipulation = X5...manipulation,
           X4_mentalphysicalcapacity = X5...mental.physical.capacity,
           X4_climatechange = X5...climate.change,
           X4_race = X5...race,
           X4_other = X5...x_other,
           X4_dontknow = X5...y_don.t.know,
           X5_explicitlanguage = X6...explicit.language,
           X5_implicitlanguage = X6...implicit.subtle.language,
           X5_dontknow = X6...y_don.t.know)

#merge scale of relevance
  labelled_round2 <- labelled_round2 %>% mutate(hatescale = ifelse(X1_hate5 ==1,5,
                                              ifelse(X1_hate4 ==1,4,
                                              ifelse(X1_hate3 ==1,3,
                                              ifelse(X1_hate2 ==1,2, 
                                              ifelse(X1_hate1 ==1,1,
                                              ifelse(X1_dontknow ==1,NA,NA)))))))
  labelled_round2 <- labelled_round2 %>% mutate(hatescale6 = ifelse(X1_hate5 ==1,5,
                                              ifelse(X1_hate4 ==1,4,
                                              ifelse(X1_hate3 ==1,3,
                                              ifelse(X1_hate2 ==1,2, 
                                              ifelse(X1_hate1 ==1,1,
                                              ifelse(X1_dontknow ==1,0,NA)))))))
                                              
#merge scale or hate binary
  labelled_round2 <- labelled_round2 %>% mutate(hatenomNA = ifelse(X1_hate5 ==1,1,
                                              ifelse(X1_hate4 ==1,1,
                                              ifelse(X1_hate3 ==1,1,
                                              ifelse(X1_hate2 ==1,1, 
                                              ifelse(X1_hate1 ==1,0,
                                              ifelse(X1_dontknow ==1,NA,NA)))))))
  labelled_round2 <- labelled_round2 %>% mutate(hatenom = ifelse(X1_hate5 ==1,1,
                                              ifelse(X1_hate4 ==1,1,
                                              ifelse(X1_hate3 ==1,1,
                                              ifelse(X1_hate2 ==1,1, 
                                              ifelse(X1_hate1 ==1,0,
                                              ifelse(X1_dontknow ==1,0,NA)))))))

#subset data in three datasets for majority decision code unisample tag 2
  labelledunisample2 <- labelled_round2 %>%       #tweets that are coded by everyone
    filter(tag__title == "Universal tag 2")
  labelledunisample2 <- labelledunisample2 %>%
    filter(user__username %in% (labelledunisample2 %>% 
                                   group_by(user__username) %>% 
                                   summarise(count = n()) %>% 
                                   filter(count==100))$user__username) 

#IRR scores unisample2

#function for continuous variable hatescale / hatescale6
  irr_cont <- function(unisample_irr, var){
    
    unisample_wide <- unisample_irr %>% 
      spread(user__username, var)
    unisample_wide <- unisample_wide %>%
      dplyr::select(-tweet__id)

    #PERCENTAGE AGREEMENT
      perc <- unisample_wide %>% 
        agree(tolerance=1)

    #Intraclass Correlation Coefficient
      ICC <- unisample_wide %>%
        icc(model = "twoway", type = "agreement", unit = "average")
    
    #Finn coefficient
      finn <- unisample_wide %>%    
        finn(s.levels = nlevels(as.factor(unisample_irr[,3])), model = "twoway")
    
    
    #Kendall Concordance for Ordinal Data
      kendall <- unisample_wide %>%    
        kendall(correct = TRUE)

    #CRONBACHS ALPHA
      alpha <- unisample_wide %>% 
        alpha(check.keys = TRUE)

    table <- data.frame(Measurement = c("Agreement", "ICC", "Kendall", "Cronbarch's alpha"),
                        Batch2 = c(perc$value, ICC$value, kendall$value, alpha$total$raw_alpha))
    table$Batch2 <- round(table$Batch2, digits = 2)
    return(table)
  }

#1 hatescale with "dont know" coded as NA - continuous variable
  unisample2_irr <- labelledunisample2 %>%
    dplyr::select(tweet__id, user__username, hatescale)
  irr5 <- irr_cont(unisample2_irr, names(unisample2_irr[3]))

#2 hatescale with "dont know" coded as 0 - continuous variable
  unisample2_irr <- labelledunisample2 %>%
    dplyr::select(tweet__id, user__username, hatescale6)
  irrcont_batch2 <- left_join(irr5, irr_cont(unisample2_irr, names(unisample2_irr[3])), by = "Measurement")

#3 hatescale with "dont know" coded as 0 - continuous variable
  unisample2_irr <- labelledunisample2 %>%
    dplyr::select(tweet__id, user__username, hatenomNA)
  irr6 <- irr_nom(unisample2_irr, names(unisample2_irr[3]))

#2 hatescale with "dont know" coded as 0 - continuous variable
  unisample2_irr <- labelledunisample2 %>%
    dplyr::select(tweet__id, user__username, hatenom)
  irrcat_batch2 <- left_join(irr6, irr_nom(unisample2_irr, names(unisample2_irr[3])), by = "Measurement")

#create table for visualisation
  names(irrcont_batch2) <- c("Measurement", "Hatefulness (1-5)", "hatescale6")
  names(irrcat_batch2) <- c("Measurement", "Hate/Off (1) / Neither (0)", "hatenom")
  table_irr <- left_join(irr_batch1, irrcat_batch2, by = "Measurement")
  table_irr <- left_join(table_irr, irrcont_batch2, by = "Measurement")
  table_irr <- table_irr %>% dplyr::select(-hatenom, -hatescale6, -rel1, -rel2)

  names(table_irr) <- c("Measurement", "Hate/Off/Neither Batch 1", "Hate-Off/Neither Batch 1", "Hate-Off/Neither Batch 2", "Hatefulness Batch 2")
  table_irr$Measurement[1] <- "Agreement %"
  formattable(table_irr, 
              align =c("l","r","r","r", "r"), 
              list(`Measurement` = formatter(
                "span", style = ~ style(color = "black",font.weight = "bold")),
  `Hate/Off/Neither Batch 1`= color_tile(customGreen, customGreen0),
  `Hate-Off/Neither Batch 1`= color_tile(customGreen, customGreen0),
  `Hate-Off/Neither Batch 2`= color_tile(customGreen, customGreen0),
  `Hatefulness Batch 2`= color_tile(customGreen, customGreen0)))
  

  
#subset rest of data for next processes

#subset data in three datasets for majority decision code: unisample tag 1 (new screening)
  labelledunisample1_round2 <- labelled_round2 %>%       
    subset(tag__title == "Universal sample") 
  labelledunisample1_round2 <- labelledunisample1_round2 %>%
    filter(user__username %in% (labelledunisample1_round2 %>% 
                                  group_by(user__username) %>% 
                                  summarise(count = n()) %>% 
                                  filter(count==100))$user__username)

#subset data that can be used for majority decision because they were coded after the modification of the classification scheme  
  labelledunisample1_newlabels <- labelledunisample1_round2 %>%
    subset(!user__username == levels(factor((labelledunisample1_round2$user__username)))[4] | user__username == levels(factor((labelledunisample1_round2$user__username)))[10])

#labelledunisample was originally set to different categories - they are no longer valid for the current scheme so Nina's classifications are used
  labelledunisample1_others <- labelledunisample1_round2 %>% 
    subset(!user__username %in% labelledunisample1_newlabels$user__username) %>%
    mutate(X1_hate5 = ifelse(X1_hate5 ==1,NA,NA)) %>%
    mutate(X1_hate4 = ifelse(X1_hate4 ==1,NA,NA)) %>%
    mutate(X1_hate3 = ifelse(X1_hate3 ==1,NA,NA)) %>%
    mutate(X1_hate2 = ifelse(X1_hate2 ==1,NA,NA)) %>%
    mutate(X1_hate1 = ifelse(X1_hate1 ==1,1,NA)) %>%
    mutate(X1_dontknow = ifelse(X1_dontknow ==1,1,NA)) %>%
    mutate(hatescale = ifelse(hatescale == 5,NA, 
                       ifelse(hatescale == 4,NA,
                       ifelse(hatescale == 3,NA, 
                       ifelse(hatescale == 2,NA, 
                       ifelse(hatescale == 1,1,NA)))))) %>%
    mutate(hatescale6 = ifelse(hatescale6 ==5,NA, 
                       ifelse(hatescale6 == 4,NA,
                       ifelse(hatescale6 == 3,NA, 
                       ifelse(hatescale6 == 2,NA, 
                       ifelse(hatescale6 == 1,1,
                       ifelse(hatescale6 == 0,0,NA))))))) %>%
    mutate(X2_environmentalgroup = ifelse(X2_environmentalgroup ==1,1,NA)) %>%
    mutate(X2_gretathunberg = ifelse(X2_gretathunberg == 1,1,NA)) %>%
    mutate(X2_other = ifelse(X2_other == 1,1,NA)) %>%
    mutate(X2_dontknow = ifelse(X2_dontknow == 1,1,NA)) %>%
    mutate(X3_disparagement = ifelse(X3_disparagement == 1,1,NA)) %>%
    mutate(X3_mocking = ifelse(X3_mocking == 1,1,NA)) %>%
    mutate(X3_violence = ifelse(X3_violence == 1,1,NA)) %>%
    mutate(X3_prejudice = ifelse(X3_prejudice == 1,1,NA)) %>%
    mutate(X3_insult = ifelse(X3_insult == 1,1,NA)) %>%
    mutate(X3_other = ifelse(X3_other == 1,1,NA)) %>%
    mutate(X3_dontknow = ifelse(X3_dontknow == 1,1,NA)) %>%
    mutate(X5_explicitlanguage = ifelse(X5_explicitlanguage == 1,1,NA)) %>%
    mutate(X5_implicitlanguage = ifelse(X5_implicitlanguage == 1,1,NA)) %>%
    mutate(X5_dontknow = ifelse(X5_dontknow == 1,1,NA))
  
  labelledunisample1_round2 <- rbind(labelledunisample1_newlabels, labelledunisample1_others)

#save data
  write.csv(labelledunisample2, "Data/tweets_unisample_tag2.csv", row.names = FALSE, quote = TRUE)
  write.csv(labelledunisample1_round2, "Data/tweets_unisample_tag1.csv", 
    row.names = FALSE, quote = TRUE)
  
  tweets_indiv <- labelled_round2 %>%             #tweets that are coded only by one person
    subset(tag__title != "Universal tag 2" & tag__title != "Universal sample")
  write.csv(tweets_indiv, "Data/tweets_indivsample2.csv", row.names = FALSE, quote = TRUE)
  
