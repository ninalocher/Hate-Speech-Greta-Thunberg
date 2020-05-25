#load packages
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  list.of.packages <- c("dplyr","tidyverse")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  invisible(lapply(list.of.packages, require, character.only = TRUE))
  

#load data
  tweets_indiv <- read.csv("Data/tweets_indivsample2.csv", stringsAsFactors = FALSE, numerals = "no.loss")
  tweets_unisample_tag2 <- read.csv("Data/tweets_unisample_tag2.csv", stringsAsFactors = FALSE, numerals = "no.loss")
  tweets_unisample_tag1 <- read.csv("Data/tweets_unisample_tag1.csv", stringsAsFactors = FALSE, numerals = "no.loss")
  datacoders <- read.csv("Data/registration_coders.csv", stringsAsFactors = FALSE)
  dfrelevant <- read.csv("Data/tweets_en_082018_311219_idchar_relvar.csv", stringsAsFactors = FALSE, numerals = "no.loss")
  

#merge coder information with tweet classification
  datacoders <- datacoders %>%
    dplyr::select(-Zeitstempel, -Your.Full.Name) %>%
    rename(user__username=Your.Email.Address, 
           gender_rater=What.is.your.gender., 
           native=Is.English.your.native.language.)
  datacoders$native <- factor(datacoders$native)
  levels(datacoders$native) <- c("nonnative", "native")
  datacoders <- datacoders %>%
    mutate(demographic = ifelse(native == "native" & gender_rater == "Female","nativefemale",
                         ifelse(native == "native" & gender_rater == "Male", "nativemale",
                         ifelse(native == "nonnative" & gender_rater == "Female", "nonnativefemale",
                         ifelse(native == "nonnative" & gender_rater == "Male", "nonnativemale",NA)))))
  
  
  tweets_unisample_tag2_coders <- left_join(tweets_unisample_tag2, datacoders, by = "user__username")
  tweets_unisample_tag1_coders <- left_join(tweets_unisample_tag1, datacoders, by = "user__username")

#withdraw notes, ids and text
  notes <- tweets_unisample_tag2_coders %>%     #notes from second unisample
    dplyr::select(tweet__id, note__text, tweet__text) %>%
    dplyr::filter(note__text != "")
  notes <- rbind(notes,                         #notes from first unisample
             tweets_unisample_tag1_coders %>%
               dplyr::select(tweet__id, note__text, tweet__text) %>%
               dplyr::filter(note__text != ""))
  notes <- rbind(notes,                       #notes from individual sample
             tweets_indiv %>%
               dplyr::select(tweet__id, note__text, tweet__text) %>%
               dplyr::filter(note__text != ""))
  tweets_indiv <- tweets_indiv %>%
    dplyr::select(-note__text)


#median majority vote for all numeric categories in unisample tag2
  ###separate gender
    unisample_majority_gender_sep <- tweets_unisample_tag2_coders %>% 
      dplyr::select(-X,-tweet__text,-user__username,-tag__title,-note__text,-native, -demographic) %>% 
      group_by(tweet__id,gender_rater) %>%
      summarize_if(is.numeric,median,na.rm=TRUE) %>% 
      arrange(tweet__id)
    unisample_majority_gender_sep[,-c(1,2)] <- round(unisample_majority_gender_sep[,-c(1,2)],0)
    
  ###separate native
    unisample_majority_native_sep <- tweets_unisample_tag2_coders %>% 
      dplyr::select(-X,-tweet__text,-user__username,-tag__title,-note__text, -gender_rater, -demographic) %>% 
      group_by(tweet__id,native) %>%
      summarize_if(is.numeric,median,na.rm=TRUE) %>% 
      arrange(tweet__id)
    unisample_majority_native_sep[,-c(1,2)] <-round(unisample_majority_native_sep[,-c(1,2)],0)
  
  #separate all demographics
    unisample_majority_demo_sep <- tweets_unisample_tag2_coders %>% 
      dplyr::select(-X,-tweet__text,-user__username,-tag__title,-note__text, -gender_rater, -native) %>% 
      group_by(tweet__id,demographic) %>%
      summarize_if(is.numeric,median,na.rm=TRUE) %>% 
      arrange(tweet__id)
    unisample_majority_demo_sep[,-c(1,2)] <-round(unisample_majority_demo_sep[,-c(1,2)],0)

  #results
    table(unisample_majority_gender_sep$gender_rater, unisample_majority_gender_sep$hatescale, exclude=NULL)
    table(unisample_majority_native_sep$native, unisample_majority_native_sep$hatescale, exclude=NULL)
    table(unisample_majority_demo_sep$demographic, unisample_majority_demo_sep$hatescale, exclude=NULL)
    table(unisample_majority_demo_sep$demographic, unisample_majority_demo_sep$hatenomNA, exclude=NULL)
    table(unisample_majority_gender_sep$gender_rater, unisample_majority_gender_sep$hatenomNA, exclude=NULL)
    table(unisample_majority_native_sep$native, unisample_majority_native_sep$hatenomNA, exclude=NULL)

  #average score  
    unisample_majority_tag2 <- tweets_unisample_tag2_coders %>%
      dplyr::select(-X,-tweet__text,-user__username,-tag__title,-note__text,-native,
                    -gender_rater, -demographic) %>% 
      group_by(tweet__id)%>%
      summarize_if(is.numeric,median,na.rm=TRUE) %>% 
      arrange(tweet__id)
    unisample_majority_tag2[,-c(1,2)] <-round(unisample_majority_tag2[,-c(1,2)],0)
    table(unisample_majority_tag2$hatescale, exclude=NULL)


##majority vote for unisample tag1
  ##separate gender
    unisample_majority_tag1_gender_sep <- tweets_unisample_tag1_coders %>% 
      dplyr::select(-X,-tweet__text,-user__username,-tag__title,-note__text,-native, -demographic) %>% 
      group_by(tweet__id,gender_rater) %>%
      summarize_if(is.numeric,median,na.rm=TRUE) %>% 
      arrange(tweet__id)
    unisample_majority_tag1_gender_sep[,-c(1,2)] <-round(unisample_majority_tag1_gender_sep[,-c(1,2)],0)
    
  ###separate native
    unisample_majority_tag1_native_sep <- tweets_unisample_tag1_coders %>% 
      dplyr::select(-X,-tweet__text,-user__username,-tag__title,-note__text, -gender_rater, -demographic) %>% 
      group_by(tweet__id,native) %>%
      summarize_if(is.numeric,median,na.rm=TRUE) %>% 
      arrange(tweet__id)
    unisample_majority_tag1_native_sep[,-c(1,2)] <-round(unisample_majority_tag1_native_sep[,-c(1,2)],0)
  
  #separate all demographics
    unisample_majority_tag1_demo_sep <- tweets_unisample_tag1_coders %>% 
      dplyr::select(-X,-tweet__text,-user__username,-tag__title,-note__text, -gender_rater, -native) %>% 
      group_by(tweet__id,demographic) %>%
      summarize_if(is.numeric,median,na.rm=TRUE) %>% 
      arrange(tweet__id)
    unisample_majority_tag1_demo_sep[,-c(1,2)] <-round(unisample_majority_tag1_demo_sep[,-c(1,2)],0)
    
  #results
    table(unisample_majority_tag1_gender_sep$gender_rater, 
          unisample_majority_tag1_gender_sep$hatenomNA, exclude=NULL)
    table(unisample_majority_tag1_native_sep$native, unisample_majority_tag1_native_sep$hatenomNA,
          exclude=NULL)
    table(unisample_majority_tag1_demo_sep$demographic, unisample_majority_tag1_demo_sep$hatenomNA, exclude=NULL)
    
  #majority vote
    unisample_majority_tag1 <- tweets_unisample_tag1_coders %>%
      dplyr::select(-X,-tweet__text,-user__username,-tag__title,-note__text,
                    -native,-gender_rater, -demographic) %>% 
      group_by(tweet__id)%>%
      summarize_if(is.numeric,median,na.rm=TRUE) %>% 
      arrange(tweet__id)
    unisample_majority_tag1[,-c(1,2)] <-round(unisample_majority_tag1[,-c(1,2)],0)
    table(unisample_majority_tag1$hatenomNA, exclude=NULL)

    
    
#merge unisample and indiv sample back
  #merge unisample majority with unisample to add text and tag title
    unisample_majority_tag1 <- left_join(unisample_majority_tag1, 
                                              (tweets_unisample_tag1 %>%
                                                dplyr::filter(user__username == levels(factor(tweets_unisample_tag1$user__username))[10]) %>%
                                                dplyr::select(tweet__id, tweet__text, tag__title)), 
                                              by = ("tweet__id"))
    
    unisample_majority_tag2 <- left_join(unisample_majority_tag2, 
                                              tweets_unisample_tag2 %>%
                                                dplyr::filter(user__username == levels(factor(tweets_unisample_tag2$user__username))[10]) %>%
                                                dplyr::select(tweet__id, tweet__text, tag__title), 
                                              by = ("tweet__id"))
  
  #merge majority unisample with indivsample
    tweets_classified <- rbind(unisample_majority_tag1,
                               unisample_majority_tag2,
                               tweets_indiv %>%
                                 dplyr::select(-user__username, -X))
  
  #gets rid of duplicates because it contains retweets
    tweets_classified <- tweets_classified %>%
      filter(tag__title != "2020-01-15 12:48") %>%
      dplyr::select(-relevant)
    tweets_classified$hatenom <- tweets_classified$hatenom %>% replace_na(99)
    tweets_classified$hatenomNA <- tweets_classified$hatenomNA %>% replace_na(99)
    tweets_classified$hatescale <- tweets_classified$hatescale %>% replace_na(99)
    tweets_classified$hatescale6 <- tweets_classified$hatescale6 %>% replace_na(99)
    
  #merge with twitter data
    dfrelevant <- dfrelevant %>%
      dplyr::rename(tweet__id = id, 
              tweet__text = text) %>%
      dplyr::select(-tag, -favorites_users, -contributors, -retweeted_status, -docusercat, -docownership, 
             -note, -source_url, -retweeted, -truncated, -favorited)
  
  #merge twitter scraping data with labelled classification data
    tweets_all_merged <- full_join(dfrelevant, 
                                   tweets_classified, 
                                   by = "tweet__id")
    tweets_all_merged <- tweets_all_merged %>%
      rename(tweet__text = tweet__text.x) %>%
      dplyr::select(-tweet__text.y)  %>%
      distinct()
    tweets_unlabelled_merged <- tweets_all_merged[is.na(tweets_all_merged$tag__title),]
    tweets_classified_merged <- tweets_all_merged[!is.na(tweets_all_merged$tag__title),]
    ##note: as symbols and smileys were coded differently in the web-scraped twitter data and the classified twitter data 
    ##from the APSIS scoping platform, it is only merged by tweet__id. A manual investigation double-checked the sameness of the twitter text. 

  write.csv(tweets_classified_merged, "Data/tweets_classified_merged.csv", row.names = FALSE, quote = TRUE)
  write.csv(tweets_all_merged, "Data/tweets_all_merged.csv", row.names = FALSE, quote = TRUE)
  write.csv(tweets_unlabelled_merged, "Data/tweets_unlabelled_merged.csv", row.names = FALSE, quote = TRUE)
  write.csv(notes, "Data/tweet_notes.csv", row.names = FALSE, quote = TRUE)
