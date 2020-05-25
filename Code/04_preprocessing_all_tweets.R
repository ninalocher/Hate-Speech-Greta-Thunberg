###04: pre-processing

#load packages
  list.of.packages <- c("stringr","dplyr","tidyr", "readtext", "quanteda", "quanteda.corpora", "readr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = TRUE)


#load data
  tweets_all_merged <- read.csv("Data/tweets_all_merged.csv", stringsAsFactors = FALSE, numerals = "no.loss")


#subset data
  tweets_all_merged <- tweets_all_merged %>% 
    filter(is.na(hatenomNA) | hatenomNA == 0 | hatenomNA == 1 & X2_gretathunberg == 1) %>%
    dplyr::select(-hatenom, -hatescale6)
  tweets_all_merged <- tweets_all_merged %>% distinct()
  tweets_all_merged$hatescaleM <- as.numeric(tweets_all_merged$hatescale)
  
#add intersectional categories  
  tweets_all_merged <- tweets_all_merged %>%
    mutate(X4_manip_age = ifelse(X4_manipulation == "1" & X4_age == "1", "1", "0")) %>%
    mutate(X4_manip_cc = ifelse(X4_manipulation == "1" & X4_climatechange == "1", "1", "0")) %>%
    mutate(X4_mental_age = ifelse(X4_mentalphysicalcapacity == "1" & X4_age == "1", "1", "0")) %>%
    mutate(X4_cc_age = ifelse(X4_climatechange == "1" & X4_age == "1", "1", "0")) %>%
    mutate(X4_manip_mental = ifelse(X4_manipulation == "1" & X4_mentalphysicalcapacity == "1", "1", "0")) %>%
    mutate(X4_manip_cc_age = ifelse(X4_manipulation == "1" & X4_climatechange == "1" & X4_age == "1", "1", "0")) %>%
    mutate(X4_manip_mental_age = ifelse(X4_manipulation == "1" & X4_mentalphysicalcapacity == "1" & X4_age == "1", "1", "0")) %>%
    mutate(X4_gender_age = ifelse(X4_gender == "1" & X4_age == "1", "1", "0")) %>%
    mutate(X4_manip_class = ifelse(X4_manipulation == "1" & X4_class == "1", "1", "0")) %>%
    mutate(X4_mental_cc = ifelse(X4_mentalphysicalcapacity == "1" & X4_climatechange == "1", "1", "0")) %>%
    mutate(X4_gender_manip_age = ifelse(X4_gender == "1" & X4_manipulation == "1" & X4_age == "1", "1", "0")) %>%
    mutate(X4_gender_manip = ifelse(X4_gender == "1" & X4_manipulation == "1", "1", "0")) %>%
    mutate(X4_class_age = ifelse(X4_class == "1" & X4_age == "1", "1", "0"))
  tweets_all_merged[13:43] <- lapply(tweets_all_merged[13:43], factor) 
  tweets_all_merged[46:58] <- lapply(tweets_all_merged[46:58], factor)
    #all intersectional categories with a frequency of at least 40 times
  
#stringr package - text preprocessing
  tweets_all_merged$tweet__text <- tweets_all_merged$tweet__text %>%
    str_replace_all("@[:graph:]*","") %>%
    str_replace_all("@[:graph:]*","") %>%
    str_replace_all("<[:graph:]*>", " ") %>%
    str_replace_all("â€™", "'")  %>%
    str_replace_all("ð|Ÿ|€|™|â|¤|£|˜|ª|ï", "")
  tweets_all_merged$tweet__text <- gsub(
    "(s?)(f|ht)tp(s?)://\\S+\\b", " ", tweets_all_merged$tweet__text)
  tweets_all_merged$tweet__text <- gsub(
    "pic.twitter.com/\\S+\\b", " ", tweets_all_merged$tweet__text)
  tweets_all_merged$tweet__text <- str_replace_all(tweets_all_merged$tweet__text,"\\."," ")
  tweets_all_merged$tweet__text <- str_replace_all(tweets_all_merged$tweet__text,"\\,"," ")


#create corpus
  corptwitter_all <- corpus(tweets_all_merged %>% dplyr::select(tweet__text, tweet__id),
                                   docid_field = "tweet__id", 
                                   text_field = "tweet__text", 
                                   metacorpus = NULL, compress = FALSE)
 
#pre-processing tokens
  toksnostop_all <- 
    tokens(corptwitter_all, 
                          remove_url = TRUE, 
                          remove_numbers = TRUE, 
                          remove_punct = TRUE,                      
                          remove_symbols = TRUE) %>%
    tokens_tolower() %>%
    tokens_select(pattern = stopwords('en'), min_nchar = 2,
                              selection = 'remove')
    
#create dfm
   dfmnostop_all <- dfm(toksnostop_all) %>%
     dfm_trim(min_termfreq = 2, min_docfreq = 2) %>%
     dfm_select(pattern = c("", "amp"), selection = "remove")

#save data
   saveRDS(tweets_all_merged, "Data/tweets_all_merged_preprocessed.RData")
   saveRDS(dfmnostop_all, "Data/dfmnostop_all.RDS")