###Pre-processing for classification tests

#load packages
  list.of.packages <- c("stringr","dplyr","tidyr", "readtext", "quanteda", "quanteda.corpora", "readr",
  "splitstackshape")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = TRUE)


#load data: restrict classification tests to a number of 2600 labelled tweets
tweets_all_merged <- readRDS("Data/tweets_all_merged_preprocessed.RData")
tweets_sample <- tweets_all_merged %>% filter(!is.na(tag__title))
tweets_sample <- stratified(tweets_sample, 0.3, group = "date", replace = FALSE)



#create corpus
  corptwitter <- corpus(tweets_sample %>% dplyr::select(tweet__text, tweet__id),
                                   docid_field = "tweet__id", 
                                   text_field = "tweet__text", 
                                   metacorpus = NULL, compress = FALSE)
 
#pre-processing tokens dataset A
  tokshashtag <- tokens(corptwitter, 
                          remove_url = TRUE, 
                          remove_numbers = TRUE, 
                          remove_punct = TRUE,                     , 
                          remove_symbols = TRUE)
  tokshashtag <- tokens_tolower(tokshashtag)

#toksnostop dataset B
  toksnostop <- tokens_select(tokshashtag, 
                              pattern = stopwords('en'), min_nchar = 2,
                              selection = 'remove')

#stemmed tokens dataset C
  toksstem <- tokens_wordstem(toksnostop)

#create dfm for all four datasets
   dfmhashtag <- dfm(tokshashtag) %>%
     dfm_trim(min_termfreq = 2, min_docfreq = 2) %>%
     dfm_select(pattern = c("", "amp"), selection = "remove")
   dfmnostop <- dfm(toksnostop) %>%
     dfm_trim(min_termfreq = 2, min_docfreq = 2) %>%
     dfm_select(pattern = c("", "amp"), selection = "remove")
   dfmstem <- dfm(toksstem)  %>%
     dfm_trim(min_termfreq = 2, min_docfreq = 2)
   dfmtfidf <-  dfm_tfidf(dfmnostop) %>% round(digits = 2)


#save data
   saveRDS(dfmhashtag, "Data/dfmhashtag.RDS")
   saveRDS(dfmnostop, "Data/dfmnostop.RDS")
   saveRDS(dfmstem, "Data/dfmstem.RDS")
   saveRDS(dfmtfidf, "Data/dfmtfidf.RDS")
   saveRDS(tweets_sample, "Data/tweets_sample.RData")