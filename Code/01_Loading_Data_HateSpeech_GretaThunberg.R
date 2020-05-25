setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

list.of.packages <- c("doParallel","textcat","xlsx","dplyr","jsonlite","reshape2","splitstackshape","readr","lubridate","stringr","irr","tidyr","epiR","gridExtra","ggplot2","quanteda")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = TRUE))




batch_clean <- function(path){
  #load dataset
  ##transform text as string
  twitter_df <- jsonlite::fromJSON(path, flatten=TRUE, bigint_as_char = TRUE)
  twitter_df$text <- as.character(twitter_df$text)
  
  #language classifier with textcat package
  twitter_df <- subset(twitter_df, lang == "en" | lang == "und" | is.na(twitter_df$lang)) #this includes  English twitter_df recognized by twitter
  twitter_df$textlang <- textcat(twitter_df$text)
  
  #subset dataset to English twitter_df only
  twitter_df <- subset(twitter_df, lang == "en" | lang == "und" | textlang == "english" & is.na(twitter_df$lang))
  
  #remove retwitter_df = twitter_df that start with "RT"
  twitter_df <- twitter_df %>%
    filter(!str_detect(text, "^RT /*")) %>% 
    dplyr::filter(is.na(retweeted_status))
  
  #save small dataset
  twitter_df <- subset(twitter_df, select = c(-place, -retweeted_by_user_id, -entities.urls, -entities.symbols, -entities.hashtags, -entities.user_mentions, -entities.media))
  twitter_df <- twitter_df %>% select(-matches("geo"),-matches("coordinates"))
  return(twitter_df)
}

## Perform Batch cleaning and language detection for each batch, and bind them to one dataframe

twitter_df <- data.frame()
files <- list.files("../@GretaThunberg/")[str_detect(list.files("../@GretaThunberg/"),"/*_tweets.json")]

#files <- c("19_tweets.json","14_tweets.json")

for (i in 1:length(files)){
  
  path <-paste("../@GretaThunberg/",files[i],sep="")
  
  if(length(twitter_df)==0) twitter_df <- batch_clean(path) 
  else twitter_df <- twitter_df %>% bind_rows(batch_clean(path))
}

#create date and restrict to 31-12-19
twitter_df <- twitter_df %>%
  mutate(created_at_date = ymd_hms(created_at),
         date = date(created_at_date)) %>% 
  filter(date < "2020-01-01") %>%
  select(-textlang, -lang)


dir.create(file.path(getwd(), "Data/"), showWarnings = FALSE)
write.csv(twitter_df, "Data/twitter_df_en_082018_311219_idchar_relvar.csv", row.names = FALSE, quote = TRUE)





  

  

