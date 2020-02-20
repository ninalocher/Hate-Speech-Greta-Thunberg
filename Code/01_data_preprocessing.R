#R.version.string
#install.packages("installr") 
#updateR()
#library(installr)
#install.packages("textcat")
library(textcat)
#install.packages("dplyr")
library(dplyr)
library(jsonlite)
#install.packages("reshape2")
library(reshape2)
#install.packages("splitstackshape")
library(splitstackshape)
#install.packages("readr")
library(readr)
#install.packages("lubridate")
require(lubridate)
#install.packages("stringr")
library(stringr)

batch_clean <- function(path){
  #load dataset
  ##transform text as string
  tweets <- jsonlite::fromJSON(path, flatten=TRUE, bigint_as_char = TRUE)
  tweets$text <- as.character(tweets$text)
  
  #language classifier with textcat package
  tweets_lang_en <- subset(tweets, lang == "en" | lang == "und" | is.na(tweets$lang)) #this includes  English tweets recognized by twitter
  tweets_lang_en$textlang <- textcat(tweets_lang_en$text)
  
  #subset dataset to English tweets only
  tweets_en_all <- subset(tweets_lang_en, lang == "en" | lang == "und" | textlang == "english" & is.na(tweets_lang_en$lang))
  
  #remove retweets = tweets that start with "RT"
  clean_tweets <- tweets_en_all %>%
    filter(!str_detect(text, "^RT /*")) %>% 
    dplyr::filter(is.na(retweeted_status))
  
  #save small dataset
  tweets_processed <- subset(clean_tweets, select = c(text, id, created_at))
  return(tweets_processed)
}

# Get a list of files and run our pipeline on them
files <- list.files(path="/usr/local/apsis/slowhome/galm/exports/@GretaThunberg", pattern="_tweets.json", full.names=TRUE, recursive=FALSE)

# do.call flattens a list of lists
ids <- do.call(c, lapply(files, function(x) {
  print(x)
  GT_tweets <- batch_clean(path = x)
  return(GT_tweets$id)
}))

write.table(ids,"/usr/local/apsis/slowhome/galm/exports/@GretaThunberg/ids.csv",row.names = F)


