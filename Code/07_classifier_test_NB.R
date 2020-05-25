###classification test NB

#load packages
list.of.packages <- c("tidytext","dplyr","tidyr", "readtext", "quanteda", "quanteda.corpora", "kernlab",
                      "caret", "vecsets", "doParallel", "vip", "rsample", "foreach", "klaR", "e1071", 
                      "quanteda.textmodels", "naivebayes", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


#load data
dfmhashtag <- readRDS("Data/dfmhashtag.RDS")
dfmnostop <- readRDS("Data/dfmnostop.RDS")
dfmstem <- readRDS("Data/dfmstem.RDS")
dfmtfidf <- readRDS("Data/dfmtfidf.RDS")
tweets_sample <- readRDS("Data/tweets_sample.RData")


#prep
  Accuracy <- c()
  Precision_1 <- c()
  Precision_2 <- c() 
  Precision_3 <- c() 
  Precision_4 <- c() 
  Precision_5 <- c() 
  Recall_1 <- c()
  Recall_2 <- c()
  Recall_3 <- c()
  Recall_4 <- c()
  Recall_5 <- c()
  F1_1 <- c()
  F1_2 <- c()
  F1_3 <- c()
  F1_4 <- c()
  F1_5 <- c()

#programme function for classification for all data
cat_classifier_nb <- function(category,var,df,k){
  
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
  train <- dfm_trim(train, min_termfreq = 1)
  feat <- featnames(train)
  test <- dfm_match(test, feat)
  if(var != "hatescale" & var != "hatescaleM"){
    levels(docvars(test, var)) <- c("1", "2")
  }
  
  train <- train[sample(ndoc(train)),]  
  folds <- createFolds(docvars(train, var), k = k, list = TRUE, returnTrain = TRUE)
  
  Accuracy <- c()
  Precision_1 <- c()
  Precision_2 <- c() 
  Precision_3 <- c() 
  Precision_4 <- c() 
  Precision_5 <- c() 
  Recall_1 <- c()
  Recall_2 <- c()
  Recall_3 <- c()
  Recall_4 <- c()
  Recall_5 <- c()
  F1_1 <- c()
  F1_2 <- c()
  F1_3 <- c()
  F1_4 <- c()
  F1_5 <- c()
  
  #tune
   for (i in 1:k) {
      #train model
     set.seed(124)
     model_tune <- textmodel_nb(
        x = train[folds[[1]],], 
        y = docvars(train, var)[folds[[1]]], prior = "uniform")
     pred <- predict(object = model_tune, newdata = test)
    
    if(var == "hatenomNA"){ 
     Accuracy <- c(Accuracy, confusionMatrix(pred, (docvars(test, var)), positive = "2", mode = "everything")$overall[[1]])
     Precision_2 <- c(Precision_2, confusionMatrix(pred, docvars(test, var), positive = "2", mode = "everything")$byClass[[5]])              
     Recall_2 <- c(Recall_2, confusionMatrix(pred, docvars(test, var), positive = "2", mode = "everything")$byClass[[6]])
     F1_2 <- c(F1_2, confusionMatrix(pred, docvars(test, var), positive = "2", mode = "everything")$byClass[[7]])
     mean_Accuracy <- mean(Accuracy)
     mean_Precision <- mean(Precision_2)
     mean_Recall <- mean(Recall_2)
     mean_F1 <- mean(F1_2)
    }
    
     else if(var == "hatescale"){
       Accuracy <- c(Accuracy, confusionMatrix(pred, (docvars(test, var)), mode = "everything")$overall[[1]])
       Precision_1 <- c(Precision_1, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[21]])
       Precision_2 <- c(Precision_2, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[22]]) 
       Precision_3 <- c(Precision_3, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[23]]) 
       Precision_4 <- c(Precision_4, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[24]]) 
       Precision_5 <- c(Precision_5, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[25]]) 
       Recall_1 <- c(Recall_1, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[26]])
       Recall_2 <- c(Recall_2, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[27]])
       Recall_3 <- c(Recall_3, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[28]])
       Recall_4 <- c(Recall_4, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[29]])
       Recall_5 <- c(Recall_5, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[30]])
       F1_1 <- c(F1_1, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[31]])
       F1_2 <- c(F1_2, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[32]])
       F1_3 <- c(F1_3, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[33]])
       F1_4 <- c(F1_4, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[34]])
       F1_5 <- c(F1_5, confusionMatrix(pred, docvars(test, var), mode = "everything")$byClass[[35]])
       mean_Precision_1 <- mean(Precision_1)
       mean_Precision_2 <- mean(Precision_2)
       mean_Precision_3 <- mean(Precision_3)
       mean_Precision_4 <- mean(Precision_4)
       mean_Precision_5 <- mean(Precision_5)
       mean_Recall_1 <- mean(Recall_1)
       mean_Recall_2 <- mean(Recall_2)
       mean_Recall_3 <- mean(Recall_3)
       mean_Recall_4 <- mean(Recall_4)
       mean_Recall_5 <- mean(Recall_5)
       mean_F1_1 <- mean(F1_1)
       mean_F1_2 <- mean(F1_2)
       mean_F1_3 <- mean(F1_3)
       mean_F1_4 <- mean(F1_4)
       mean_F1_5 <- mean(F1_5)
       mean_Accuracy <- mean(Accuracy)
  }}


  #dataframe
  if(var == "hatenomNA"){
    metric <- data.frame(Accuracy = mean_Accuracy, 
                         Precision_1 = NA,
                         Precision_2 = mean_Precision,
                         Precision_3 = NA,
                         Precision_4 = NA, 
                         Precision_5 = NA,
                         Recall_1 = NA, 
                         Recall_2 = mean_Recall, 
                         Recall_3 = NA, 
                         Recall_4 = NA, 
                         Recall_5 = NA, 
                         F1_1 = NA, 
                         F1_2 = mean_F1,
                         F1_3 = NA, 
                         F1_4 = NA, 
                         F1_5 = NA, 
                         RMSE = NA,
                         row.names = var)
  }else if(var == "hatescale"){
    metric <- data.frame(Accuracy = mean_Accuracy, 
                         Precision_1 = mean_Precision_1,
                         Precision_2 = mean_Precision_2,
                         Precision_3 = mean_Precision_3,
                         Precision_4 = mean_Precision_4, 
                         Precision_5 = mean_Precision_5,
                         Recall_1 = mean_Recall_1, 
                         Recall_2 = mean_Recall_2, 
                         Recall_3 = mean_Recall_3, 
                         Recall_4 = mean_Recall_4, 
                         Recall_5 = mean_Recall_5, 
                         F1_1 = mean_F1_1, 
                         F1_2 = mean_F1_2,
                         F1_3 = mean_F1_3, 
                         F1_4 = mean_F1_4, 
                         F1_5 = mean_F1_5, 
                         RMSE = NA,
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
    
#1: CLASSIFIER FOR HATE   
    outcome_hate <- foreach (i = c(42,43), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip", "quanteda.textmodels")) %dopar%  {
      result <- cat_classifier_nb(dfmhashtag,names(tweets_sample)[i], tweets_sample,10)
    }
               
    print(Sys.time() - start.time)
    stopCluster(cluster)
    
  #empty dataframes for merging results
    performance_checks_nb <- data.frame(metric = c("Accuracy", 
                                                "Precision_1", "Precision_2", "Precision_3", "Precision_4", "Precision_5", 
                                                "Recall_1", "Recall_2", "Recall_3", "Recall_4", "Recall_5", 
                                                "F1_1" ,"F1_2" ,"F1_3" ,"F1_4" ,"F1_5", "RMSE"))
    
    
    #left join all predids and metrics together 
    for(i in 1:2){
      metric <- outcome_hate[i][[1]]
      performance_checks_nb <- left_join(performance_checks_nb, metric, by="metric")
    }
    colnames(performance_checks_nb)[2:3] <- paste(colnames(performance_checks_nb[2:3]), "dfmhashtag", sep = "_")
  
  
##B: dfmnostop
  #prediction on hatenom and hatescale on all data
    cluster <- makeCluster(detectCores()-1)
    registerDoParallel(cluster)
    start.time <- Sys.time()
  
  #1: CLASSIFIER FOR HATE   
    outcome_hate <- foreach (i = c(42,43), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip", "quanteda.textmodels")) %dopar%  {
      result <- cat_classifier_nb(dfmnostop,names(tweets_sample)[i], tweets_sample,10)
    }
    
    print(Sys.time() - start.time)
    stopCluster(cluster)
    
  
  #left join all predids and metrics together 
    for(i in 1:2){
      metric <- outcome_hate[i][[1]]
      performance_checks_nb <- left_join(performance_checks_nb, metric, by="metric")
    }
    colnames(performance_checks_nb)[4:5] <- paste(colnames(performance_checks_nb[4:5]), "dfmnostop", sep = "_")
  
  
##C: dfmstem
  #prediction on hatenom and hatescale on all data
    cluster <- makeCluster(detectCores()-1)
    registerDoParallel(cluster)
    start.time <- Sys.time()
  
  #1: CLASSIFIER FOR HATE   
    outcome_hate <- foreach (i = c(42,43), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip", "quanteda.textmodels")) %dopar%  {
      result <- cat_classifier_nb(dfmstem,names(tweets_sample)[i], tweets_sample,10)
    }
    
    print(Sys.time() - start.time)
    stopCluster(cluster)
    
  
  #left join all predids and metrics together 
    for(i in 1:2){
      metric <- outcome_hate[i][[1]]
      performance_checks_nb <- left_join(performance_checks_nb, metric, by="metric")
    }
    colnames(performance_checks_nb)[6:7] <- paste(colnames(performance_checks_nb[6:7]), "dfmstem", sep = "_")
  
##D: tfidf nostop
  #prediction on hatenom and hatescale on all data
    cluster <- makeCluster(detectCores()-1)
    registerDoParallel(cluster)
    start.time <- Sys.time()
    
  #1: CLASSIFIER FOR HATE   
    outcome_hate <- foreach (i = c(42,43), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip", "quanteda.textmodels")) %dopar%  {
      result <- cat_classifier_nb(dfmtfidf,names(tweets_sample)[i], tweets_sample,10)
    }
    
    print(Sys.time() - start.time)
    stopCluster(cluster)
    
  
  #left join all predids and metrics together 
    for(i in 1:2){
      metric <- outcome_hate[i][[1]]
      performance_checks_nb <- left_join(performance_checks_nb, metric, by="metric")
    }
    colnames(performance_checks_nb)[8:9] <- paste(colnames(performance_checks_nb[8:9]), "nostop_tfidf", sep = "_")
  
  #save data
    saveRDS(performance_checks_nb, "Data/performance_checks_nb.RData")
  