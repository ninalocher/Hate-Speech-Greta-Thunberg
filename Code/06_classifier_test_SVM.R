
###classification test SVM

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

#empty dataframes for merging results
  performance_checks <- data.frame(metric = c("Accuracy", 
                                              "Precision_1", "Precision_2", "Precision_3", "Precision_4", "Precision_5", 
                                              "Recall_1", "Recall_2", "Recall_3", "Recall_4", "Recall_5", 
                                              "F1_1" ,"F1_2" ,"F1_3" ,"F1_4" ,"F1_5", "RMSE"))


#programme function for classification for all data
  cat_classifier_svm <- function(category,var,df){
    
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
    
    #tune
    set.seed(124)
    model_tune <- tune.svm(
                       x = train,  
                       y = docvars(train, var), 
                       kernel = "linear", 
                       tunecontrol = tune.control(
                         sampling = "cross", cross = 10, best.model = TRUE, performances = TRUE),
                       gamma = c(0.5,1,2), cost = 10^(-1:2))
    cost <- model_tune$best.parameters$cost
    gamma <- model_tune$best.parameters$gamma
    pred <- predict(model_tune$best.model, newdata = test, decision.values = TRUE)
    
    if(var != "hatescaleM"){
      cv <- confusionMatrix(pred, docvars(test, var), positive = "2", mode = "everything")
      print(cv)
    }else{
      rmse <- data.frame(var = docvars(test, var), pred = pred)
      rmse <- rmse %>%
        mutate(residual = var - pred)  %>% 
        summarize(rmse  = sqrt(mean(residual^2)))
      print(rmse)
    }
    print(summary(model_tune$best.model))
  
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
  cluster <- makeCluster(detectCores())
  registerDoParallel(cluster)
  start.time <- Sys.time()
  
  outcome_hate <- foreach (i = c(42,43,45), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar%  
    (result <- cat_classifier_svm(dfmhashtag,names(tweets_sample)[i], tweets_sample) ) 
             
  print(Sys.time() - start.time)
  stopCluster(cluster)
  
  #left join metrics together 
   for(i in 1:3){
    metric <- outcome_hate[i][[1]]
    performance_checks <- left_join(performance_checks, metric, by="metric")
  }
  colnames(performance_checks)[2:4] <- paste(colnames(performance_checks[2:4]), "dfmhashtag", sep = "_")
  
  
##B: dfmnostop
  #prediction on hatenom and hatescale on all data
  cluster <- makeCluster(detectCores()-1)
  registerDoParallel(cluster)
  start.time <- Sys.time()
  
  #1: CLASSIFIER FOR HATE   
  outcome_hate <- foreach (i = c(42,43,45), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar%  {
    result <- cat_classifier_svm(dfmnostop,names(tweets_sample)[i], tweets_sample)
  }
  
  print(Sys.time() - start.time)
  stopCluster(cluster)

  #left join metrics together 
  for(i in 1:3){
    metric <- outcome_hate[i][[1]]
    performance_checks <- left_join(performance_checks, metric, by="metric")
  }
  colnames(performance_checks)[5:7] <- paste(colnames(performance_checks[5:7]), "dfmnostop", sep = "_")
  
  
##C: dfmstem
  #prediction on hatenom and hatescale on all data
  cluster <- makeCluster(detectCores()-1)
  registerDoParallel(cluster)
  start.time <- Sys.time()
  
  #1: CLASSIFIER FOR HATE   
  outcome_hate <- foreach (i = c(42,43,45), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar%  {
    result <- cat_classifier_svm(dfmstem,names(tweets_sample)[i], tweets_sample)
  }
  
  print(Sys.time() - start.time)
  stopCluster(cluster)
  
  #left join metrics together 
  for(i in 1:3){
    metric <- outcome_hate[i][[1]]
    performance_checks <- left_join(performance_checks, metric, by="metric")
  }
  colnames(performance_checks)[8:10] <- paste(colnames(performance_checks[8:10]), "dfmstem", sep = "_")
  
  
##D: dfmtfidf
  #prediction on hatenom and hatescale on all data
  cluster <- makeCluster(detectCores()-1)
  registerDoParallel(cluster)
  start.time <- Sys.time()
  
  #1: CLASSIFIER FOR HATE   
  outcome_hate <- foreach (i = c(42,43,45), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar%  {
    result <- cat_classifier_svm(dfmtfidf,names(tweets_sample)[i], tweets_sample)
  }
  
  print(Sys.time() - start.time)
  stopCluster(cluster)
  
  #left join metrics together 
  for(i in 1:3){
    metric <- outcome_hate[i][[1]]
    performance_checks <- left_join(performance_checks, metric, by="metric")
  }
  colnames(performance_checks)[11:13] <- paste(colnames(performance_checks[11:13]), "nostop_tfidf", sep = "_")
  saveRDS(performance_checks, "Data/performance_test_SVM.RData")
  