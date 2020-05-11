require(caret)
require(dplyr)
require(stringr)
require(tidyr)
#require(readtext)
require(quanteda)
require(quanteda.textmodels)
#install.packages("readtext")
#install.packages("quanteda")
#install.packages("quanteda.textmodels")
#install.packages("devtools")
#devtools::install_github("quanteda/quanteda.corpora")
require(quanteda.corpora)
require(tm)
require(tidytext)
require(e1071)
require(klaR)
#install.packages("caret")
#install.packages("naivebayes")
require(naivebayes)
#install.packages("h2o")
#require(h2o)
#install.packages("kernlab")
require(kernlab)
#install.packages("vecsets")
require(vecsets)
#install.packages("doParallel")

library("Rmpi", quietly = TRUE)
library("doMPI", quietly = TRUE)

#install.packages("vip")
require(vip)
#install.packages("rsample")
require(rsample)

options(error=quote(assign(".mpi.err", FALSE, env = .GlobalEnv)))

#read data for classifier
tweets_all_merged <- readRDS("Data/tweets_all_merged.RData")
dfm <- readRDS("Data/dfmnostop_all.RDS")
dfm <- dfm_trim(dfm, min_docfreq = 5)

load("~/ownCloud/twitter/@GretaThunberg/Data/Output/outcome_hate.RData")

#programme function for classification for all data
cat_classifier <- function(category,var,df, do_varimp=FALSE, sample=FALSE){
  
  #split in labelled (for CV) and unlabelled (for prediction)
  docvars(category, var) <- df %>% dplyr::select(var)  
  labelled <- category %>% 
    dfm_subset((docvars(category, var)) == "0"| (docvars(category, var)) == "1" | (docvars(category, var)) == "2" | (docvars(category, var)) == "3" | 
                 (docvars(category, var)) == "4" | (docvars(category, var)) == "5")
  
  if (sample==TRUE) {
    labelled <- labelled[1:200,]
  }
  
  split <- sample(c("train", "test"), size=ndoc(labelled), prob=c(0.80, 0.20), replace=TRUE)
  train <- which(split=="train") 
  test <- which(split=="test")
  train <- labelled[train,]
  test <- labelled[test,]
  
  train <- dfm_trim(train, min_termfreq = 1)
  
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
  
  train <- balanced_dfm
  print(dim(train))
  feat <- featnames(train)
  test <- dfm_match(test, feat)
  if(var != "hatescale" & var != "hatescaleM"){
    levels(docvars(test, var)) <- c("1", "2")
  }
  
  #tune
  print("cross validating")
  set.seed(124)
  start.time <- Sys.time()
  model_tune <- tune(svm, 
                     train.x = train,  
                     train.y = docvars(train, var), 
                     kernel = "linear", 
                     tunecontrol = tune.control(
                       nrepeat = 1, sampling = "cross", cross = 10, best.model = TRUE, performances = TRUE),
                     ranges = list(gamma = c(0.5,1,2), cost = 10^(-1:2)))
  print(Sys.time() - start.time)
  cost <- model_tune$best.parameters$cost
  gamma <- model_tune$best.parameters$gamma
  pred <- predict(model_tune$best.model, newdata = test)
  
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
  print(summary(model_tune))
  save(model_tune, file=paste("Data/model_tune_svm_",var,".RData",sep=""))
    
  #variable importance
  set.seed(4567)
  if (do_varimp==TRUE) {
    if(var != "hatescaleM"){
      varimp <- vi(model_tune$best.model, method = "permute", train = train, 
                   target = docvars(train, var), metric = "accuracy", 
                   pred_wrapper = predict, parallel=TRUE) %>% head(50)
    } else {
      varimp <- vi(model_tune$best.model, method = "permute", train = train, 
                   target = docvars(train, var), metric = "rmse",  
                   pred_wrapper = predict) %>% head(50)
    }    
  } else {
    varimp <- NULL
  }

  
  
  #predicting unlabelled tweets
  if(var != "hatescaleM"){
    unbalanced <- data.frame(id = docnames(test), 
                             var = as.factor(docvars(test, var)),
                             stringsAsFactors = FALSE)
    balanced_test <- caret::upSample(unbalanced %>% dplyr::select(-var), unbalanced$var,
                                     list=FALSE, yname = var)
    ids <- balanced_test %>% subset(duplicated(balanced_test) == TRUE)
    balanced_test <- rbind(test, test[ids$id,])
    docvars(balanced_test, var) <-  c((docvars(test, var)), (docvars(test[ids$id,], var)))
    labelled <- rbind(train, balanced_test)
    labelled <- dfm_match(labelled, feat)
    docvars(labelled, var) <-  as.factor(c((docvars(train, var)), (docvars(balanced_test, var))))
  } else {
    labelled <- dfm_match(labelled, feat)
  }
  
  set.seed(7890)
  start.time <- Sys.time()
  pred_tune <- svm(
    x = labelled, 
    y = docvars(labelled, var), scale = TRUE,
    kernel = "linear", cost = cost, gamma = gamma)
  write.svm(pred_tune, paste("Data/pred_tune_svm_",var,".file",sep=""),  paste("Data/pred_tune_scale_",var,".file",sep=""))
  print(Sys.time() - start.time)
  
  
  
  unlabelled <- category[is.na(docvars(category, var)),]
  if (sample==TRUE) {
    unlabelled <- unlabelled[1:200,] 
  }     #here, I subset --> this would be the full / stratified data then [1:200,]
  unlabelled <- dfm_match(unlabelled, feat)
  
  if(var != "hatescale" & var != "hatescaleM"){
    levels(docvars(unlabelled, var)) <- c("1", "2")
  }
  
  start.time <- Sys.time()
  pred <- predict(object = pred_tune, newdata = unlabelled)
  print(Sys.time() - start.time)
  print(length(pred))
  
  
  #dataframe
  predids <- data.frame(name = pred, tweet = docnames(unlabelled)) %>% 
    mutate (tweet__id = stringr::str_replace(tweet,"\\.[:digit:]*","")) %>% 
    dplyr::select(name, tweet__id) %>%
    distinct()
  names(predids) <- c(var,"tweet__id")
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
  
  results <- list(predids = predids, metric = metric, varimp = varimp)
  return(results)
}


#empty dataframes for results
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
  if (do_varimp==TRUE) {
    varimp_hate <- list(hatescale = outcome_hate[1][[1]]$varimp, hatenomNA = outcome_hate[2][[1]]$varimp, hatescaleM = outcome_hate[3][[1]]$varimp)
  }
  classified$hatescaleM <- round(classified$hatescaleM, digits = 0)

#data for second classifier
  ids_pred <- classified %>% dplyr::filter(hatenomNA == 2 |hatescale != "1" | hatescaleM > 1)      #select ids as vector with hatenom = 1
  tweets_categories <- tweets_all_merged %>% 
    dplyr::filter(tweet__id %in% (ids_pred$tweet__id) | hatenomNA == 1 & X2_gretathunberg == 1) 
  dfm_categories <- dfm[tweets_categories$tweet__id,] %>% 
    dfm_trim(min_termfreq = 2, min_docfreq = 2)
  
##prediction on categories only for predicted hatenom
  cluster <- startMPIcluster()
  registerDoMPI(cluster)
  start.time <- Sys.time()
  
  
#2: CLASSIFIER FOR CATEGORIES  
  outcome_cat <- foreach (i = 23:41, .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar% {   
    result <- cat_classifier(dfm_categories, names(tweets_categories[i]), tweets_categories)
  }

  print(Sys.time() - start.time)
  
  save(outcome_cat, file="Data/outcome_cat.RData")
  
  closeCluster(cluster) 
  mpi.quit()
  
  

#left join all predids and metrics together for categories
  for(i in 1:19){
    predids <- outcome_cat[i][[1]]$predids
    metric <- outcome_cat[i][[1]]$metric
    classified <- left_join(classified, predids, by="tweet__id")
    performance <- left_join(performance, metric, by="metric")
  }
  
  
