
### final classification for hate and all subcategories of hate

#load packages
  list.of.packages <- c("tidytext","dplyr","tidyr", "readtext", "quanteda", "quanteda.corpora", "kernlab",
                        "caret", "vecsets", "doParallel", "vip", "rsample", "foreach", "klaR", "e1071",  
                        "quanteda.textmodels", "naivebayes", "stringr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = TRUE)


#load data
  tweets_all_merged <- readRDS("Data/tweets_all_merged_preprocessed.RData")
  dfm <- readRDS("Data/dfmnostop_all.RDS")


#programme function for classification for all data
cat_classifier <- function(category,var,df){
  
  #split in labelled (for CV) and unlabelled (for prediction)
  docvars(category, var) <- df %>% dplyr::select(var)  
  labelled <- category %>% 
    dfm_subset((docvars(category, var)) == "0"| (docvars(category, var)) == "1" | (docvars(category, var)) == "2" | (docvars(category, var)) == "3" | 
                 (docvars(category, var)) == "4" | (docvars(category, var)) == "5")
  unlabelled <- category[is.na(docvars(category, var)),]     
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
  unlabelled <- dfm_match(unlabelled, feat)
  if(var != "hatescale" & var != "hatescaleM"){
    levels(docvars(unlabelled, var)) <- c("1", "2")
  }
  
  #tune
  set.seed(124)
  model_tune <- tune.svm(
    x = train,  
    y = docvars(train, var), 
    kernel = "linear", 
    tunecontrol = tune.control(sampling = "cross", cross = 10, best.model = TRUE, performances = TRUE),
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
    docvars(labelled, var) <-  as.factor(c((docvars(train, var)), (docvars(balanced_test, var))))
  } else {
    labelled <- dfm_match(labelled, feat)
  }
  
  set.seed(7890)
  pred_tune <- svm(
    x = labelled, 
    y = docvars(labelled, var), scale = TRUE, kernel = "linear", cost = cost, gamma = gamma)
  pred <- predict(object = pred_tune, newdata = unlabelled)
  
  
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
  
  results <- list(predids = predids, metric = metric) 
  return(results)
}


#prediction on hatenom and hatescale on all data
    cluster <- makeCluster(detectCores()-1)
    registerDoParallel(cluster)
    start.time <- Sys.time()

  
  #1: CLASSIFIER FOR HATE   
    outcome_hate <- foreach (i = c(42,43,45), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar%  {
      result <- cat_classifier(dfm,names(tweets_all_merged)[i], tweets_all_merged)
    }
                      
    print(Sys.time() - start.time)
    stopCluster(cluster)
  

#data for second classifier
  ids_pred <- classified %>% dplyr::filter(hatenomNA == 2 |hatescale != "1" | hatescaleM > 1)      
  tweets_categories <- tweets_all_merged %>% 
    dplyr::filter(tweet__id %in% (ids_pred$tweet__id) | hatenomNA == 1 & X2_gretathunberg == 1) 
  dfm_categories <- dfm[tweets_categories$tweet__id,] %>% 
    dfm_trim(min_termfreq = 2, min_docfreq = 2)
  
  
##prediction on separate categories only for predicted hatenom
    cluster <- makeCluster(detectCores())
    registerDoParallel(cluster)
    start.time <- Sys.time()
  
  #2: CLASSIFIER FOR CATEGORIES  
  outcome_cat <- foreach (i = 23:41, .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar% {   
    result <- cat_classifier(dfm_categories, names(tweets_categories)[i], tweets_categories)
  }
  
  print(Sys.time() - start.time)
  stopCluster(cluster) 
  
  
##prediction on intersectional categories only for predicted hatenom
  cluster <- makeCluster(detectCores())
  registerDoParallel(cluster)
  start.time <- Sys.time()
  
  #3: CLASSIFIER FOR INTERSECTIONAL CATEGORIES  
  outcome_inter <- foreach (i = 46:58, .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar% {   
    result <- cat_classifier(dfm_categories, names(tweets_categories)[i], tweets_categories)
  }

  print(Sys.time() - start.time)
  stopCluster(cluster) 


#save Output  
  saveRDS(outcome_hate, "Data/outcome_hate.RData")
  saveRDS(outcome_cat, "Data/outcome_cat.RData")
  saveRDS(outcome_inter, "Data/outcome_inter.RData")
  saveRDS(tweets_categories, "Data/tweets_categories.RData")
  saveRDS(dfm_categories, "Data/dfm_categories.RDS")
  