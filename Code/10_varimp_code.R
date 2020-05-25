
### variable importance plots for subcategories of hate

#load packages
  list.of.packages <- c("tidytext","dplyr","tidyr", "readtext", "quanteda", "quanteda.corpora", "kernlab",
                        "caret", "vecsets", "doParallel", "vip", "rsample", "foreach", "klaR", "e1071",  
                        "quanteda.textmodels", "stringr", "gridExtra", "ggplot2", "tidyverse")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = TRUE)


#load data
  tweets_categories <- readRDS("Data/tweets_categories.RData")
  dfm_categories <- readRDS("Data/dfm_categories.RDS")


#function for creating data frames for variable importance
  varimp_classifier <- function(category,var,df){
    
    #split in labelled (for CV) and unlabelled (for prediction)
    docvars(category, var) <- df %>% dplyr::select(var)  
    labelled <- category %>% 
      dfm_subset((docvars(category, var)) == "0"| (docvars(category, var)) == "1" | (docvars(category, var)) == "2" | (docvars(category, var)) == "3" | 
                   (docvars(category, var)) == "4" | (docvars(category, var)) == "5")
    unlabelled <- category[is.na(docvars(category, var)),]      #here, I subset --> this would be the full / stratified data then [1:200,]
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
      tunecontrol = tune.control(
        sampling = "cross", cross = 10, best.model = TRUE, performances = TRUE),
      gamma = c(0.5,1,2), cost = 10^(-1:2))
    
    #varimp
    set.seed(4567)
    varimp <- vi_permute(model_tune$best.model, method = "permute", train = train, 
                         target = docvars(train, var), metric = "accuracy", 
                         pred_wrapper = predict) 
    
    results <- list(varimp = varimp) #, varimp = varimp
    return(results)
  }


##prediction on categories only for predicted subcategories
  cluster <- makeCluster(detectCores())
  registerDoParallel(cluster)
  start.time <- Sys.time()

  #1: CLASSIFIER FOR CATEGORIES  
    outcome_varimp_41 <- foreach (i = 23:41, .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar% {   
      result <- varimp_classifier(dfm_categories, names(tweets_categories)[i], tweets_categories)
    }
    
    print(Sys.time() - start.time)
    stopCluster(cluster) 


##prediction on categories  for intersectional subcategories
  cluster <- makeCluster(detectCores())
  registerDoParallel(cluster)
  start.time <- Sys.time()
  
  #2: CLASSIFIER FOR RELEFANT INTERSECTIONAL CATEGORIES  
    outcome_varimp_inter <- foreach (i = c(46:48,53), .packages = c("caret", "dplyr", "stringr", "e1071", "quanteda", "tidytext", "tidyr", "vip")) %dopar% {   
      result <- varimp_classifier(dfm_categories, names(tweets_categories)[i], tweets_categories)
    }
    
    print(Sys.time() - start.time)
    stopCluster(cluster) 



##### GRAPHS

  varimp_total <- c(outcome_varimp_41, outcome_varimp_inter)
  names_varimp <- c(names(tweets_all_merged[c(23:41, 46:48, 53)]))
  names_varimp[20:23] <- c("X6_manip_age", "X6_manip_cc", "X6_mental_age", "X6_gender_age")
  
  P <- list()
  for(i in 1:23){
    p<- ggplot((varimp_total[i][[1]]$varimp %>% arrange(desc(Importance)) %>% head(10)), 
          aes(x = reorder(Variable, Importance), weight = Importance, fill = desc(Importance)))+
      geom_bar() + 
      coord_flip()+
      ylab(label = NULL) + 
      ggtitle(label= names_varimp[i]) + 
      theme(plot.title = element_text(size = 8, face = "bold"),
            axis.title.y=element_blank(),
            legend.position = "none", 
            plot.margin = margin(5,5,5,5, "pt")) + 
      scale_fill_gradient(high="#DeF7E9",low= "#71CA97")
      
    P <- c(P, list(p))
  }
  customGreen0 = "#DeF7E9"
  customGreen = "#71CA97"
  
  n <- length(P)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(P, ncol=nCol))
