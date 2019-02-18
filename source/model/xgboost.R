library(tidyverse)
library(xgboost)

project.root <- Sys.getenv("PROJECT_ROOT")
source(str_c(project.root, "/source/service/prepro.R"))



train_and_predict_xgb <- function(X_train, X_valid, Y_train, Y_valid) {
  X_train <- X_train %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate_if(is.factor, as.integer) %>% 
    as.matrix()
  Y_train <- Y_train %>% unlist() %>% as.integer()
  
  X_valid <- X_valid %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate_if(is.factor, as.integer) %>% 
    as.matrix()
  Y_valid <- Y_valid %>% unlist() %>% as.integer()
  
  ## 学習
  bst <- xgboost(
    data = X_train,
    label = Y_train,
    max.depth = 6,
    eta = 1,
    nthread = 2,
    nrounds = 2,
    objective = "binary:logistic"
  )
  
  ## 変数重要度
  imp <- xgb.importance(names(X_train), model=bst)
  
  ## 予測                        
  pred <- predict(bst, X_valid)
  pred_binary <- as.numeric(pred > 0.5)
  
  ## 正解率
  ac.rate <- accuracy.rate(table(Y_valid, pred_binary))
  return(list(imp, pred_binary, ac.rate, pred))
}