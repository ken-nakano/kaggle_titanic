library(tidyverse)
library(modelr)

project.root <- Sys.getenv("PROJECT_ROOT")
source(str_c(project.root, "/source/service/prepro.R"))

train_and_predict_rb <- function(X_train, X_valid, Y_train, Y_valid) {
  ## 学習
  train <- cbind(X_train, Y_train)
  agg <- train %>%
    group_by(Pclass, Sex, age_group) %>%
    summarise(survive_rate = mean(Survived))
  
  ## 予測
  pred <- X_valid %>% left_join(agg, by=c("Pclass", "Sex", "age_group")) %>% select(survive_rate)
  pred_binary <- as.numeric(pred > 0.5)

  ## 正解率
  Y_valid <- Y_valid %>% unlist() %>% as.integer()
  ac.rate <- accuracy.rate(table(Y_valid, pred_binary))
  return(list(pred_binary, ac.rate, pred))
}
