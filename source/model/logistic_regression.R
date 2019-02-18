library(tidyverse)
library(modelr)

project.root <- Sys.getenv("PROJECT_ROOT")
source(str_c(project.root, "/source/service/prepro.R"))

train_and_predict_lr <- function(X_train, X_valid, Y_train, Y_valid) {
  ## 学習
  train <- cbind(X_train, Y_train)
  fit <- glm(Survived ~ ., data = train, family = binomial(link = "logit"))
  
  ## 予測
  pred <- predict(fit, newdata = X_test, type = "response")
  pred_binary <- as.numeric(pred > 0.5)
  
  ## 正解率
  Y_valid <- Y_valid %>% unlist() %>% as.integer()
  ac.rate <- accuracy.rate(table(Y_valid, pred_binary))
  return(list(pred_binary, ac.rate, pred))
}
