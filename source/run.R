library(tidyverse)
library(xgboost)
library(tictoc)

project.root <- Sys.getenv("PROJECT_ROOT")
exec_mode = commandArgs(trailingOnly=TRUE)[1]

source(str_c(project.root, "/source/config.R"))
source(str_c(project.root, "/source/service/prepro.R"))
source(str_c(project.root, "/source/feature/create.R"))
source(str_c(project.root, "/source/feature/complement_missing.R"))
source(str_c(project.root, "/source/model/xgboost.R"))


tic()

# Load Data ---------------------------------------------------------------
raw.train <- read_csv(str_c(project.root, "/data/input/train.csv"))
raw.test <- read_csv(str_c(project.root, "/data/input/test.csv"))

## 一旦trainとtestをひとまとめにする
tr_index <- 1:nrow(raw.train)
tr_te <- raw.train %>% bind_rows(raw.test)


# Complement Missing ------------------------------------------------------------
CM <- ComplementMissing$new()

## Complement Missing(train,test一緒に)
tr_te <- CM$comp.Fare(tr_te)



# Feature Engineering -----------------------------------------------------
FE <- FeatureEngineering$new()

## Feature Engineering(train,test一緒に)
tr_te <- FE$familysize(tr_te) 
tr_te <- FE$cabintype(tr_te) 
tr_te <- FE$agegroup(tr_te)
tr_te <- FE$fareperperson(tr_te)

## 再度train,test分ける
fe.train <- tr_te[tr_index, ]
fe.test <- tr_te[-tr_index, ]



# Train Test Split --------------------------------------------------------
features <- c()
accuracy_rate <- c()

for (i in 1:ite) {
  df.li <- train_test_split(fe.train, 0.8)

  train <- df.li[[1]]
  valid <- df.li[[2]]

  tr_va <- train %>% bind_rows(valid)
  tr_index <- 1:nrow(train)

  X_train <- tr_va[tr_index, ] %>% select(-Survived)
  Y_train <- tr_va[tr_index, ] %>% select(Survived)
  X_valid <- tr_va[-tr_index, ] %>% select(-Survived)
  Y_valid <- tr_va[-tr_index, ] %>% select(Survived)
  
  # Train and Predict -------------------------------------------------------
  tmp.result <- train_and_predict_xgb(X_train, X_valid, Y_train, Y_valid)
  add.features <- tmp.result[[1]]$Feature
  add.accuracy_rate <- tmp.result[[3]]
  
  features <- c(features, add.features)
  accuracy_rate <- c(accuracy_rate, add.accuracy_rate)
}



toc() -> kiroku

feature_importance <- table(features) %>% as.data.frame() %>% arrange(desc(Freq)) %>% mutate(con = paste(features, Freq)) %>% head(10)
feature_importance <- feature_importance$con %>% as.vector()
avg_accuracy_rate <- mean(accuracy_rate)
elapsed_time <- kiroku$toc - kiroku$tic

log_script <- str_c(
  "Accuracy:", round(avg_accuracy_rate, 4), "  ",
  "実行時間:", round(elapsed_time, 4), "  ",
  "変数重要度(頻出順):", paste(feature_importance, collapse = ","),
  "変数一覧:", paste(names(train), collapse=",")
)

# 結果のログ出力
print(log_script)
filename <- str_c("log_ACCURACY", round(avg_accuracy_rate, 4), "_TIME", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".txt")
write(log_script, str_c(project.root, "/log/", filename))




# submit用ファイル出力 -----------------------------------------------------------
if (exec_mode == "submit") {
  
  X_train <- fe.train %>% select(-Survived)
  Y_train <- fe.train %>% select(Survived)
  X_test <- fe.test %>% select(-Survived)
  Y_test <- fe.test %>% select(Survived)
  
  # Train and Predict -------------------------------------------------------
  result <- train_and_predict_xgb(X_train, X_test, Y_train, Y_test)
  
  submit_file <- cbind(X_test$PassengerId, result[[2]]) %>% as.data.frame()
  names(submit_file) <- c("PassengerId", "Survived")
  
  output_dir <- str_c(project.root, "/data/output/submit_file_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
  write.csv(submit_file, output_dir, row.names = F, quote = F)

}
