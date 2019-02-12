train_test_split <- function(df, train_rate) {
  ro <- nrow(df)
  train_index <- sample(ro, size = ro*train_rate)
  
  train <- df[train_index, ]
  test <- df[-train_index, ]
  
  return(list(train, test))
}


accuracy.rate <- function(table){
  sum(table[row(table)==col(table)])/sum(table)
}
