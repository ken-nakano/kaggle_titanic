library(tidyverse)

project.root <- Sys.getenv("PROJECT_ROOT")
source(str_c(project.root, "/source/service/prepro.R"))

ensemble <- function(list_data) {
  n <- list_data %>% length()
  
  add_result <- rep(0, length(result[[1]]))
  for (i in 1:n) {
    add <- result[[i]]
    add_result <- add_result + add
  }
  
  ensembled <- as.numeric(add_result >= n/2)
  return(ensembled)
}
