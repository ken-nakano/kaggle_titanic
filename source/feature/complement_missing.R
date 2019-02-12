library(R6)
library(tidyverse)

#
# sex
# pclass
# cabin
# age
# ticket


ComplementMissing <- R6Class(
  classname = "ComplementMissing",
  private = list(),
  public = list(
    comp.Fare = function(data){
      edit.data <- data
      edit.data[is.na(edit.data)] <- 7.88
      
      return(edit.data)
    }
  )
)