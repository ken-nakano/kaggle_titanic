library(R6)
library(tidyverse)

#
# sex
# pclass
# cabin
# age
# ticket


FeatureEngineering <- R6Class(
  classname = "FeatureEngineering",
  private = list(),
  public = list(
    familysize = function(data){
      edit.data <- data %>% mutate(
        familysize = SibSp + Parch + 1 #本人込み
      )
      return(edit.data)
    },
    
    cabintype = function(data){
      edit.data <- data %>% mutate(
        cabin_type = substr(Cabin, 1, 1)
      )
      return(edit.data)
    },
    
    agegroup = function(data){
      edit.data <- data %>% mutate(
        age_group = case_when(
          is.na(Age) ~ as.character(NA),
          Age <= 10 ~ "under10",
          TRUE ~ "over10"
        )
      )
      return(edit.data)
    },
    
    fareperperson = function(data){
      edit.data <- data %>% mutate(
        fare_per_person = Fare / familysize
      )
    }
    
  )
)