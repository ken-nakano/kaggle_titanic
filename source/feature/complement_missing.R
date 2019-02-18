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
      edit.data$Fare[is.na(edit.data$Fare)] <- 7.88
      
      return(edit.data)
    },
    
    comp.Embarked = function(data){
      edit.data <- data
      edit.data$Embarked[is.na(edit.data$Embarked)] <- "S"
      
      return(edit.data)
    },
    
    comp.Age = function(data){
      edit.data <- data %>% 
        mutate(
          Age = case_when(
            is.na(Age) & title_of_honor == "ELSE" & Sex == "female" ~ 32.8,
            is.na(Age) & title_of_honor == "ELSE" & Sex == "male" ~ 45.5,
            is.na(Age) & title_of_honor == "Master." ~ 5.48,
            is.na(Age) & title_of_honor == "Miss." ~ 21.8,
            is.na(Age) & title_of_honor == "Mr." ~ 32.3,
            is.na(Age) & title_of_honor == "Mrs." ~ 36.8,
            TRUE ~ Age
          )
        )
      
      return(edit.data)
    },
    
    comp.cabintype = function(data){
      edit.data <- data %>% 
        mutate(
          cabin_type = case_when(
            is.na(cabin_type) ~ "Unknown",
            TRUE ~ cabin_type
          )
        )
      
      return(edit.data)
    }
    
  )
)