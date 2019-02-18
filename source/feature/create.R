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
    age_guess_flg = function(data){
      edit.data <- data %>% mutate(
        age_guess_flg = case_when(Age %% 1 == 0 ~ 0, TRUE ~ 1)
      )
      return(edit.data)
    },
    
    familysize = function(data){
      edit.data <- data %>% mutate(
        familysize = SibSp + Parch + 1 #本人込み
      )
      return(edit.data)
    },
    
    familysize_class = function(data){
      edit.data <- data %>% mutate(
        familysize_class = case_when(
          familysize == 1 ~ "single",
          familysize %in% c(2,3,4) ~ "small",
          TRUE ~ "large"
        )
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
      return(edit.data)
    },
    
    titleofhonor = function(data){
      edit.data <- data %>% mutate(
        title_of_honor = case_when(
          str_detect(Name, "Mrs.") ~ "Mrs.",
          str_detect(Name, "Miss.") ~ "Miss.",
          str_detect(Name, "Master.") ~ "Master.",
          str_detect(Name, "Mr.") ~ "Mr.",
          TRUE ~ "ELSE"
        )
      )
      return(edit.data)
    },
    
    mother = function(data){
      edit.data <- data %>% mutate(
        mother_flg = case_when(
          Sex == "female" & Age >= 18 & Parch > 0 & title_of_honor != "Miss." ~ 1,
          TRUE ~ 0
        )
      )
      return(edit.data)
    },
    
    fellowpassengernum = function(data){
      num_table <- data %>%
        group_by(Ticket) %>%
        summarise(
          fellow_passenger_num = n(),
          survived_num = sum(case_when(is.na(Survived) ~ 0, TRUE ~ Survived)),
          dead_num = sum(case_when(is.na(Survived) ~ 0, TRUE ~ abs(Survived-1))),
          unknown_num = sum(case_when(is.na(Survived) ~ 1, TRUE ~ 0))
        )
      
      edit.data <- data %>%
        left_join(num_table, by="Ticket") %>% 
        mutate(
          survived_num = case_when(Survived == 1 ~ survived_num - 1, TRUE ~ survived_num),
          dead_num = case_when(Survived == 0 ~  dead_num - 1, TRUE ~ dead_num),
          unknown_num = case_when(is.na(Survived) ~  unknown_num - 1, TRUE ~ unknown_num),
          fellow_passenger_num = fellow_passenger_num - familysize
        )
      
      return(edit.data)
    },
    
    ticketnumofchar = function(data){
      edit.data <- data %>%
        mutate(
          ticket_num_of_char = nchar(Ticket)
        )
      
      return(edit.data)
    }
    
    
    
  )
)