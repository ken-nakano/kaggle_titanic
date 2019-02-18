library(tidyverse)

project.root <- Sys.getenv("PROJECT_ROOT")
source(str_c(project.root, "/source/feature/create.R"))

FE <- FeatureEngineering$new()

# Load Data ---------------------------------------------------------------
raw.train <- read_csv(str_c(project.root, "/data/input/train.csv"))
raw.test <- read_csv(str_c(project.root, "/data/input/test.csv"))

tr_te <- raw.train %>% bind_rows(raw.test)

# カラム別欠損数 -----------------------------------------------------------------
na_num_train <- sapply(raw.train, function(y) sum(is.na(y))) 
na_num_test <- sapply(raw.test, function(y) sum(is.na(y)))



# Feature Engineering -----------------------------------------------------
tr_te <- FE$familysize(tr_te) 
tr_te <- FE$fareperperson(tr_te)
tr_te <- FE$cabintype(tr_te)


# 欠損補完(Fare) --------------------------------------------------------------
tr_te %>% filter(is.na(Fare)) %>% as.data.frame()
# Pclass:3
# Sex:male
# Age:60.5
# SibSp:0
# Parch:0
# Ticket:3701
# Cabin:NA
# Embarked:S

## チケット番号ごとのFare：コレを見る限りは7 or 8あたり？
tr_te %>%
  mutate(ticket_ch_length = nchar(Ticket)) %>% 
  arrange(ticket_ch_length, Ticket) %>% 
  distinct(Ticket, Fare) %>%
  as.data.frame()

## Embarked == S, Pclass == 3 でのfamilysizeとFareの関係：やはり一桁後半あたりが濃厚



# 欠損補完(Embarked) --------------------------------------------------------------
tr_te %>% filter(is.na(Embarked)) %>% as.data.frame()
# familysize:1
# Pclass:1
# Fare:80
# Cabin:B29

## Pclass=1のEmbark別Fare分布:S or Cが候補
tr_te %>% filter(Pclass == 1) %>% ggplot(., aes(x=Fare, fill=Embarked)) + geom_histogram()

## Ticketの並び順的にはSがありえそう
tr_te %>% filter(Pclass == 1, Fare < 100) %>% group_by(Embarked, Ticket) %>% summarise(records = n()) %>% arrange(Ticket) %>% as.data.frame()



# 欠損値補完(Age) --------------------------------------------------------------
## 敬称との関連度が高そう
tr_te %>% 
  group_by(title_of_honor, Sex) %>% 
  summarise(
    uu = n(),
    nas = sum(ifelse(is.na(Age), 1, 0)),
    avg_age = mean(Age, na.rm = T)
  )



# 欠損値補完(Age) --------------------------------------------------------------
tr_te %>%
  ggplot(., aes(x=familysize, fill=factor(Survived))) + geom_bar(position="dodge")
