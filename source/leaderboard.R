library(tidyverse)

project.root <- Sys.getenv("PROJECT_ROOT")


# Load Data ---------------------------------------------------------------
scores <- read_csv(str_c(project.root, "/data/input/titanic-publicleaderboard_20190214.csv"))

scores.pre <- scores %>%
  filter(Score >= 0.75) %>% 
  select(TeamName, Score) %>%
  group_by(TeamName) %>%
  summarise(Score = max(Score)) %>%
  ungroup() %>%
  arrange(desc(Score))


# 可視化 ---------------------------------------------------------------------
ggplot(scores.pre, aes(x=reorder(TeamName, desc(Score)), y=Score)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Kaggle Titanic", y="Accuracy", x="TeamName")

scores.pre
