rm(list = ls())

load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(caret))
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(library(tree))
  suppressPackageStartupMessages(library(MASS))
  suppressPackageStartupMessages(library(mice))
  suppressPackageStartupMessages(require(xgboost))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(Matrix))
  library(data.table)
  library(mlr)
}
load_lb()

# Importing the data

df <- fread("E:\\Study\\R Projects\\Common files\\ign.csv")
glimpse(df)
# 18,625 rows X 11 columns

clas <- split(names(df),sapply(df, function(x) class(x)))
char_col <- clas[[1]]
num_col <- setdiff(names(df),char_col)

# summary of character columns
summarizeColumns(df[,..char_col])  # few columns have missing values
summary(df[,..char_col])

# summary of numeric columns 
summarizeColumns(df[,..num_col])
summary(df[,..num_col])

# there are no NA's in numeric (tidy data)
sapply(df[,..num_col],function(x) sum(is.na(x)))

# column 'genre' has #36 blanks
sapply(df[,..char_col],function(x) sum(x==""))

# removing blanks from data
df %>% filter(!genre=="") -> df

names(df1)
# checking for year 
sort(unique(df1$release_year), decreasing = TRUE)
## we have 1970 in data, lets remove

df %>% filter(!release_year==1970) -> df

library(lubridate)
# date column creation
df %>%
  mutate(rel_date = ISOdate(release_year,release_month,release_day)) %>%
  mutate(release_year = year(rel_date),
         release_month = month(rel_date),
         release_day = day(rel_date)) -> df

length(df$title) - length(unique(df$title))

# creat unique title dataset

df_uniq <- df[!duplicated(df$title),]

head(df_uniq)  

############################## EDA ##########################################

# games by genre
names(df_uniq)
df_uniq %>%
  group_by(genre) %>%
  summarise(cnt = n()) %>%
  arrange(-cnt) %>%
  top_n(10) %>%
  ggplot(aes(x=reorder(genre,cnt), y=cnt, fill = -cnt)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = cnt), hjust = -0.2) +
  labs(title = "# of games by Genre", x = "genre") +
  coord_flip()

# score distribution among genre

df_uniq %>%
  group_by(genre) %>%
  summarise(cnt = n()) %>%
  arrange(-cnt) %>%
  top_n(10) -> score_genre
  
df_uniq %>%
  filter(genre %in% score_genre$genre) %>%
  ggplot(aes(x=genre, y = score, fill = genre)) +
  geom_boxplot(varwidth = TRUE) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  geom_hline(yintercept = median(df_uniq$score))

# top platforms

names(df_uniq)

df_uniq %>%
  group_by(platform) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round((cnt/sum(cnt))*100,1)) %>%
  arrange(-cnt) %>%
  top_n(10) -> top_platforms

names(df_uniq)
top_platforms %>%
  ggplot(aes(x=reorder(platform,freq), y=freq, fill = -freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = freq), hjust = -0.2) +
  labs(title = "Top platforms", x = "Platforms") +
  coord_flip()


# top platforms in recent years

df_uniq %>%
  filter(release_year > 2013) %>%
  group_by(platform, release_year) %>%
  summarise(cnt = n()) %>%
  arrange(release_year,-cnt) %>%
  ggplot(aes(x=reorder(platform,cnt), y = cnt, color = release_year)) +
  geom_point() +
  geom_segment(aes(x=platform, xend = platform, y= 0, yend = cnt)) +
  labs(title= "Platform trend in recent years", x="",y="") +
  facet_wrap(~release_year) + 
  coord_flip()

# Games release by months for recent years

df_uniq %>%
  filter(release_year > 2005) %>%
  group_by(release_month, release_year) %>%
  summarise(cnt = n()) %>%
  arrange(release_month, release_year) -> df_mon_yr

df_mon_yr %>%
  mutate(rel_mon = as.Date(paste0(as.numeric(release_month),"-01"),"%m-%d")) %>%
  ggplot(aes(x = rel_mon, y = cnt)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~release_year,ncol = 4) +
  labs(title = "Game release by month", x = "month", y = "game release")

# score phrase distribution (scoring)

df_uniq %>%
  distinct(score_phrase,score) %>%
  mutate(sc_round = trunc(score)) %>%
  arrange(-score) %>%
  distinct(score_phrase, sc_round) %>%
  ggplot(aes(x=reorder(score_phrase,sc_round), y=sc_round, fill = score_phrase)) +
  geom_bar(stat = "identity") + 
  coord_flip()


# No of games by score phase

df_uniq %>%
  mutate(sc_round = trunc(score)) %>%
  group_by(score_phrase,sc_round) %>%
  summarise(cnt = n()) %>%
  arrange(sc_round) %>%
  ggplot(aes(x=reorder(score_phrase,sc_round), y=cnt, fill = score_phrase)) +
  geom_bar(stat = "identity")

## Most of the games are having good and great scores.
## negatively skewed


# Editor's choice and rating
df_uniq %>%
  mutate(sc_round = trunc(score)) %>%
  group_by(score_phrase,editors_choice,sc_round) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(x = reorder(score_phrase,sc_round), y = cnt, fill = score_phrase)) +
  geom_bar(stat = "identity") +
  labs(x="score pharse", y = "editor choice") +
  facet_wrap(~editors_choice) 


  


