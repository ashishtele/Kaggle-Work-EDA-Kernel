
##############################################################
############### Twitter Data Analysis ########################
##############################################################
rm(list = ls())
silent(gc())

load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(caret))
  suppressPackageStartupMessages(require(Matrix))
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(treemap))
  suppressPackageStartupMessages(require(highcharter))
}

load_lb()

df <- read_excel("E:\\Study\\R Projects\\Common files\\gender-classifier-DFE-791531.xlsx")

head(df)
glimpse(df)
# 20,050 X 26 

## Column names changed
colnames(df)[1:5] <- c("Unit_ID", "Golden", "Unit_state", "Trusted_jud", "Last_jud_at")
names(df)

## Finding NULL 
sort(sapply(df, function(x) sum(is.na(x))))

df %>% 
  mutate(profile_yn_gold = NULL,
         gender_gold = NULL,
         tweet_coord = NULL) %>% 
         filter(!is.na(gender))-> df
glimpse(df)

head(df)

# no duplicate Unit_ID
length(unique(df$Unit_ID))

fct = unique(df$Golden)
df$Golden = as.integer(factor(df$Golden, labels = fct))

## Check for Golden proportion
df %>% 
  group_by(Golden) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(Golden), cnt)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = cnt), vjust = -0.2)+
  labs(title = "Golden distribution", x = "status", y = "Count")

## Very few people in Golden status

unique(df$Unit_state)
  
df %>% 
  group_by(Unit_state) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(Unit_state, cnt)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = cnt), vjust = -0.2)+
  labs(title = "Unit state distribution", x = "status", y = "Count")
  
## redundant column


unique(df$gender)
  
df %>% 
  group_by(gender) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(reorder(gender,cnt), cnt, fill = cnt)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = cnt), hjust = -0.2)+
  labs(title = "Gender distribution", x = "Gender", y = "Count") +
  coord_flip()

glimpse(df)  

df %>% 
  mutate(year = year(created),
         Month = month(created)) -> df
  
df %>% 
  group_by(year) %>% 
  summarise(cnt  = n()) %>% 
  arrange(-cnt) %>% 
  ggplot(aes(reorder(year,cnt), cnt, fill = cnt)) +
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_text(aes(label = cnt), hjust = -0.1) +
  coord_flip()
  
df %>% 
  group_by(Month) %>% 
  summarise(cnt  = n()) %>% 
  arrange(-cnt) %>% 
  ggplot(aes(reorder(Month,cnt), cnt, fill = cnt)) +
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_text(aes(label = cnt), hjust = -0.1) +
  coord_flip()

df %>% 
  mutate(created_dt = as.Date(as.POSIXct(created))) -> df

glimpse(df)

df %>% 
  filter(!is.na(user_timezone)) %>% 
  filter(user_timezone %in% c("Pacific Time (US & Canada)")) %>% 
  group_by(created_dt,user_timezone) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt) %>% 
  ggplot(aes(created_dt,cnt))+
  geom_line()+
  facet_wrap(~user_timezone)

## Tweets by user timezone

df %>% 
  filter(!is.na(user_timezone)) %>% 
  group_by(user_timezone) %>% 
  summarise(cnt  = n()) %>% 
  arrange(-cnt) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(user_timezone,cnt), cnt, fill = cnt)) +
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_text(aes(label = cnt), hjust = -0.1) +
  coord_flip()

df_t <- as_tibble(df$text)
head(df_t)




