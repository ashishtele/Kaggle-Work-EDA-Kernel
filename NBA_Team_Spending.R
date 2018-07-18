#####################################################################
#################### NBA Salary Trend ###############################
#####################################################################

rm(list = ls())

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
  suppressPackageStartupMessages(require(ggthemes))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(treemap))
  suppressPackageStartupMessages(require(highcharter))
}

load_lb()

# Load the files

team_sal <- read_excel("E:\\Study\\R Projects\\Common files\\NBA_Salary_History.xlsx",
                 sheet = "Team Salaries")
ply_sal <- read_excel("E:\\Study\\R Projects\\Common files\\NBA_Salary_History.xlsx",
                      sheet = "Player Salaries")

glimpse(team_sal)
#816 X 4
glimpse(ply_sal)
#13,297 X 4

# combined files

df <- ply_sal %>% 
  left_join(team_sal, by = c("Season", "Team")) %>% 
  arrange(Season)

df %>% 
  separate(Season, c("Start","End")) %>% 
  mutate(End = paste0(substr(Start, 1,2),End)) %>% 
  mutate(End = if_else(End == "1900","2000",End))-> df


# Salary cap trend
glimpse(df)
df %>% 
  select(End, `Salary Cap`) %>% 
  distinct() %>% 
  drop_na() %>% 
  ggplot(aes(End, `Salary Cap`/1000000, group = 1))+
  geom_line(color = "#CFB292", size = 1)+
  geom_point(size = 3, color = "#c49871")+
  theme_economist()+
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.background = element_rect(fill = "#f9f5f1"),
    #text = element_text(size = 7),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, vjust = 0.4, size = 7),
    axis.ticks.x = element_blank()
  )+
  labs(title = "Salary Cap Range Over the Years",
       subtitle = "Salary Cap has seen sharp increase in 2016-17",
       x = "Season end year",
       y = "$ in million")

## Total salary trend

glimpse(df)

df %>% 
  select(Team, End, `Total Salary`) %>% 
  distinct() %>% 
  drop_na() %>% 
  ggplot(aes(End, `Total Salary`/1000000))+
  geom_point(size = 3, color = "#c49871", show.legend = FALSE)+
  theme_economist()+
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.background = element_rect(fill = "#f9f5f1"),
    #text = element_text(size = 7),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, vjust = 0.4, size = 7)
  )+
  labs(title = "The Spread of Total Salary Over the Years",
       subtitle = "Total Salary has shown increasing trend over the years",
       x = "Season end year",
       y = "$ in million") 
  
## Top salary by a player

df %>% 
  filter(End == "2018") %>% 
  arrange(-Salary) %>% 
  top_n(10, Salary) %>% 
  ggplot(aes(reorder(Player,Salary), Salary/1000000))+
  geom_bar(stat = "identity", fill = "#c49871")+
  coord_flip()+
  geom_text(aes(label = round(Salary/1000000,1), hjust = -0.3), color = "#8e511b")+
  theme_economist()+
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.background = element_rect(fill = "#f9f5f1"),
    #text = element_text(size = 7),
    strip.text = element_text(size = 8),
    axis.ticks.x = element_blank(),
    axis.text.x=element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.text.y = element_text(size = 7, face = "bold")
  )+
  labs(title = "Top Salaried Players in the Season 2017-18",
       subtitle = "Stephen Curry and LeBron James are leading the chart",
       x = "",
       y = "$ in million") 


# Micheal jordan

df %>% 
  filter(Player == "Michael Jordan") %>% 
  filter(!Start == End) %>% 
  ggplot(aes(End, Salary/1000000, group = 1))+
  geom_line(color = "#CFB292", size = 1)+
  geom_point(size = 3, color = "#8e511b")+
  theme_economist()+
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.background = element_rect(fill = "#f9f5f1"),
    #text = element_text(size = 7),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(vjust = 0.4, size = 7),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 7, face = "bold")
  )+
  labs(title = "Jordan made $33.1 million in the 1997-98 season",
       subtitle = "NBA Salary of Jordan over the years",
       x = "Season end year",
       y = "$ in million") 


df %>% 
  drop_na() %>% 
  mutate(Costly = (Salary/`Salary Cap`),
         Ply_yr = paste(Player, End, sep = "-")) %>% 
  arrange(-Costly) %>% 
  top_n(10, Costly) %>% 
  ggplot(aes(reorder(Ply_yr,Costly), Costly))+
  geom_bar(stat = "identity", fill = "#c49871")+
  coord_flip()+
  geom_text(aes(label = scales::percent(round(Costly,2)), hjust = -0.3), color = "#8e511b")+
  theme_economist()+
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.background = element_rect(fill = "#f9f5f1"),
    #text = element_text(size = 7),
    strip.text = element_text(size = 8),
    axis.ticks.x = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.text.y = element_text(size = 7, face = "bold")
  )+
  labs(title = "Jordan received 23% more than the salary cap in 1997-98",
       subtitle = "The percentage of market cap received by a single player",
       x = "",
       y = "") 






