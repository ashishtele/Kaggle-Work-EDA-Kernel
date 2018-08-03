
# remove previous objects
rm(list = ls())
gc()

# load libraries
load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(caret))
  suppressPackageStartupMessages(require(Matrix))
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(treemap))
  suppressPackageStartupMessages(require(highcharter))
  suppressPackageStartupMessages(require(mice))
}

load_lb()

# import data file

df <- read_excel("E:\\Study\\R Projects\\Common files\\Volcano Eruptions.xlsx")
glimpse(df)
# 1436 X 13

head(df)

sort(sapply((df), function(x) {sum(is.na(x))}), decreasing = TRUE)
# 14 missing in 'Domninant Rock type'
# 5 in 'Tectonic setting'


library(mice)
md.pattern(df)
  
df_impute <- mice(data = df,
                  method = c('rf'))

summary(df_impute)
cm <- complete(df_impute, "long")
cm

complete_df <- right_join(df[,c('Volcano Number','Dominant Rock Type','Tectonic Setting')],
                          complete(df_impute,"long"),
                          by = c("Volcano Number" = "Volcano.Number"))

# map plot

library(leaflet)
df %>%
  leaflet() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by 
           <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> 
           &mdash; 
           Map data &copy; 
           <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  addCircles(lat = ~Latitude, lng = ~Longitude, color = "blue",
             radius = 1, opacity = 0.1)

# Region with maximum eruptions

df %>% 
  group_by(Region) %>% 
  summarise(cnt = n()) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(Region,cnt), cnt, fill = cnt))+
  geom_bar(stat = "identity")
  

# Subregions

df %>% 
  group_by(Subregion) %>% 
  summarise(cnt = n()) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(Subregion,cnt), cnt, fill = cnt))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  coord_flip() +
  geom_text(aes(label = cnt), hjust = -0.2)+
  labs(title = "Volcano Eruptions by Subregions",
       x = "No.of Eruptions",
       y = "Subregions")

# Country

df %>% 
  group_by(Country) %>% 
  summarise(cnt = n()) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(Country,cnt), cnt, fill = cnt))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = cnt), hjust = -0.2)+
  labs(title = "Volcano Eruptions by regions",
       x = "No.of Eruptions",
       y = "Regions")

glimpse(df)

# Lets see volcano names by Elevation (highest)

df %>% 
  select('Volcano Name', ele = 'Elevation (m)') %>% 
  arrange(-ele) %>% 
  top_n(10, ele) %>% 
  ggplot(aes(x = reorder(`Volcano Name`,ele), y = ele, color = `Volcano Name`))+
  geom_point(stat = "identity", size = 2, show.legend = FALSE)+
  geom_segment(aes(x = `Volcano Name`, xend = `Volcano Name`, y = 0, yend = ele), show.legend = FALSE)+
  coord_flip()+
  geom_text(aes(label = ele), hjust = -0.2, show.legend = FALSE)+
  labs(title = "Volcanos by Elevation (m)",
       x = "Volcano Names",
       y = "Elevation (m)")
  
# Lets see volcano names by Elevation (below ground level)

df %>% 
  select('Volcano Name', ele = 'Elevation (m)') %>% 
  arrange(ele) %>% 
  top_n(10, -ele) %>% 
  ggplot(aes(x = reorder(`Volcano Name`,-ele), y = ele, color = `Volcano Name`))+
  geom_point(stat = "identity", size = 2, show.legend = FALSE)+
  geom_segment(aes(x = `Volcano Name`, xend = `Volcano Name`, y = 0, yend = ele), show.legend = FALSE)+
  coord_flip()+
  geom_text(aes(label = ele), hjust = 1.2, show.legend = FALSE)+
  labs(title = "Volcanos by Elevation (m)",
       subtitle = "'Udintsev Transform' in pacific ocen",
       x = "Volcano Names",
       y = "Elevation (m)") +
  theme_minimal()

glimpse(df)

# which are most common eruptions by volcano type

df %>% 
  group_by('Primary Volcano Type') %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt) %>% 
  top_n(10) %>% 
  ggplot(aes(x = reorder(`Volcano Name`,-ele), y = ele, color = `Volcano Name`))+
  geom_point(stat = "identity", size = 2, show.legend = FALSE)+
  geom_segment(aes(x = `Volcano Name`, xend = `Volcano Name`, y = 0, yend = ele), show.legend = FALSE)+
  coord_flip()+
  geom_text(aes(label = ele), hjust = 1.2, show.legend = FALSE)+
  labs(title = "Volcanos by Elevation (m)",
       subtitle = "'Udintsev Transform' in pacific ocen",
       x = "Volcano Names",
       y = "Elevation (m)") +
  theme_minimal()

# which 'Tectonic settings' are most active

df %>% 
  group_by(`Tectonic Setting`) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt) %>% 
  top_n(10) %>% 
  filter(!`Tectonic Setting` == "NA") %>% 
  ggplot(aes(x = reorder(`Tectonic Setting`,cnt), y = cnt, color = `Tectonic Setting`))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  coord_flip()+
  geom_text(aes(label = cnt), hjust = 1.2, show.legend = FALSE)+
  labs(title = "Eruptions by Tectonic setting",
       subtitle = "'Subduction zone (>25 km)' most active",
       x = "Tectonic setting",
       y = "no.of eruptions") +
  theme_minimal()

# vulnerable rock type

df %>% 
  group_by(`Dominant Rock Type`) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt) %>% 
  top_n(10) %>% 
  filter(!`Dominant Rock Type` == "NA") %>% 
  ggplot(aes(x = reorder(`Dominant Rock Type`,cnt), y = cnt, color = `Dominant Rock Type`))+
  geom_point(stat = "identity", size = 2, show.legend = FALSE)+
  geom_segment(aes(x = `Dominant Rock Type`, xend = `Dominant Rock Type`, y = 0, yend = cnt), show.legend = FALSE)+
  coord_flip()+
  geom_text(aes(label = cnt), hjust = -1.1, show.legend = FALSE)+
  labs(title = "Rock type vs No. of Eruptions",
       subtitle = "'Andesite / Basaltic Andesite' most vulnerable",
       x = "Rock type",
       y = "no.of eruptions") +
  theme_minimal()









