
##############################################################
############ Global Terrorist Attacks ########################
##############################################################l
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
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(treemap))
  suppressPackageStartupMessages(require(highcharter))
}

load_lb()

# Load the files

df <- fread(file.choose())         # local system path in E drive

glimpse(df)
## 170,350 X 135 dimension  

summary(df$iyear)                 # 1970 to 2016 

## yearwise killings in world

df %>% 
  select (iyear, nkill) %>% 
  filter(nkill > 0) %>% 
  group_by(iyear) %>% 
  summarise(cnt = sum(nkill)) %>% 
  ggplot(aes(x = iyear, y = cnt, fill = cnt)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "# of killings in each year", x = "Year", y = "# of kills")+
 #geom_text(aes(label = cnt), vjust = -0.2) +
  theme_minimal()

## 2012 to 2016 have maximum no. of kills
# palette : https://color.hailpixel.com/#E7B6CC,C95EB8,5A3091,191745,09181B c("#5A3091","#C95EB8","#E7B6CC")

df %>% 
  select(iyear, nkill) %>% 
  filter(nkill > 0) %>% 
treemap(index = c("iyear"), vSize = "nkill" , palette = c("#5A3091","#C95EB8","#E7B6CC"),
        title = "# of kills in each year") %>% 
  hctreemap(allowDrillToNode = TRUE, layoutAlgorithm = "squarified")


## countrywise killings in world

df %>% 
  select (country_txt, nkill) %>% 
  filter(nkill > 0) %>% 
  group_by(country_txt) %>% 
  summarise(cnt = sum(nkill)) %>% 
  filter(cnt > 5000) %>% 
  ggplot(aes(x = reorder(country_txt,cnt), y = cnt, fill = cnt)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "# of kills by country", x = "country", y = "# of kills")+
  geom_text(aes(label = cnt), hjust = -0.2) +
  coord_flip() +
  theme_minimal()

## Regionwise killings in world

df %>% 
  select (region_txt, nkill) %>% 
  filter(nkill > 0) %>% 
  group_by(region_txt) %>% 
  summarise(cnt = sum(nkill)) %>% 
  filter(cnt > 5000) %>% 
  ggplot(aes(x = reorder(region_txt,cnt), y = cnt, fill = cnt)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "# of killingss by regions", x = "Region names", y = "# of deaths")+
  geom_text(aes(label = cnt), hjust = -0.2) +
  coord_flip() +
  theme_minimal()

df %>% 
  select(country_txt, nkill) %>% 
  filter(nkill > 0) %>% 
  treemap(index = c("country_txt"), vSize = "nkill" , palette = c("#5A3091","#C95EB8","#E7B6CC"),
          title = "# of kills by country") %>% 
  hctreemap(layoutAlgorithm = "squarified")


## Lets check the trend in my country (India)

df %>% 
  select (iyear, nkill, country_txt) %>% 
  filter(nkill > 0,country_txt == "India") %>% 
  group_by(iyear) %>% 
  summarise(cnt = sum(nkill)) %>% 
  ggplot(aes(x = reorder(iyear,cnt), y = cnt, fill = cnt)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "# of kills in each year in India", x = "Year", y = "# of kills")+
  geom_text(aes(label = cnt), hjust = -0.2) +
  coord_flip() +
  theme_minimal()

df %>% 
  select (iyear, nkill,everything()) %>% 
  filter(nkill > 0,country_txt == "India", !is.na(latitude)) -> df_india

## no of casualities, clustered on map

library(leaflet)
leaflet(df_india) %>% 
  addTiles() %>% 
  addCircleMarkers(clusterOptions = markerClusterOptions(),
                   lat = df_india$latitude, lng = df_india$longitude)


## no of deaths by states in India

df_india %>% 
  group_by(provstate) %>% 
  summarise(cnt = sum(nkill)) %>% 
  ggplot(aes(x = reorder(provstate,cnt), y = cnt, fill = cnt)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "# of deaths by States", x = "States", y = "Deaths") +
  geom_text(aes(label = cnt), hjust = -0.2) +
  coord_flip() +
  theme_light()
  
# J and K has maximum deaths

df_india %>% 
  group_by(iyear, provstate) %>% 
  summarise(cnt = sum(nkill)) %>% 
  ungroup() %>% 
  ggplot(aes(x = iyear, y = cnt, color = provstate)) +
  geom_line() +
  geom_point() +
  labs(title = "# of deaths over the years", x = "year", y = "deaths") +
  theme_minimal() 
## J&K and Punjab show a couple of peaks on graph



# lets check for J&K
## addtile image - Thanks to Pranav Pandya fot this

df_india %>% 
  filter(provstate == "Jammu and Kashmir") %>%
  leaflet() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by 
           <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> 
           &mdash; 
           Map data &copy; 
           <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  addCircles(lat = ~latitude, lng = ~longitude, color = "#8B1A1A")

df_india %>% 
  filter(provstate == "Jammu and Kashmir") %>% 
  group_by(city) %>% 
  summarise(cnt = sum(nkill)) %>% 
  filter(cnt > 50) %>% 
  ggplot(aes(x = reorder(city,cnt), y = cnt, fill = city)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "# of deaths by city in J&K", x = "City", y = "Deaths") +
  geom_text(aes(label = cnt), hjust = -0.2) +
  coord_flip() +
  theme_minimal() 


# lets check for the attack_type for each incident in India

df_india %>% 
  group_by(attacktype1_txt) %>% 
  summarise(cnt = sum(nkill)) %>% 
  ggplot(aes(x = reorder(attacktype1_txt,cnt), y = cnt, fill = cnt)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Killings by attack type", x = "Attack type", y = "Deaths") +
  geom_text(aes(label = cnt), hjust = -0.2) +
  coord_flip() +
  theme_light()
## we do have hijacking incidents as well

# Did terrorists get success always?
df_india %>% 
  group_by(success) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(x = reorder(success,cnt), y = cnt/sum(cnt), fill = success)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Success ratio of terrorists attack", x = "Success or not", y = "Deaths") +
  geom_text(aes(label = round(cnt*100/sum(cnt),1)), vjust = -0.2) +
  theme_minimal()
## 98.2% of the attacks were successful

# Who were the targets?
head(df_india)

df_india %>% 
  group_by(targtype1_txt) %>% 
  summarise(cnt = sum(nkill)) %>% 
  ggplot(aes(x = reorder(targtype1_txt,cnt), y = cnt, fill = cnt)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Killings by target type", x = "Target type", y = "Deaths") +
  geom_text(aes(label = cnt), hjust = -0.2) +
  coord_flip() +
  theme_light()

## The target has always been innocent people

# check for subtargets

df_india %>% 
  group_by(targsubtype1_txt) %>% 
  summarise(cnt = sum(nkill)) %>% 
  filter(cnt > 100) %>% 
  ggplot(aes(x = reorder(targsubtype1_txt,cnt), y = cnt, color = cnt)) +
  geom_point(show.legend = FALSE) +
  geom_segment(aes(x = targsubtype1_txt, xend = targsubtype1_txt, y = 0, yend = cnt), show.legend = FALSE) +
  labs(title = "Killings by target sub-type", x = "Target sub-type", y = "Deaths") +
  geom_text(aes(label = cnt), hjust = -0.2) +
  coord_flip() +
  theme_minimal()

# Lets check for the groups behind the attacks over the years

df_india %>% 
  group_by(gname) %>% 
  summarise(cnt = sum(nkill)) %>% 
  filter(cnt > 100) %>% 
  ggplot(aes(x = reorder(gname,cnt), y = cnt, fill = cnt)) + 
  geom_bar(stat = "identity") +
  labs(title = "Groups and killings",  x = "Group", y = "deaths") +
  geom_text(aes(label = cnt), hjust = -0.2) +
  coord_flip() +
  theme_light()
## most of the killings are not related to any of the groups


# Source of the attacks over the years

df_india %>% 
  group_by(dbsource) %>% 
  summarise(cnt = sum(nkill)) %>% 
  filter(cnt > 100) %>% 
  ggplot(aes(x = reorder(dbsource,cnt), y = cnt, fill = dbsource)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Reporting sources",  x = "Group", y = "reported count") +
  geom_text(aes(label = cnt), hjust = -0.2) +
  coord_flip() +
  theme_minimal()




