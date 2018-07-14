
##############################################################
############ Make Over Monday - Rat Sightings ################
##############################################################
rm(list = ls())

load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(require(Matrix))
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(treemap))
  suppressPackageStartupMessages(require(highcharter))
}

load_lb()

# Load the dataset
rat <- read_excel("E:\\Study\\R Projects\\Common files\\Rat_Sightings.xlsx")

glimpse(rat)
# 111,972 X 28
head(rat)

## Checking the data quality

sort(sapply(rat, function(x) {sum(is.na(x))}), decreasing = TRUE)
# landmark is null: remove
# Intersection 1 & 2: remove
# cross street 1 & 2: remove
# status close data and created date: not proper format
# street name and incident address: not important
# unique key: can be taken out
# facility type: all N/A : remove

rat %>% 
  select('Park Facility Name') %>% 
  unique()
# just one type in 'Park facility name': remove

rat %>% 
  select(Descriptor) %>% 
  unique()
# only rat sighting: remove

rat %>% 
  select('Complaint Type') %>% 
  unique()
# only Rodent

rat %>% 
  select(Agency) %>% 
  unique()
# agency and agency name: only one

rem_col <- c('Unique Key','Park Facility Name','Descriptor','Complaint Type',
             'Agency','Agency Name','Landmark','Intersection Street 1',
             'Intersection Street 2','Cross Street 2','Cross Street 1','Closed Date',
             'Street Name','Incident Address','Facility Type','Resolution Action Updated Date',
             'Due Date')

rat_df <- rat %>% 
  select(-one_of(rem_col))
# removing the original dataset
rm(rat)

glimpse(rat_df)
rat_df$`Created Date` <- date(rat_df$`Created Date`)

# removing unknown lat and long
rat_df <- rat_df %>% 
  filter(!is.na(Latitude)) %>% 
  filter(!is.na(`Incident Zip`)) %>% 
  filter(!`Incident Zip` == "83", !`Incident Zip` == "10000")
sum(is.na(rat_df$Latitude))  
sum(is.na(rat_df$Longitude)) 
sum(is.na(rat_df$`Incident Zip`))

# lets plot on the map

rat %>% 
  select(Borough) %>% 
  unique() -> domain

# https://stackoverflow.com/questions/41533583/r-leaflet-adding-colors-for-character-variables
pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'purple', 'orange', 'white'),
  domain = domain$Borough
)

library(leaflet)
rat_df %>%
  leaflet() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by 
           <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> 
           &mdash; 
           Map data &copy; 
           <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  addCircles(lat = ~Latitude, lng = ~Longitude, color = ~pal(Borough),
             radius = 1, opacity = 0.1)

## it is too much

# checking the trend

library(scales)
rat_df %>% 
  group_by(`Created Date`) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(`Created Date`,cnt))+
  geom_point(color = "grey")+
  geom_smooth(color = "red")+
  scale_x_date(labels = date_format("%Y-%m-%d"))+
  labs(title = "Sighting's Trend (2010 - 2018)", x = "Date", y = "no. of sightings")
## sightings trend is going high
## There seems a seasonal trend in sightings

# monthly trend

rat_df %>% 
  group_by(mn = month(`Created Date`)) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(factor(mn),cnt))+
  geom_bar(stat = "identity", aes(fill = cnt))+
  geom_text(aes(label = cnt), vjust = -0.3)+
  labs(title = "Monthly Trend (2010 - 2018)", x = "Month", y = "no. of sightings")+
  theme_minimal()
## may and june !!

# Numbers by Borough

rat_df %>% 
  group_by(Borough) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(reorder(Borough,cnt),cnt))+
  geom_bar(stat = "identity", aes(fill = cnt), show.legend = FALSE)+
  geom_text(aes(label = cnt), hjust = 1, colour = "white")+
  coord_flip()+
  labs(title = "Monthly Trend (2010 - 2018)", x = "Month", y = "no. of sightings")+
  theme_minimal()
## Brooklyn and manhattan

# Number by Address Type
rat_df %>% 
  group_by(`Address Type`) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(reorder(`Address Type`,cnt),cnt))+
  geom_bar(stat = "identity", aes(fill = cnt), show.legend = FALSE)+
  geom_text(aes(label = cnt), hjust = -0.1, colour = "black")+
  coord_flip()+
  labs(title = "Trend by Address Type", x = "Address", y = "No.of sightings")+
  theme_minimal()


glimpse(rat_df)

# Number by City
rat_df %>% 
  group_by(City = toupper(City)) %>% 
  summarise(cnt = n()) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(City,cnt),cnt))+
  geom_bar(stat = "identity", aes(fill = cnt), show.legend = FALSE)+
  geom_text(aes(label = cnt), hjust = -0.1, colour = "black")+
  coord_flip()+
  labs(title = "Trend by City", x = "City", y = "No.of sightings")+
  theme_minimal()

# Number by location type
rat_df %>% 
  group_by(`Location Type`) %>% 
  summarise(cnt = n()) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(`Location Type`,cnt),cnt))+
  geom_point(stat = "identity", aes(fill = cnt), show.legend = FALSE, size =3)+
  geom_segment(aes(x = `Location Type`, xend = `Location Type`, y = 0, yend = cnt))+
  geom_text(aes(label = cnt), hjust = -0.1, colour = "black")+
  coord_flip()+
  labs(title = "Location Type",
       subtitle = "'3+ Family Apt. Building' type has reported maximum rodent sightings",
       x = "Location", y = "No.of sightings")+
  theme_minimal()

# Number by location type
rat_df %>% 
  group_by(`Location Type`) %>% 
  summarise(cnt = n()) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(`Location Type`,cnt),cnt))+
  geom_point(stat = "identity", aes(fill = cnt), show.legend = FALSE, size =3)+
  geom_segment(aes(x = `Location Type`, xend = `Location Type`, y = 0, yend = cnt))+
  geom_text(aes(label = cnt), hjust = -0.1, colour = "black")+
  coord_flip()+
  labs(title = "Location Type",
       subtitle = "'3+ Family Apt. Building' type has reported maximum rodent sightings",
       x = "Location", y = "No.of sightings")+
  theme_minimal()

# status check

rat_df %>% 
  group_by(Status) %>% 
  summarise(cnt = n()) %>% 
  top_n(10) %>% 
  ggplot(aes(Status,cnt))+
  geom_bar(stat = "identity", aes(fill = cnt), show.legend = FALSE)+
  geom_text(aes(label = cnt), vjust = -0.2, colour = "black")+
  labs(title = "What is the status trend?",
       subtitle = "Most of the cases are closed, but ~ 24K casesa are pending.",
       x = "Status", y = "No.of sightings")+
  theme_minimal()

library(stringr)
library(zipcode)
data("zipcode")
head(zipcode)
glimpse(zipcode)

rt_z <- rat_df %>% select(`Incident Zip`)
rt_z$`Incident Zip` <- str_pad(rt_z$`Incident Zip`,5,pad = "0")
colnames(rt_z) <- "zip"
rt_z <- merge(rt_z, zipcode, by = "zip", all.x = "TRUE")

rt_z %>% 
  group_by(zip,city,state, latitude, longitude) %>% 
  summarise(cnt = n()) -> rt_z

# validating using zipcode
rt_z %>% 
group_by(city) %>% 
  summarise(cnt = n()) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(city,cnt),cnt))+
  geom_bar(stat = "identity", aes(fill = cnt), show.legend = FALSE)+
  geom_text(aes(label = cnt), hjust = -0.1, colour = "black")+
  coord_flip()+
  labs(title = "Trend by City (zipcode package)", x = "City", y = "No.of sightings")+
  theme_minimal()

# ploting using city

rt_z %>% 
  select(city) %>% 
  unique() -> domain

pal <- colorFactor(
  palette = 'viridis',
  domain = domain$city
)

rt_z %>%
  leaflet() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by 
           <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> 
           &mdash; 
           Map data &copy; 
           <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  addCircles(lat = ~latitude, lng = ~longitude, color = ~pal(city),
             radius = ~cnt, opacity = 0.5)

# weekday trend
rat_df$week.day <- weekdays(rat_df$`Created Date`)

rat_df %>% 
  group_by(week.day) %>% 
  summarise(cnt = n()) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(week.day,cnt),cnt))+
  geom_bar(stat = "identity", aes(fill = cnt), show.legend = FALSE)+
  geom_text(aes(label = cnt), hjust = 1, colour = "white")+
  coord_flip()+
  labs(title = "Weekday trend of sightings",
       subtitle = "The reporting count is less over weekends",x = "Weekday", y = "No.of sightings")+
  theme_minimal()






