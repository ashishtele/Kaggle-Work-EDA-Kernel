

##############################################################
####################### Zomato ###############################
##############################################################
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

## Import the data files (zomato and country code)

df <- read_excel("E:\\Visual\\New folder\\zomato.xlsx")
head(df)
code <- read_excel("E:\\Visual\\New folder\\Country-Code.xlsx")
head(code)

## Merge the datasets to bring the country names 

df_combined <- df %>% 
  left_join(code, by = c("Country Code" = "Country Code"))

glimpse(df_combined)
# Dimension: 9545 X 22 

## lets modify few columns 

df_combined %>% 
  mutate(`Restaurant ID` = as.character(`Restaurant ID`),
         `Country Code` = NULL) -> df_combined
glimpse(df_combined)

## start with the important graph

unique(df_combined$Color)                  # 15 countries

library(leaflet)
df_combined %>% 
  leaflet() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by 
           <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> 
           &mdash; 
           Map data &copy; 
           <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') -> df_theme

df_theme %>% 
  addCircles(data = df_combined, lat = ~Latitude, lng = ~Longitude,
             color = df_combined$Color, fillOpacity = 0.6 ) -> leaf
leaf

## no duplicate 'rest_id'
sum(duplicated(df_combined$`Restaurant ID`))

## city wise # of restaurants

glimpse(df_combined)

df_combined %>% 
  group_by(City) %>% 
  dplyr::summarise(cnt = n()) %>% 
  arrange(-cnt) %>% 
  top_n(10) %>% 
  ggplot(aes( reorder(City,cnt), cnt, fill = cnt)) +
  geom_bar(stat = "identity", show.legend = FALSE)+
  labs(title = "# of Restaurants listed", x = "City", y = "Count")+
  geom_text(aes(label = cnt), hjust = -0.2)+
  coord_flip()
## seems most of the captured data is from India

## what about the country?

df_combined %>% 
  group_by(Country) %>% 
  dplyr::summarise(cnt = n()) %>% 
  arrange(-cnt) %>% 
  top_n(10) %>% 
  ggplot(aes( reorder(Country,cnt), cnt, fill = cnt)) +
  geom_bar(stat = "identity", show.legend = FALSE)+
  labs(title = "# of Restaurants listed by Country", x = "Country", y = "Count")+
  geom_text(aes(label = cnt), hjust = -0.2)+
  coord_flip()

## Lets focus on India 

df_combined %>% 
  filter(Country == "India") %>% 
  leaflet() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by 
           <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> 
           &mdash; 
           Map data &copy; 
           <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  addCircles(lat = ~Latitude, lng = ~Longitude, color = "springgreen")

## few coordinates are misplaced

# focus on Delhi,Noida and Gurgaon

glimpse(df_combined)

df_combined %>% 
  filter(City == "New Delhi" | City == "Gurgaon" | City == "Noida") %>% 
  group_by(Locality) %>% 
  dplyr::summarise(cnt = n()) %>% 
  arrange(-cnt) %>% 
  top_n(10) %>% 
  ggplot(aes( reorder(Locality,cnt), cnt, fill = cnt)) +
  geom_bar(stat = "identity", show.legend = FALSE)+
  labs(title = "# of Restaurants listed by Locality", x = "Locality", y = "Count")+
  geom_text(aes(label = cnt), hjust = -0.2)+
  coord_flip()

df_combined %>% 
  filter(City == "New Delhi" | City == "Gurgaon" | City == "Noida") %>% 
  filter(Latitude > 0 & Longitude > 0) %>% 
  filter(!Latitude == 35) %>% 
  leaflet() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by 
           <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> 
           &mdash; 
           Map data &copy; 
           <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  addCircles(lat = ~Latitude, lng = ~Longitude, color = "#03F", popup = ~Latitude)

## it looks few lan and lng are not populated correctly


# lets check variety of food

df_combined %>% 
  filter(City == "New Delhi" | City == "Gurgaon" | City == "Noida") %>% 
  filter(Latitude > 0 & Longitude > 0) %>% 
  filter(!Latitude == 35) %>% 
  group_by(Cuisines) %>% 
  dplyr::summarise(cnt = n()) %>% 
  arrange(-cnt) %>% 
  top_n(10) %>% 
  ggplot(aes( reorder(Cuisines,cnt), cnt, color = Cuisines)) +
  geom_point(show.legend = FALSE)+
  geom_segment(aes(x = Cuisines, xend = Cuisines, y = 0, yend = cnt), show.legend = FALSE)+
  labs(title = "# of Restaurants listed by Locality", x = "Locality", y = "Count")+
  geom_text(aes(label = cnt), hjust = -0.2)+
  coord_flip()

## Of course!! North indian is preferred one




