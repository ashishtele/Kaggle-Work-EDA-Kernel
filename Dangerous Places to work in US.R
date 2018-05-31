rm(list = ls())

pkgs <- c("readr", "data.table", "dplyr", "tidyr", "DT", "reshape2", "tm", "stringr", "gsubfn", "lubridate",
          "ggplot2", "gridExtra", "highcharter", "plotly", "ggrepel", "leaflet", "leaflet.extras", "ggmap", 
          "RColorBrewer", "viridisLite", "countrycode", "ggmap", "zipcode") 


for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages())))
  { install.packages(pkg) }
  require(pkg, character.only = TRUE)
}
rm(pkgs, pkg)

# load data

getwd()
df <- fread("E:\\Study\\R Projects\\Common files\\severeinjury.csv",
            na.strings = "",
            stringsAsFactors = FALSE,
            data.table = FALSE                         # false = data.frame
            )

glimpse(df)
# 21,578 X 26

df %>%
  head(10) %>%
  datatable(style = "bootstrap",
            class = "table-condensed",
            extensions = "Responsive")

# column names: ' ' has been changed to '_'
vas <- names(df)
vas <- gsub(' ','_',vas)
colnames(df) <- vas

df %>%
  select(c(EventDate,Employer,Zip,City,State, Longitude, Latitude,
           NatureTitle, Part_of_Body_Title, Hospitalized, Amputation,
           EventTitle, SourceTitle, Secondary_Source_Title, Final_Narrative)) %>%
  mutate(EventDate = mdy(EventDate)) -> df

head(df,4)

#### Zipcode package: to load zipcode based lat,long, city, state.
## can be pulled from govn site as well

data("zipcode")
head(zipcode)

latlong <- zipcode %>%
           rename(Zip = zip)
head(latlong)

library(stringr)
df_z <- df %>% select(Zip)
df_z$Zip <- str_pad(df_z$Zip,5,pad = "0")                  # making the format same
colnames(df_z) <- c("Zip")
head(df_z)
df_z <- merge(df_z,latlong, by = "Zip", all.x = TRUE)
head(df_z,10)

df$Longitude <- df_z$longitude
df$Latitude <- df_z$latitude
df$State <- df_z$state
df$City <- df_z$city

vas <- names(df)

df$Employer =gsub(".*usps|us postal|united states postal|u.s. postal|u.s postal|u. s postal|u. s. postal.*","US_Postal_Service", 
                  ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*US_Postal_Service.*","USPS", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*united parcel|ups |ups,.*","United_Parcel_Service", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*United_Parcel_Service.*","UPS", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*american airl.*","American Airlines", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*AT &|AT&.*","AT_T", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*AT_T.*","AT&T Inc", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*walmart|wallmart|wal-mart.*","wal_mart", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*wal_mart.*","Walmart", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*Publix.*","Publix_", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*Publix_.*","Publix", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*Asplundh.*","Asplundh_", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*Asplundh_.*","Asplundh", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*sodexo.*","sodexo_", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*sodexo_.*","Sodexo", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*Waste Management.*","Waste_Management", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*Waste_Management.*","Waste Management", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*Tyson Foods.*","Tyson_Foods", ignore.case = TRUE, df$Employer)
df$Employer =gsub(".*Tyson_Foods.*","Tyson Foods", ignore.case = TRUE, df$Employer)

# Loading palette

library(RColorBrewer)
display.brewer.all()

# count by employer

df %>% group_by(Employer) %>% 
  filter(Hospitalized != 0 || Amputation != 0) %>%
  summarise(cnt = n()) %>%
  arrange(-cnt) %>%
  top_n(10)-> emp

ggplot(emp, aes(x = reorder(Employer,-cnt), y = cnt, fill = Employer)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = cnt), vjust = -0.5) +
  labs(title = "Top 10 Employees (Danger)", y="Count", x = "Employer") +
  scale_fill_brewer(palette = 'YlOrRd') + 
  theme_minimal(base_size = 11) 

# count by City

df %>% group_by(City) %>% 
  filter(Hospitalized != 0 || Amputation != 0) %>%
  summarise(cnt = n()) %>%
  arrange(-cnt) %>%
  top_n(10)-> ct

ggplot(ct, aes(x = reorder(City,-cnt), y = cnt, fill = City)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = cnt), vjust = -0.5) +
  labs(title = "Top 10 Cities (Danger)", y="Count", x = "City") +
  scale_fill_brewer(palette = 'YlOrRd') + 
  theme_minimal(base_size = 11)

# count by state

df %>% group_by(State) %>% 
  filter(Hospitalized != 0 || Amputation != 0) %>%
  summarise(cnt = n()) %>%
  arrange(-cnt) %>%
  top_n(10)-> stat

ggplot(stat, aes(x = reorder(State,-cnt), y = cnt, fill = State)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = cnt), vjust = -0.5) +
  labs(title = "Top 10 States (Danger)", y="Count", x = "State") +
  scale_fill_brewer(palette = 'YlOrRd') + 
  theme_minimal(base_size = 11)


## State and Employer

df %>% group_by(Employer,State) %>% 
  filter(Hospitalized != 0 || Amputation != 0) %>%
  summarise(cnt = n()) %>%
  arrange(-cnt) %>%
  head(30) %>%
  plot_ly(x = ~Employer, y = ~State, z = ~cnt, color = ~cnt) %>%
  add_markers()

## By Source (hchart)

df %>% group_by(SourceTitle) %>% 
  filter(Hospitalized != 0 || Amputation != 0) %>%
  summarise(cnt = n()) %>%
  arrange(-cnt) %>%
  head(30) %>%
  hchart("pie", innerSize = '40%', showInLegend = F,
         hcaes(x = SourceTitle, y= cnt, color = -cnt)) %>%
  hc_add_theme(hc_theme_ffx()) %>%
  hc_title(text = "Top 30 Sources")

## Top injuries

df %>% group_by(NatureTitle) %>% 
  filter(Hospitalized != 0 || Amputation != 0) %>%
  summarise(cnt = n()) %>%
  arrange(-cnt) %>%
  head(10) %>%
  hchart("bar", innerSize = '40%', showInLegend = F,
         hcaes(x = NatureTitle, y= cnt, color = -cnt)) %>%
  hc_add_theme(hc_theme_economist()) %>%
  hc_title(text = "Top 10 injury type")


## Injury Body parts

df %>% group_by(Part_of_Body_Title) %>% 
  filter(Hospitalized != 0 || Amputation != 0) %>%
  summarise(cnt = n()) %>%
  arrange(-cnt) %>%
  head(10) %>%
  hchart("bar",
         hcaes(x = Part_of_Body_Title, y= cnt, color = -cnt)) %>%
  hc_add_theme(hc_theme_economist()) %>%
  hc_title(text = "Top 10 injured body parts")

## by events

df %>% group_by(EventTitle) %>% 
  filter(Hospitalized != 0 || Amputation != 0) %>%
  summarise(cnt = n()) %>%
  arrange(-cnt) %>%
  head(20) %>%
  hchart(type = "treemap",
         hcaes(x = EventTitle, y= cnt, color = cnt)) %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_title(text = "Top 20 Events")









