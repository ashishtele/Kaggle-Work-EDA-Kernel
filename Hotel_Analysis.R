################################################ Library Load ###################################################
rm(list = ls())
gc()

# Souring the function files
source(file="C:/Users/NWTeleAs/Desktop/R/ImpFunctions.R")

# Loading the required packages
load_lb()

# Importing the file

df <- read_excel('C:\\Users\\NWTELEAS\\Desktop\\First Day Documents\\cities.xlsx')
glimpse(df)

# No of Cities

df %>% 
  select(CityName) %>% 
  distinct() %>% 
  count()

## 42 Cities

# Get the unique list of hotels. It seems hotel address and hotel pincodes are not unique given the hotel name
# Hotel names are not unique either.
# we can fetch it a couple of ways. I am using the date to get the correct address.

df %>% 
  mutate(date_formated = as.Date(Date, "%b %d %Y")) %>%  
  filter(date_formated == "2016-12-18") -> unique_list_data


# Cities and no of hotels

unique_list_data %>% 
  group_by(CityName) %>% 
  summarise(cnt = n()) %>% 
  top_n(25) %>% 
  ggplot(aes(reorder(CityName,cnt),cnt))+
  geom_bar(stat = "identity", aes(fill = cnt), show.legend = FALSE)+
  geom_text(aes(label = cnt), hjust = -0.5, colour = "black")+
  coord_flip()+
  labs(title = "City Names with Hotel count",
       x = "City", y = 'No of Hotels')+
  theme_minimal()


cb(unique_list_data)
