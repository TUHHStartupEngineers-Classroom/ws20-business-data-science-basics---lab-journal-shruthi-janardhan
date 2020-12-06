library(tidyverse)
library(lubridate)
library(maps)
library(ggplot2)
library(scales)


covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
# Data Manipulation
covid_data_tbl_1 <- covid_data_tbl %>%
  
  distinct(cases, dateRep, countriesAndTerritories) %>% 
  filter(countriesAndTerritories == 'Germany' | 
           countriesAndTerritories == 'United_Kingdom' | 
           countriesAndTerritories == 'Spain' | 
           countriesAndTerritories == 'France' | 
           countriesAndTerritories == 'United_States_of_America') %>%
  mutate(date       = lubridate::dmy(dateRep)) %>% 
  mutate(date_floor = floor_date(date, unit = "month")) %>%
  arrange(date) %>%
  filter(cases>0) %>%
  group_by(countriesAndTerritories) %>%
  mutate(cum_sum_total = cumsum(cases)) %>%
  ungroup()
 # Data Visualization

date_label_text <- c("January","February","March","April","May","June",
                     "July","August", "September","October", "November", "December")

covid_data_tbl_1 %>%
  
  ggplot(aes(date, cum_sum_total, color = countriesAndTerritories)) +
  
  geom_line(size = 1, linetype =1 ) +
  
  scale_x_date(date_breaks = "1 month" , date_labels = date_label_text) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    prefix = "",
                                                    suffix = "M €")) +


labs(
  title = str_glue("COVID- 19 confirmed cases worldwide"),
  subtitle = str_glue(
    "As of 11/02/2020, Europe had more cases than the USA"),
  x = "Year 2020",
  y = "Cumulative Cases"

) +
  theme_minimal() +
  
   theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )) +
  theme(legend.position = "bottom")

# Challenge 2

 covid_data_tbl_2 <- covid_data_tbl %>%
   mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
   mutate(countriesAndTerritories = case_when(
     
     countriesAndTerritories == "United Kingdom" ~ "UK",
     countriesAndTerritories == "United States of America" ~ "USA",
     countriesAndTerritories == "Czechia" ~ "Czech Republic",
     TRUE ~ countriesAndTerritories )) %>%
      select (deaths,popData2019, countriesAndTerritories) %>%
 
     group_by(countriesAndTerritories) %>%
     mutate(total_deaths = sum(deaths)) %>%
     mutate(mortality_rate = total_deaths/popData2019) %>%
     distinct(countriesAndTerritories,popData2019,mortality_rate) %>%
     ungroup()
 
   world <- map_data("world") %>%
   select(lat,long,region) %>%
   distinct(lat,long,region)
 
   covid_data_tbl_2 <-  covid_data_tbl_2 %>%
   left_join(x = world , by = c("region" = "countriesAndTerritories"))
   
     covid_data_tbl_2 %>%
     ggplot() + 
     
     geom_map(aes(x = long,y = lat, map_id = region, fill = mortality_rate), map = world ) +
      
 
   
     labs(
       title = "Confirmed COVID- 19 Deaths relative to the size of the population",
       subtitle = "More than 1.2 Million confirmed COVID- 19 deaths worldwide",
       x = "",
       y = ""
     )
    
 

  
