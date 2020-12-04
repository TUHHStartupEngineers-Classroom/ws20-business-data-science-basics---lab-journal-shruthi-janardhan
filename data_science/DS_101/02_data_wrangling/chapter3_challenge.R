
# Load Libraries
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(httr)

# Challenge 1
resp <- GET("https://sheetlabs.com/IND/rv") 
list_API <- resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()

rig_veda_tbl <- as_tibble(list_API)
rig_veda_tbl %>% head(n= 10)

#Challenge 2

# Collect the category families
url_home   <- "https://www.rosebikes.com/bikes/kids"
xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
html_home  <- read_html(url_home)
# Web scrape the the categories of kids
bike_model <- html_home %>%
  
 html_nodes(css = ".product-tile.product-tile > a") %>%
 ## html_nodes(css = "#kinder_bikes_produckte") %>%
  html_attr("title") %>%
  enframe(name = "position", value = "model_name") 
  #as_tibble() %>%
  #rename(value = model_name)

bike_price <- html_home %>%
  #html_nodes(css = ".product-tile-price__current") %>%
  html_nodes(css = ".product-tile-price__current-value") %>%
  html_text() %>%
  
  str_remove_all(pattern = "\n") %>%
  str_remove_all(pattern = "\200") %>%
  as.numeric() %>%
  as_tibble()
  

bike_model<- mutate (bike_model,bike_price)
bike_model



  