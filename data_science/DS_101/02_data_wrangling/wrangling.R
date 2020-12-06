library(tidyverse)
library(readxl)
# 1
bikes_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikes.xlsx") %>%
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))
# Column operations
bikes_tbl %>% select(bike_id, model, model_year) # select using column names

bikes_tbl %>% select(1:3) # select using indices

bikes_tbl %>% select(1, contains("model")) # select using select_helpers

bikes_tbl %>% select(model,price)

bikes_tbl %>% select(category_1:category_3, everything()) # Rearranging the columns by bringing category before everything

# Alternatively

bikes_tbl %>% relocate(category_1:category_3)
# select columns that start with model

bikes_tbl %>% select(contains("model"))
bikes_tbl %>%select(starts_with("model"))

# extract content from a tibble column
bikes_tbl %>%
  # select(price) %>% Does not work
  pull(price) %>%
  mean()

bikes_tbl %>%
  select(where(is.character))

bikes_tbl %>%
  select(where(is.numeric))

bikes_tbl %>%
  select(!where(is.numeric))
bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  rename(
    Model           = model,
    `Bike Family`   = category_1,
    `Ride Style`    = category_2,
    `Bike Category` = category_3,
    `Price in Euro` = price
  )
bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(c("Model", "Bike Family", "Ride Style", "Bike Category", "Price in Euro"))

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())

# Row operations

bikes_tbl %>%
  select(model, price) %>%
  arrange(desc(price)) %>%
  View()

# filtering rows
bikes_tbl %>%
  select(model, price) %>%
  filter(price > mean(price))

bikes_tbl %>%
  select(model, price) %>%
  filter((price > 5000) | (price < 1000)) %>%
  arrange(desc(price)) %>%
  View()

bikes_tbl %>%
  select(model, price) %>%
  filter(price > 5000,
         model %>% str_detect("Endurace")
  )

bikes_tbl %>%
  filter(category_1 %in% c("Hybrid / City", "E-Bikes"))

bikes_tbl %>%
  filter(category_2 == "E-Mountain")

bikes_tbl %>%
  filter(category_2 != "E-Mountain")

bikes_tbl %>%
  filter(!(category_2 %in% c("Hybrid / City", "E-Bikes")))

bikes_tbl %>%
  arrange(desc(price)) %>%
  slice(1:5)

bikes_tbl %>%
  arrange(price) %>%
  slice(1:5)

bikes_tbl %>%
  arrange(desc(price)) %>%
  slice((nrow(.)-4):nrow(.))

bikes_tbl %>%
  distinct(category_1)

bikes_tbl %>%
  distinct(category_1, category_2)

bikes_tbl %>%
  distinct(category_1, category_2, category_3)

# Column transformations ( feature based calculations)
bike_orderlines_tbl <- bikes_tbl

bike_orderlines_tbl %>%
  mutate(freight_costs = 2 * weight)

bike_orderlines_tbl %>%
  mutate(total_price = log(total_price))

# data.table
library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)
class(covid_data_dt)
# create a data.table
test_dt <- data.table(ID = c("b","b","b","a","a","c"),
                      a  = 1:6,
                      b  = 7:12,
                      c  = 13:18)

## FROM[WHERE, SELECT/ORDER BY/UPDATE, GROUP BY]

covid_data_dt[i, j, by]

# Example (filter by year, sum cases, group by continent)
covid_data_dt[year == 2019, sum(cases), by = continentExp]

covid_data_dt[countriesAndTerritories == "Germany" & 
                lubridate::month(dateRep, label = T, abbr = F) == "June"]

covid_data_dt[1:2]
# using - will sort in descending order
covid_data_dt[order(year, month, day, -countriesAndTerritories)]

covid_data_dt[,geoId]

covid_data_dt[,c("geoId", "countriesAndTerritories")]

covid_data_dt[,list(geoId)]
covid_data_dt[,.(geoId)]
covid_data_dt[,.(geoId, countriesAndTerritories)]
# Rename the columns
covid_data_dt[,.(CountryCode = geoId, country = countriesAndTerritories)]

select_cols = c("cases", "deaths")
covid_data_dt[, ..select_cols]

# ,, similar to one level up
colnames(covid_data_dt)
setnames(covid_data_dt, "dateRep", "date")
setnames(covid_data_dt, "countriesAndTerritories", "country")
setnames(covid_data_dt, "continentExp", "continent")

# Exercise 1
data()
data("airquality")
aq_dt <- data.table(airquality)
aq_dt[!is.na(Ozone), .(Solar.R, Wind, Temp)]

covid_data_dt[,sum(deaths > 1000)]
covid_data_dt[deaths > 1000]
# data.table lets you create column from within square brackets using the := operator. 
#This saves key strokes and is more efficient:
covid_data_dt[, deaths_per_capita := deaths / popData2019]

covid_data_dt[,  `:=`(deaths_per_capita = deaths / popData2019,
                      cases_per_capita = cases / popData2019,
                      deaths_per_cases = deaths / cases)]

# To delete a column, assign it to NULL
covid_data_dt[, deaths_per_cases := NULL]
covid_data_dt[,date := lubridate::dmy(date)]

# Exercise 2
data("mtcars")
mtcars_dt <- data.table(mtcars)
mtcars_dt[, mileage_type := ifelse(mpg > 20, 'high', 'low')]

covid_data_dt[country == "Germany" & month == 4, 
              .(m_cases = mean(cases), 
                m_death = mean(deaths)
              )
]

covid_data_dt[country == "United_States_of_America" & 
                month == 5 & deaths < 1000, 
              length(day)
]

covid_data_dt

covid_data_dt[deaths > 1000, .N, by = country]
covid_data_dt[,.I[deaths > 1000]]

covid_data_dt[continent == "Europe",
              .(mean(cases), mean(deaths)),
              by = .(country, month, year)
]

mtcars_dt <- data.table(mtcars)
library(magrittr) # to use the pipe
mtcars_dt[, .(.N, mileage = mean(mpg) %>% round(2)), by=gear]

covid_cases_means <- covid_data_dt[,.(m_cases  = mean(cases) %>% round(1), 
                                      m_deaths = mean(deaths) %>% round(1)), 
                                   by = .(country)
]

covid_data_dt[, .(
  m_cases  = round(mean(cases),  digits = 1), 
  m_deaths = round(mean(deaths), digits = 1)
), 
by = .(country)][order(-m_cases)]

covid_data_dt[, .N, by = .(death_gt_1k = deaths > 1000, cases_lt_1k = cases < 1000, year)
            
]

covid_data_dt[, print(.SD), by = year]

covid_data_dt[, lapply(.SD, mean), by = year]
covid_data_dt[, lapply(.SD, mean), 
              by = .(year, month), 
              .SDcols = c("cases", "deaths")
]
setkey(covid_data_dt, date, country)

covid_data_EUR_dt <- covid_data_dt[ continent == "Europe", 
                                    lapply(.SD, function(x) {
                                      x %>% 
                                        mean() %>% 
                                        round(1)
                                    }
                                    ), 
                                    by = .(country), 
                                    .SDcols = c("cases", "deaths")
]

# Set key
setkey(covid_data_EUR_dt, country)
key(covid_data_EUR_dt)

cd_dt1 <- covid_data_EUR_dt[, .(country, cases)]
cd_dt2 <- covid_data_EUR_dt[1:20, .(country, deaths)]

cd_dt1[cd_dt2]

# Remove keys
setkey(cd_dt1, NULL)
setkey(cd_dt2, NULL)
# Join
cd_dt1[cd_dt2, on = "country"]
# If they had different colnames
cd_dt1[cd_dt2, on = c(colA = "colB")]

# Alternatively you can use the function merge()
# Inner Join
merge(cd_dt1, cd_dt2, by='country')
# Left Join
merge(cd_dt1, cd_dt2, by='country', all.x = T)
# Outer Join
merge(cd_dt1, cd_dt2, by='country', all = T)
# If they had different colnames use by.x="colA", by.y="colB"

cd_dt1[cd_dt2, on = "country", deaths := i.deaths]