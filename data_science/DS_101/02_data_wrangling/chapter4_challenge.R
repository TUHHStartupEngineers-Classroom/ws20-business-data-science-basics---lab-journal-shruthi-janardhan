library(tidyverse)
library(data.table)
library(vroom)

library(readr)
# import assignee.tsv
col_types <- list(
  id = col_character(),
  type = col_integer(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
assignee_tbl %>% glimpse()

# import patent_assignee.tsv
col_types_pa <- list(
  patent_id = col_character(),
  assignee_id= col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_pa,
  na         = c("", "NA", "NULL")
)

#class(assignee_tbl)
setDT(assignee_tbl)
setDT(patent_assignee_tbl)
#class(assignee_tbl)
#assignee_tbl %>% glimpse()
#patent_assignee_tbl %>% glimpse()
combined_data <- merge(x = assignee_tbl, y = patent_assignee_tbl,
                       by.x =   "id", by.y = "assignee_id",
                       all = FALSE)
combined_data %>% glimpse()

setkey(combined_data, "id")
key(combined_data)

keep_cols <- c("id",
               "type",
               "organization",
               "patent_id")
combined_data <- combined_data[, ..keep_cols]

#combined_data %>% glimpse()

c1_tb <- combined_data[type == 2, .N, by = organization][order(-N)]
c1_tb[1] # US Company with most number of patents
c1_tb <-c1_tb[1:10] # 10 US companies with most patents
write_rds(c1_tb, "C:/Users/Shruthi/Desktop/Documents/GitHub/ws20-business-data-science-basics---lab-journal-shruthi-janardhan/data_science/DS_101/02_data_wrangling/c1_tb")

#2
col_types_patent <- list(
  id = col_character(),
  type = col_skip(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_skip(),
  title = col_skip(),
  kind = col_skip(),
  num_claims = col_double(),
  filename = col_skip(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)

#patent_tbl %>% glimpse()

setDT(patent_tbl)
class(patent_tbl)

patent_combined_data <- merge(x = combined_data, y = patent_tbl, 
                       by.x    = "patent_id", by.y = "id",
                      all = FALSE)

patent_combined_data %>% glimpse()

setkey(patent_combined_data, "patent_id")
key(patent_combined_data)

keep_cols_1 <- c("patent_id",
                 "type",
                 "organization",
                 "country",
                 "date",
                 "num_claims",
                 "withdrawn")

patent_combined_data <- patent_combined_data[, ..keep_cols_1]
patent_combined_data %>% glimpse()

library(lubridate)
patent_combined_data[,date := lubridate::year(date)]
c2_tb <- patent_combined_data[!is.na(withdrawn) & type == 2 & date == 2019, .N, by = organization ][order(-N)]
c2_tb <- c2_tb[1:10]

write_rds(c2_tb, "C:/Users/Shruthi/Desktop/Documents/GitHub/ws20-business-data-science-basics---lab-journal-shruthi-janardhan/data_science/DS_101/02_data_wrangling/c2_tb")

# 3

col_types_uspc <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_integer()
)

uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)
#uspc_tbl %>% glimpse()
setDT(uspc_tbl)
class(patent_tbl)

uspc_combined_data <- merge(x = combined_data, y = uspc_tbl, 
                              by = "patent_id",
                              all = FALSE)
uspc_combined_data %>% glimpse()

setkey(uspc_combined_data, "patent_id")
key(uspc_combined_data)

keep_cols_2 <- c("patent_id",
                 "organization",
                 "mainclass_id",
                 "sequence")
uspc_combined_data <- uspc_combined_data[, ..keep_cols_2]
uspc_combined_data %>% glimpse()

c3_tb <- uspc_combined_data[sequence == 0 & !is.na(organization), .N, by = .(organization,mainclass_id)][order(-N)]
c3_tb <- c3_tb[1:10]
write_rds(c3_tb, "C:/Users/Shruthi/Desktop/Documents/GitHub/ws20-business-data-science-basics---lab-journal-shruthi-janardhan/data_science/DS_101/02_data_wrangling/c3_tb")


