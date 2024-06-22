#AirBnB prilimenary data exploration- round 1
#Last update- June 10, 2024
#Author- Shefali C.

library(tidyverse)
library(janitor)


#working directory
working_dir <- getwd()
#data directory
data_dir <- paste0(working_dir, "/data/")
#output directory
op_dir <- paste0(working_dir, "/output/")


#read data
#data <- readr::read_csv(paste0(data_dir, "Airbnb_Open_Data.csv"))
#check dtype of all columns
#glimpse(data) #license gets 'logical' datatype since all rows are NA except two.

#check warning message
#1 11116    26 1/0/T/F/TRUE/FALSE (expected); 41662/AL (actal value)
#2 72949    26 1/0/T/F/TRUE/FALSE (expected); 41662/AL (actal value)
#problems(data)

##26th column is license column; it is all NA except 2 rows above which are char
## read_csv() takes the first 1000 (default) rows of data and tries detecting
# datatype, here, since all rows were NA in license col in the sample,
#hence expected dtype becomes logical but since 2 chars encountered, warning thrown

##To avoid this warning, use `col_types` argument to explicitly specify dtype.
data2 <- readr::read_csv(paste0(data_dir, "Airbnb_Open_Data.csv"),
                        col_types = cols(license = col_character()))

#check dtype of all cols; now license is 'char' and the 2 license values are not lost
glimpse(data2)

#see the structure
str(data2)

#check for duplicate rows--541 rows
sum(duplicated(data2))

#see all duplicate rows--contains 1082 rows; 541 unique rows, each with 2 copies
duplicate_rows <- data2[duplicated(data2) | duplicated(data2, fromLast = T),]

#remove duplicate rows
data2 <- data2 %>% unique()

##find missing values in each column
missing_values <- data2 %>% 
                    summarise(across(everything(), ~ sum(is.na(.)))) %>% 
                    pivot_longer(everything(),
                                 names_to = "column_name",
                                 values_to = "total_NA") %>% 
                    #add a column indicating % of NA rows
                    mutate(percent_blank_rows = (total_NA/nrow(data2))*100) %>% 
                    #add a "%" sign after rounding off
                    mutate(percent_blank_rows = round(percent_blank_rows,2))

#License col can be removed as it has values in only 2 rows out of 102,600.
data2 <- data2 %>% select(-license)

##Clean column names
data2 <- janitor::clean_names(data2)

## Price columns

##1. column- 'price'; currently in char-type

#check for all characters in it except digits
## prices are either like '$100' or '$1,500'
unique(gsub(pattern = "\\d+", replacement = "", data2$price))

#check price values which contain "$,"
row_indices_price_comma <- grep(pattern = ",", data2$price)
price_wit_commas <- data2[row_indices_price_comma, c('host_id', 'name', 'price')]

#remove '$' and ',' symbols with ""
data2$price <- str_replace_all(data2$price, pattern = "[$,]",
                               replacement = "")

#convert price to numeric
data2$price <- as.numeric(data2$price)

## Column- 'Service Fee'; currently in char

#check for all unique characters-- prices only have '$' symbol
unique(gsub(pattern = "\\d+", replacement = "", data2$service_fee))

#replace '$' with ""
data2$service_fee <- gsub(pattern = '\\$', replacement = "",
                          data2$service_fee)
#convert to numeric
data2$service_fee <- as.numeric(data2$service_fee)

## Column- 'Last review'; contains dates but dtype is char.

#check the format of digits- dd/dd/dddd--returns false
all(grepl(pattern = "\\d{1,2}/\\d{1,2}/\\d{4}", data2$last_review))

#find row indices where this pattern is present in last-review column
correct_date_format_row_index <- grepl(pattern = "\\d{1,2}/\\d{1,2}/\\d{4}",
                                        data2$last_review)

#filter out rows where this pattern isn't present
data_incorrect_dates <- data2[!correct_date_format_row_index,
                              c('id', 'name', 'last_review')]

##check whether all last_reviews values are NA in data_incorrect_dates-- all are 0
sum(!is.na(data_incorrect_dates$last_review))

##Now check range of numbers in each component of date.
# If range(first 2 digits in all rows) is 1-12 => first 2 digits indicate month
# If range(middle 2 digtis) is 1-31 => days.

##Check first 2 digits
# range is 1-12
unique(str_extract(data2$last_review, pattern = "^\\d{1,2}(?=/)"))

#extract middle 2 digits
#range 1-31
summary(unique(as.integer(str_extract(data2$last_review, pattern = "(?<=^\\d{1,2}/)\\d{1,2}"))))

#So, 'last_review' column is in format- mm/dd/yyyy

#conver this column to date
data2$last_review <- lubridate::mdy(data2$last_review)

#check range of dates
summary(data2$last_review) ##- max value is "2058" which seems odd

#checking rows where year is greater than 2022
#the dataset's last update seems to be year 2022.
#there are 5 rows where last-review date is greater than 2022.
#Years are 2024, 2058, 2040... 
#Replace these years with '2022'

invalid_year_rows <- data2 %>% 
                      filter(year(last_review) > 2022) %>% 
                      select(id, name, last_review)

invalid_year_rows <- invalid_year_rows %>% 
                      mutate(last_review = case_when(
                        year(last_review) > 2022 ~ `year<-`(last_review, 2022),
                        TRUE ~ last_review
                      ))

#convert year values greater than 2022 to 2022
data2 <- data2 %>% 
          mutate(last_review = case_when(
            year(last_review) > 2022 ~ `year<-`(last_review, 2022),
            TRUE ~ last_review
          ))

#make a copy of dataframe so far
data3 <- data2

#Column- Host-identity-verified (unique values check)
#2 values- "unconfirmed", "verified"
unique(data3$host_identity_verified)

#find total percentage of verified/unconfirmed
#Proportion of confirmed identity/ unconfirmed identity is almost same at ~50%
data3 %>% group_by(host_identity_verified) %>% 
  summarise(total = n()) %>% 
  mutate(percent_share = round((total/nrow(data3)*100),2))

#Column- Cancellation policy
#3 categories: strict, moderate, flexible
unique(data3$cancellation_policy)

#check for proportion for each
#roughly same for all 3, ~33%
data3 %>% group_by(cancellation_policy) %>% 
  summarise(total = n()) %>% 
  mutate(percent_share = round((total/nrow(data3))*100,2))

#Column- Room type
#4 cats- "Private room", "Entire home/apt", "Shared room", "Hotel room"
unique(data3$room_type)

#Most listings are either Entire home/apt OR private room
data3 %>% group_by(room_type) %>% 
  summarize(total = n()) %>% 
  mutate(percent_share = round((total/nrow(data3))*100,2))


##------TEXT ANALYSIS OF house-rules COLUMN-----------
