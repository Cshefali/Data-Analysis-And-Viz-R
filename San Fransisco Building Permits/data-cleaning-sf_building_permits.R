#Data Cleaning- San Fransisco Building Permits 
#Last Update- Apr 7, 2024
#Author- Shefali C.

library(tidyverse)
library(data.table)
library(janitor)
library(readxl)
library(writexl)


#data folder
data_path <- paste0(getwd(),"/sf_building_data")
#image folder path to save images
img_path <- paste0(getwd(),"/images/")

#read csv file
sf_data <- readr::read_csv(paste0(data_path, "/Building_Permits.csv"))

#make column names uniform
sf_data <- janitor::clean_names(sf_data)

#checking for missing values- represented as % of empty rows
#View(round((rbind(colSums(is.na(sf_data)))/nrow(sf_data))*100,2))

#Display the number of missing values stats as in column form

na_count <-sapply(sf_data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count <- tibble::rownames_to_column(na_count, "column_name")
#add a column with percentage of NA values in each column
na_count$percent_blank_rows <- round((na_count$na_count/nrow(sf_data))*100,1)

#make a copy of the dataframe
sf_data_copy <- sf_data


##Step 1: Remove columns with more than 90% blank rows.

#filter all columns with more than 80% blank rows
na_count %>% filter(percent_blank_rows > 80)

#REmove columns which have more than 80% NA values
sf_data <- sf_data %>% select(-c(street_number_suffix, #98.9% blank
                                 unit, #85.2% blank
                                 unit_suffix, #99% blank
                                 structural_notification, #96.5% blank
                                 voluntary_soft_story_retrofit, #100%
                                 fire_only_permit, #90.5%
                                 tidf_compliance, #100%
                                 site_permit) #97.3% blank
                              )#select 

##filter columns with more than 50% and less than 80% blank rows
#Only 1, completed date: 51.1% blank rows
na_count %>% filter(percent_blank_rows>=50 & percent_blank_rows<=80)

##Step 2- Check for duplicate rows
#None found
sum(duplicated(sf_data)) # 0

##Step 3- Check whether data type of column matches with the type of values
dplyr::glimpse(sf_data)

#display only character columns
glimpse(sf_data %>% select(where(is.character))) #21 columns

#display only float-type columns
glimpse(sf_data %>% select(where(is.double))) #14 columns

#display columns with dates
glimpse(sf_data %>% select(contains("date"))) # 7

###Column 1- Permit number

#check whether values are either numeric or alphanumeric.
alpha_num_permits <- sf_data[which(stringr::str_detect(sf_data$permit_number, 
                                                       pattern = "[A-Za-z]")),"permit_number"]

#extract only alphabets from permit number to understand the pattern
alpha_num_permits$alphabets <- stringr::str_extract_all(alpha_num_permits$permit_number,
                                                        pattern = "[A-Za-z]")

#unique alphabets
unique(alpha_num_permits$alphabets) #only 'M'

#observe is there any thing special in rows with permit number starting with M
#m_permit_numbers <- sf_data[which(str_detect(sf_data$permit_number, pattern = "M")),]

#look for other special characters in permit_number
any(str_detect(sf_data$permit_number, pattern = "[\\?$*^.,{}()#@]")) #None
#better-- check if any other character present except alphabets, digits.
any(str_detect(sf_data$permit_number, pattern = "[^a-zA-Z0-9]"))

###Column 2- Permit Type

#Objective- match numbers along with their definitions

#permit_types numerical representation with their description
permit_types <- sf_data %>% 
                  select(permit_type, permit_type_definition) %>% 
                  distinct() %>% 
                  arrange(permit_type)

#convert the permit type columns to factors
permit_types <- permit_types %>% 
                  mutate(
                         permit_type_definition = factor(permit_type_definition,
                                                        levels = permit_type_definition))

#convert permit_type in sf_data df to factors
sf_data$permit_type_definition <- factor(sf_data$permit_type_definition,
                                         levels = permit_types$permit_type_definition)

### Address Columns

address_data <- sf_data %>% select(block, lot, contains("street"))

glimpse(address_data)


#1. Block
glimpse(address_data$block)
sum(is.na(address_data$block))
#checking whether the column is only numeric or alphanumeric
any(stringr::str_detect(address_data$block, pattern = "[A-Za-z]"))
#filter rows with alphanumeric block num
alphanum_blocks <- sf_data[which(str_detect(sf_data$block,
                                            pattern = "[A-Za-z]")),
                           c("permit_number","block")]
#finding all unique alphabets in the block numbers
unique(str_extract(alphanum_blocks$block, pattern = "[A-Za-z]")) #A-G, T, Z

#checking for any other special characters in the block column
any(stringr::str_detect(sf_data$block, pattern = "[^a-zA-Z0-9]"))

#Lot
glimpse(sf_data$lot)
#missing values
sum(is.na(sf_data$lot)) #none
#checking whether column is alphanumeric
alphanum_lot <- sf_data[which(str_detect(
                      sf_data$lot,
                      pattern = "[A-Za-z]"
)),c("permit_number", "lot")]
#all unique alphabets in lot column
unique(str_extract(sf_data$lot,
                       pattern = "[A-Za-z]")) #many

#unique street name
unique(address_data$street_suffix)

#Date Columns

date_columns <- sf_data %>% select(contains("date"))
#checking for all unique separators in each date column

#Permit Creation Date
unique(str_extract_all(date_columns$permit_creation_date, 
                   pattern = "[^0-9]")) #only "/"

#Current Status Date
unique(str_extract_all(date_columns$current_status_date,
                   pattern = "[^0-9]")) #only "/"

#Filed Date
unique(str_extract_all(date_columns$filed_date,
                   pattern = "[^0-9]"))

#All date columns below either contain NA or have used only "/" as separator

#Issued Date--NA values present
unique(str_extract_all(date_columns$issued_date,
                   pattern = "[^0-9]")) #some columns contain "NA"
#checking which columns contain "NA" as character
#View(date_columns %>% filter(issued_date == "NA"))

#Completed Date
unique(str_extract_all(date_columns$completed_date,
                       pattern = "[^0-9]"))

#First Construction Document Date
unique(str_extract_all(date_columns$first_construction_document_date,
                       pattern = "[^0-9]"))

unique(str_extract_all(date_columns$permit_expiration_date,
                       pattern = "[^0-9]"))

##Check the arrangement of day,month, year in each column
##At first glance, pattern seems like- mm/dd/yyyy

#Permit Creation Date
# all(str_detect(date_columns$permit_creation_date,
#                pattern = "\\d{2}/\\d{2}/\\d{4}")) #TRUE
# 
# #Check whether the numbers fall in correct range or not
# #e.g. month shouldn't be less than 01 and greater than 12
# 
# #check month- all between 1 and 12
# #look-ahead regex used
# month_values <- as.integer(unique(str_extract(date_columns$permit_creation_date,
#             pattern = "\\d+(?=/)")))
# min(month_values)
# max(month_values)
# 
# #check day- all should be between 1 and 31
# day_values <- as.integer(
#   unique(str_extract(date_columns$permit_creation_date,
#                      pattern = "(?<=/)(\\d+)(?=/)"))
# )
# 
# #check min value
# min(day_values)
# max(day_values)
# 
# #checking years
# years_values <- as.integer(
#   unique(
#     str_extract(date_columns$permit_creation_date,
#                 pattern = "(?<=/\\d{2}/)\\d+")
#     )
#   )
# 
# #Years proceed from 2012 to 2018
# min(years_values)
# max(years_values)
# 
# #Now, we can convert this column to date format
# date_columns$permit_creation_date <- lubridate::mdy(date_columns$permit_creation_date)

#Current Status Date

#check for the pattern- 2 digits/2 digits/4 digits in all dates column
dates_correct_format <- date_columns %>% 
                        mutate(across(everything(), 
                                      ~str_detect(.,
                                                  pattern = "\\d{2}/\\d{2}/\\d{4}"
                                                  )))

#checking if all values return TRUE, implies dates follow correct pattern
all(dates_correct_format)
#checking number of false values in each date column--None
false_date_formats <- colSums(!dates_correct_format, na.rm = T)

#Checking whether first 2 digits reflect month or not
month_values <- date_columns %>% 
                  mutate(across(everything(),
                                ~str_extract(.,
                           pattern = "^\\d+(?=/)")
                  ))

#keep only unique values--#gives error because number of unique values different
#in different columns resulting in columns with different sizes
# month_values <- month_values %>% 
#                   mutate(across(everything(),
#                              ~unique(.)
#                   ))

#convert all columns to numeric
month_values <- month_values %>% 
                  mutate(across(everything(),
                                ~as.integer(.)))

#check min-max values in each column
month_range <- month_values %>% 
                summarise(across(everything(),
                                 list(min_value = ~min(.,na.rm = T),
                                      max_value = ~max(., na.rm = T)))) %>% 
                pivot_longer(everything(),
                             names_to = "stat",
                             values_to = "value")

#checking whether 'day' column contains values between 1 and 31 or not
day_values <- date_columns %>% 
                mutate(across(everything(),
                              ~str_extract(.,
                                           pattern = "(?<=/)\\d{2}(?=/)")))

#convert all day values to numeric
day_values <- day_values %>% 
                mutate(across(everything(),
                              ~as.integer(.)))

#checking min/max values in dates. They should fall between 01 to 31
#ALL GOOD
day_range <- day_values %>% 
              summarise(across(everything(),
                               list(
                                 min_value = ~min(., na.rm = T),
                                 max_value = ~max(., na.rm = T)
                               ))
                        ) %>% 
            pivot_longer(everything(),
                         names_to = "stat",
                         values_to = "value")

#checking all unique years in the dataframe
years_value <- date_columns %>% 
                mutate(across(everything(),
                              ~as.integer(str_extract(.,
                                                      pattern = "(?<=/)\\d+$"))))

#Unique years in each column
unique(years_value$permit_creation_date)
unique(years_value$current_status_date)
unique(years_value$filed_date)
unique(years_value$issued_date)
unique(years_value$completed_date)
unique(years_value$first_construction_document_date)
unique(years_value$permit_expiration_date)

#Convert all date columns in sf_data to date data type
sf_data <- sf_data %>% 
            mutate(across(contains("date"), mdy))

#Convert columns with numeric value-definition pair into factors.

##Existing Construction type

#total NA values
sum(is.na(sf_data$existing_construction_type)) #43366
sum(is.na(sf_data$existing_construction_type_description)) #43366

#All unique value and their definitions
existing_cons_type <- sf_data %>% 
                      select(existing_construction_type,
                             existing_construction_type_description) %>% 
                      distinct() %>% 
                      arrange(existing_construction_type)

#convert all types to factor
existing_cons_type <- existing_cons_type %>% 
                      mutate(existing_construction_type_description =
                               as.factor(existing_construction_type_description))

#convert the construction type in sf_Data to factor
sf_data$existing_construction_type_description <- factor(
  sf_data$existing_construction_type_description,
  levels = existing_cons_type$existing_construction_type_description
)

##Proposed Construction Type

#total na values
sum(is.na(sf_data$proposed_construction_type)) #43,162
sum(is.na(sf_data$proposed_construction_type_description)) #43,162

#all unique categories in these columns
proposed_cons_type <- sf_data %>% 
                        select(proposed_construction_type,
                               proposed_construction_type_description) %>% 
                        distinct() %>% 
                        arrange(proposed_construction_type)

#convert all proposed cons type categories to factors
proposed_cons_type <- proposed_cons_type %>% 
                        mutate(proposed_construction_type_description = 
                                 factor(proposed_construction_type_description))

#convert this column in sf_dataframe to factor
sf_data$proposed_construction_type_description <- factor(
  sf_data$proposed_construction_type_description,
  levels = proposed_cons_type$proposed_construction_type_description
)

##Fix Location column

#check for all special characters
unique(str_extract_all(sf_data$location,
                   pattern = "[^0-9]"))

#remove paranthesis
sf_data$location <- stringr::str_replace_all(sf_data$location,
                                             pattern = "[(|)]",
                                             replacement = "")
#checking whether any regular expression remains
any(str_detect(sf_data$location,
               pattern = "[(|)]"),
    na.rm = T)

#split the longitude and latitude into 2 columns
sf_data <- sf_data %>%
            tidyr::separate_wider_delim(cols = "location",
                                        delim = ",",
                                        names = c("longitude", "latitude"))