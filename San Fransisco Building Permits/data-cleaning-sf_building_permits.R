#Data Cleaning- San Fransisco Building Permits 
#Last Update- Apr 6, 2024
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
View(date_columns %>% filter(issued_date == "NA"))

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
all(str_detect(date_columns$permit_creation_date,
               pattern = "\\d{2}/\\d{2}/\\d{4}")) #TRUE

#Check whether the numbers fall in correct range or not
#e.g. month shouldn't be less than 01 and greater than 12

#check month- all between 1 and 12
#look-ahead regex used
month_values <- as.integer(unique(str_extract(date_columns$permit_creation_date,
            pattern = "\\d+(?=/)")))
min(month_values)
max(month_values)

#check day- all should be between 1 and 31
day_values <- as.integer(
  unique(str_extract(date_columns$permit_creation_date,
                     pattern = "(?<=/)(\\d+)(?=/)"))
)

#check min value
min(day_values)
max(day_values)

#checking years
years_values <- as.integer(
  unique(
    str_extract(date_columns$permit_creation_date,
                pattern = "(?<=/\\d{2}/)\\d+")
    )
  )

#Years proceed from 2012 to 2018
min(years_values)
max(years_values)

#Now, we can convert this column to date format
date_columns$permit_creation_date <- lubridate::mdy(date_columns$permit_creation_date)
