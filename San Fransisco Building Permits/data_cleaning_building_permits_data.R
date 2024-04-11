#Data Cleaning- San Fransisco Building Permits 
#Last Update- Apr 10, 2024
#Author- Shefali C.


#LOAD LIBRARIES
library(tidyverse)
library(janitor)
library(readxl)
library(writexl)

#data folder
data_path <- paste0(getwd(),"/sf_building_data")

#read csv file
sf_data <- readr::read_csv(paste0(data_path, "/Building_Permits.csv"))

##DATA CLEANING STEPS

#1. Uniform column names
sf_data <- janitor::clean_names(sf_data)

#2. Missing Values

#Display the number of missing values stats as in column form

#creates a named integer list with column name & no. of NA values
total_na_values <-sapply(sf_data, function(y) sum(length(which(is.na(y)))))
#convert to dataframe
missing_data_summary <- data.frame(total_na_values)
#convert rownames to a column.
missing_data_summary <- tibble::rownames_to_column(missing_data_summary, "column_name")
#add a column with percentage of NA values in each column
missing_data_summary$percent_blank_rows <- round((missing_data_summary$total_na_values/nrow(sf_data))*100,1)

#arrange the dataframe in decreasing order of total missing values
missing_data_summary <- missing_data_summary %>% arrange(-total_na_values)


##Step 2.1) Remove columns with more than 90% blank rows.

#filter all columns with more than 80% blank rows
missing_data_summary %>% filter(percent_blank_rows > 80)

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

##Step 2b)
##filter columns with more than 50% and less than 80% blank rows
#Only 1, completed date: 51.1% blank rows
missing_data_summary %>% filter(percent_blank_rows>=50 & percent_blank_rows<=80)

##Step 3- Check for duplicate rows
#None found
sum(duplicated(sf_data)) # 0

##Step 4- Data type check.
#Check whether data type of column matches with the type of values
dplyr::glimpse(sf_data)

#display only character columns
glimpse(sf_data %>% select(where(is.character))) #21 columns

#display only float-type columns
glimpse(sf_data %>% select(where(is.double))) #14 columns

#display columns with dates
glimpse(sf_data %>% select(contains("date"))) # 7

###               WORKING ON EACH COLUMN GROUP ONE BY ONE

##------------------DATE COLUMNS---------------------------------------

# Fix Date Columns- check pattern, convert to date-type

#create a subset of only date columns
date_columns <- sf_data %>% select(contains("date"))

#Checking for all kinds of separators used inbetween mm-dd-yyyy
unique(str_extract_all(unlist(date_columns), pattern = "[^0-9]")) #only /


#Check for the pattern- 2 digits/2 digits/4 digits in all dates column

#returns TRUE in a cell where pattern found otherwise FALSE
dates_correct_format <- date_columns %>% 
  mutate(across(everything(), 
                ~str_detect(.,
                            pattern = "\\d{2}/\\d{2}/\\d{4}"
                )))


#checking number of false values in each date column--None
(false_date_formats <- colSums(!dates_correct_format, na.rm = T))

#Check whether first 2 digits fall within range 1 to 12 indicating month
unique(str_extract(unlist(date_columns), pattern = "^\\d+(?=/)"))

#checking range of middle 2 digits to be 1-31, indicating days
unique(str_extract(unlist(date_columns), pattern = "(?<=/)\\d+(?=/)"))

##Another long-method of checking the same as above, but column-wise.
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

#check whether last 4 digits are valid years or not--ALL GOOD
unique(str_extract(unlist(date_columns), pattern = "(?<=/)\\d+$"))

#Convert all date columns in sf_data to date data type
sf_data <- sf_data %>% 
  mutate(across(contains("date"), mdy))

##------------------COLUMNS WITH TYPE-DEFINITION PAIR.---------------------

#Objective- convert definition column values to factors.
#3 pairs of columns. 
definition_cols <- sf_data %>% select(contains('type'))

#check for unique values in each definition column

##PERMIT TYPE & DEFINITION
permit <- definition_cols %>% 
            select(permit_type, permit_type_definition) %>% 
            drop_na() %>% 
            distinct() %>% 
            arrange(permit_type)

#convert definition column to factor in the same order as permit type number.
permit$permit_type_definition <- factor(
                    permit$permit_type_definition,
                    levels = permit$permit_type_definition
)

#Convert permit definition column in sf_data to factors using levels set above
sf_data$permit_type_definition <- factor(
                        sf_data$permit_type_definition,
                        levels = permit$permit_type_definition
)

##EXISTING CONSTRUCTION TYPE & DESCRIPTION
existing_construction <- definition_cols %>% 
                          dplyr::select(contains('exist')) %>% 
                          tidyr::drop_na() %>%  
                          dplyr::distinct() %>% 
                          dplyr::arrange(existing_construction_type)

#convert the description column to factor
existing_construction$existing_construction_type_description <- 
  factor(existing_construction$existing_construction_type_description,
         levels = existing_construction$existing_construction_type_description)

#using levels above, convert the same column in sf_dataframe to factor
sf_data$existing_construction_type_description <- 
  factor(sf_data$existing_construction_type_description,
         levels = existing_construction$existing_construction_type_description)

##PROPOSED CONSTRUCTION TYPE & DESCRIPTION
proposed_construction <- definition_cols %>% 
                          dplyr::select(contains('propose')) %>% 
                          tidyr::drop_na() %>% 
                          dplyr::distinct() %>% 
                          dplyr::arrange(proposed_construction_type)

#convert the description column to factor
proposed_construction$proposed_construction_type_description <- 
  factor(proposed_construction$proposed_construction_type_description,
         levels = proposed_construction$proposed_construction_type_description)

#convert the same column in sf_data to factor using levels set above
sf_data$proposed_construction_type_description <- 
  factor(sf_data$proposed_construction_type_description,
         levels = proposed_construction$proposed_construction_type_description)

##-----------------------CHECKS FOR ALPHANUMERIC COLUMNS--------------

#checking cols which can be numeric but have 'char' datatype
alphanum_cols <- sf_data %>% 
                  select(permit_number,block,lot)

#checking whether each of these have only digits or alpha-numeric values
#check if characters apart from digits present or not.
#returns a dataframe with TRUE/FALSE in each cell
alphanum_cols_checks <- alphanum_cols %>% 
                        mutate(across(everything(),
                                      ~str_detect(.,
                                                  #checks for alphabets, 
                                                  #upper/lower cases
                                                  pattern = "[[:alpha:]]")))

#checking number of alphanumeric values in each column
colSums(alphanum_cols_checks, na.rm = T)

#checking for any other special characters in the columns
#pattern excludes digits and upper/lower alphabets-----> none found
unique(str_extract_all(unlist(alphanum_cols), 
                       pattern = "[^0-9a-zA-Z]"))

###-------------------LOCATION COLUMN---------------

#check for all characters in the column except digits.
#Only separators like decimal point, comma, hyphen for negative grids, spaces.
unique(str_extract_all(sf_data$location, 
                       pattern = "[^0-9]"))

#remove paranthesis and white spaces from all values
sf_data$location <- stringr::str_replace_all(sf_data$location,
                                             pattern = "[(|)\\s+]",
                                             replacement = "")

#split the column into longitude and latitude
sf_data <- sf_data %>% 
            tidyr::separate_wider_delim(cols = "location",
                                        delim = ",",
                                        names = c("longitude", "latitude"))

#checking for total missing values before conversion to float type
#1700 NA values in each
rbind(colSums(is.na(subset(sf_data, select = c(latitude, longitude)))))


#convert both columns to double type
sf_data <- sf_data %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

###----------------------------ZIPCODES----------------------------

#San Fransisco's zipcode lies between 94,102 to 94, 188. 
#Check for presence of Zipcodes beyond San Fransisco

summary(sf_data$zipcode) #max is 94,158

###--------------CURRENT STATUS COLUMN-------------------------------

#total missing values in this column--None
missing_data_summary %>% filter(column_name == "current_status")

#check for any non-alphabetic characters withing the values--NONE
any(str_detect(sf_data$current_status,
               pattern = "[^a-zA-Z]"))

#unique values
unique(sf_data$current_status) #14 unique terms

#convert Current status to factors
#levels given in alphabetic order.
sf_data$current_status <- factor(sf_data$current_status)

#----------------COST COLUMNS------------------
cost_data <- sf_data %>% 
              select(contains('cost'))

#basic stats about cost
summary(cost_data)

#checking rows where revised cost is 0
nill_revised_cost <- sf_data %>% 
                       filter(revised_cost == 0) %>% 
                       select(permit_number, 
                              permit_type_definition,
                              first_construction_document_date,
                              current_status,
                              existing_use,
                              estimated_cost,
                              description
                              )

#checking permit type in such observations--- Alterations. 
unique(nill_revised_cost$permit_type_definition)

#checking rows where estimated cost is below $10
estimated_cost_10 <- sf_data %>% 
                      filter(estimated_cost < 10) %>% 
                      select(permit_number, 
                             permit_type_definition,
                             first_construction_document_date,
                             current_status,
                             existing_use,
                             estimated_cost,
                             description
                      )

##--------------NEIGHBOURHOOD------------------------------

#quick view
glimpse(sf_data$neighborhoods_analysis_boundaries) #names

#total unique neighbours
length(unique(sf_data$neighborhoods_analysis_boundaries)) #42 unique values

#total number of observations for each unique neighborhood
neighbourhood_unique <- sf_data %>% 
  filter(!is.na(neighborhoods_analysis_boundaries)) %>% 
  count(neighborhoods_analysis_boundaries,
        name = "total") %>% 
  arrange(-total)