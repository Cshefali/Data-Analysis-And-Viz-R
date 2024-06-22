##Analyse Layoffs data and build beautiful tables using formattable and DT
##Last Update- May 28, 2024
## Author- Shefali C.

library(tidyverse)
library(formattable)
library(DT)
library(lubridate)

#working directory
working_dir <- getwd()
#data directory
data_dir <- paste0(working_dir, "/data/")
#outputs directory
op_dir <- paste0(working_dir, "/output/")

#read data
data1 <- read_csv(paste0(data_dir, "layoffs.csv"))

##Check for NA values in each column
#rbind(colSums(is.na(data1)))

# Assuming your data frame is named 'data1'
#Returns table with missing values and column names.
missing_values <- data1 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "MissingValues")

#structure of dataframe
str(data1)

#check the min, max date in dataset
summary(data1$date) #Mach 11, 2023 to May 24, 2024
colnames(data1)

#1. Total number of layoffs by year---max in 2023;
layoffs_per_year <- data1 %>% 
                      group_by(year(date)) %>% 
                      summarise(total = sum(total_laid_off, na.rm = T))

#total number of unique companies--2582 unique companies.
length(unique(data1$company))


#2. Companies by maximum layoffs between 2020-2024
layoffs_per_company <- data1 %>% 
                        group_by(company) %>% 
                        summarise(total = sum(total_laid_off, na.rm = T)) %>% 
                        slice_max(n = 10, order_by = total)

#3. Layoffs per company in 2023 & 2024 so far.
#Amazon, Tesla, Google, Microsoft and Dell top the chart.
layoffs_company_2023_24 <- data1 %>% 
                            filter(year(date) %in% c(2023, 2024)) %>% 
                            group_by(company) %>% 
                            summarize(total = sum(total_laid_off, na.rm = T)) %>% 
                            slice_max(n = 15, order_by = total)

#4. Layoffs only in 2024--by month
#Maximum layoffs in January and then April
layoffs_2024_per_month <- data1 %>% 
                          filter(year(date) == 2024) %>% 
                          group_by(month = lubridate::month(date, label = T)) %>% 
                          summarize(total = sum(total_laid_off, na.rm = T))

#5. Find the trend in layoffs for companies with max layoffs in 2023-24

#create a list of those companies.
companies_with_max_layoffs <- layoffs_company_2023_24$company

#build a table with layoffs in these companies in each year
companies_per_year <- data1 %>% 
                      filter(company %in% companies_with_max_layoffs) %>% 
                      group_by(company, year = year(date)) %>% 
                      summarize(total = sum(total_laid_off, na.rm = T))

#companies with years in wide format
companies_per_year_wide <- companies_per_year %>% 
                            pivot_wider(names_from = year,
                                        values_from = total) 

#add a column with total
companies_per_year_wide <- companies_per_year_wide %>% 
                            mutate(total = rowSums(across(-vars(company))))

#Create a table of the above

#set colors
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"