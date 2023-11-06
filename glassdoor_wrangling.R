#Data Cleaning- Glassdoor-Scraped Data Scientist Jobs
#Author- Shefali C.
#Last Update- Nov 6, 2023

library(tidyverse)
library(janitor)
library(lubridate)

#working directory
working_dir <- getwd()

#read the unclean csv file
data <- readr::read_csv(file.path(working_dir, 
                                  "data/DS_Job_Postings_Glassdoor/Uncleaned_DS_jobs.csv"))

#structure of the dataframe
str(data)

#to check attributes of a dataframe columns
#attr(data, "spec")

#View(data)

#remove spaces from column names
data <- janitor::clean_names(data)

#1. Remove 'index' column
data <- subset(data,select = -index)

#2. Column- job_title

#all unique Job Titles
unique_titles <- data.frame(job_title = unique(data$job_title))

#2. Simplify the job profiles
data <- data %>% 
  mutate(job_profile = case_when(
    str_detect(string = job_title,
               pattern = regex('manager', ignore_case = T)) ~ 'Manager',
    str_detect(string = job_title,
               pattern = regex("Business", ignore_case = T)) ~ 'Business Analyst',
    str_detect(string = job_title,
               pattern = regex("Model", ignore_case = T))~'Data Engineer',
    str_detect(string = job_title,
               pattern = regex("Data Analytics Engineer", ignore_case = T)) ~ 'Data Analyst',
    str_detect(string = job_title, 
               pattern = regex("(Data Scientist)|(Data Science)|(Decision Scientist)", ignore_case = T)) ~ 'Data Scientist',
    str_detect(string = job_title,
               pattern = regex("(Machine Learning)|ML|(Computer Vision)|(Deep Learning)|AI", ignore_case = T)) ~ 'Machine Learning Engineer',
    str_detect(string = job_title, 
               pattern = regex("(Data Engineer)|(Data Architect)", ignore_case = T)) ~ 'Data Engineer',
    str_detect(string = job_title,
               pattern = regex('Analyst|analysis', ignore_case = T)) ~ 'Data Analyst',
    str_detect(string = job_title,
               pattern = regex('[Computer|Computational] Scientist', ignore_case = T)) ~ 'Computer Scientist',
    TRUE ~ NA
  ))

# temp <- data.frame(job_title = data$job_title)
# 
# #create a column with main job profiles
# temp <- temp %>% 
#   mutate(job_profile = case_when(
#     str_detect(string = job_title,
#                pattern = regex('manager', ignore_case = T)) ~ 'Manager',
#     str_detect(string = job_title,
#                pattern = regex("Business", ignore_case = T)) ~ 'Business Analyst',
#     str_detect(string = job_title,
#                pattern = regex("Model", ignore_case = T))~'Data Engineer',
#     str_detect(string = job_title,
#                pattern = regex("Data Analytics Engineer", ignore_case = T)) ~ 'Data Analyst',
#     str_detect(string = job_title, 
#                pattern = regex("(Data Scientist)|(Data Science)|(Decision Scientist)", ignore_case = T)) ~ 'Data Scientist',
#     str_detect(string = job_title,
#                pattern = regex("(Machine Learning)|ML|(Computer Vision)|(Deep Learning)|AI", ignore_case = T)) ~ 'Machine Learning Engineer',
#     str_detect(string = job_title, 
#                pattern = regex("(Data Engineer)|(Data Architect)", ignore_case = T)) ~ 'Data Engineer',
#     str_detect(string = job_title,
#                pattern = regex('Analyst|analysis', ignore_case = T)) ~ 'Data Analyst',
#     str_detect(string = job_title,
#                pattern = regex('[Computer|Computational] Scientist', ignore_case = T)) ~ 'Computer Scientist',
#     TRUE ~ NA
#   ))

#seniorty keywords
#seniority_keywords <- c("Sr", "Senior", "Experienced", "Principal")

#3. Seniority level
data$senior <- ifelse(str_detect(data$job_title, 
                                 pattern = regex('Sr|Senior|Experienced|Principal|Lead|manager',
                                                 ignore_case = T)), 1, 0)

#4. Salary 
unique(data$salary_estimate)

#create new salary column and modify it
data$salary_in_1000_dollar <- data$salary_estimate


#remove the 'Glassdoor est.' part
data$salary_in_1000_dollar <- gsub(pattern = " \\(.*", replacement = "",
                             x = data$salary_in_1000_dollar)


#remove '$' and 'K'
data$salary_in_1000_dollar <- gsub(pattern = regex('\\$|K'), '', data$salary_in_1000_dollar)

#split the salary column using '-' as point of separation
data <- data %>% 
          tidyr::separate_wider_delim(cols = salary_in_1000_dollar,
                                      delim = "-",
                                      names = c("min_salary", "max_salary"))

#5. Remove numeric ratings from company names
data$company_name <- stringr::str_replace(
                          string = data$company_name,
                          pattern = regex(" -?[0-9].[0-9]"),
                          replacement = "")