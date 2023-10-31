#Data Cleaning- Glassdoor-Scraped Data Scientist Jobs
#Author- Shefali C.
#Last Update- Oct 31, 2023

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

#keywords for job title
job_profile_keywords <- c("Data Analy", "Machine Learning", "Business Intelligence",
                          "Analytics", "Analysis")

#create a column with main job profiles
data <- data %>% 
          mutate(job_profile = case_when(
            job_title == ""
          ))

#seniorty keywords
seniority_keywords <- c("Sr", "Senior", "Experienced", "Principal")