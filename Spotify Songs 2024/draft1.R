library(tidyverse)
#Last update- August 23, 2024

#data dir
data_dir <- paste0(getwd(), "/data/")

#guess encoding of the file
readr::guess_encoding(paste0(data_dir, 'Most Streamed Spotify Songs 2024.csv'))

#read dataset
data = readr::read_csv(paste0(data_dir,'Most Streamed Spotify Songs 2024.csv'),
                       locale = locale(encoding = "ISO-8859-1"))

## see all column-names
colnames(data)

#description
glimpse(data)
##OBSERVATIONS:
# 1. release data is in char format; should be date.

#clean names of columns
data <- janitor::clean_names(data)

##1. Check date format

#check for all symbols except digits- only "//" & NA present
unique(gsub(pattern = "\\d+", replacement = "", data$release_date))

##Identify date format- mm-dd-yyyy or dd-mm-yyyy--range is 1-12.
##first 2 digits represent months
unique(str_extract(string = data$release_date, pattern = "^\\d+(?=/)"))

##middle 2 digits.--range is from 1 to 31, indicating days in correct range
range(unique(as.integer(str_extract(string = data$release_date, 
                   pattern = "(?<=/)\\d+(?=/)"))))

##last two digits-- checking whether all year entries are valid or not
#all years are valid.
unique(as.integer(str_extract(string = data$release_date,
                   pattern = "(?<=/)\\d{2,4}$")))

#convert release-date column to detected format- dd/mm/yyyy
data$release_date <- lubridate::mdy(data$release_date)

#checking all columns and their types again
str(data)