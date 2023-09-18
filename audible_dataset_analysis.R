#Data Wrangling and EDA of Audible Dataset
#Author- Shefali C.
#Last Update- Sept 18, 2023

library(tidyverse)
library(lubridate)
library(janitor)

#the working directory
working_dir <- getwd()

#read data
audible_data <- readr::read_csv(paste0(working_dir, "/data/audible_dataset/audible_uncleaned.csv"))

#see first 5 rows of the dataframe
View(head(audible_data))

#See 5 random rows from the dataframe
View(audible_data[sample(nrow(audible_data),5),])

#some more information about the dataset
#All columns are of character type

#compact information 
glimpse(audible_data)

#detailed structure of the dataframe
str(audible_data)

#check for duplicate rows-- none found
any(duplicated(audible_data)) #FALSE

#remove duplicate rows if any
audible_data <- audible_data %>% dplyr::distinct()

#check for missing values in each column
#right now, no missing values. 
View(rbind(colSums(is.na(audible_data))))

#Since all cols are char type, check for string "NA"

sum(grepl("NA|na|Na|nA", audible_data$time, fixed = T))

#applying above to all columns
apply(audible_data, MARGIN = 2, sum(grepl("NA|na|Na|nA", .,fixed = T)))

count_occurances <- function(df){
  all_columns <- colnames(df)
  total_na <- list()
  for (i in 1:length(all_columns)) {
    total_count <- sum(grepl(pattern = "NA|na|Na|nA", audible_data[,all_columns[i]],
                          fixed = T))
    append(total_na, total_count)
  #df <- data.frame(colname = all_columns, count = total_na)
  #df
  }
}

t <- count_occurances(audible_data)