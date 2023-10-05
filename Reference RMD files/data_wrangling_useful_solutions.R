#Some useful solutions for data manipulation.
#Author - Shefali C.
#Date- July 10, 2023 (start).
#Status- regular upates.

library(tidyverse)


##1. Filter out rows where 2 columns have duplicate values.
#Help- https://stackoverflow.com/questions/52507418/filter-to-all-rows-where-there-are-duplicate-values-in-two-columns-dplyr

#data prep
id <- c(1:10)
dob <- as.Date(c("1900-01-01", "1900-01-01", "1900-01-01", "1901-01-01", "1901-01-01", "1902-01-01", "1902-01-01", "1902-01-01", "1903-01-01", "1903-01-01"))
lname <- c("a", "b", "b", "c", "d", "e", "e", "f", "g", "h")
df <- data.frame("id" = id, "dob" = dob, "lname" = lname)

#See the dataframe, objective is to filter out rows 2, 3, 6 and 7.
#columns date of birth and lname have same values in (2,3) and (6,7)

result <- df[duplicated(df[,2:3]) | duplicated(df[,2:3], fromLast = T),]
View(result)

#-----------------------------------------------------------------------------

##2.Rows with missing values

col1 <- c(1:6)
col2 <- c("a","b","p",NA,"t",NA)
col3 <- c(12.10, 4.55, NA, 23.40, 98.7, 0)
df <- data.frame(col1 = col1, col2 = col2, col3 = col3)

#filter rows with missing values in any column
df_na <- df[!complete.cases(df),]

#-----------------------------------------------------------------------------

##3. Compare 2 columns and filter values not present in one or the other

#match() returns index postion of 'value1' of first vector in second vec.

#-----------------------------------------------------------------------------

##4. Print values in a list separated by comma
#HELP- https://stackoverflow.com/questions/6347356/creating-a-comma-separated-vector

#METHOD 1- all individual elements come under one quotation mark.
# "shef, avril, maya"
list1 = list("shef", "avril", "maya")
paste(list1, collapse = ", ")

#METHOD 2- individual elements get separated by comma and their own "".
# "shef", "avril", "maya"

cat(paste(shQuote(list1, type = "cmd"), collapse = ", "))

#----------------------------------------------------------------------

##5. 
