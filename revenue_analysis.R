#Revenue Analysis of Retail Store Customer data
#Author- Shefali C.
#Last Updated- Aug 17, 2023

#Data Source- https://www.kaggle.com/datasets/ishanshrivastava28/tata-online-retail-dataset

library(tidyverse)
library(patchwork)
library(ggthemes)
library(janitor)
library(lubridate)

#read data
retail_data <- read_csv(paste0(getwd(),"/data/tata_retail_data/Online Retail Data Set.csv"))

## Data Wrangling

##Add date and date-time columns

#first convert the character date format to datetime
retail_data$date_time <- lubridate::dmy_hm(retail_data$InvoiceDate)
#now extract the date part from date-time value
retail_data$invoice_date <- lubridate::as_date(retail_data$date_time)

#remove the original 'InvoiceDate' column
retail_data <- subset(retail_data, select = -InvoiceDate)
#rearrange the columns
retail_data <- retail_data[,c("InvoiceNo", "StockCode", "invoice_date", 
                              "Description", "Quantity", "UnitPrice",
                              "CustomerID", "Country", "date_time")]

#Understanding date conversions
date_data <- retail_data %>% select(InvoiceNo, Description, InvoiceDate)



