#Data Visualization of World Bank data
#Author- Shefali C.
#Last updated- Aug 12, 2023

library(tidyverse)
library(ggthemes)
library(lubridate)
library(patchwork)
library(ggtext)
library(showtext)


#data prep
wb_data <- readr::read_csv(file = paste0(getwd(),"/data/world_bank_data/world_bank_development_indicators.csv"))

#renaming inflation column
# wb_data <- wb_data %>% 
#   rename(annual_inflation_perc = `inflation_annual%`)

#selecting only relevant columns
wb_data <- wb_data %>%
  select(country, date, population, population_density, GDP_current_US,
         human_capital_index, life_expectancy_at_birth, 
         `inflation_annual%`, `agricultural_land%`,
         `forest_land%`,`renewvable_energy_consumption%`,CO2_emisions,
         other_greenhouse_emisions,
         `research_and_development_expenditure%`,
         `military_expenditure%`,`government_expenditure_on_education%`,
         `government_health_expenditure%`
  )

#renaming columns
wb_data <- wb_data %>% 
            rename(inflation_perc = "inflation_annual%",
                   agri_land_perc = "agricultural_land%",
                   forest_land_perc = "forest_land%",
                   renewable_energy_consump_perc = "renewvable_energy_consumption%",
                   r_and_d_exp_perc = "research_and_development_expenditure%",
                   military_exp_perc = "military_expenditure%",
                   education_exp_perc = "government_expenditure_on_education%",
                   health_exp_perc = "government_health_expenditure%")

#add year column
wb_data$year <- lubridate::year(wb_data$date)

#remove rows from world data with following keywords to get only official countries
keywords <- "dividend|income|Asia|Euro|OECD|small states|IDA|IBRD|Middle East|island|countries|Eastern|Western|Southern|world|Latin America|Sahara"


#filter out only legit countries 
countries_data <- wb_data[-grep(pattern = keywords, wb_data$country, ignore.case = T),]

#removing "Noth America" separately
countries_data <- countries_data[-grep(pattern  ="North America", countries_data$country,
                                       ignore.case = T),]

#Filter out only World data
world_data <- wb_data[grep(pattern = "world", wb_data$country, ignore.case = T),]
#removing rows with "Arab World" in them
world_data <- world_data[-grep(pattern = "Arab World", world_data$country, ignore.case = T),]


##WORLD DATA VISUALIZATIONS

#line chart for population density.
ggplot(data = world_data, aes(x = year))+
  geom_line(aes(y = population_density), color = "#076FA1")+
  geom_line(aes(y = agri_land_perc), color = "darkgreen")+
  scale_y_continuous(limits = c(0,80),
                     breaks = seq(0,80,10),
                     labels = seq(0,80,10))+
  scale_x_continuous(limits = c(1960,2022),
                     breaks = seq(1960,2022,10),
                     labels = seq(1960,2022,10))+
  theme_fivethirtyeight()+
  theme(panel.grid = element_line(color = "white"),
        #panel.background = element_rect(fill = "white")
        )