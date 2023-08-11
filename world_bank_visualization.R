#World bank visualization
#author- Shefali c.
#Date- Aug 10, 2023

library(tidyverse)
library(showtext)
library(ggthemes)
library(gghighlight)
library(lubridate)
library(janitor)
library(patchwork)
library(ggtext)
library(MetBrewer)

#data prep
wb_data <- readr::read_csv(file = paste0(getwd(),"/data/world_bank_data/world_bank_development_indicators.csv"))

#renaming inflation column
wb_data <- wb_data %>% 
            rename(annual_inflation_perc = `inflation_annual%`)

keywords <- "dividend|income|Asia|Euro|OECD|small states|IDA|IBRD|Middle East|island|countries|Eastern|Western|Southern|world|Latin America|Sahara"


#filter out only legit countries 
countries_data <- wb_data[-grep(pattern = keywords, wb_data$country, ignore.case = T),]

#removing "Noth America" separately
countries_data <- countries_data[-grep(pattern  ="North America", countries_data$country,
                                       ignore.case = T),]

#find top 10 countries by GDP in 2020
top_10 <- countries_data %>%
            filter(lubridate::year(date) == 2022) %>% 
            #arrange(-GDP_current_US) %>% 
            slice_max(n = 9, order_by = GDP_current_US)

#select only country column, year column and inflation.
inflation_data <- countries_data %>% 
                    select(country, date, annual_inflation_perc) %>% 
                    mutate(year = lubridate::year(date)) %>% 
                    #remove the date column
                    select(-date) %>% 
                    #select only 2022 data
                    filter(year %in% c(2015,2016,2017,2018,2019,2020))

#5 countries with highest inflation in 2020
top5_inflation <- inflation_data %>% 
                    slice_max(n = 5, order_by = annual_inflation_perc)

#5 countries with lowest inflation in 2020
bottom5_inflation <- inflation_data %>% 
                        slice_min(n = 5, order_by = annual_inflation_perc)

#bind the top 5 and bottom 5 inflation countries in a dataframe
#data_inflation_plot <- dplyr::bind_rows(top5_inflation, bottom5_inflation)

#shuffle the rows in the above dataframe
#data_inflation_plot <- data_inflation_plot[sample(1:nrow(data_inflation_plot)),]

#INFLATION FROM 2015-2022 FOR TOP 10 COUNTRIES WITH HIGHEST GDP IN 2022
data_inflation_plot <- inflation_data %>% 
                          filter(country %in% top_10$country) %>% 
                          mutate(country = factor(country, levels = unique(country))
                          )

data_inflation_2020 <- data_inflation_plot %>% filter(year == 2020)

##Font settings; Plot theme settings

#font settings
font <- "Gudea"
font_add_google(family = font, name = font, db_cache = T)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)

#theme settings
theme_set(theme_minimal(base_family = font, base_size = 10))
bg <- "#F4F5F1"
txt_col <- "black"
showtext_auto(enable = TRUE)

#set caption of plot
caption_text  <- str_glue("**Design by:** Gilbert Fontana<br>","**Data: ** World Bank")



#Main Plot
(inflation_plot <- data_inflation_plot %>% 
                    ggplot()+
                    geom_hline(yintercept = 2, linetype = "solid", linewidth = 0.50)+
                    geom_point(data = data_inflation_2020, 
                               aes(x = year, y = annual_inflation_perc, color = country), shape = 16)+
                    geom_line(aes(x = year, y = annual_inflation_perc, color = country))+
                    gghighlight::gghighlight(use_direct_label = F, 
                                             unhighlighted_params = list(colour = alpha("grey85", alpha = 1)))+
                    #add annual inflation value in 2020 at the end of each line
                    geom_text(data = data_inflation_2020, aes(x = year, y = annual_inflation_perc, 
                                                              label = round(annual_inflation_perc,2), color = country),
                                      hjust = -0.5, vjust = -0.5, fontface = "bold", family = font, size = 2)+
                    scale_color_met_d(name = "Redon")+
                    coord_cartesian(clip = "off")+
                    scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8, 10, 12, 14, 16, 18),
                                       labels = c("","","2",rep("",8)))+
                    facet_wrap(~country)+
                    theme(
                      axis.title = element_blank(),
                      axis.text = element_text(size = 7, color = txt_col),
                      strip.text.x = element_text(face = "bold"),
                      plot.title = element_markdown(hjust=.5,size=34, color=txt_col,lineheight=.8, face="bold", margin=margin(20,0,30,0)),
                      plot.subtitle = element_markdown(hjust=.5,size=18, color=txt_col,lineheight = 1, margin=margin(10,0,30,0)),
                      plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=8, color=txt_col, lineheight = 1.2),
                      plot.caption.position = "plot",
                      plot.background = element_rect(color = bg, fill = bg),
                      plot.margin = margin(10,10,10,10),
                      legend.position = "none"
                    ))

##ADDING TTTLE AND SUBTITLES

#subtitle

subtitle_text <- tibble(x = 0, y = 0,
                   label = "The optimal inflation rate for any country is 2%. Source: Investopedia.com")

(subtitle <- ggplot(data = subtitle_text, aes(x = x, y = y))+
                    geom_textbox(aes(label = label), box.color = bg, fill = bg,
                                 width = unit(10, units = "lines"), family =font,
                                 size = 7, lineheight = 0.5)+
                    coord_cartesian(clip = "off", expand = F)+
                    theme_void()+
                    theme(plot.background = element_rect(fill = bg, color = bg))
)

#title
title_text <- tibble(x = 0, y = 0,
                     label = "Inflation of High GDP countries in 2020")

(title <- ggplot(data = title_text, aes(x = x, y = y, label = label))+
                geom_textbox(box.color = bg, fill = bg, width = unit(8, units = "lines"),
                             family = font, size = 7, lineheight = 0.5)+
                theme_void()+
                theme(plot.background = element_rect(fill = bg, color = bg))
)

##FINAL PLOT WITH TITLE AND SUBTITLE
(title + subtitle)/inflation_plot +
  plot_layout(height = c(0.5,2.5))+
  plot_annotation(
              caption = caption_text,
              theme = theme(
                plot.caption = element_markdown(hjust=0, margin=margin(20,0,0,0), 
                                                size=9, color=txt_col, lineheight = 0.8),
                plot.margin = margin(10,10,10,10)
                            )#theme
                  )#plot annotation

#save the image

ggsave(filename = "world_bank_inflation.png", 
       path = paste0(getwd(),"/images"),
       bg = bg)