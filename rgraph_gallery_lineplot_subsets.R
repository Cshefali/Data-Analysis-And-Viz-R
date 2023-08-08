#Infographic with subsets of Line chart
#Author- Shefali C.
#Last modified- Aug 8, 2023

#Source- https://r-graph-gallery.com/web-line-chart-small-multiple-all-group-greyed-out.html

library(tidyverse)
library(patchwork)
library(ggtext)
library(gghighlight)
library(janitor)
library(showtext)
library(MetBrewer)
library(scico)

#Load and prepare data
df1 <- read_csv("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/dataConsumerConfidence.csv")

#str(df1)

#Time column is in character format; will be converted to date-type
df2 <- df1 %>% 
        mutate(date = lubridate::my(Time)) %>% 
        select(-Time)

#convert data into long format
df2 <- df2 %>% 
        pivot_longer(cols = !date, names_to = "country", values_to = "value")

#remove NA
df2 <- df2 %>% drop_na()

#convert country column to factor with country names as levels
df2$country <- factor(df2$country, 
                      levels = c('USA','China','Japan','Germany', 'UK','France', 'Italy', 'South Korea', 'Australia'))


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
caption_text  <- str_glue("**Design:** Gilbert Fontana<br>","**Data:** OECD, 2022")

#this code takes the most recent date for all 9 countries.
#Note, in this grouped df, China has date Sep-2022, rest all have Oct-2022.
highest_values_data <- df2 %>% group_by(country) %>% slice_max(date)

##MAIN LINE CHART
(plot1 <- df2 %>% 
            ggplot() +
            geom_hline(yintercept = 100, linetype = "solid", linewidth= 0.25)+
            geom_point(data = highest_values_data, 
                       aes(x = date, y = value, color = country), shape = 16)+
            geom_line(aes(x = date, y = value, color = country))+
            gghighlight(use_direct_label = F,
                        unhighlighted_params = list(colour = alpha(colour = "grey85", alpha = 1)))+
            
            #this will add rounded values at the end of each line
            geom_text(data = highest_values_data,
                      aes(x= date, y = value, color = country, label = round(value)),
                      hjust = -0.5, vjust = -0.5, size = 2.5, family = font, fontface = "bold")+
            #Using color palette from metBrewer
            scale_color_met_d(name = "Redon")+
            scale_x_date(date_labels = "%y")+
            #create labels but only display 100 on Y-axis
            scale_y_continuous(breaks = c(90,95,100,105,110),
                               labels = c("","","100","",""))+
            
            #create subsets with each country
            facet_wrap(~country)+
            #in each facet, the value label was being clipped due to small size of facet.
            #prevent clipping by using the following line
            coord_cartesian(clip = "off")+
            theme(
              #remove the titles 'value' and 'date' from both axis
              axis.title = element_blank(),
              axis.text = element_text(size = 7, color = txt_col),
              #makes the title of each facet, i.e. country name bold
              strip.text.x = element_text(face = "bold"),
              plot.title = element_markdown(hjust=.5,size=34, color=txt_col,lineheight=.8, face="bold", margin=margin(20,0,30,0)),
              plot.subtitle = element_markdown(hjust=.5,size=18, color=txt_col,lineheight = 1, margin=margin(10,0,30,0)),
              plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=8, color=txt_col, lineheight = 1.2),
              plot.caption.position = "plot",
              plot.background = element_rect(color = bg, fill = bg),
              plot.margin = margin(10,10,10,10),
              legend.position = "none"
              #legend.title = element_text(face = "bold")
            ))

##ADD TITLE AND SUBTITLE

#SUBTITLE

#text to be used as subtitle
subtitle_text <- tibble(
                    x = 0, y = 0,
                    label = "The consumer confidence indicator provides an indication of future developments of households’ consumption and saving. An indicator above 100 signals a boost in the consumers’ confidence towards the future economic situation. Values below 100 indicate a pessimistic attitude towards future developments in the economy, possibly resulting in a tendency to save more and consume less. During 2022, the consumer confidence indicators have declined in many major economies around the world.<br>"
                  )

#customize the look of subtitle
subtitle <- ggplot(data = subtitle_text, aes(x = x, y = y))+
              geom_textbox(aes(label = label),
                           box.color = bg, fill = bg, width = unit(10, units = "lines"),
                           family = font, size =3, lineheight = 1)+
              coord_cartesian(expand = F, clip = "off")+
              theme_void()+
              theme(plot.background = element_rect(fill = bg, color = bg))

#TITLE

#text to be used as plot title
title_text <- tibble(
  x = 0, y = 0,
  label = "**Consumer Confidence Around the World**<br>"
)

#customize the look of title
title <- ggplot(title_text, aes(x = x, y = y)) +
            geom_textbox(
              aes(label = label),
              box.color = bg, fill=bg, width = unit(12, "lines"),
              family=font, size = 8, lineheight = 1
            ) +
            coord_cartesian(expand = FALSE, clip = "off") +
            theme_void() +
            theme(plot.background = element_rect(color=bg, fill=bg))

#FINAL PLOT WITH TITLE AND SUBTITLE

(title + subtitle)/plot1 +
  plot_layout(heights = c(1,2))+
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.caption = element_markdown(hjust=0, margin=margin(20,0,0,0), size=6, color=txt_col, lineheight = 1.2),
      plot.margin = margin(20,20,20,20)
    )
  )

#save the image

ggsave(filename = "consumer_confidence.png", bg = bg)