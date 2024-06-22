#RGraph Gallery- https://r-graph-gallery.com/web-lollipop-plot-with-R-the-office.html
#IMDB Ratings of Each Episode of The Office (US)
#Practice Date- Feb 2, 2024
#Author- Shefali C.

library(tidyverse)
library(cowplot)
library(showtext)
#package used to fetch and add external picture; here The Office logo
library(magick)
library(pdftools)

#following function indicates that showtext is being used to draw text in plot
showtext_auto()

working_dir <- getwd()
img_dir <- paste0(working_dir, "/images/")

#Add fonts from google
font_add_google("Roboto Mono", "Roboto Mono")
font_add_google("Open Sans", "Open Sans")
font_add_google("Special Elite", "Special Elite")

#Override the default theme and set the following, for all plots
ggplot2::theme_set(theme_minimal(base_family = "Roboto Mono"))
theme_update(
  plot.background = element_rect(fill = "#fafaf5", color="#fafaf5"),
  panel.background = element_rect(fill=NA, color=NA),
  panel.border = element_rect(fill = NA, color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 10),
  axis.ticks = element_blank(),
  axis.title.y = element_text(size = 13),
  legend.title = element_text(size = 9),
  plot.caption = element_text(
                  family = "Special Elite",
                  size = 10,
                  color = "grey70",
                  face = "bold",
                  hjust = 0.5,
                  margin = margin(t=5,r=0,b=20,l=0)
                  ),
  plot.margin = margin(10,25,10,25)
)

#Turn on showtext
showtext_auto()

#Read Dataset
df_office <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')


df_office_avg <- df_office %>% 
                  arrange(season, episode) %>% 
                  mutate(episode_id = dplyr::row_number()) %>% 
                  group_by(season) %>% 
                  mutate(
                    avg = mean(imdb_rating),
                    episode_mod = episode_id + (9 * season),
                    mid = mean(episode_mod)
                  ) %>% 
                  ungroup() %>% 
                  mutate(season = factor(season))

#dataframe to be used to draw vertical lines and horizontal lines connecting
#episodes and ratings change
df_lines <-
        df_office_avg %>% 
        group_by(season) %>% 
        summarize(
          start_x = min(episode_mod) - 5,
          end_x = max(episode_mod) + 5,
          y = unique(avg)
        ) %>% 
        pivot_longer(
          cols = c(start_x, end_x),
          names_to = "type",
          values_to = "x"
        ) %>% 
        mutate(
          x_group = if_else(type == "start_x", x + .1, x - .1),
          x_group = if_else(type == "start_x" & x == min(x), x_group - .1, x_group),
          x_group = if_else(type == "end_x" & x == max(x), x_group + .1, x_group)
        )

# First, horizontal lines that are used as scale reference. 
# They are added first to ensure they stay in the background.
p1 <- df_office_avg %>% 
  ggplot(aes(episode_mod, imdb_rating)) +
  geom_hline(
    data = tibble(y = 7:10),
    aes(yintercept = y),
    color = "grey82",
    linewidth = .5
  )


# Add vertical segments. 
# These represent the deviation of episode's rating from the mean rating of 
# the season they appeared.
(p2 <- p1 + 
  geom_segment(
    aes(
      xend = episode_mod,
      yend = avg, 
      color = season,
      color = after_scale(colorspace::lighten(color, .2))
    )
  )
)

# Add lines and dots.
# These represent the mean rating per season. 
# The dots mark each episode's rating, with its size given by the number of votes.
(p3 <- p2 + 
  geom_line(
    data = df_lines,
    aes(x, y),
    color = "grey40"
  )+
    geom_line(
      data = df_lines,
      aes(
        x_group, 
        y, 
        color = season, 
        color = after_scale(colorspace::darken(color, .2))
      ),
      linewidth = 2.5
    ) + 
    #dots for each episodes ratings. Size of dots varies with total votes.
    geom_point(
      aes(size = total_votes, color = season)
    )
)

#Adding labels and the grey vertical line connnecting all seasons' avg rating
(p4 <- p3 + 
    geom_label(
      aes(
        mid, 
        10.12, # vertical position of labels
        label = glue::glue(" Season {season} "),
        color = season, 
        color = after_scale(colorspace::darken(color, .2))
      ),
      fill = NA,
      family = "Special Elite",
      fontface = "bold",
      label.padding = unit(.2, "lines"),
      label.r = unit(.25, "lines"), # radius of the rounder corners.
      label.size = .5
    ) 
  
)

# Scale and labels customization.
# Override default colors with a much better looking palette.
(p5 <- p4 + 
    scale_x_continuous(expand = c(.015, .015)) +
    scale_y_continuous(
      expand = c(.03, .03),
      limits = c(6.5, 10.2),
      breaks = seq(6.5, 10, by = .5),
      sec.axis = dup_axis(name = NULL)
    ) +
    scale_color_manual(
      values = c("#486090", "#D7BFA6", "#6078A8", "#9CCCCC", "#7890A8", 
                 "#C7B0C1", "#B5C9C9", "#90A8C0", "#A8A890"),
      guide = "none" # don't show guide for the color scale.
    ) +
    scale_size_binned(name = "Votes per Episode", range = c(.3, 3)) +
    labs(
      x = NULL, 
      y = "IMDb Rating",
      caption = "Visualization by Cédric Scherer  •  Data by IMDb via data.world  •  Fanart Logo by ArieS"
    ) +
    guides(
      size = guide_bins(
        show.limits = TRUE,
        direction = "horizontal",
        title.position = "top",
        title.hjust = .5
      )
    ) +
    theme(
      legend.position = c(.5, .085), 
      legend.key.width = unit(2, "lines")
    )
  
)

#Adding 'The Office' logo
# The logo is located in the folder 'img' in the root of our project. 
# x and y coords run from 0 to 1, where (0, 0) is lower left corner of the canvas.
logo <- magick::image_read(paste0(img_dir,"the_office_series_logo.jpg"))
(p6 <- ggdraw(p5) +
  draw_image(logo, x = -.35, y = -.34, scale = .12))

#save image as PDF
ggsave(filename = "lollipop-plot-with-R-the-office.pdf",
       path = img_dir,
       width = 15, height = 9, device = cairo_pdf)

#save image as png
ggsave(filename = "lollipop-plot-with-R-the-office.png",
       path = img_dir,
       width = 15, height = 9, units = "in")