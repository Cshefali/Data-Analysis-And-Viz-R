GGplot2- Bubble Chart Variations
================
Shefali C.

Gapminder data has been used to create the following bubble charts.
Starting with the most basic bubble chart, plots progress to an
interactive version towards the end.  
Gapminder dataset contains GDP per capita, life expectancy, population
of countries from year 1952 to 2007.

``` r
#libraries required for manipulation and viz
library(dplyr)
library(ggplot2)
#library required for data
library(gapminder)
#virdis package for nice color palette
library(viridis)
#hrbrthemes for theme_ipsum() function
library(hrbrthemes)
#for interactive plots
library(plotly)
#for saving the interactive widget
library(htmlwidgets)
library(widgetframe)
```

``` r
#taking stats for year 2007
data <- gapminder::gapminder %>% 
  filter(year == 2007) %>% 
  select(-year)
```

Following is the basic version of a bubble chart, size of the bubbles
vary with the population of the country.

``` r
#Most basic bubble plot
ggplot(data = data, aes(x = gdpPercap, y = lifeExp, size = pop))+
  geom_point(color = "red", alpha = 0.5)
```

![](bubble_charts_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

In the following code:  
1. *`arrange(desc(pop))`*- ensures that countries get sorted in
descending order of population so that larger bubbles stay on top of the
chart.  
2. *`country = factor(country, country)`*- ensures that levels in the
country column are in the order of descending population.  
3. *`scale_size()`*- sets the size of smallest and largest bubbles.
*name* parameter sets the name of the legend.

``` r
#following code scales the size of bubbles based on population.
#in prev plot, not much difference between large vs small pop countries.
#scale_size() sets the size of biggest and smallest bubble. 
#in order to avoid bigger bubble overlapping smaller bubble, reordering has been done.

data %>% 
  arrange(desc(pop)) %>% 
  mutate(country = factor(x = country, levels = country)) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop)) + 
  geom_point(alpha = 0.7) + 
  #scale_size() scales area of the viz element, here bubble,
  #name parameter sets name of the legend.
  scale_size(range = c(0.1, 15), name = "Population (in Millions)")
```

![](bubble_charts_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The above plot can be further enhanced by adding color parameter based
on continent of the countries.

``` r
#Adding a 4th dimension- color representing Continent.
data %>% 
  arrange(desc(pop)) %>% 
  mutate(country = factor(x = country, levels = country)) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent))+
  geom_point(alpha = 0.7)+
  scale_size(range = c(0.1, 15), name = "Population (M)")
```

![](bubble_charts_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

*`Viridis`* package offers better palettes, here “rocket” has been
used.  
*`theme_ipsum()`* function of *hrbrthemes* package sets the plot theme
including size, font appearance, position etc of the titles, subtitles,
axis labels and so on.

``` r
#making the bubble chart prettier
data %>% 
  arrange(desc(pop)) %>% 
  mutate(country = factor(x = country, levels = country)) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, fill = continent, size = pop))+
  geom_point(alpha = 0.5, shape = 21, color = "black")+
  scale_size(range = c(0.1, 15), name = "Population (M)")+
  viridis::scale_fill_viridis(discrete = TRUE, option = "rocket", guide = "none")+
  hrbrthemes::theme_ipsum()+
  #theme(legend.position = "bottom")+
  xlab("GDP per capita")+
  ylab("Life Expectancy")+
  theme(legend.position = "none")
```

![](bubble_charts_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

In the following code:  
1. The 3 columns *lifeExp*, *pop* and *gdpPercap* has been rounded off
to fit in the tooltip of the interactive plot.  
2. A *text* column has been added to store the content of the tooltip.  
Remaining portion of the code is same as above.

``` r
#Making the bubble plot interactive
p <- data %>% 
      mutate(lifeExp = round(lifeExp, 1),
             pop = round(pop/1000000, 2),
             gdpPercap = round(gdpPercap, 0)) %>% 
      #reordering data to have big bubbles on top
      arrange(desc(pop)) %>% 
      mutate(country = factor(x = country, levels = country)) %>% 
      #creating text for tooltip in interative version
      mutate(text = paste("Country: ", country, "\nPopulation: ", pop,
                          "\nLife Expectancy: ", lifeExp, "\nGDP per capita: ", gdpPercap, sep = "")) %>% 
      
      #main plot code
      ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, fill = continent, text = text))+
      geom_point(shape = 21, color = "black", alpha = 0.5)+
      scale_size(range = c(0.1, 15), name = "Population(M)")+
      scale_fill_viridis(option = "rocket", guide = FALSE, discrete = TRUE)+
      theme_ipsum()+
      xlab("GDP per capita")+
      ylab("Life Expectancy")+
      theme(legend.position = "none")
```

*`ggplotly`* function of *plotly* package converts the simple plot above
into interactive type. The tooltip parameter takes in the column
containing content for the tip.

``` r
#interactive version
interactive_p <- plotly::ggplotly(p = ggplot2::last_plot(), tooltip = "text")
widgetframe::frameWidget(interactive_p)
```

<div id="htmlwidget-0b849f58bd6c9a481aaa" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-0b849f58bd6c9a481aaa">{"x":{"url":"bubble_charts_files/figure-gfm//widgets/widget_unnamed-chunk-8.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

``` r
#saving the widget
htmlwidgets::saveWidget(widget = interactive_p, 
                        file = paste0(getwd(),"/gdp_vs_life_expectancy.html"))
```
