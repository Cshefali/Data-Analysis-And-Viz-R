Palmer Penguins Dataset - Practice Analysis (EDA)
================
Shefali C.

## About the data:

-   The Palmer Penguins dataset is pre-loaded in Rstudio.This dataset
    was prepared by Dr. Kristen Gormon.  
-   Our penguins belong to 3 islands- *Dream*, *Biscoe*, *Torgersen*.
-   They belong to 3 species- *Gentoo*, *Chinstrip*, *Adelie*.
-   Data has been collected for 3 years- 2007, 2008 and 2009.
-   It contains the body measurements of these penguins: *flipper length
    (in mm)*, *body mass (in grams)*, *bill length (in mm)*, *bill depth
    (in mm)*.

## Why did I analyse this data?

Just for practice!

## Environment Setup

Setting up my environment by loading all relevant packages-
`tidyverse`,`ggplot2` (for visualization) & `palmerpenguins` dataset.

The dataset contains some missing values which will be omitted before
usage.

A basic theme that will be applied to all the plots has been stored in a
variable. All other components of theme layout specific to a particular
plot will be added as another layer on this basic layer.

### 1. From 2007 to 2009, which penguin species accounted for highest population in the 3 islands?

``` r
ggplot(data = clean_penguins_data,aes(x = species, fill =species)) + 
  geom_bar(width = 0.4) +
  geom_text(aes(label=..count..), vjust = 1.5,stat = "count", color = "black") +
  labs(title = "Count of Each Species in 3 Years", caption = caption_line, y = "Count of each species") +
  plot_theme +
  theme(legend.position = "none") + 
  labels_theme
```

![](eda_penguins_files/figure-gfm/penguin%20population-1.png)<!-- -->

***Adelie*** species of penguins account for highest population on all 3
islands combined, followed by ***Gentoo*** and then ***Chinstrap***.

### 2. Do penguin species co-habitate? Which species accounts for highest population on each island?

``` r
ggplot(data = clean_penguins_data,aes(x = species, fill = species)) + 
  geom_bar() +
  geom_text(aes(label = ..count..),vjust = 1.5,stat = "count",color = "black")+
  labs(title = "Count of Each Species per Island", caption = caption_line) +
  facet_wrap(~island) + plot_theme +  labels_theme +
  theme(axis.text.x = element_text(angle = 90),legend.position = "none")
```

![](eda_penguins_files/figure-gfm/penguin%20per%20island-1.png)<!-- -->

Yes!

-   **Biscoe Island**- Both *Adelie* and *Gentoo* species found on this
    island.
-   **Dream**- *Adelie* and *Chinstrap* penguins are found on this
    island.
-   **Torgersen**- Sadly, only **Adelie** penguins live on this island.
    :(

### 3. Population of male and female penguins per year.

``` r
ggplot(data = clean_penguins_data,aes(x = species, fill = sex)) +
  geom_bar() + 
  labs(title = "Population of Male and Female Penguins per Year", caption=caption_line, x="penguin species", y="total count")+
  plot_theme + labels_theme +
  theme(axis.text.x = element_text(angle=90))+
  facet_wrap(~year)
```

![](eda_penguins_files/figure-gfm/male%20female%20population-1.png)<!-- -->

Well, the gender ratio seems balanced among these folks (in this data,
of course. :\|)

### 4. Do penguins with higher body mass have longer flippers?

``` r
ggplot(data=clean_penguins_data, aes(x=body_mass_g, y=flipper_length_mm)) +
  geom_jitter(mapping = aes(color = species))+
  geom_smooth(color = "black")+
  labs(title="Body mass vs. flipper length", caption = caption_line, 
       x = "body mass(in grams)", y = "flipper length(in mm)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) + 
  labels_theme
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](eda_penguins_files/figure-gfm/flip%20length%20body%20mass-1.png)<!-- -->

More the body mass, longer are the flippers with ***Gentoo*** penguins
being the heaviest and with longest flippers. ***Adelie*** penguins are
the lightest of them all with smallest flippers. They must be cute!

##### Penguins_stats is a dataframe created to store the average body measurements of each penguin species.

``` r
penguins_stats <- clean_penguins_data %>% 
  group_by(species) %>% 
  drop_na() %>% 
  summarize(avg_bill_length = mean(bill_length_mm,2), avg_bill_depth = mean(bill_depth_mm,2),
            avg_flipper_length = mean(flipper_length_mm,2), avg_body_mass = mean(body_mass_g,2))
```

##### The stats are as follows:

``` r
head(penguins_stats)
```

    ## # A tibble: 3 x 5
    ##   species   avg_bill_length avg_bill_depth avg_flipper_length avg_body_mass
    ##   <fct>               <dbl>          <dbl>              <dbl>         <dbl>
    ## 1 Adelie               38.8           18.4                190          3700
    ## 2 Chinstrap            49.6           18.4                196          3700
    ## 3 Gentoo               47.4           15                  216          5050

### Average bill length of each species.

``` r
ggplot(data = penguins_stats,aes(x=species, y=avg_bill_length, fill=species)) +
  geom_bar(stat = "identity",width=0.3)+
  geom_text(aes(label=avg_bill_length), vjust = 1.5,stat = "identity", color = "black") +
  labs(title = "Average Bill Length(mm) of Each Species",caption = caption_line,
       y = "average bill length(mm)")+
  plot_theme + labels_theme +
  theme(legend.position = "none")
```

![](eda_penguins_files/figure-gfm/average_bill_length-1.png)<!-- -->

***Chinstrap*** penguins have the longest beak on an average, followed
by ***Gentoo*** and then ***Adelie***.

### Average bill depth of each species.

``` r
ggplot(data = penguins_stats, aes(x=species,y=avg_bill_depth,fill=species))+
  geom_bar(stat = "identity",width = 0.3)+
  geom_text(aes(label=avg_bill_depth), vjust = 1.5,stat = "identity", color = "black") +
  labs(title = "Average Bill Depth(mm) of Each Species", caption = caption_line,
       y = "average bill depth(mm)")+
  plot_theme + labels_theme +
  theme(legend.position = "none")
```

![](eda_penguins_files/figure-gfm/average_bill_depth-1.png)<!-- -->

So, ***Gentoo*** penguins have the deepest beaks. And the other 2 size
almost equal in terms of bill depth.

### Average flipper length of each species

``` r
ggplot(data=penguins_stats, aes(x=species,y=avg_flipper_length,fill=species))+
  geom_bar(stat = "identity",width = 0.3)+
  geom_text(aes(label=avg_flipper_length), vjust = 1.5,stat = "identity", color = "black")+
  labs(title="Average Flipper Length(mm) of Each Species",caption = caption_line,
       y = "average flipper length(mm)")+
  plot_theme + labels_theme +
  theme(legend.position = "none")
```

![](eda_penguins_files/figure-gfm/average_flipper_length-1.png)<!-- -->

As seen in the scatter plot above and even this chart, ***Gentoo’s***
have the longest beaks.

### Average body mass of each species

``` r
ggplot(data = penguins_stats, aes(x=species,y=avg_body_mass,fill=species))+
  geom_bar(stat = "identity",width = 0.3)+
  geom_text(aes(label=avg_body_mass), vjust = 1.5,stat = "identity", color = "black")+
  labs(title = "Average Body Mass(grams) of Each Species",caption=caption_line,
       y = "average body mass(grams)")+
  plot_theme + labels_theme +
  theme(legend.position = "none")
```

![](eda_penguins_files/figure-gfm/average_body_mass-1.png)<!-- -->

From the above charts, we can summarize the following:

-   **Adelie** penguins are the smallest- they are light, have short
    flippers and shortest beaks.
-   **Gentoo** penguins are the heaviest- they have the highest body
    mass, longest flippers, longest beaks and their beaks have higher
    depth compared to the other two.

##### gender_count stores the male and female population of each species per island.

``` r
gender_count <- clean_penguins_data %>% 
  group_by(species,island) %>% 
  drop_na() %>% 
  summarize(male = sum(sex == 'male'), female = sum(sex=='female'))
```

##### A quick peak into the wide-form of gender_count aggregation.

    ## # A tibble: 5 x 4
    ## # Groups:   species [3]
    ##   species   island     male female
    ##   <fct>     <fct>     <int>  <int>
    ## 1 Adelie    Biscoe       22     22
    ## 2 Adelie    Dream        28     27
    ## 3 Adelie    Torgersen    23     24
    ## 4 Chinstrap Dream        34     34
    ## 5 Gentoo    Biscoe       61     58

##### gender_count_long_form is just the same aggregation as earlier, converted to long format.

``` r
gender_count_long_form <- pivot_longer(gender_count, cols = c(male,female),names_to = "gender",values_to = "total_count")
```

#### A quick peak into the long-form of the same data.

``` r
head(gender_count_long_form)
```

    ## # A tibble: 6 x 4
    ## # Groups:   species [1]
    ##   species island    gender total_count
    ##   <fct>   <fct>     <chr>        <int>
    ## 1 Adelie  Biscoe    male            22
    ## 2 Adelie  Biscoe    female          22
    ## 3 Adelie  Dream     male            28
    ## 4 Adelie  Dream     female          27
    ## 5 Adelie  Torgersen male            23
    ## 6 Adelie  Torgersen female          24

The gender_count data was converted from wide format to long format in
order to plot male and female population of each species side by side
and not as a stacked bar chart.

### Plotting male and female count of each species separately.

``` r
ggplot(data = gender_count_long_form, aes(x=species,y=total_count,fill=gender))+
  geom_bar(stat = "identity",width = 0.4,position = "dodge")+
  labs(title="Male and Female Penguin Population per Species",caption = caption_line, x="penguin species", y="total count")+
  plot_theme + labels_theme
```

![](eda_penguins_files/figure-gfm/male_female_count_per_species-1.png)<!-- -->

The male to female ration is pretty much balanced among the 3 species of
penguins here.

### Plotting male and female population per species per island.

``` r
ggplot(data = gender_count_long_form, aes(x=species,y=total_count,fill=gender))+
  geom_bar(stat = "identity",width = 0.4,position = "dodge")+
  labs(title="Male and Female Penguin Population per Species",caption = caption_line, x="penguin species", y="total count")+
  plot_theme + labels_theme + facet_wrap(~island) +
  theme(axis.text.x = element_text(angle = 90))
```

![](eda_penguins_files/figure-gfm/male_female_count_per_island_per_species-1.png)<!-- -->

Lastly, this is the viz for male to female population of each species on
each island.

#### Here is the summary of the analysis:

-   **Torgersen** is the only island here with just 1 species
    population, i.e. *Adelie*.
-   **Gentoo** penguins are only found on *Biscoe* island.
-   **Gentoos** are the plump ones- the heaviest and largest, with
    longest beaks and flippers.
-   **Adelie** are the smallest and lightest with smallest beaks and
    flippers.
-   **Chinstrap** penguins have the longest beaks on average.
-   The male to female population on each island seems pretty much
    balanced.

###### THE END (for now :))
