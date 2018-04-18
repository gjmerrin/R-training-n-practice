# This script was created to help me learn essential R programs and play with scripts.

# Install packages

install.packages("tidyverse")

# Load tidyverse

library(tidyverse)

# Instal packages with datasets included i.e. data from airline flights, world development, and baseball

install.packages(c("nycflights13", "gapmind", "Lahman", "hexbin", "ggbeeswarm"))

# Get working directory

getwd()

# Info about the dataset

?mpg

# Creating a simple scatterplot

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# Creating a reusable template for making graphs - TEMPLATE 1

#ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# Adding 'jitter' to give a bit of noise to the plot - I LIKE THIS SCATTER PLOT 

ggplot(
  data = mpg
) +
  geom_point(
    mapping = aes(x = displ, y = hwy),
    position = "jitter"
)

ggplot(
  data = mpg
) +
  geom_jitter(mapping = aes(x = displ, y = hwy)
)


# Adding color to the scatterplot

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# Adding size differences

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# Make class variable transparent on different categories

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Make class variable different shapes based on categories - note - only six categories at a time more will go unplotted

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# Adding size and color differences

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class, color = class))

# Making points blue

ggplot(data = mpg) +
  geom_point(
    mapping = aes(
                  x = displ, 
                  y = hwy), 
              color = "blue")

# Using facet to plot a continuous variable by a categorical variable

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)

# Using facet grid to plot to categorical variables

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

# Smooth functions makes a curved line 

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# linetype adds an option to fit smoothed line by a categorical variable

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv))

# Removing legend

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv),
              show.legend = FALSE)

# Combinging the line and scatter plot and adding color to both
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv))+
  geom_point(mapping  = aes(x= displ, y= hwy, color = drv))

# HOWEVER look at the previous code... it is redundant. Instead we can 'pass' the same code across geoms

ggplot(data = mpg, mapping = aes(x = displ, y= hwy))+
  geom_point() +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y= hwy, color = drv))+
  geom_point() +
  geom_smooth()

# You can then extend or overwrite setting by specifying within geoms

ggplot(data = mpg, mapping = aes(x = displ, y= hwy))+
  geom_point(mapping = aes(color = class)) +
  geom_smooth()

# Same logic can be used to specify different data for each layer

ggplot(data = mpg, mapping = aes(x = displ, y= hwy))+
  geom_point() +
  geom_smooth(
    data = filter(mpg, class == "subcompact"),
    se = FALSE
  )

# Plot with colored lines and dots by drv class

ggplot(
  data = mpg,
  mapping = aes(x = displ, y= hwy, color = drv)
  )+
  geom_point() +
  geom_smooth(se = FALSE)

# SE adds the stanard bands - true is the default

ggplot(
  data = mpg,
  mapping = aes(x = displ, y= hwy, color = drv)
)+
  geom_point() +
  geom_smooth(se = TRUE)

# Basic bar chart

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut))

# Override count to proportion

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, y = ..prop.., group = 1)
    )

# Getting summary stats in a plot

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

ggplot(data = diamonds) +
  geom_boxplot(
    mapping = aes(x = cut, y = depth)
  )

# Coloring bar charts

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, color = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))

# Change the position of the stacked bars
 
ggplot(
  data = diamonds,
  mapping = aes(x = cut, fill = clarity)
) +
  geom_bar(alpha = 1/5, position = "identity")

ggplot(
  data = diamonds,
  mapping = aes(x = cut, color = clarity)
) +
  geom_bar(fill = NA, position = "identity")

# Keeping bars the same height

ggplot(data = diamonds) +
  geom_bar(
     mapping = aes(x = cut, fill = clarity), 
     position = "fill"
     )

# Put categories next to rather than stacked on to of -  I LIKE THIS BAR CHART!!!

ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = clarity),
    position = "dodge"
  )

# Flip coordinates

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()

# Adding polar coordinates

bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

# Adding a line 

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_abline()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_abline() + 
  coord_fixed()

# TEMPLATE 2

# ggplot(data = <DATA>) +
#<GEOM_FUNCTION>(
#   mapping = aes(<MAPPINGS>),
#   stat = <STAT>,
#   position = <POSItion>
#) +
#<COORDINATE_FUNCTION> +
#<FACET_FUNCTION>

# Getting sequences of numbers

seq(1, 10)

y <- seq(1, 10, length.out = 5)

y

# Will print if the entire command is in parentheses

(y <- seq(1, 10, length.out = 5))


library(nycflights13)
library(tidyverse)

# Look at data in a short form

flights

# View full data

View(flights)

# Main verbs include filter, arrange, select, mutate, summarize, and group_by

# Filter example

filter(flights, month == 1, day == 1)

jan1 <- filter(flights, month == 1, day ==1)

jan1

# Or wrap the assingment in parentheses (>, >=, <, <=, != (not equal), and == (equal))

(jan1 <- filter(flights, month == 1, day ==1))

(dec25 <- filter(flights, month == 12, day ==25))

# Boolean operations - & is "and", | is "or", and ! is "not"

# Flights departing November or December

filter(flights, month == 11 | month ==12)

# can use %in% to rewrite above code

nov_dec <- filter(flights, month %in% c(11, 12))
nov_dec

# De Morgan's law: !(x & y) is the same as !x | !y --- and !(x | y) is the same as !x & !y

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

# To determine if a value is missing 

is.na(nov_dec)

# To include missing values you have to ask for them

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

# Sandbox time

View(flights)

# Chapter 3
# 1a-g
filter(flights, arr_delay >= 120)
filter(flights, dest == "HOU" | dest == "IAH")
filter(flights, carrier == "UA" | carrier == "AA" | carrier == "DL")
filter(flights, month %in% c(7, 8, 9))
filter(flights, arr_delay > 120, !dep_delay)
filter(flights, dep_delay > 60, arr_delay <= 30)
filter(flights, dep_time >= 1200 | dep_time <= 600)
#2
filter(flights, between(dep_time, 600, 1200))

# Arrange and descending

arrange(flights, desc(arr_delay))

# Missing values always sorted at the end

df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

# Select columns with select - narrow in on variables of interest
#Select all columns by name 
select(flights, year, month, day)
#Select all columns between year and day
select(flights, year:day)
#Select all columns except those from year to day
select(flights, -(year:day))
# Other options include: starts_with, ends_with, contains, matches, num_range see ?select
# You can rename variables as well
rename(flights, tail_num = tailnum)
# Everything command moves specific variables to the start of the data set
select(flights, time_hour, air_time, everything())

vars <- c("year", "month", "day", "dep_delay", "arr_delay")

select(flights, contains("TIME"))

# Mutate variables

flights_sml <- select(flights,
  year:day,
  ends_with("delay"),
  distance,
  air_time
  )

mutate(flights_sml,
  gain = arr_delay - dep_delay,
  speed = distance / air_time*60
)

mutate(flights,
  gain = arr_delay - dep_delay,
  hours = air_time / 60,
  gain_per_hour = gain/hours
)

# Summarize

summarize(flights, delay = mean(dep_delay, na.rm = TRUE))

# Group by - get means by grouping variable 

by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))

# The Pipe

by_dest <- group_by(flights, dest)
by_dest
delay <- summarize(by_dest, 
  count = n(),
  dist  = mean(distance, na.rm = TRUE),
  delay = mean(arr_delay, na.rm = TRUE)
)
delay
delay <- filter(delay, count > 20, dest != "HNL")
delay

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
    geom_point(aes(size = count), alpha = 1/3) +
    geom_smooth(se = FALSE)

# Writing with pipes - read each pipe as "then"

delays <- flights %>%
  group_by(dest) %>%
  summarize(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest !="HNL")

flights %>% 
  group_by(year, month, day) %>% 
  summarize(mean = mean(dep_delay, na.rm = TRUE))

# Removing cancelled flights

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(mean = mean(dep_delay))

# Counts

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize(
    delay = mean(arr_delay)
  )
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) +
   geom_point(alpha = 1/10)

# Baseball example
# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)
batters <- batting %>% 
  group_by(playerID) %>% 
  summarize(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Subsetting 

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    # average delay:
    avg_delay1 = mean(arr_delay),
    # average positive delay:
    avg_delay2 = mean(arr_delay[arr_delay > 0])
  )

# Standard Deviation

not_cancelled %>% 
  group_by(dest) %>% 
  summarize(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

# Min and max values i.e. when do the first and last flights leave each day

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

# To count the nmber of non-missing values = sum(!is.na(x)) - 
# to count the number of distinct (unique) values n_distinct(x)

not_cancelled %>%
  group_by(dest) %>% 
  summarize(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    n_early = sum(dep_time < 500)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    hour_perc = mean(arr_delay > 60)
  )

# Sum add TRUES and mean gives proportions for dichotomous variables

daily <- group_by(flights, year, month, day)
(per_day <- summarize(daily, flights = n()))
(per_month <- summarize(per_day, flights = sum(flights)))
(per_year <- summarize(per_month, flights = sum(flights)))

# Ungrouping

daily %>% 
  ungroup() %>% 
  summarize(flights = n())

# Grouped mutates

flights_sml %>% 
  group_by(year, month, day) %>% 
  filter(rank(desc(arr_delay)) < 10)

# Find groups bigger than a threshold

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay/ sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

# Chapter 5

# Bar chart - categorical variables
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))

# Count of categories
diamonds %>% 
  count(cut)

# Histogram for continous variables
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat))

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat, 0.5))

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# Multiple histograms in one plot - geom_freqpoly() ****************************************************************

ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

# Looking for outliers

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  arrange(y)
unusual

# Drop

diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

# Replace value with missing value

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()

# Supress the warning

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(
    mapping = aes(color = cancelled),
    binwidth = 1/4
  )

# Plot adjustments

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# Density is the count standardized so that the area under each freq is one

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)
       ) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

# Box plots

ggplot(data = diamonds, mapping = aes(x = cut, y  = price)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median)
    )
  )

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median)
    )
  ) +
    coord_flip()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_jitter(
    mapping = aes(
      x = reorder(class, hwy, FUN = median)
    )
  ) 

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_jitter(
    mapping = aes(
      x = reorder(class, hwy, FUN = median)
    )
  ) +
  ggbeeswarm::geom_beeswarm(mapping = aes())

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>% 
  count(color, cut)

# Geom Tile

diamonds %>% 
  count(color, cut) %>% 
  ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = n))

# Two continuous variables

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

# Adding transparency if you have a lot of data points

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price),
             alpha = 1 / 100)

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price)) 

# Treat continous as categorical and bin values

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1), varwidth = TRUE))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

ggplot(data = faithful) +
  geom_point(mapping = aes(x = eruptions, y = waiting))

# Predicting price

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>%
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) +
  geom_boxplot(mapping = aes(x = cut, y = resid))












































