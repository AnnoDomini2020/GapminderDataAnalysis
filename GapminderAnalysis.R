#### Heading ####
# "Gapminder Foundation data analysis,"
# by Andrew Infantino
# Spring 2020 Lesson & Assignment Material Compilation
# HarvardX Data Science Professional Certificate Program
# Compiled 26 June 2021

#### Libraries and Data Load ####
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
data(gapminder) # From library(dslabs)

#### Assessment Visualizations and Exploratory Analysis ####
# Fertility vs. life expectancy in Africa:
gapminder %>% filter(year == 2012 & continent  == "Africa") %>%
  ggplot(aes(fertility, life_expectancy, color = region)) +
  geom_point() +
  ggtitle("Fertility vs. Life Expectancy in Africa (2012)") + 
  ylab("Life Expectancy in Years") +
  xlab("Average Number of Children per Woman")

# List of African countries with high life expectancy and low fertility:
gapminder %>%
  filter(year == 2012 & 
           continent == "Africa" & 
           fertility <= 3 & 
           life_expectancy >= 70) %>%
  select(country, region)

# How did the Vietnam War affect life expectancy in US and Vietnam?
gapminder %>%
  filter(year %in% 1960:2010 & 
           country %in% c("Vietnam", "United States")) %>% 
  ggplot(aes(year, life_expectancy, color=country)) + 
  geom_line() +
  ggtitle("Life Expectancy in Vietnam and USA (1960-2010)") +
  xlab("Year") +
  ylab("Life Expectancy in Years")

# How did Pol Pot's regime affect vital stats in Cambodia?
gapminder %>% 
  filter(year %in% 1960:2010 & 
           country == "Cambodia") %>% 
  ggplot(aes(year, life_expectancy)) + 
  geom_line() +
  ggtitle("Life Expectancy in Cambodia (1960-2010)") +
  xlab("Year") +
  ylab("Life Expectancy in Years")

# Daily income distribution in Africa, 2010:
gapminder %>%
  filter(year == 2010 & 
           continent == "Africa" & 
           !is.na(gdp)) %>%
  mutate(dollars_per_day = gdp/population/365) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_density() +
  scale_x_continuous(trans = "log2") + # Logarithmic transformation
  ggtitle("Distribution of Mean National Daily Income in Africa (2010)") +
  xlab("Dollars per Day") +
  ylab("Density")

# Daily income in Africa, 1970 vs. 2010: (Grid)
gapminder %>%
  filter(year %in% c(1970, 2010) & 
           continent == "Africa" & 
           !is.na(gdp)) %>%
  mutate(dollars_per_day = gdp/population/365) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_density() +
  facet_grid(year ~ .) +
  scale_x_continuous(trans = "log2") + 
  ggtitle("Distribution of Mean National Daily Income in Africa (1970 vs. 2010)") +
  xlab("Dollars per Day") +
  ylab("Density")

# Daily income in Africa, 1970 vs. 2010: (Stacked Regional Distributions)
gapminder %>%
  filter(year %in% c(1970, 2010) & continent == "Africa" & !is.na(gdp)) %>%
  mutate(dollars_per_day = gdp/population/365) %>% 
  ggplot(aes(dollars_per_day, fill = region)) +
  geom_density(bw = 0.5, position = "stack") +
  facet_grid(year ~ .) +
  scale_x_continuous(trans = "log2") +
  ggtitle("Distribution of Mean National Daily Income in Africa (1970 vs. 2010)") +
  xlab("Dollars per Day") +
  ylab("Density")

# Scatter plot, income vs. infant mortality by region in Africa:
gapminder %>%
  filter(year == 2010 & continent == "Africa" & !is.na(gdp)) %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
  geom_point() +
  scale_x_continuous(trans = "log2") +
  ggtitle("National Daily Income vs. Infant Mortality in Africa (2010)") +
  xlab("Dollars per Day") +
  ylab("Infant Deaths per 1000")

# Income vs. infant mortality by region in Africa: (Countries Labelled)
gapminder %>%
  filter(year == 2010 & continent == "Africa" & !is.na(gdp)) %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_text() +
  scale_x_continuous(trans = "log2") +
  ggtitle("National Daily Income vs. Infant Mortality in Africa (2010)") +
  xlab("Dollars per Day") +
  ylab("Infant Deaths per 1000")

# Income vs. infant mortality by region in Africa: (Countries Labelled, 1970 vs. 2010)
gapminder %>%
  filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(gdp) & !is.na(infant_mortality)) %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_text() +
  facet_grid(year ~ .) +
  scale_x_continuous(trans = "log2") +
  ggtitle("National Daily Income vs. Infant Mortality in Africa (1970 vs. 2010)") +
  xlab("Dollars per Day") +
  ylab("Infant Deaths per 1000")

#### Visualization Lesson: Principles & Practice ####
# Scatterplots are good for relating different variables in large datasets. Slope plots can be better for comparing common variables over time in small datasets.
# Compare life expectancy between Western countries from 2010 to 2015:
west <- c("Western Europe", "Northern Europe", "Southern Europe",
          "Northern America", "Australia and New Zealand")
gapminder %>%
  filter(year %in% c(2010, 2015) & 
           region %in% west &
           !is.na(life_expectancy) & 
           population > 10^7) %>%    
  mutate(location = ifelse(year == 2010, 1, 2),               # Spatially separate 2010 and 2015 values across countries.
         location = ifelse(year == 2015 & country %in%        # UK & Greece, Portugal & Germany overlap in 2015.
                             c("United Kingdom", "Portugal"), # Spatially separate them.
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust),
            show.legend = FALSE) +
  xlab("Year") +
  ylab("Life Expectancy") +
  ggtitle("Western Life Expectancy Trends from 2010 to 2015")

# The above format loses utility when too many points clutter the graph. The Bland-Altman or Tukey mean difference plot compares differences to averages 
# of two measures. Let's apply it to the dataset above. Note that we don't have to move any entries on the graph:
gapminder %>%
  filter(year %in% c(2010, 2015) & 
           region %in% west &
           !is.na(life_expectancy) & 
           population > 10^7) %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>%
  spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010)  %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() + 
  geom_abline(lty = 2) + 
  xlab("Life Expectancy Average (2010-2015)") +
  ylab("Life Expectancy Increase (2010-2015)") +
  ggtitle("Western Life Expectancy Bland-Altman Plot (2010-2015)")

#### Wrangling Lesson: Tidy vs. Wide Data ####
# We've been using a tidy set of Gapminder data. To appreciate it, let's start with a list and visualization:
gapminder %>%
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility) %>%
  head()

gapminder %>%
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility) %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_line() +
  ggtitle("Fertility Rates Over Time, SK & GER") +
  xlab("Year") +
  ylab("Average Number of Children per Woman")

# Tidy data lets our code work seamlessly. Each point represents a table row. We can define tidy data as having rows and columns respectively representing
# single observations and variables. We have another file for a non-tidy version of the same datset, imitating it as it was originally saved in a spreadsheet:
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

# We'll refer to this format as wide data. Let's observe a sample of it:
select(wide_data, country, `1960`:`1967`)

# Defining wide format:
# 1) Each row includes several observations.
# 2) One of the variables (year) is in the header.

# The plot code above is incompatible with wide data. There's no year available, so we need to wrangle the data into tidy data in order to use tidyverse.

#### Wrangling Lesson: Reshaping Wide Data ####
# Formatting data as "tidy" allows tidyverse to work. The first step in data analysis is importing data; the most common step to follow is reshaping the 
# data into a form that facilitates the rest of the analysis. Tidyverse includes the tidyr package, which includes several useful functions for tidying data.
# gather() converts wide data to tidy data. By default, it gathers all columns, so we often have to specify them.

# For example, we'll gather all wide data annual columns from 1960 to 2015: 
## Arg.1 sets the name of the column to hold the variable currently kept in the wide header. (Year)
## Arg.2 sets the name for the column to hold values in the column cells. (Fertility)
## Arg.3 specfies all columns to be gathered. (1960-2015)

# The file never states explicitly that this is fertility data. That's clear from the file name. Not the best way to store data, but that's what we have.
# Now lets create a new tidy dataset via gather():
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

# Note, the only column not gathered is "country". This was implicit; we asked for all othe columns to be gathered. A quicker way to write this code is to 
# specify what NOT to gather:
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)
head(new_tidy_data)

# The class has been converted from integer to character:
class(gapminder$year)
class(new_tidy_data$year)

# gather() assumes column names as characters, so we'll have to wrangle a bit more before plotting. We need the annual column in numerical form.
# numeric() works, but gather() can do that via the convert argument:
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

# With tidy data, we can now use the same ggplot commands to replicate the same plot:
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# Sometimes, for wrangling purposes, wide is more useful than tidy. spread() converts tidy data to wide data:
## Arg.1 determines which variables will serve as column names.
## Arg.2 specifies the variables used to fill out cells.
new_wide_data <- new_tidy_data %>%
  spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

#### Wrangling Lesson: Separate and Unite ####
# The examples above are much simpler than common real-life examples tend to be. We have a more complicated and realistic one in dslabs. It contains life 
# expectancy as an additional variable. It is not stored in tidy or optimal format. We can read it as such:
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, 
                      "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

# It's in wide format. Four columns each represent two variables at once. We can use gather(), but "year" no longer works as a new column name. "Year" would 
# contain the variable type. "Key" works better as the default of this function. As such:
dat <- raw_dat %>%
  gather(key, value, -country)
head(dat)

# The result isn't tidy -- each observation is associated w/ two rows, not only one. We want to separate the fertility and life expectancy columns from year.

# First, we separate key into year and variable type. Note that each year and variable name are separated by underscore:
dat$key[1:5]

# The readr package includes separate() to split up multi variable columns. It takes three arguments:
## 1) Name of column to be separated.
## 2) Names to be used for new columns.
## 3) Character that separates the variables.

# Try this:
dat %>% separate(key, c("year", "variable_name"), "_")

# "_" is the function's default separator:
dat %>% separate(key, c("year", "variable_name"))

# Both lines return too many values at 112 locations and truncate life_expectancy to life. We can add a third column to get around this and idetnify columns to fill
# with missing values (such as NAs) when third values don't exist. Let's designate the right column for that purpose:
dat %>% separate(key,
                 c("year", "first_variable_name", "second_variable_name" ),
                 fill = "right")

# It's better to merge the last two variables specified above when an extra separation arises. Use the "extra" argument:
dat %>% separate(key,
                 c("year", "variable_name"),
                 sep = "_",
                 extra = "merge")

# Via spread(), we can finally create a column for each variable. Make it tidy:
dat %>% separate(key,
                 c("year", "variable_name"),
                 sep = "_",
                 extra = "merge") %>%
  spread(variable_name, value)

# Sometimes it can be useful to merge rather than separate two columns. Although not optimal, we can do it via separate(), unite(), spread(), and rename():
dat %>%
  separate(key,
           c("year", "first_variable_name", "second_variable_name"),
           fill = "right") %>%
  unite(variable_name, 
        first_variable_name, 
        second_variable_name,
        sep = "_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

#### String Parsing Lesson: Variable Recoding ####
# Categorical variable recoding is a useful string-parsing exercise, such as when we want to shorten country names. Tidyverse has a dedicated function for it.
# For example, examine a life expectancy time series for Caribbean countries:
gapminder %>%
  filter(region == "Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# The legend takes up too much space on a small resolution! Four country names are longer than twelve characters:
gapminder %>%
  filter(region == "Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country)

# Let's change them for every year in the dataset:
gapminder %>% 
  filter(region == "Caribbean") %>%
  mutate(country = recode(country,
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic'="DR",
                          'St. Vincent and the Grenadines'="St. Vincent",
                          'Trinidad and Tobago'="Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line() +
  xlab("Year") +
  ylab("Life Expectancy in Years") +
  ggtitle("Life Expectancy Over Time in Carribbean Countries")

# Other useful functions include tidyverse::recode_factor(), dplyr::case_when(), and forcats::fct_recode().