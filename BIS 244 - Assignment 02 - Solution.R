# Solution file for BIS 244 Assignment 02, Spring 2022
library(here)
library(tidyverse)
# Import data into dataframe
US <- read_csv(here("covid-19-data","us-states.csv"))

# Filter down to PA only
PA <- US %>% filter(state=="Pennsylvania")

# Set n to length of data set
n <- length(PA$date)

# Initialize new variables in data frame
PA$incr_deaths <- 0
PA$incr_cases <- 0

PA$incr_deaths[1] <- PA$deaths[1]
PA$incr_cases[1] <- PA$cases[1]

# Calculate values for incremental cases and deatchs
for (i in 2:n) {
  PA$incr_cases[i] <- PA$cases[i] - PA$cases[i-1]
  PA$incr_deaths[i] <- PA$deaths[i] - PA$deaths[i-1]
}

# Calculating sum of all adjusted deaths as checksum
sd(PA$incr_cases)
