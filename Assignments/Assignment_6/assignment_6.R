setwd(./)
 
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gganimate)

dat <- read_csv("../../Data/BioLog_Plate_Data.csv")
glimpse(dat)
View(dat)

#1. Cleaning the data into tidy form

tidy <- pivot_longer(
  dat,
  cols = c(Hr_24, Hr_48, Hr_144),
  names_to = "Hour",
  values_to = "Absorbance",
  names_prefix = "Hr_",
  values_drop_na = TRUE)

head(tidy)
dim(tidy)
dim(dat)
View(tidy)

#2. Creates a new column specifying whether a sample is from soil or water.

tidy2 <- tidy %>% mutate(Source_Type = if_else(str_detect(`Sample ID`, "Soil"), "Soil", "Water"))
View(tidy2)

#3. Generates a plot that matches that one just plotting dilution == 0.01

ggplot(aes(
  x = Hour,
  y = Absorbance,
  color = Source_Type )) +
  geom_line(linewidth = 1) +
  labs(x = "Time", y = "Absorbance") +
  scale_color_manual(values = c(
    "Soil"   = 'red',
    "Water" = 'blue'
    ))