setwd(./)
 
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

dat <- read_csv("../../Data/BioLog_Plate_Data.csv")
glimpse(dat)
View(dat)
