library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gganimate)
library(gifski)

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
tidy2 <- tidy2 %>% mutate(Hour = as.numeric(Hour))
View(tidy2)

#3. Generates a plot that matches that one just plotting dilution == 0.01

follow_example <- tidy2 %>%
  filter(Dilution == 0.1) %>% 
  group_by(Source_Type, Hour, Substrate) %>%
  summarize(Absorbance = mean(Absorbance), .groups = "drop") %>%
  ggplot(aes(
  x = Hour,
  y = Absorbance,
  color = Source_Type
  )) +
  facet_wrap(~Substrate) +
  geom_line(linewidth = 1) +
  labs(x = "Time", y = "Absorbance", color = "Type") +
  theme_minimal()
print(follow_example)
ggsave("Seperate_Substrate_Graph.png", plot = follow_example,
       width = 10, height = 8, dpi = 300)

#4
graph_4 <- tidy2 %>%
  filter(Substrate == "Itaconic Acid") %>% 
  group_by(Hour, Dilution) %>%
  summarize(Absorbance = mean(Absorbance), .groups = "drop") %>%
  ggplot(aes(
    x = Hour,
    y = Absorbance,
    color = `Sample ID`
  )) +
  facet_wrap(~Substrate) +
  geom_line(linewidth = 1) +
  labs(x = "Time", y = "Absorbance", color = "Type") +
  transition_reveal(hour, range = NULL, keep_last = TRUE) +
  ease_aes('linear') +
  theme_minimal()

print(follow_example)
ggsave("Seperate_Substrate_Graph.png", plot = follow_example,
       width = 10, height = 8, dpi = 300)
