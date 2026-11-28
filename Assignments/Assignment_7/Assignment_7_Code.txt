#Assignment 7
library(tidyverse)

#Import the Assignment_7/Utah_Religions_by_County.csv data set
utah <- read_csv("./Utah_Religions_by_County.csv")
View(utah)

#Clean it up into “tidy” shape
utah_pivot <- pivot_longer(
  utah,
  cols = !c(1:4),
  names_to = "Religion",
  values_to = "Proportion")
View(utah_pivot)

#Explore the cleaned data set with a series of figures (I want to see you exploring the data set)

#population per county vs proportion religious
population_religion_ratio_plot <- utah_pivot %>%
  distinct(County, Pop_2010, Religious) %>%
  ggplot(aes(
    x = (Pop_2010),
    y = (Religious)
  )) +
  geom_point() +
  scale_x_log10() +
  labs(x = "Population", y = "Proportion Religious") +
  theme_minimal()
print(population_religion_ratio_plot)
ggsave("population_religion_ratio_plot.png", plot = population_religion_ratio_plot,
       width = 10, height = 8, dpi = 300)
#no clear association

#most common religion per county
religion_per_county <- utah_pivot %>%
  group_by(County) %>%
  slice_max(Proportion, n = 1) %>%
  ggplot(aes(
    x = "Top religion",
    y = County,
    fill = Religion
  )) +
  geom_tile() +
  geom_text(aes(
    label = Religion),
    size = 3
  ) +
  labs(x = NULL, y = "County") +
  theme_minimal() +
  theme(legend.position = "none")
print(religion_per_county)
ggsave("religion_per_county.png", plot = religion_per_county,
       width = 10, height = 8, dpi = 300)
#LDS is the most common religion per county

#number per religion
number_per_religion <- utah_pivot %>%
  mutate(num_people = Pop_2010 * Proportion) %>%
  group_by(Religion) %>%
  summarize(total_people = sum(num_people)) %>%
  ggplot(aes(
    x = reorder(Religion, total_people),
    y = total_people,
    fill = Religion
  )) +
  geom_col() +
  labs(x = "Religion", y = "# Pop") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")
print(number_per_religion)
ggsave("number_per_religion.png", plot = number_per_religion,
       width = 10, height = 8, dpi = 300)
#LDS is the most common by far

#“Does population of a county correlate with the proportion of any specific religious group in that county?”
#I've done some work on this already, so I'll use my old work.
q1_cor <- utah_pivot %>%
  ggplot(aes(
    x = (Pop_2010),
    y = (Proportion)
  )) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "lm") +
  facet_wrap(~Religion, scales = "free_y") +
  labs(x = "Population", y = "Proportion of County") +
  theme_minimal()
print(q1_cor)
ggsave("q1_cor.png", plot = q1_cor,
       width = 10, height = 8, dpi = 300)
#there appears to be some correlation between population of a county and proportion of Cathloic, Pentecostal Church of God, Assemblies of God, and Non Denominational

#“Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”
q2_plot <- ggplot(utah_pivot, aes(
  x = Proportion,
  y = `Non-Religious`
  )) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Religion, scales = "free_x") +
  labs(x = "Proportion of religion in county",
       y = "Proportion non-religious") +
  theme_minimal()
print(q2_plot)
ggsave("q2_plot.png", plot = q2_plot,
       width = 10, height = 8, dpi = 300)
#There appears to be a correlation between Southern Baptist Convention, Episcopal Church, United Methodist Church,non-religious people, and LDS.