library(tidyverse)

#load data
df <- read_csv("unicef-u5mr.csv")
glimpse(df)
View(df)
head(df)
tail(df)

#tidy data
tidy <- pivot_longer(
  df,
  cols = U5MR.1950:U5MR.2015,
  names_to = "Year",
  values_to = "U5MR",
  names_prefix = "U5MR.",
  values_drop_na = TRUE
)
View(tidy)
class(tidy$Year)
class(tidy$U5MR)
tidy <- tidy %>%
mutate(Year = as.integer(Year))
class(tidy$Year)

#Plot each country U5MR over time
# line plot, not smooth trend line
# facet by continent

line_plot <- ggplot(tidy, aes(
  x = Year,
  y = U5MR,
  group = CountryName)) +
  geom_line() +
  facet_wrap(~Continent) +
  labs(x = "Year", y = "U5MR", title = "U5MR Per Country and Continent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(line_plot)
#save line plot
ggsave(
  filename = "Reed_Plot_1.png",
  plot = line_plot
)

#another line plot with color

color_plot <- 
  tidy %>%
  group_by(Continent, Year) %>%
  summarise(Continent_Mean = mean(U5MR, na.rm = TRUE)) %>% 
  
  ggplot(aes(
  x = Year,
  y = Continent_Mean,
  color = Continent )) +
  geom_line(linewidth = 1.5) +
  labs(x = "Year", y = "U5MR", title = "U5MR Per Continent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c(
    "Africa"   = "#F89040",
    "Americas" = "#70A830",
    "Asia"     = "#D0B800",
    "Europe"   = "#80E8D0",
    "Oceania"  = "#F8A8A0"
  ))

print(color_plot)
#save color plot
ggsave(
  filename = "Reed_Plot_2.png",
  plot = color_plot
)

#create three models of U5MR
glimpse(tidy)
mod1 <- lm(U5MR ~ Year, data = tidy) 
mod2 <- lm(U5MR ~ Year + Continent, data = tidy) 
mod3 <- lm(U5MR ~ Year * Continent, data = tidy)
anova(mod1, mod2, mod3)
#mod3 is the best model because the RSS is the smallest of the three models,
#and the low p-value says that the improvement from adding the interaction is too large to be chance.

# Plot the 3 models predictions as shown
predictions <- tidy %>%
  mutate(
    mod1 = predict(mod1, newdata = tidy),
    mod2 = predict(mod2, newdata = tidy),
    mod3 = predict(mod3, newdata = tidy)
  ) %>%
  pivot_longer(cols = c(mod1, mod2, mod3), names_to = "model", values_to = "pred")

ggplot(predictions, aes(
  x = Year, 
  y = pred, 
  color = Continent, 
  group = Continent)) +
  geom_line(linewidth = 1) +
  facet_wrap(~model) +
  labs(x = "Year", y = "Predicted U5MR", title = "Model predictions") 

  