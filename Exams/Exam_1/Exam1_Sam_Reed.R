# Question 1
sick <- read.csv("cleaned_covid_data.csv")
class(sick)

# Question 2
library(tidyverse)
A_states <- sick%>%
  filter(grepl("^A", Province_State))
View(A_states)

# Question 3
A_states <- A_states %>%
mutate(Last_Update = as.Date(Last_Update))
ggplot(A_states, aes(x= Last_Update, 
                     y= Deaths)) +
  geom_point(size = .1, alpha = 1) +
  geom_smooth(method= "loess", se = FALSE, na.rm = TRUE)+
  facet_wrap(~Province_State, scales = "free") +
  labs(x = "Date", y = "Deaths", title = "Deaths over Time")+
  scale_x_date(date_breaks = "6 months", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Question 4
state_max_fatality_rate <- sick %>%
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))
view(state_max_fatality_rate)

# Question 5
ggplot(state_max_fatality_rate, aes(
  x = factor(Province_State, levels = unique(Province_State)),
  y = Maximum_Fatality_Ratio,
  fill = Maximum_Fatality_Ratio)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "State", y = "Maximum Fatality Ratio") +
  scale_fill_distiller(palette = "Oranges", direction = 1)

# Question 6
deaths_over_time <- sick %>%
  group_by(Last_Update) %>%
  summarize(Deaths = sum(Deaths, na.rm = TRUE)) %>%
  arrange(desc(Last_Update))

ggplot(deaths_over_time, aes(
  x = as.Date(Last_Update),
  y = Deaths)) +
  geom_line(size = 2, alpha = 1, color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Time", y = "Deaths") 

