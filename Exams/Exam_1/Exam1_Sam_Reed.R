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
sick %>%
  group_by(Province_State) %>%


state_max_fatality_rate <- sick %>%
  rename(State = Province_State,
         maximum_fatality_ratio = group_by(Province_State) %>%
           summarize(avg = mean(Case_Fatality_Ratio, na.rm = TRUE))) %>%
  arrange(.by_group = TRUE, desc(factor(maximum_fatality_ratio)))
