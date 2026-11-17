library(tidyverse)
library(here)

# Works on its own from the cleaned file in this folder; data_cleaning.R
# does not need to have been run in the same session
here::i_am("Assignments/Assignment_4/build_inital_graph.R")
a4 <- function(...) here("Assignments", "Assignment_4", ...)

df <- read_csv(a4("data", "clean", "asthma_er_county_clean.csv"), show_col_types = FALSE)

scatter_plot <- ggplot(df, aes(
  x = avgpm25,
  y = asthma_ed_visits,
  color = county)) +
  geom_point() +
  facet_wrap(~county, scales = "free_y") +
  labs(x = "Average PM2.5", y = "ER Visits", title = "PM2.5 Vs. ER Visits by Month") +
  # scale_x_date(date_breaks = "1 months", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(scatter_plot) # plots only display under source()/Rscript when printed

seasonal <- df %>%
  group_by(county, month) %>%
  summarise(avgpm25 = mean(avgpm25, na.rm = TRUE),
            asthma_ed_visits = mean(asthma_ed_visits, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(cols = c(avgpm25, asthma_ed_visits),
               names_to = "measure", values_to = "value")

seasonal_plot <- ggplot(seasonal, aes(x = month, y = value, color = county)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ measure, scales = "free_y") +
  scale_x_continuous(breaks = 1:12) +
  labs(title = "Seasonal patterns: PM2.5 and asthma ER visits (2004-2014)",
       x = "Month", y = "Monthly average")
print(seasonal_plot)

ggsave(a4("seasonal_plot.png"), plot = seasonal_plot,
       width = 10, height = 5, dpi = 300)
