csv_files <- list.files(path = "Data/", pattern = "\\.csv$", full.names = TRUE)

df <- read.csv(("Data/cleaned_bird_data.csv"))
df 
mean(df$Egg_mass, na.rm = TRUE)
big_eggs <- df[df$Egg_mass > (mean(df$Egg_mass, na.rm = TRUE)) & !is.na(df$Egg_mass),]
big_eggs
write.csv(big_eggs, "BigEggs.csv", row.names = FALSE)
 
library(tidyverse)
obj <- read.csv(("Data/cleaned_bird_data.csv"))
obj %>%
  dplyr::filter(Egg_mass > mean(Egg_mass, na.rm = TRUE)) %>%
  write_csv("EGGMASS.csv")

## control = c(98, 101, 96, 100, 99)
treatment = c(88, 85, 90, 87, 86)
control
treatment

mean_control = mean(control)
mean_treatment = mean (treatment)

print(mean_control)
print(mean_treatment)

my_data <- data.frame(
  Control = c(98, 101, 96, 100, 99),
  Treatment = c(88, 85, 90, 87, 86)
)

colMeans(my_data)
sapply(my_data, sd)

setwd(\Users\samue\OneDrive\Stat 2040 Fall)
df<-read.csv("SleepStudy.csv")
head(df)
table(df$Gender)
mean(df$GPA)
IQR = IQR(df$CognitionZscore, na.rm = TRUE)
IQR
sd = sd(df$GPA)
DAS = table(df$DASScore)

library(ggplot2)
df <- data.frame(femur_length = c( 61, 18, 43, 83, 29),
                 body_length = c(9.2, 2.5, 6.2, 12.5, 3.8)
)
plot(df$femur_length, df$body_length, col='blue')
abline(lm(df$body_length~df$femur_length), col='red') #abline(lm(y~x))
r = cor(df$femur_length, df$body_length)
abline(v=133)
sy = sd(df$body_length)
sx = sd(df$femur_length)
m = r*(sy/sx)
xbarx = mean(df$femur_length)
xbary = mean(df$body_length)
B = (xbary - (m*xbarx))
y = B + (m*133)
y

95% rule = if we know 1) the shape of the data symmetic 2) mean (xbar) 3) std(s)
then we can answer what percent of observation falls in specific interval.
 
##
sleep_study <- read.csv("SleepStudy.csv")
sum(sleep_study$GPA <3)
m <- mean(sleep_study$DASScore)
s <- sd(sleep_study$DASScore)
max(sleep_study$DASScore)
round((82-m)/s, 2)
lm(sleep_study$DASScore ~ sleep_study$PoorSleepQuality, data = sleep_study)

What percentage of students have happiness scores more than one standard deviations above the mean? Enter your answer to two decimal places. 
round(cor(as.numeric(sleep_study$ClassesMissed), as.numeric(sleep_study$GPA)), 2)
round(cor(sleep_study$DepressionStatus, sleep_study$Happiness), 2)

hours <- c(2,3,3,4,5,5,6,6,7,7,8,8,9,9,10,
           10,11,12,12,13,14,14,15,16,16,17,18,19,20,22)
score <- c(52,55,61,58,64,67,63,70,72,68,75,71,78,74,80,
           77,82,85,81,88,86,90,89,92,87,94,91,96,95,98)
study <- data.frame(hours, score)
# is actually study <- read.csv("filename.csv")

#use session > set working directory > source file location. or files pane > import dataset
#using study here, but use study$hours

#`1. How to count how many observations in a series less or more than a specific value
`# x < 10 give true/false and sum() counts trues
study$hours < 8
sum(study$hours < 8)
sum(study$score > 85)
sum(study$score <= 70)
# & means both conditions true. | means either condition true
sum(study$hours >= 10 & study$hours <=15)

#2. How to find the percent of the observations less (more) than a specific value
# TRUE = 1, FALSE = 0, use mean here. mean of TRUE/FALSE is the proportion
# so basically it counts true and false and ddivides by mean ex 1+0+1+0/4 = .5
mean(study$hours < 8) *100
round(mean(study$score < 70) *100, 1)

#z = (value − mean) / standard deviation. R has no z-score function; you build it from mean() and sd()

scatterplot = plot(study$hours, study$score)
cor(study$hours, study$score)

lm() means linear model
y ~ x means 'y explained by x'
fit <- lm(score ~ hours, data = study)

library(qrcode) #library means open package

url <- 'https:countryoaks.skepsecurity.com'
qr <- qr_code(url)
qr
plot(qr)

install.packages('tidyverse')
library(tidyverse)
%>% #pipe, object from left is input to right
  
  ?palmerpenguins
library(palmerpenguins)
library(tidyverse)
penguins
View(penguins)

mean(penguins$bill_length_mm, na.rm=TRUE)
penguins %>%
  group_by(species) %>%
  summarise(lengthbill = mean(bill_length_mm, na.rm = TRUE),
            maxoflength = max(bill_length_mm, na.rm = TRUE)
  )

  
### 9/29/26
# find pengins body mass > 5000 and bill length > avg
# how many male and female
# what are their max weight and max bill length for each island
# add new col to dataset indicating if they meet the cririas or not

library(tidyverse)
library(palmerpenguins)
criteria <- penguins %>%
  filter(body_mass_g > 5000 &
         bill_length_mm > (mean(penguins$bill_length_mm, na.rm = TRUE)))

criteria %>% count(sex)

criteria %>%
  group_by(island) %>%
  summarise(max_mass = max(body_mass_g, na.rm = TRUE),
            max_bill = max(bill_length_mm, na.rm = TRUE))

penguins <- penguins %>%
  mutate(meet_criteria = body_mass_g > 5000 &
           bill_length_mm > (mean(penguins$bill_length_mm, na.rm = TRUE)))
  
View(penguins)
library(ggplot2)
penguins %>%
  drop_na() %>%
  ggplot(aes(x = body_mass_g,
           y= bill_length_mm,
           color = sex,
           shape = island))+
  geom_point() +
  geom_abline() 
ggsave()

library(palmerpenguins)
library(ggplot2)
view(penguins)
penguins %>%
  group_by(island) %>%
  summarize(mean_body_mass = mean(body_mass_g, na.rm=TRUE)) %>%
    ggplot(aes(x = island, y = mean_body_mass)) +
    geom_col()
view(penguins)
 # data set sources
google data search
kaggle

library(dplyr)

#create data frame with scores
# info: N(73,11) N means "normal distribution/bell curve" 
# U or M is the mean. Sigma is the standard deviation, indicates how data is spread around the curve.
# So in N(73,11) 72 is mean and sigma is 11.

exam_data <- data.frame(
  student = c("Student A", "Student B"),
  first_score = c(44, 95)
)

old_mean <- 75
old_sd <- 10

new_mean <- 62
new_sd <- 18

# in this problem we take find the z score using the origional data
# then we solve for x in the new distribution using the old known Z
# Z = X-U/Sigma (u is is M)

#mutate() creates new colums in the data set by calulating existing columsn. Old>new, goes column by column
processed_data <- exam_data %>%
  mutate(
    z_score = ((first_score - old_mean)/old_sd)
    # Calculate the zscore using the old distrbution
    # Then calulate the new Z score using the target distribution
    # Then round the nearest integer 
    
    #view results
    print(processed_data)
  )

#9/23/26

#set seed for reproducibility
set.seed(123)

#define sample parameters
n <- 100 #total sample size
x <- 35 # number who agree
p_hat <- x/n #sample proportion

#recreate origional sample
original_sample <- c(rep(1, x), rep(0, n-x))

#generate bootstrap distribution
bootstrap <- replicate(10000, {
  boot_sample <- sample(original_sample, size = n, replace = TRUE)
  mean(boot_sample)
})

# estimate standard error
se <- sd(bootstrap)
round(se, 3)

# calulated 95% confidence interval
ci_lower <- p_hat - (2 * se)
ci_upper <- p_hat + (2 * se)

round(c(lower = ci_lower, ci_upper), 3)

#graph it
library(ggplot2)
# Store vector as data frame
df_boot <- data.frame(p_hat_boot = bootstrap)

ggplot(df_boot, aes(x = p_hat_boot)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 30) +
  geom_vline(xintercept = p_hat, color = "red", linewidth = 1) +
  geom_vline(xintercept = c(ci_lower, ci_upper), color = "blue", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Bootstrap Distribution",
    x = "Sample Proportion",
    y = "Count"
  ) +
  theme_minimal()


#q27
male = c(3,1,12,12,0,4,10,4,5,5,2,10,10)
n1 = length(male)
female = c(10,3,2,10,3,2,0,1,6,1,5)
n2 = length(female)

stat = mean(male) - mean (female)

xbar.d = replicate(2000, mean(sample(male, size = , replace = TRUE)) - 
                     mean(sample(male, size = , replace = TRUE))
)
SE = sd(xbar.d)
stat+c(-1, 1)*2*SE #95% confidance interval
quantile(xbar.d, c(0.025, 0.975))
light = c(9.17,6.94,4.99,1.71,5.43,10.26,4.67,11.67,7.15,5.33)
n1= length(light)
n1

dark = c(2.83,4.60,6.52,2.27,5.95,4.21,4.00,2.53)
n2 = length(dark)
n2

df <- data.frame(weight= c(light, dark), group = c(rep(1, n1, rep(2, n2)))
                 View(df)
                 
                 library(UsingR)
                 DOTplot(weight ~ group, df)
                 
                 library(dplyr)
                 
                 df %>% 
                   group_by(group) %>%
                   summarize(xbar = mean(weight),
                             sd = sd(weight)
                             n=n()
                   )
                 
                 
                 Dat %>%
                   Group_By(dataset) %>%
                   summarize(avg)
                 
                 #package GGally
                 install.packages(c("GGally", "palmerpenguins"))
                 library(tidyverse)
                 library(ggplot2)
                 library(GGally)
                 library(palmerpenguins)
                 penguins
                 ggpairs(penguins)
                 
                 
                 #make a graph using penguin data
                 #try to use new geom
                 #overlap at least two geom
                 #add title for the plot
                 #bonus: adjust title, axis, or other labels
                 
                 library(ggplot2)
                 library(palmerpenguins)
                 library(tidyverse)
                 library(ggimage)
                 View(penguins)
                 
                 penguins %>%
                   drop_na() %>%
                   ggplot(aes(x = body_mass_g,
                              y= bill_length_mm,
                              color = sex,
                              shape = island))+
                   geom_point() +
                   geom_abline() 
                 
                 penguin_img_url <- ""
                 
                 data_prepared <- penguins %>%
                   drop_na() %>%
                   mutate(image_url = penguin_img_url)
                 
                 data_prepared %>%
                   drop_na() %>%
                   ggplot(aes(x = sex,
                              y = body_mass_g,
                   )) +
                   geom_violin(fill = "skyblue", color = "navy", alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75))+
                   stat_summary(fun = "median", feom = "point", color = "navy", size = 2)
                 labs(title = "Distribution of Penguin Body Mass by Sex",
                      x = NULL,
                      y = 'Body Mass (g)') +
                   theme_minimal(base_size = 12) +
                   theme(
                     plot.title = element_text(face = "bold"),
                     panel.grid.major.x = element_blank()
                   )
                 
                 
                 penguins %>%
                   drop_na() %>%
                   ggplot(aes(x = sex, y = body_mass_g, fill = sex)) +
                   geom_violin(alpha = 0.7, color = "white", linewidth = 0.8) +
                   geom_boxplot(width = 0.1, fill = "white", color = "grey20", outlier.shape = NA) +
                   scale_fill_brewer(palette = "Pokemon") +
                   labs(
                     title = "Body Mass Distribution Across Penguin Sexes",
                     x = NULL,
                     y = "Body Mass (g)"
                   ) +
                   theme_classic(base_size = 12) +
                   theme(
                     legend.position = "none",
                     plot.title = element_text(face = "bold")
                   )
                 
                 
                 
                 
                 library(palmerpenguins)
                 library(ggplot2)
                 library(dplyr)
                 library(ggimage)
                 library(magick)
                 library(tidyr)
                 
                 original_path <- "C:/Users/samue/Bio_Stats/Data_Course_REED/close-up-of-a-gentoo-penguin-with-detailed-plumage-and-orange-beak-cut-out-png-2685192404.png"
                 small_path    <- "C:/Users/samue/Bio_Stats/Data_Course_REED/penguin_small.png"
                 
                 image_read(original_path) |>
                   image_trim() |>            # crops away empty transparent border
                   image_scale("x100") |>     # 100 px tall, width keeps aspect ratio
                   image_write(small_path, format = "png")
                 
                 file.size(small_path) / 1024  # size in KB; should be small
                 
                 # Set the path to your downloaded JPG file
                 local_image_path <- small_path
                 
                 # Filter out missing values and assign the local file path
                 data_prepared <- penguins %>%
                   drop_na() %>%
                   mutate(image_path = local_image_path)
                 
                 # Generate the plot
                 p <- data_prepared %>%
                   ggplot(aes(x = sex, y = body_mass_g, fill = sex)) +
                   
                   # Base violin and boxplot layers:
                   geom_violin(alpha = 0.5, color = "white", linewidth = 0.8) +
                   geom_boxplot(width = 0.1, fill = "white", color = "grey20", outlier.shape = NA) +
                   
                   # Jittered image layer using your local file
                   geom_image(aes(x = sex, y = body_mass_g, image = image_path), 
                              size = 0.05,
                              by = "height",
                              position = position_jitter(width = 0.2, height = 0),
                              alpha = 0.8) +
                   
                   # Styling and labels:
                   scale_fill_brewer(palette = "Set2") +
                   labs(
                     title = "Body Mass Distribution with Jittered Penguin Images",
                     x = NULL,
                     y = "Body Mass (g)"
                   ) +
                   theme_minimal(base_size = 12) +
                   theme(
                     legend.position = "none",
                     plot.title = element_text(face = "bold"),
                     panel.grid.major.x = element_blank()
                   )
                 print(p)  
                 
                 
                 obj <- read.delim("C:/Users/samue/Bio_Stats/Data_Course_REED/Data/DatasaurusDozen.tsv")
                 View(obj)
                 
                 
                 library(gapminder)
                 library(gganimate)
                 library(gifski)
                 library(av)
                 
                 View(gapminder)
                 after2000 <- gapminder %>%
                   drop_na() %>%
                   group_by(country, year) %>%
                   print(after2000)
                 ggplot(aes(x = country_codes)) +
                   geom_histogram(fill = 'blue')
                 names(gapminder)\
                 glimpse(gapminder)
                 unique(gapminder$country)
                 range(gapminder$year)
                 
                 gapminder %>%
                   ggplot(aes(x = lifeExp,
                              y = gdpPercap,
                              color = continent)) +
                   geom_point(aes(size = 4)) + 
                   geom_text(aes(label = 'country', vjust = 1.5) +
                               facet_wrap(~ continent) 
                             transition_time(time = year) 
                             labs(title = '2007{frame_time}')
                             
                             anim_save('my_anim.gif')
                             ##gganimate.com
                             library(ggplot2)
                             library(dplyr)
                             library(tidyr)
                             library(scales)
                             
                             # ---- Raw hemocytometer counts: 5 squares per plate ----
                             raw <- tibble(
                               day   = c(1, 1, 2, 2, 3, 3),
                               plate = c(1, 2, 1, 2, 1, 2),
                               sq1 = c(13,  7, 19, 27, 13,  4),
                               sq2 = c( 9,  5, 24, 20, 14,  6),
                               sq3 = c(13,  7, 28, 15, 11,  7),
                               sq4 = c( 8,  0, 13, 17,  9, 10),
                               sq5 = c( 4,  3, 20, 15, 18,  5)
                             )
                             
                             hrs_elapsed <- c(`1` = 26.5, `2` = 22.0, `3` = 23.5)  # hours since previous count
                             susp_vol    <- c(`1` = 2,    `2` = 2,    `3` = 5)     # mL; 4 mL media added on Day 3
                             
                             counts <- raw %>%
                               rowwise() %>%
                               mutate(avg_sq = mean(c(sq1, sq2, sq3, sq4, sq5))) %>%
                               ungroup() %>%
                               mutate(
                                 cells_per_mL    = avg_sq * 2 * 1e4,
                                 cells_per_plate = cells_per_mL * susp_vol[as.character(day)],
                                 cum_hours       = cumsum(hrs_elapsed)[as.character(day)]
                               )
                             
                             day0 <- tibble(day = 0, plate = c(1, 2), cells_per_plate = 2e5, cum_hours = 0)
                             
                             curve_dat <- bind_rows(day0, counts %>% select(day, plate, cells_per_plate, cum_hours)) %>%
                               mutate(plate = factor(plate, labels = c("Plate 1", "Plate 2")))
                             
                             mean_dat <- curve_dat %>%
                               group_by(cum_hours) %>%
                               summarise(cells_per_plate = mean(cells_per_plate), .groups = "drop")
                             
                             # ---- Monotone spline interpolation (smooth, never overshoots the data) ----
                             smooth_it <- function(x, y, n = 400) {
                               f <- splinefun(x, y, method = "monoH.FC")
                               xx <- seq(min(x), max(x), length.out = n)
                               tibble(cum_hours = xx, cells_per_plate = f(xx))
                             }
                             
                             sm_plates <- curve_dat %>%
                               group_by(plate) %>%
                               group_modify(~ smooth_it(.x$cum_hours, .x$cells_per_plate)) %>%
                               ungroup()
                             
                             sm_mean <- smooth_it(mean_dat$cum_hours, mean_dat$cells_per_plate)
                             
                             # Shaded band between the two plates = spread across replicates
                             band <- sm_plates %>%
                               pivot_wider(names_from = plate, values_from = cells_per_plate) %>%
                               rename(p1 = `Plate 1`, p2 = `Plate 2`) %>%
                               mutate(lo = pmin(p1, p2), hi = pmax(p1, p2))
                             
                             pal <- c("Plate 1" = "#1B9E8F", "Plate 2" = "#E07A3F")
                             
                             ggplot() +
                               geom_ribbon(data = band, aes(cum_hours, ymin = lo, ymax = hi),
                                           fill = "#4C6EF5", alpha = 0.10) +
                               geom_line(data = sm_plates,
                                         aes(cum_hours, cells_per_plate, colour = plate),
                                         linewidth = 0.7, linetype = "22", alpha = 0.9) +
                               geom_line(data = sm_mean, aes(cum_hours, cells_per_plate),
                                         colour = "#2B3A67", linewidth = 1.5) +
                               geom_point(data = curve_dat,
                                          aes(cum_hours, cells_per_plate, colour = plate, shape = plate),
                                          size = 2.6, stroke = 1) +
                               geom_point(data = mean_dat, aes(cum_hours, cells_per_plate),
                                          colour = "#2B3A67", fill = "white", shape = 21,
                                          size = 3.4, stroke = 1.3) +
                               scale_colour_manual(values = pal, name = NULL) +
                               scale_shape_manual(values = c(16, 17), name = NULL) +
                               scale_x_continuous(breaks = c(0, 26.5, 48.5, 72),
                                                  expand = expansion(mult = c(0.02, 0.04))) +
                               scale_y_continuous(labels = label_comma(),
                                                  expand = expansion(mult = c(0.04, 0.08))) +
                               labs(x = "Hours Elapsed Since Plating",
                                    y = "Cells Per Plate") +
                               theme_minimal(base_size = 11) +
                               theme(
                                 panel.grid.minor  = element_blank(),
                                 panel.grid.major.x = element_blank(),
                                 panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
                                 axis.line          = element_line(colour = "grey30", linewidth = 0.4),
                                 axis.ticks         = element_line(colour = "grey30", linewidth = 0.3),
                                 legend.position    = c(0.14, 0.88),
                                 legend.background  = element_rect(fill = alpha("white", 0.8), colour = NA),
                                 legend.key.height  = unit(0.8, "lines"),
                                 plot.margin        = margin(6, 10, 4, 6)
                               )
                             
                             ggsave("growth_curve.png", width = 6.2, height = 3.1, dpi = 300)
                             data(package = .packages(all.available = TRUE))
                             
                             library(tidyverse)
                             library(MASS)
                             data(mammals)
                             
                             ?mammals
                             View(mammals)
                             str(mammals)
                             
