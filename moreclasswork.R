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
                 
                 
                 
