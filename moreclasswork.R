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

