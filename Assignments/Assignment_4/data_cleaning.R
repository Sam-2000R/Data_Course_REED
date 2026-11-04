library(tidyverse)
library(dplyr)

#file paths and view data
path <- "./"
outpath <- "./asthma_ed_clean.csv"
list.files()
davis <- read_csv("asthma_ed_Davis.csv")
glimpse(davis)
head(davis)
summary(davis)
tail(davis)

#filter which files will be imported
files <- list.files(path = "./", pattern = 'asthma_ed',
           full.names = TRUE) 
list(files)

#standardize country names
asthma_er <-  read_csv(files) #read.csv is base r accepts one path, read_csv is tidyverse accepts several and stacks into a table
count(asthma_er, county)
asthma_er <-mutate(asthma_er, county = recode(county, "Utah county" = "Utah", "Weber1" = "Weber"))
count(asthma_er, county)

#remove 2015 rows
asthma_er <- filter(asthma_er, year != "2015 Jan-Sep")
count(asthma_er)

#make year a number
asthma_er <- mutate(asthma_er, year = as.integer(year))
glimpse(asthma_er)

#look at data to see if it makes sense
sum(is.na(asthma_er)) #checks if any na values exist, >0 if yes
summary(asthma_er$asthma_ed_visits) #checks on column, sanity check
count(asthma_er, county, year, month) %>% #checks if any coumn combination is more than once
  filter(n >1)

#Save file
write_csv(asthma_er, outpath)


#clean aq data
path <- "./"
outpath <- "./aq_clean.csv"
list.files()
aq_2010 <- read_csv("ad_viz_plotval_data_2010.csv")
glimpse(aq_2010)
head(aq_2010)
summary(aq_2010)
tail(aq_2010)

files <- list.files(path = "./", pattern = 'ad_viz_plotval_data_',
                    full.names = TRUE) 

#filter county names
aq_tot <-  read_csv(files)
count(aq_tot, County)
aq_tot <- filter(aq_tot, County %in% c("Salt Lake", "Davis", "Utah", "Weber"))
 
#filter columns
count(aq_tot, Units)
aq_tot <- select(aq_tot, Date, 'Site ID', POC, 'Daily Mean PM2.5 Concentration', 'Daily AQI Value', County, 'Site Latitude', 'Site Longitude', 'Local Site Name', 'Site ID')
glimpse(aq_tot)

#Date repair
mdy("03/15/2004")
aq_tot <- mutate(aq_tot, Date = mdy(Date),
                 year = year(Date),
                 month = month(Date)
                 ) 
glimpse(aq_tot)
#select() keeps or drops columns, filter() keeps or drops rows
write_csv(aq_tot, outpath)

#POC is instrument, and a single site has several POC. So I'll
#average values for all interements per site, then for all instraments per day per site

avg_pm_location <- aq_tot %>%
  group_by(County, month, year, `Site ID`, Date) %>%
  summarise(avg_pm = mean(`Daily Mean PM2.5 Concentration`, na.rm = TRUE), .groups = "drop")
glimpse(avg_pm_location)
nrow(avg_pm_location)

#average across sites for one value per county per month
pm25_monthly <- avg_pm_location %>%
  group_by(County, month, year) %>%
  summarise(avgpm25 = mean(avg_pm, na.rm = TRUE),n_obs = n(), .groups = "drop")
glimpse(pm25_monthly)
summary(pm25_monthly$n_obs)
write_csv(pm25_monthly, outpath)

#going to make an interactive map eventually, so I need a table for it
cities <- distinct(aq_tot, `Site ID`, `Local Site Name`, County, `Site Latitude`, `Site Longitude`)
count(cities, `Site ID`)
sum(is.na(cities$`Local Site Name`)) #one of the sites has no name
filter(cities, is.na(`Local Site Name`)) # It's site 490353008
cities <- mutate(cities, `Local Site Name` = coalesce(`Local Site Name`, paste("Site", `Site ID`))) #labels that site
# fixed outputs 0

#combine aq_clean with asthma_ed_clean
path <- "./"
outpath <- "./asthma_er_county_clean.csv"

aq_clean <- read_csv("aq_clean.csv")
asthma_ed_clean <- read_csv("asthma_ed_clean.csv")
aq_clean <- rename(aq_clean, county = County)
aq_er_county_utah <- inner_join(aq_clean, asthma_ed_clean, by = c("county", "year", "month"))
glimpse(aq_er_county_utah)
sum(is.na(aq_er_county_utah))
write.csv(aq_er_county_utah, outpath)
