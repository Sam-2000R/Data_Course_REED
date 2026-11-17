library(tidyverse)
library(lubridate)
library(here)

# Build every path from the repo root so the script works no matter where R
# was started
here::i_am("Assignments/Assignment_4/data_cleaning.R")
a4 <- function(...) here("Assignments", "Assignment_4", ...)

# seperation of raw and clean data
raw_dir   <- a4("data", "raw")
clean_dir <- a4("data", "clean")
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

asthma_outpath <- file.path(clean_dir, "asthma_ed_clean.csv")
aq_outpath     <- file.path(clean_dir, "aq_clean.csv")
sites_outpath  <- file.path(clean_dir, "aq_sites.csv")
joined_outpath <- file.path(clean_dir, "asthma_er_county_clean.csv")

#---- clean asthma ER data ----

#filter which files will be imported
files <- list.files(path = raw_dir, pattern = "^asthma_ed_.*\\.csv$",
                    full.names = TRUE)
files

#standardize county names
asthma_er <- read_csv(files, show_col_types = FALSE) #read.csv is base r accepts one path, read_csv is tidyverse accepts several and stacks into a table
count(asthma_er, county)
asthma_er <- mutate(asthma_er, county = recode(county, "Utah county" = "Utah", "Weber1" = "Weber"))
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
dups <- count(asthma_er, county, year, month) %>% #checks if any column combination is more than once
  filter(n > 1)
stopifnot(nrow(dups) == 0) #stop instead of silently writing duplicated data

#Save file (write_csv overwrites, it never appends)
write_csv(asthma_er, asthma_outpath)


#---- clean aq data ----

files <- list.files(path = raw_dir, pattern = "^ad_viz_plotval_data_.*\\.csv$",
                    full.names = TRUE)
files

#filter county names
aq_tot <- read_csv(files, show_col_types = FALSE)
count(aq_tot, County)
aq_tot <- filter(aq_tot, County %in% c("Salt Lake", "Davis", "Utah", "Weber"))

#filter columns
count(aq_tot, Units)
aq_tot <- select(aq_tot, Date, `Site ID`, POC, `Daily Mean PM2.5 Concentration`, `Daily AQI Value`, County, `Site Latitude`, `Site Longitude`, `Local Site Name`)
glimpse(aq_tot)

#Date repair
aq_tot <- mutate(aq_tot, Date = mdy(Date),
                 year = year(Date),
                 month = month(Date)
                 )
glimpse(aq_tot)
#select() keeps or drops columns, filter() keeps or drops rows

#POC is instrument, and a single site has several POC. So I'll
#average values for all instruments per site, then for all instruments per day per site

avg_pm_location <- aq_tot %>%
  group_by(County, month, year, `Site ID`, Date) %>%
  summarise(avg_pm = mean(`Daily Mean PM2.5 Concentration`, na.rm = TRUE), .groups = "drop")
glimpse(avg_pm_location)
nrow(avg_pm_location)

#average across sites for one value per county per month
pm25_monthly <- avg_pm_location %>%
  group_by(County, month, year) %>%
  summarise(avgpm25 = mean(avg_pm, na.rm = TRUE), n_obs = n(), .groups = "drop")
glimpse(pm25_monthly)
summary(pm25_monthly$n_obs)
write_csv(pm25_monthly, aq_outpath)

#going to make an interactive map eventually, so I need a table for it
cities <- distinct(aq_tot, `Site ID`, `Local Site Name`, County, `Site Latitude`, `Site Longitude`)
count(cities, `Site ID`)
sum(is.na(cities$`Local Site Name`)) #one of the sites has no name
filter(cities, is.na(`Local Site Name`)) # It's site 490353008
cities <- mutate(cities, `Local Site Name` = coalesce(`Local Site Name`, paste("Site", `Site ID`))) #labels that site
sum(is.na(cities$`Local Site Name`)) # fixed, outputs 0
write_csv(cities, sites_outpath)


#---- combine aq with asthma ----
#use the tables already in memory instead of re-reading the files just written

aq_er_county_utah <- pm25_monthly %>%
  rename(county = County) %>%
  inner_join(asthma_er, by = c("county", "year", "month"))
glimpse(aq_er_county_utah)
sum(is.na(aq_er_county_utah))
write_csv(aq_er_county_utah, joined_outpath) #write_csv adds no row-number column

