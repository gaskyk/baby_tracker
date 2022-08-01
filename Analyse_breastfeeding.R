#######################################
##                                   ##                                   
## Analyse breastfeeding patterns    ##
##                                   ##
## Date: July 2022                   ##
## Author: Karen                     ##
##                                   ##  
#######################################

# Load libraries
library(tidyverse)
library(here)
library(lubridate)

# Read in breastfeeding data from Baby Tracker app
here::here()
my_info <- read.csv(here("./DataScience/Baby_tracker/data/Baby_nursing.csv"), stringsAsFactors = FALSE)


# Clean dataset - rename some columns, correct formats to date, 
# create new columns, remove others
my_info <- my_info %>%
  rename(Left_duration = Left.duration..min.,
         Right_duration = Right.duration..min.) %>%
  mutate(Total_duration = (Left_duration + Right_duration),
         Start_date_time = as.POSIXct(Time, format =  "%d/%m/%Y, %H:%M"),
         Start_date = lubridate::as_date(Start_date_time),
         Day = as.integer(Start_date - lubridate::as_date("2020-09-09")),
         End_date_time = Start_date_time + lubridate::minutes(Total_duration),
         End_date = lubridate::as_date(End_date_time),
         Start_time = strftime(Start_date_time, format="%H:%M"),
         End_time = strftime(End_date_time, format="%H:%M")) %>%
  select(-c(ï..Baby, Note, Time, Total.Duration..min.))

# Add a month - data starts on 10th
my_info <- my_info %>%
  mutate(Month = case_when(
    Start_date < lubridate::as_date("2020-10-09") ~ "Month 1",
    Start_date < lubridate::as_date("2020-11-09") ~ "Month 2",
    Start_date < lubridate::as_date("2020-12-09") ~ "Month 3",
    Start_date < lubridate::as_date("2021-01-09") ~ "Month 4",
    Start_date < lubridate::as_date("2021-02-09") ~ "Month 5",
    Start_date < lubridate::as_date("2021-03-09") ~ "Month 6",
    Start_date < lubridate::as_date("2021-04-09") ~ "Month 7"
  ))

# Create total time breastfeeding by day and month
daily_total <- my_info %>%
  group_by(Month, Day) %>%
  summarise(Daily_duration_mins = sum(Total_duration),
            Daily_duration_hours = Daily_duration_mins / 60)

# Average and total time spent breastfeeding per day within month
monthly_av_total <- daily_total %>%
  ungroup %>%
  group_by(Month) %>%
  summarise(Average_daily_hours = mean(Daily_duration_hours),
            Total_monthly_hours = sum(Daily_duration_hours))

# Total time spent breastfeeding
total_time <- daily_total %>%
  ungroup %>%
  summarise(Total_mins = sum(Daily_duration_mins),
            Total_hours = sum(Daily_duration_hours))
  
# Graph time spent breastfeeding each day for all months
ggplot(data = daily_total, aes(x = Day, y = Daily_duration_hours)) + 
  geom_bar(stat = "identity") +
  coord_flip() + scale_x_reverse() + facet_grid(Month ~ ., scales = "free",
                                                switch="y") +
  labs(y = "Hours spent breastfeeding") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + theme_bw()

# Graph time spent breastfeeding each day for month 1
month1 <- daily_total %>%
  filter(Month == "Month 1")

ggplot(data = month1, aes(x = Day, y = Daily_duration_hours)) + 
  geom_bar(stat = "identity") +
  coord_flip() + scale_x_reverse() +
  labs(y = "Hours spent breastfeeding") + theme_bw()

# Graph all feeds and remove one line which crosses midnight
my_info2 <- my_info %>%
  filter(Start_date == End_date)

ggplot(my_info2, aes(x=Start_time, xend=End_time, y=Day,
                     yend=Day)) + geom_segment() + scale_y_reverse() +
  scale_x_discrete(breaks = c("00:00", "06:00", "12:00", "18:00")) +
  xlab("Time") + theme_bw()


