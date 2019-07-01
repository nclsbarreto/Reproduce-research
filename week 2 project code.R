##### REPRODUCABLE RESEARCH 
#### week 2 project

library(tidyverse)
library(psych)
library(lubridate)

fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipfile <- "./repdata_data_activity.zip" 
# downloading the file as data.zip to the folder course4week4 under the working
# directory
filedir <- getwd()
unzip_path <- "./data"  # store unzipped file 
if (!file.exists(filedir)){
    dir.create(filedir)
}
download.file(fileurl,file.path(zipfile))
unzip(zipfile,exdir=unzip_path) ####### exdir is the extract directory ##########
datafile <- file.path(unzip_path,"activity.csv")

activity <- as_tibble(read.csv(datafile))

activity$date <- ymd(activity$date) #set date as a date
activity$weekend <- as.factor(ifelse(weekdays(activity$date)=="Saturday" | weekdays(activity$date)=="Sunday","weekend","weekday")) # make a weekend vs weekday varible
activity$dayofweek <- as.factor(weekdays(activity$date)) # get the days of the week

activity$steps %>% hist()
dat$date %>% unique()

barplot(dat$steps, dat$date)

steps_per_day <- activity %>% group_by(date) %>% summarize(day.steps = sum(steps, na.rm = TRUE))

# Question 2 
ggplot(steps_per_day) + geom_histogram(aes(x = day.steps), binwidth = 500) + 
    labs(title="Question 2: Hist of total number of steps per day", x = "Steps/Days", y = "Count of days in bins of 500")

# Question 3
aggs.of.day <-  steps_per_day %>% 
    summarize(average.day = mean(day.steps, na.rm = TRUE), median.day = median(day.steps, na.rm = TRUE))
aggs.of.day

# Question 4
avg.interval <-  activity %>% 
    group_by(interval) %>%
    summarize(avg.int = mean(steps, na.rm = TRUE))

with(avg.interval, qplot(interval, avg.int, geom = "line", xlab = "5 minute interval", ylab = "Average # of step", 
                         main = "average number of steps per interval"))

# Question 5
avg.interval[which(avg.interval$avg.int == max(avg.interval$avg.int)),]

# Question 6
table(complete.cases(activity))["FALSE"] # number of rows with a missing value

# mean imputation number of steps by interval
activity.impute <- activity %>%
    group_by(interval) %>%
    mutate(steps = replace(steps,
                           is.na(steps),
                           mean(steps, na.rm = TRUE)))

table(complete.cases(activity.impute))

# redo steps in a day with imputed data
steps_day.impute <- activity.impute %>% group_by(date) %>% summarize(day.steps = sum(steps, na.rm = TRUE))

# redo graph
ggplot(steps_day.impute) + 
    geom_histogram(aes(x = day.steps), binwidth = 500) + 
    labs(title="Question 6: Hist of total number of steps/day after imputation", x = "Steps/Days", y = "Count of days in bins of 500")

# redo average
aggs.day.impute <-  steps_day.impute %>% 
    summarize(average.day = mean(day.steps, na.rm = TRUE), median.day = median(day.steps, na.rm = TRUE))
aggs.day.impute

# question 7 weekend vs weekday
avg.int.impute <-  activity.impute %>% 
    group_by(interval, weekend) %>%
    summarize(avg.int = mean(steps, na.rm = TRUE))

with(avg.int.impute, qplot(interval, avg.int, geom = "line", facets = weekend~.,
                           xlab = "5 minute interval", ylab = "Average # of step", 
                           main = "average number of steps per interval by weekday vs weekend"))

