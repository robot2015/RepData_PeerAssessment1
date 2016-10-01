# R Reading in a zip data file without unzipping it
# http://stackoverflow.com/questions/12460938/r-reading-in-a-zip-data-file-without-unzipping-it

# Using R to download zipped data file, extract, and import data
# http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data

rm(list = ls())
library(plyr)
library(ggplot2)
library(lattice)

## Loading and preprocessing the data.

# 1. Code for reading in the dataset and/or processing the data.
temp <- tempfile(fileext = ".zip")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
              temp)
activity <- read.csv(unz(temp, "activity.csv"))
unlink(temp)
str(activity)

## What is mean total number of steps taken per day?

# Calculate the total number of steps taken per day
activity.date <- ddply(activity,
                       .(date),
                       plyr::summarize,
                       sum_steps = sum(steps, na.rm = TRUE))

# If you do not understand the difference between a histogram and a barplot,
# research the difference between them. Make a histogram of the total number of
# steps taken each day

# 2. Histogram of the total number of steps taken each day.
hist(
    activity.date$sum_steps,
    breaks = 20,
    main = "Histogram of Total Steps per Day",
    xlab = "Total steps per day"
)

# Calculate and report the mean and median of the total number of steps taken per day

# 3. Mean and median number of steps taken each day.
mean(activity.date$sum_steps, na.rm = TRUE)
median(activity.date$sum_steps, na.rm = TRUE)

## What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)

# 4. Time series plot of the average number of steps taken.
activity.interval <- ddply(activity,
                           .(interval),
                           plyr::summarize,
                           mean_steps = mean(steps, na.rm = TRUE))
plot(activity.interval$interval,
     activity.interval$mean_steps,
     type = "l")

# Which 5-minute interval, on average across all the days in the dataset, contains the
# maximum number of steps?

# 5. The 5-minute interval that, on average, contains the maximum number of steps.
activity.interval[activity.interval$mean_steps == max(activity.interval$mean_steps), "interval"]

## Imputing missing values

# Calculate and report the total number of missing values in the dataset (i.e.
# the total number of rows with NAs)
apply(activity, 2, function(x)
    sum(is.na(x)))
sum(apply(activity, 1, function(x)
    sum(sum(is.na(
        x
    )) > 0)))

# Devise a strategy for filling in all of the missing values in the dataset. The
# strategy does not need to be sophisticated. For example, you could use the
# mean/median for that day, or the mean for that 5-minute interval, etc.
steps2 <- ddply(activity,
                .(interval),
                plyr::summarize,
                median_steps = median(steps, na.rm = TRUE))

# Create a new dataset that is equal to the original dataset but with the
# missing data filled in.
activity2 <- activity
activity2$id <- 1:nrow(activity2)
activity2 <- merge(activity2, steps2)
activity2 <- activity2[order(activity2$id), ]
activity2$id <- NULL

# 6. Code to describe and show a strategy for imputing missing data.
activity2$steps[is.na(activity2$steps)] <-
    activity2$median_steps[is.na(activity2$steps)]
activity2$median_steps <- NULL
activity2 <- activity2[, c(2, 3, 1)]
str(activity2)

# Make a histogram of the total number of steps taken each day and Calculate and
# report the mean and median total number of steps taken per day. Do these
# values differ from the estimates from the first part of the assignment? What
# is the impact of imputing missing data on the estimates of the total daily
# number of steps?
activity$missing_steps <- "Has NA"
activity2$missing_steps <- "Replaced NA"
activity3 <- rbind(activity, activity2)
activity3$missing_steps <- as.factor(activity3$missing_steps)

activity3.date <- ddply(activity3,
                        .(date, missing_steps),
                        plyr::summarize,
                        sum_steps = sum(steps, na.rm = TRUE))

# 7. Histogram of the total number of steps taken each day after missing values are imputed.
qplot(
    sum_steps,
    data = activity3.date,
    facets = missing_steps ~ .,
    bins = 20,
    main = "Histogram of Total Steps per Day",
    xlab = "Total steps per day"
)

ddply(
    activity3.date,
    .(missing_steps),
    plyr::summarize,
    mean_sum_steps = mean(sum_steps, na.rm = TRUE),
    median_sum_steps = median(sum_steps, na.rm = TRUE)
)

## Are there differences in activity patterns between weekdays and weekends?

# Create a new factor variable in the dataset with two levels - "weekday" and
# "weekend" indicating whether a given date is a weekday or weekend day.
activity3$date <-
    as.POSIXlt(as.character(activity3$date), format = "%Y-%m-%d")
activity3$wd <- weekdays(activity3$date)

activity3[activity3$wd == "Monday", "wd"] <- "weekday"
activity3[activity3$wd == "Tuesday", "wd"] <- "weekday"
activity3[activity3$wd == "Wednesday", "wd"] <- "weekday"
activity3[activity3$wd == "Thursday", "wd"] <- "weekday"
activity3[activity3$wd == "Friday", "wd"] <- "weekday"
activity3[activity3$wd == "Saturday", "wd"] <- "weekend"
activity3[activity3$wd == "Sunday", "wd"] <- "weekend"
activity3$wd <- as.factor(activity3$wd)
str(activity3)

# Make a panel plot containing a time series plot (i.e. type = "l") of the
# 5-minute interval (x-axis) and the average number of steps taken, averaged
# across all weekday days or weekend days (y-axis).
activity4 <- activity3[activity3$missing_steps == "Replaced NA", ]
activity5 <-
    aggregate(activity4$steps,
              by = list(interval = activity4$interval, wd = activity4$wd),
              mean)
colnames(activity5) <- c("interval", "wd", "steps")

xyplot(
    steps ~ interval |
        wd,
    data = activity5,
    layout = c(1, 2),
    type = "l",
    xlab = "Interval",
    ylab = "Number of steps"
)