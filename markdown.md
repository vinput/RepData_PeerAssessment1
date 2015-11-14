---
title: "Reproducible Research: Peer Assessment 1 - Vinod Puthukkudy"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
# Reproducible Research: Peer Assessment 1 - Vinod Puthukkudy

#Loading and preprocessing the data

#Show any code that is needed to

#  Load the data (i.e. read.csv())

#   Process/transform the data (if necessary) into a format suitable for your analysis



setwd("C:/Data-Scientist/repdata-034/repdata_data_activity")

activity <- read.csv("activity.csv", colClasses = c("numeric", "character","numeric"))

head(activity)

names(activity)

library(lattice)

activity$date <- as.Date(activity$date, "%Y-%m-%d")



## What is mean total number of steps taken per day?

# For this part of the assignment, you can ignore the missing values in the dataset.

# 1 -  Calculate the total number of steps taken per day


#Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.

StepsTotal <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)

# 2 - Plot the histogram - Make a histogram of the total number of steps taken each day

hist(StepsTotal$steps, main = "Total steps by day", xlab = "day", col = "red")

# 3 -  Calculate and report the mean and median of the total number of steps taken per day

# mean
mean(StepsTotal$steps)
# median 
median(StepsTotal$steps)

# answer 

##################  The Mean is 10766.19 and the Median is 10765 ######################

## What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across 
# all days (y-axis)  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

rdata <- read.csv("activity.csv")

#Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.

rdata$date <- as.Date(rdata$date, format = "%Y-%m-%d")
rdata$interval <- as.factor(rdata$interval)
steps_per_interval <- aggregate(rdata$steps,by = list(interval = rdata$interval),FUN=mean, na.rm=TRUE)
##If you do not understand the difference between a histogram and a barplot, research the difference between them. ##Make a histogram of the total number of steps taken each day

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
