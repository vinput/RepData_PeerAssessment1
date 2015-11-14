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
steps_per_interval <- aggregate(rdata$steps, by = list(interval = rdata$interval), FUN=mean, na.rm=TRUE)
steps_per_interval$interval <- as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval]) colnames(steps_per_interval) <- c("interval", "steps")
ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
        geom_line(color="red", size=1) +  
        labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
        theme_bw()
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
#Imputing missing values
 max_interval <- steps_per_interval[which.max(  
+         steps_per_interval$steps),]
>  max_interval
    interval    steps
104      835 206.1698
### Answer  835th interval has maximum 206 steps####
##############Imputing missing values#################
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
 missing_vals <- sum(is.na(rdata$steps))
> missing_vals
[1] 2304
> 
### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be ##sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
> na_fill <- function(data, pervalue) {
+         na_index <- which(is.na(data$steps))
+         na_replace <- unlist(lapply(na_index, FUN=function(idx){
+                 interval = data[idx,]$interval
+                 pervalue[pervalue$interval == interval,]$steps
+         }))
+         fill_steps <- data$steps
+         fill_steps[na_index] <- na_replace
+         fill_steps
+ }
> 
> rdata_fill <- data.frame(  
+         steps = na_fill(rdata, steps_per_interval),  
+         date = rdata$date,  
+         interval = rdata$interval)
> str(rdata_fill)
'data.frame':   17568 obs. of  3 variables:
 $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
 $ date    : Date, format: "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
 $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
> 
0
> sum(is.na(rdata_fill$steps))
[1] 0
> 
###################Zero output shows that there are NO MISSING VALUES###########################
####Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total ###number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? ##What is the impact of imputing missing data on the estimates of the total daily number of steps?
> fill_steps_per_day <- aggregate(steps ~ date, rdata_fill, sum)
> colnames(fill_steps_per_day) <- c("date","steps")
> 
> ##plotting the histogram
> ggplot(fill_steps_per_day, aes(x = steps)) + 
+        geom_histogram(fill = "orange", binwidth = 1000) + 
+         labs(title="Histogram of Steps Taken per Day", 
+              x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 
> 
### mean and Median calculation #############
steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
> steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)
> steps_mean_fill
[1] 10766.19
>  steps_median_fill
[1] 10766.19
####Do these values differ from the estimates from the first part of the assignment?
### Answer : Yes, these values do differ slightly.
##Before filling the data
#   Mean : 10766.19
#   Median: 10765
##   After filling the data
#       Mean : 10766.19
#       Median: 10766.19
# We see that the values after filling the data mean and median are equal.
###What is the impact of imputing missing data on the estimates of the total daily number of steps?

# Comparing with the calculations earlier and now we found mean value remains unchanged, the median value has shifted # & virtual matches to the mean.
# You can see t-student distribution (see both histograms), it seems that the impact of imputing missing values has #increase our peak, but it's not affect negatively our predictions. 
#########For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing # # values for this part.

###  Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given #date is a weekday or weekend day.

##   Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the #average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the #GitHub repository to see an example of what this plot should look like using simulated data.
weekdays_steps <- function(data) {
    weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),
                          FUN=mean, na.rm=T)
    # convert to integers for plotting
    weekdays_steps$interval <- 
            as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
    colnames(weekdays_steps) <- c("interval", "steps")
    weekdays_steps
}

data_by_weekdays <- function(data) {
    data$weekday <- 
            as.factor(weekdays(data$date)) # weekdays
    weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
    weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))

    weekend_steps <- weekdays_steps(weekend_data)
    weekday_steps <- weekdays_steps(weekday_data)

    weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
    weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))

    data_by_weekdays <- rbind(weekend_steps, weekday_steps)
    data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
    data_by_weekdays
}

data_weekdays <- data_by_weekdays(rdata_fill)

## Below you can see the panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends:

ggplot(data_weekdays, aes(x=interval, y=steps)) + 
        geom_line(color="orange") + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
