---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.2.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
```

# Reproducible Research Course Project 1

Using the activity monitoring data as described in the coursera assignment.

## Loading the data


```r
# url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# download.file(url, "mydata.zip")
# unzip("mydata.zip")
# monitorData <- read.csv("activity.csv")

monitorData <- read.csv("./activity.csv")
```

## What is mean total number of steps taken per day?

Finding the total number of steps taken per day, then finding the mean and median values.


```r
#calculate the number of steps taken per day
monitorDataGrouped <- monitorData |> group_by(date)
totalSteps <- monitorDataGrouped |> summarise(total_steps = sum(steps, na.rm = TRUE))

#plot histogram
hist(totalSteps$total_steps, xlab = "Total number of steps (/day)", main = "Histogram of total steps per day")
```

![](Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#calculate mean and median of steps taken per day
meanMedianSteps <- totalSteps |> summarise(mean_steps = mean(total_steps), median_steps = median(total_steps))
averageSteps <-as.integer(meanMedianSteps$mean_steps)
medianSteps <- meanMedianSteps$median_steps
```

The mean number of steps taken per day is: 9354. The median number of steps taken per day is: 10395.

## What is the average daily activity pattern?

```r
#calculate average steps per day
meanDailySteps <- monitorData |>  group_by(interval) |> summarise(mean_daily_steps = mean(steps, na.rm = TRUE))

#plot graph
with(meanDailySteps, plot(interval, mean_daily_steps, type = "l", main = "Average Daily Activity", xlab = "Interval (s)", ylab = "Average Steps (per day)"))
```

![](Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxInterval <- subset(meanDailySteps, mean_daily_steps == max(meanDailySteps$mean_daily_steps), select = interval)
```

The maximum number of steps, on average, was at 835.

## Imputing Missing Values

```r
#checking the number of NAs
naCount <- sum(is.na(monitorData$steps))
```
There are 2304 number of missing values in the dataset.


```r
#filling in the missing values
mdImputed <- monitorData #to preserve the original data

for (i in 1:nrow(mdImputed)) {
    if (is.na(mdImputed$steps[i])) {
        mds <- subset(meanDailySteps, interval == mdImputed$interval[i], select = mean_daily_steps)
        mdImputed$steps[i] <- mds
    }    
}

#plotting histogram
mdImpGrouped <- mdImputed |> group_by(date) |> summarise(total_steps_imp = sum(unlist(steps)))

hist(mdImpGrouped$total_steps_imp, xlab = "Total number of steps (/day)", main = "Histogram of total steps per day")
```

![](Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#calculate mean and median of steps taken per day
meanMedianStepsImp <- mdImpGrouped |> summarise(mean_steps = mean(total_steps_imp), median_steps = median(total_steps_imp))
averageStepsImp <- as.integer(meanMedianStepsImp$mean_steps)
medianStepsImp <- as.integer(meanMedianStepsImp$median_steps)
```

The new mean total number of steps taken per day is:10766, compared to before which was 9354. 
The new median total number of steps taken per day is:  10766 compared to before which was 10395.

This suggests that imputing the missing data increased the overall estimates of daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?

```r
#indicating whether date is weekend or weekday
mdWeekdays <- mutate(mdImputed, day = weekdays(as.POSIXct(date)))
mdWeekdays <- mutate(mdWeekdays, day = ifelse(day == "Saturday" | day == "Sunday", "Weekend", "Weekday"))


#plotting
xyplot(steps~interval|day, data = mdWeekdays, type = "l", layout = c(1,2))
```

![](Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

