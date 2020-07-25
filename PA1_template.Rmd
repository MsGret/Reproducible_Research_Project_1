---
title: "Coursera Project 1"
author: "MsGret"
date: "26 07 2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data

Load the data and process/transform the data:

```{r}
zipFilename <- "repdata_data_activity.zip"

if (!file.exists("./Data")){
    dir.create(path = "./Data")
}

if(file.exists(zipFilename)) {        
    unzip(zipFilename, exdir = "./Data")
}

activity <- read.csv("./Data/activity.csv")

Sys.setlocale("LC_TIME", "English")
activity$date <- as.Date(activity$date, "%Y-%m-%d")

head(activity)
str(activity)
```

## What is mean total number of steps taken per day?

Ignore the missing values in the dataset and calculate the total number of steps taken per day:

```{r}
StepsPerDay <- tapply(activity$steps, as.factor(activity$date), sum, na.rm = TRUE)
```

Make a histogram of the total number of steps taken each day:

```{r}
hist(StepsPerDay, col = "green", breaks = seq(0, 25000, by = 2500), ylim = c(0, 20),
     main = "Total number of steps per day", xlab = "Total steps per day")

dev.copy(png, file = "plot1.png")
dev.off()
```

Calculate and report the mean and median of the total number of steps taken per day:

```{r}
MeanStepsPerDay <- round(mean(StepsPerDay, na.rm = TRUE))
MedianStepsPerDay <- median(StepsPerDay, na.rm = TRUE)
```

So, the mean of the total number of steps taken per day is `r MeanStepsPerDay`. The median of the total number of steps taken per day is `r MedianStepsPerDay`.

## What is the average daily activity pattern?

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):

```{r}
AvrSteps <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)

plot(as.numeric(names(AvrSteps)), AvrSteps, type = "l",
     main = "The average daily activity pattern", xlab = "5-minute interval",
     ylab = "Average number of steps")

dev.copy(png, file = "plot2.png")
dev.off()
```

Find the maximum number of steps per interval:

```{r}
maxInterval <- as.numeric(names(which.max(AvrSteps)))
```

So, `r maxInterval`th interval, on average across all the days in the dataset, contains the maximum number of steps.

## Imputing missing values

The presence of missing days may introduce bias into some calculations or summaries of the data. Calculate and report the total number of missing values in the dataset:

```{r}
sum(is.na(activity$steps))
```

Fill missing values using mean number of steps in interval.

Create a new dataset that is equal to the original dataset but with the missing data filled in:

```{r}
fullActivity <- activity
fullActivity$steps <- replace(activity$steps, is.na(activity$steps),
                              AvrSteps[as.character(activity[is.na(activity$steps),"interval"])])
```

Make a histogram of the total number of steps taken each day:

```{r}
StepsPerDayNew <- tapply(fullActivity$steps, as.factor(fullActivity$date), sum, na.rm = TRUE)

hist(StepsPerDayNew, col = "green", breaks = seq(0, 25000, by = 2500), ylim = c(0, 20),
     main = "Total number of steps per day", xlab = "Total steps per day")

dev.copy(png, file = "plot3.png")
dev.off()
```

Calculate the mean and median total number of steps taken per day:

```{r}
mean(StepsPerDayNew)
median(StepsPerDayNew)
```

So, we replace missing value by average value for interval, it shift frequency of total number of steps per day to the center region (close to the mean).

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
fullActivity$weekday <- as.factor(ifelse(weekdays(fullActivity$date) == "Saturday" |
                                             weekdays(fullActivity$date) == "Sunday",
                                         "weekend", "weekday"))

head(fullActivity)
```

Find the average number of steps taken, averaged across all weekday days or weekend days:

```{r results="hide"}
library(dplyr)

AvrStepsNew <- fullActivity %>%
    group_by(weekday, interval) %>%
    summarise(avrSteps = mean(steps))
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):

```{r}
library(lattice)

xyplot(avrSteps ~ interval | weekday, data = AvrStepsNew, type = "l", layout = c(1,2),
       main = "The average daily activity pattern",
       xlab = "Interval", ylab = "Number of steps")

dev.copy(png, file = "plot4.png")
dev.off()
```
