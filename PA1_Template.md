---
title: 'Reproducable Research: Project 1'
author: "Quinn Koller"
date: "6/10/2019"
output: 
  html_document:
        keep_md: true
---
## Loading the data

We first load the data from activity.csv into a data frame named ActivityData and convert the date field form strings into proper date format.


```r
ActivityData<-read.csv("./activity.csv",header = TRUE)
ActivityData[,2]<-as.Date(ActivityData$date)
```

We next will get an overview of the data we will be working with,


```r
str(ActivityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## Q1. What is mean total number of steps taken per day?
First we create a histogram to examine the distribution of the total steps per day


```r
TotalSteps<-with(ActivityData,tapply(steps,date,sum,na.rm=TRUE))
hist(TotalSteps,col = "gray",xlab = "Total Steps",ylab = "Frequency",main = "Total Number of Steps per Day")
```

![](PA1_Template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

We now calculate the mean (average) number of steps taken per day


```r
print(mean_steps<-mean(TotalSteps))
```

```
## [1] 9354.23
```

and the median number of steps taken per day.


```r
print(median_steps<-median(TotalSteps))
```

```
## [1] 10395
```
So the average total steps taken each day is 9354.2295082 and, the average median is is calculated to be 10395

Let’s also look at the summary of the total steps taken each day


```r
summary(TotalSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

## Q2. What is the average daily activity pattern?
To answer this question we'll create a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
avg_steps<-with(ActivityData,tapply(steps,interval,mean,na.rm=TRUE))
intervals<-unique(ActivityData$interval)
new<-data.frame(cbind(avg_steps,intervals))
plot(new$intervals,new$avg_steps,type = "l",xlab = "Intervals",
     ylab = "Average Steps",main = "Average Steps per Interval")
```

![](PA1_Template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
Now, we'll determine which interval contains the maximum number of steps


```r
index<-which.max(new$avg_steps)
max<-new[index,2]
```

The interval containing the maximum number of steps is 835

## Imputing missing values

To do this we will calculate the average steps per day across all dates in the data set (ignoring NA values). We will then use this resulting value to replace all NAs.

We will create a new dataset based on the original dataset where all the missing data (NAs) have been filled in.


```r
sum(is.na(ActivityData$steps))
```

```
## [1] 2304
```


```r
index<-which(is.na(ActivityData$steps))
l<-length(index)
steps_avg<-with(ActivityData,tapply(steps,date,mean,na.rm=TRUE))
na<-mean(steps_avg,na.rm = TRUE)
for (i in 1:l) {
        ActivityData[index[i],1]<-na
}
```
We'll check to see that all NAs were replaced and see what the re dataset looks like


```r
sum(is.na(ActivityData$steps))
```

```
## [1] 0
```
All NAs have been succesfully replaced.The new dataset looks like this


```r
str(ActivityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  37.4 37.4 37.4 37.4 37.4 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
We'll next create a histogram of total steps taken each day using the new dataset


```r
TotalSteps2<-with(ActivityData,tapply(steps,date,sum,na.rm=TRUE))
hist(TotalSteps2,col = "gray",xlab = "Total Steps",ylab = "Frequency",main = "Total Number of Steps per Day")
```

![](PA1_Template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

We'll now calculate the mean and median of total steps taken each day


```r
print(mean_steps_2<-mean(TotalSteps2))
```

```
## [1] 10766.19
```


```r
print(median_steps_2<-median(TotalSteps2))
```

```
## [1] 10766.19
```

We see that both the mean and median average total steps are the same, 10766.19

## Q3. Are there differences in activity patterns between weekdays and weekends?
In this section, we will use dplyr package and we need to load it from the library.


```r
library(dplyr)
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

Also, we will need to create a new variable in the dataset named “day” that shows the day of the week in terms of weekday or weekend.


```r
activity_mod<- mutate(ActivityData, day = ifelse(weekdays(ActivityData$date) == "Saturday" | weekdays(ActivityData$date) == "Sunday", "weekend", "weekday"))
activity_mod$day<-as.factor(activity_mod$day)
str(activity_mod)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  37.4 37.4 37.4 37.4 37.4 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Now, let’s plot the weekday and weekend data in seperate graphs


```r
act_wknd<-subset(activity_mod,as.character(activity_mod$day)=="weekend")
act_wkdy<-subset(activity_mod,as.character(activity_mod$day)=="weekday")
steps_wknd<-with(act_wknd,tapply(steps,interval,mean,na.rm=TRUE))
steps_wkdy<-with(act_wkdy,tapply(steps,interval,mean,na.rm=TRUE))
int_wknd<-unique(act_wknd$interval)
int_wkdy<-unique(act_wkdy$interval)
new_wknd<-data.frame(cbind(steps_wknd,int_wknd))
new_wkdy<-data.frame(cbind(steps_wkdy,int_wkdy))
par(mfrow=c(2,1),mar=c(4,4,2,1))
plot(new_wknd$int_wknd,new_wknd$steps_wknd,type = "l",xlab = "Intervals",
     ylab = "Average Steps",main = "Weekend")
plot(new_wkdy$int_wkdy,new_wkdy$steps_wkdy,type = "l",xlab = "Intervals",
     ylab = "Average Steps",main = "Weekday")
```

![](PA1_Template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

## We see that average steps over the weekends are more than that of the weekdays


