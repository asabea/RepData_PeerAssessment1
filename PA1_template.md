---
title: "Reproducible Research: Peer Assessment 1"
author: "Ashraf ELSABEA"
date: "October 9, 2018"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
fileName<-"Dataset.zip"
lnk<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if(!file.exists(fileName))
{
  download.file(lnk,destfile="Dataset.zip", mode = "wb")
}
if(file.exists(getwd())){
  unzip("Dataset.zip", files = NULL, exdir=".")
}
```

##Code for reading in the dataset and/or processing the data

```r
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

##1.What is mean total number of steps taken per day?
##Histogram of the total number of steps taken each day

```r
known_Steps <-activity[!is.na(activity$steps),]
dailySteps <- aggregate(steps ~ date, data = known_Steps, sum)
hist(as.numeric(dailySteps$steps), breaks = 30, col = "blue", xlab = "# Steps", main= "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#mean
mean(dailySteps$steps)
```

```
## [1] 10766.19
```

```r
#Median
median(dailySteps$steps)
```

```
## [1] 10765
```


##2.What is the average daily activity pattern?
##Time series plot of the average number of steps taken

```r
AvgDailyPatern <- aggregate(steps ~ interval, data = known_Steps, mean)

plot(AvgDailyPatern$interval, AvgDailyPatern$steps, type = "l", col="blue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average Daily Activity Patern")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
MaxAvgSteps<-max(AvgDailyPatern$steps)
obsNum<-match(MaxAvgSteps,AvgDailyPatern$steps)


#Maximum Number of Steps
max(AvgDailyPatern$steps)
```

```
## [1] 206.1698
```

```r
#The 5-minute interval countain the maximum number of steps
AvgDailyPatern[obsNum,]$interval
```

```
## [1] 835
```



## Imputing missing values

```r
#Total number of missing values in the dataset
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
#update the NA value with the mean steps of the interval.
unknown_Steps <-activity[is.na(activity$steps),]
Means_steps <- aggregate(steps ~ interval, data = known_Steps, mean)
#update NA Steps with mean value for each interval
unknown_Steps$steps <- as.numeric(Means_steps$steps)

#New dataset that is equal to the original dataset but with the missing data filled in.
new_activity <- rbind(known_Steps,unknown_Steps)
new_activity<-  new_activity[order(new_activity[,2],new_activity[,3]),]

#Histogram to plot the new dataset
newdailySteps <- aggregate(steps ~ date, data = new_activity, sum)
hist(as.numeric(newdailySteps$steps), breaks = 30, col = "blue", xlab = "# Steps", main= "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#New mean
mean(newdailySteps$steps)
```

```
## [1] 10766.19
```

```r
#New Median
median(newdailySteps$steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
#Append Days to the new dataset
days <- weekdays(new_activity$date)
new_activity <- cbind(new_activity,days)

#Weekday & weekends
new_activity$Weekday <- ifelse((new_activity$days %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")), TRUE, FALSE)

#Average number of steps for weekdays
weekdaysActivities <- new_activity[new_activity$Weekday,]
weekdaysStepsIntervals <- aggregate(steps ~ interval+Weekday, data = weekdaysActivities, FUN=mean)

#Average number of steps for Weekens
weekendsActivities <- new_activity[!new_activity$Weekday,]
weekendsStepsIntervals <- aggregate(steps ~ interval+Weekday, data = weekendsActivities, FUN=mean)

allaggregatedData<- rbind(weekendsStepsIntervals, weekdaysStepsIntervals)
allaggregatedData$Daytype <- ifelse(allaggregatedData$Weekday == TRUE, "Weekday", "Weekend")


library(lattice)
#Ploting
xyplot(steps ~  interval |Daytype, data = allaggregatedData, layout = c(1,2), type ="l",xlab="Intervals", ylab="# Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
