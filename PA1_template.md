# Reproducible Research Course Project 1
Alisha Qazi  
10/16/2017  

## Loading and preprocessing the data


```r
library(ggplot2)
library(plyr)
activity <- read.csv("activity.csv")
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime <- as.POSIXct(activity$date, format="%Y-%m-%d")
usedata <- activity[!is.na(activity$steps),]
```

## What is mean total number of steps taken per day?


```r
sums <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(sums) <- c("Date","Steps")
hist(sums$Steps, breaks=5, xlab="Steps", main="Total Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
as.integer(mean(sums$Steps))
```

```
## [1] 10766
```

```r
as.integer(median(sums$Steps))
```

```
## [1] 10765
```
The mean number of steps per day is 10766 steps.
The median number of steps per day is 10765 steps.

## What is the average daily activity pattern?


```r
intervalsteps <- ddply(usedata, .(interval), summarize, Avg=mean(steps))
g <- ggplot(intervalsteps, aes(x=interval, y=Avg), xlab="5-Minute Interval", ylab="Average Number of Steps Taken")
g + geom_line() + xlab("5-Minute Interval")+ylab("Average Number of Steps Taken")+ggtitle("Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxSteps <- max(intervalsteps$Avg)
intervalsteps[intervalsteps$Avg==maxSteps,1]
```

```
## [1] 835
```
The 5-minute interval that contains the maximum number of steps is the 835 interval.

## Imputing missing values


```r
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```

```r
averagesteps <- ddply(usedata, .(interval,day), summarize, Avg=mean(steps))
dataNA <- activity[is.na(activity$steps),]
newdataset <- merge(dataNA, averagesteps, by=c("interval","day"))
NEWdata <- newdataset[,c(6,4,1,2,5)]
colnames(NEWdata) <- c("steps","date","interval","day","DateTime")
withsubstitutes <- rbind(usedata, NEWdata)
sums2 <- aggregate(withsubstitutes$steps ~ withsubstitutes$date, FUN=sum, )
colnames(sums2) <- c("Date","Steps")
as.integer(mean(sums2$Steps))
```

```
## [1] 10821
```

```r
as.integer(median(sums2$Steps))
```

```
## [1] 11015
```

```r
hist(sums2$Steps, breaks=5, xlab="Steps", main="Total Steps per Day with NAs Replaced", col="Red")
hist(sums$Steps, breaks=5, xlab="Steps", main="Total Steps per Day with NAs Replaced", col="Blue", add=T)
legend("topright", c("NAs replaced","NAs not replaced"), fill=c("red","blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
The new mean number of steps is 10821, and the old mean was 10766, so the difference is 55 more steps on average per day when the NA's are changed.
The new median number of steps is 11015, and the old median was 10765, so the difference is 250 more steps when the NA's are changed.

## Are there differences in activity patterns between weekdays and weekends?


```r
withsubstitutes$DayCategory <- ifelse(withsubstitutes$day %in% c("Saturday","Sunday"),"Weekend","Weekday")
library(lattice)
averagesteps2 <- ddply(withsubstitutes, .(interval,DayCategory), summarize, Avg=mean(steps))
xyplot(Avg~interval|DayCategory, data=averagesteps2, type="l", layout=c(1,2),main="Activity Patterns Given Type of Day",ylab="Average Number of Steps Taken",xlab="5-Minute Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

