# Reproducible Research: Peer Assessment 1
## By Raul Goycoolea

Load Required Libraries


```r
library(ggplot2)
```

## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data = read.csv("activity.csv", header = TRUE)
data$date = as.Date(data$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day


```r
stepsbd = aggregate(steps ~ date, data = data, sum)
hist(stepsbd$steps)
```

![plot of chunk mean_histogram](figure/mean_histogram.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
meansteps = mean(stepsbd$steps, na.rm = TRUE)
mediansteps = median(stepsbd$steps, na.rm = TRUE)
```

## What is the average daily activity pattern?


1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgsteps = data.frame(xtabs(steps ~ interval, aggregate(steps ~ interval, data, mean)))
qplot(interval, Freq, data = avgsteps, ylab = "Average Steps", xlab = "5-Minute Daily Interval") + 
  labs(title = "Average Number of Steps Taken by 5-Minute Interval")
```

![plot of chunk act_pattern](figure/act_pattern.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxinterval = which.max(avgsteps$Freq)
maxsteps = max(avgsteps$Freq)

maxinterval
```

```
## [1] 104
```

```r
maxsteps
```

```
## [1] 206.2
```


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
totalrecords = length(data$steps)
completedrecords = length(na.omit(data$steps))
missingrecords = totalrecords - completedrecords

missingrecords
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
data$steps2 = data$steps
for (i in 1:length(data$steps)) if (is.na(data$steps[i])) {
  data$steps2[i] = mean(data$steps, na.rm = TRUE)
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
datanewset = data.frame(steps = data$steps2, date = data$date, interval = data$interval)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsbydayNew = aggregate(steps ~ date, data = datanewset, sum)
hist(stepsbydayNew$steps)
```

![plot of chunk do_hist_steps](figure/do_hist_steps.png) 

```r
mean(stepsbydayNew$steps, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(stepsbydayNew$steps, na.rm = TRUE)
```

```
## [1] 10766
```


## Are there differences in activity patterns between weekdays and weekends?


For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
day = weekdays(datanewset$date)
dayType = vector()
for (item in day) {
  if (item == "Saturday" || item == "Sunday") {
    dayType = append(dayType, "weekend")
  } else {
    dayType = append(dayType, "weekday")
  }
}
datanewset$dayType = factor(dayType)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
avgStepsNew = data.frame(xtabs(steps ~ interval + dayType, aggregate(steps ~ interval + dayType, datanewset, mean)))
qplot(interval, Freq, data = avgStepsNew, facets = dayType ~ .)
```

![plot of chunk do_qplot](figure/do_qplot.png) 

Thanks for reviewing my Assessment. Good look on yours.
