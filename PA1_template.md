

```r
---
title: "Reproducible Research: Course Project #1"
output: html_document
---
```

```
## Error: <text>:6:0: unexpected end of input
## 4: ---
## 5: 
##   ^
```


## Part1: Loading and preprocessing the data


The location of the rawdata is assigned to the variable in_dir, the data is read to the variable
'activity'


```r
in_dir<-'C://datascience//reproducible_research//course_project_1//repdata%2Fdata%2Factivity'
setwd(in_dir)
activity <- read.csv("activity.csv",check.names=FALSE, colClasses = "character")
activity$steps<-as.numeric(activity$steps)
activity$date<-as.Date(activity$date)
activity$interval<-as.numeric(activity$interval)
```

## Part 2: What is mean total number of steps taken per day?

We use the aggregate function to find sum of activties across interval, but by day.
The mean and median steps are stored in variables 'mean_steps' and 'median_steps' respectively.

```r
summed_activity<-aggregate(activity$steps,list(activity$date),sum)
names(summed_activity)[names(summed_activity)=="x"] <- "total_steps"
names(summed_activity)[names(summed_activity)=="Group.1"] <- "date"

hist(summed_activity$total_steps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
mean_steps<-mean(summed_activity$total_steps,na.rm = TRUE)
median_steps<-median(summed_activity$total_steps,na.rm = TRUE)
```


## Part 3: What is the average daily activity pattern?
We use the aggregate function to find sum of activties across days, but by interval
The peak activity interval is 835


```r
averaged_activity<-aggregate(activity$steps,list(activity$interval),na.rm=TRUE,mean)
names(averaged_activity)[names(averaged_activity)=="x"] <- "averaged_steps"
names(averaged_activity)[names(averaged_activity)=="Group.1"] <- "interval"
plot(averaged_activity$interval,averaged_activity$averaged_steps,type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
peak_interval<-averaged_activity$interval[which.max(averaged_activity$averaged_steps)]
```

## Part 4: Inputting the missing values?
A new column titled 'steps_modified' is added to the data frame titled 'activity'.
Whenever the there is a missing value in the 'steps' column, it is replaced with the mean steps,
corresponding to the interval.
The new values for mean and median are stored in variables 'mean_steps_new' and 'median_steps_new', respectively.
No difference in mean or median steps taken per day was found.

```r
NA_count<-sum(is.na(activity$steps))
indx <- which(is.na(activity$steps), arr.ind = TRUE)
activity$steps_modified<-activity$steps
for (i in 1:length(indx)){
  activity$steps_modified[indx[i]]<-averaged_activity$averaged_steps[(which(averaged_activity$interval==activity$interval[indx[i]]))]
  }
summed_activity<-aggregate(activity$steps_modified,list(activity$date),sum)
names(summed_activity)[names(summed_activity)=="x"] <- "total_steps"
names(summed_activity)[names(summed_activity)=="Group.1"] <- "date"
hist(summed_activity$total_steps)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
mean_steps_new<-mean(summed_activity$total_steps,na.rm = TRUE)
median_steps_new<-median(summed_activity$total_steps,na.rm = TRUE)
```


##PArt 5: Are there differences in activity patterns between weekdays and weekends?
A new column 'Day' is added which determines if its a weekday or weekend.
Line plot shows similar spikes for both weekdays and weekends

```r
activity$date<-as.Date(activity$date)
week_days <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity$Day <- factor((weekdays(activity$date) %in% week_days), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
averaged_activity <- aggregate(steps_modified ~ interval + Day, activity, mean)

names(averaged_activity)[names(averaged_activity)=="x"] <- "average_steps"
names(averaged_activity)[names(averaged_activity)=="Group.1"] <- "interval"
names(averaged_activity)[names(averaged_activity)=="Group.2"] <- "Day"
data1<-subset(averaged_activity,Day=="weekend",col=rgb(0,0,1))
plot (data1$interval,data1$steps_modified,type="l",xlab="interval",ylab="average steps")
data2<-subset(averaged_activity,Day=="weekday")
lines(data2$interval,data2$steps_modified,col=rgb(1,0,0))
legend("bottomright",c("weekend","weekday"),fill=c("blue","red"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
```

