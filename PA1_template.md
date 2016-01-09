# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
dataset_filename = "/activity.csv"
dataset_path = paste(getwd(),dataset_filename,sep="")

# loading data from file
dt = read.csv(dataset_path)
```

## What is mean total number of steps taken per day?


```r
# summing up the total steps taken per day
tab = aggregate(dt[!is.na(dt$steps), 1], list(dt[!is.na(dt$steps),2]), sum)

barplot(tab[,2], main="steps taken per day",
   xlab="Days", ylab = "Total steps")
```

![](PA1_template_files/figure-html/steps_day-1.png) 

```r
# mean of the total number of steps taken per day
mean(tab[,2])
```

```
## [1] 10766.19
```

```r
# median of the total number of steps taken per day
median(tab[,2])
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
tab = aggregate(dt[, 1], list(dt[ ,3]), mean, na.rm = T)

plot(tab[,1],tab[,2], type="l", xlab = "Intervals", ylab = "Average")
```

![](PA1_template_files/figure-html/daily_activity-1.png) 

## Imputing missing values

```r
# Total number of missing values
sum(is.na(dt$steps))
```

```
## [1] 2304
```

```r
j = 1
for(i in tab[,1]){
  dt$steps[is.na(dt$steps) & dt$interval == i] = tab[j,2]
  j = j + 1
}

# summing up the total steps taken per day
tab = aggregate(dt[,1], list(dt[,2]), sum)

barplot(tab[,2], main="steps taken per day",
   xlab="Days", ylab = "Total steps")
```

![](PA1_template_files/figure-html/missing_values-1.png) 

```r
# mean of the total number of steps taken per day
mean(tab[,2])
```

```
## [1] 10766.19
```

```r
# median of the total number of steps taken per day
median(tab[,2])
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?


```r
library (lattice)

w = weekdays.POSIXt(as.POSIXct(dt$date))

# it must change to your language
weekend = w == 'sábado' | w == 'domingo'

w[weekend] = "weekend"
w[!weekend] = "weekday"

tab = aggregate(dt[, 1], list(w, dt[,3]), mean )

xyplot(x ~ Group.2 | Group.1, data = tab, type="l"  ,xlab = "Interval", ylab = "Number of steps", layout=c(1,2))
```

![](PA1_template_files/figure-html/week-1.png) 
