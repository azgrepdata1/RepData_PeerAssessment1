# Reproducible Research: Peer Assessment 1

## Initial setup


```r
Sys.setlocale("LC_TIME", "C")
library(ggplot2)
library(gridExtra)
```

## Loading and preprocessing the data


```r
dt = read.csv("activity.csv")
dt$date = as.Date(dt$date)
```

## What is mean total number of steps taken per day?


```r
steps_per_day = aggregate(dt$steps, 
                          by=list(date=dt$date), 
                          FUN=function(steps,...){sum(steps, na.rm=T,...)})
colnames(steps_per_day) = c("date", "steps")

p = ggplot(steps_per_day, aes(x=date, y=steps)) + 
    geom_bar(stat="identity") +
    scale_x_date(breaks="1 day",
                 limits = c(min(steps_per_day$date), max(steps_per_day$date)) ) +
    ylab("Steps") + 
    xlab("Date") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle=90))

print(p)
```

![](PA1_template_files/figure-html/plot_steps_per_day-1.png) 


```r
mean(steps_per_day$steps)
```

```
## [1] 9354.23
```

```r
median(steps_per_day$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
steps_per_interval = aggregate(dt$steps, 
                               by=list(interval=dt$interval), 
                               FUN=function(steps,...){
                                   #steps[is.na(steps)] = 0
                                   mean(steps,na.rm=T,...)
                                   })
colnames(steps_per_interval) = c("interval", "steps")

p = ggplot(steps_per_interval, aes(interval, steps)) + geom_line() + theme_bw()
print(p)
```

![](PA1_template_files/figure-html/plot_steps_per_interval-1.png) 


```r
steps_per_interval[which.max(steps_per_interval$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values


```r
sum(is.na(dt$steps))
```

```
## [1] 2304
```


```r
dt_nona = dt

for (i in 1:nrow(dt_nona))
{
    row = dt_nona[i,]
    
    if (is.na(row$steps))
        dt_nona[i,"steps"] = floor(steps_per_interval[steps_per_interval$interval==row$interval,]$steps)
}
```


```r
steps_per_day_nona = aggregate(dt_nona$steps, 
                          by=list(date=dt_nona$date), 
                          FUN=function(steps,...){sum(steps, na.rm=F,...)})
colnames(steps_per_day_nona) = c("date", "steps")

p = ggplot(steps_per_day_nona, aes(x=date, y=steps)) + 
    geom_bar(stat="identity") +
    scale_x_date(breaks="1 day",
                 limits = c(min(steps_per_day_nona$date), max(steps_per_day_nona$date)) ) +
    ylab("Steps") + 
    xlab("Date") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle=90))

print(p)
```

![](PA1_template_files/figure-html/plot_steps_per_day_nona-1.png) 


```r
mean(steps_per_day_nona$steps)
```

```
## [1] 10749.77
```

```r
median(steps_per_day_nona$steps)
```

```
## [1] 10641
```

## Are there differences in activity patterns between weekdays and weekends?


```r
dt_nona_wd = dt_nona
dt_nona_wd$daytype = ifelse(weekdays(dt_nona_wd$date) %in% c("Saturday", "Sunday"),
                                       "weekend",
                                       "weekday")
dt_nona_wd$daytype <- as.factor(dt_nona_wd$daytype)
```


```r
subs = dt_nona_wd[dt_nona_wd$daytype == "weekend", ]

subs_steps_per_interval = aggregate(subs$steps, 
                                  by=list(interval=subs$interval), 
                                  FUN=mean)
colnames(subs_steps_per_interval) = c("interval", "steps")

p1 = ggplot(subs_steps_per_interval, aes(interval, steps)) + 
    geom_line() + theme_bw()  + ggtitle("weekend")



subs = dt_nona_wd[dt_nona_wd$daytype == "weekday", ]

subs_steps_per_interval = aggregate(subs$steps, 
                                    by=list(interval=subs$interval), 
                                    FUN=mean)
colnames(subs_steps_per_interval) = c("interval", "steps")

p2 = ggplot(subs_steps_per_interval, aes(interval, steps)) + 
    geom_line() + theme_bw()  + ggtitle("weekday")

grid.arrange(p2, p1, nrow=2)
```

![](PA1_template_files/figure-html/plot_subs_steps_per_interval-1.png) 
