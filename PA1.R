# config

Sys.setlocale("LC_TIME", "C")
library(ggplot2)
library(gridExtra)

# Load and preprocess data

dt = read.csv("activity.csv")

dt$date = as.Date(dt$date)

# Mean total number of steps taken per day
# (ignore the missing values in the dataset)

## Make a histogram of the total number of steps taken each day

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

## Calculate and report the mean and median total number of steps taken per day

mean(steps_per_day$steps)

median(steps_per_day$steps)

# The average daily activity pattern

## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
## and the average number of steps taken, averaged across all days (y-axis)

steps_per_interval = aggregate(dt$steps, 
                               by=list(interval=dt$interval), 
                               FUN=function(steps,...){
                                   #steps[is.na(steps)] = 0
                                   mean(steps,na.rm=T,...)
                                   })
colnames(steps_per_interval) = c("interval", "steps")

p = ggplot(steps_per_interval, aes(interval, steps)) + geom_line() + theme_bw()
print(p)

## Which 5-minute interval, on average across all the days in the dataset,
## contains the maximum number of steps?

steps_per_interval[which.max(steps_per_interval$steps),]$interval

# Imputing missing values

# Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)

sum(is.na(dt$steps))

## Devise a strategy for filling in all of the missing values in the dataset. The
## strategy does not need to be sophisticated. For example, you could use
## the mean/median for that day, or the mean for that 5-minute interval, etc.

## Create a new dataset that is equal to the original dataset but with the
## missing data filled in.

dt_nona = dt

for (i in 1:nrow(dt_nona))
{
    row = dt_nona[i,]
    
    if (is.na(row$steps))
        dt_nona[i,"steps"] = floor(steps_per_interval[steps_per_interval$interval==row$interval,]$steps)
}

## Make a histogram of the total number of steps taken each day

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

## Calculate and report the mean and median total number 
## of steps taken per day. 

mean(steps_per_day_nona$steps)

median(steps_per_day_nona$steps)


## Do these values differ from the estimates from the first part of the assignment?
## What is the impact of imputing missing data on the estimates of the total
## daily number of steps?

# Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels – “weekday”
## and “weekend” indicating whether a given date is a weekday or weekend
## day.

dt_nona_wd = dt_nona
dt_nona_wd$daytype = ifelse(weekdays(dt_nona_wd$date) %in% c("Saturday", "Sunday"),
                                       "weekend",
                                       "weekday")
dt_nona_wd$daytype <- as.factor(dt_nona_wd$daytype)

## Make a panel plot containing a time series plot (i.e. type = "l") of the
## 5-minute interval (x-axis) and the average number of steps taken, averaged
## across all weekday days or weekend days (y-axis).


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



