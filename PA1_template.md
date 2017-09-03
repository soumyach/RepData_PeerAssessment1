loading the data from the url given.

```{r, echo = TRUE}
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
data <- read.csv(unz(temp, "activity.csv"))
unlink(temp)
```

Processing the activity data

```{r, echo=TRUE}
day<- weekdays(as.Date(data$date))
date_time<- as.POSIXct(data$date, format="%Y-%m-%d")
```

pulling data without nas

```{r, echo= TRUE}
clean_data <- data[!is.na(data$steps),]
```

Histogram of total number of steps taken each day.

Calculating the total number of steps taken each day.

```{r, echo= TRUE}
sum1 <- aggregate(data$steps~data$date,FUN=sum,)
colnames(sum1) <- c("date","steps")
png("plot1.png", width=480, height=480)
hist(sum1$steps, col= "red",xlab= "steps", main= "Total steps taken in a day")
dev.off()
```

mean and median of the number of steps taken each day.

```{r, echo= TRUE}
mean1 <- mean(sum1$steps)
median1 <-median(sum1$steps)
mean1
median1
```

time series plot of the average number of steps taken.
creating the intervals for average number of steps taken.

```{r, echo= TRUE}
library(plyr)
library(ggplot2)
interval_data <- ddply(clean_data, .(interval), summarize, avg = mean(steps))
png("plot2.png", width=480, height=480)
g <- ggplot(interval_data, aes(x=interval, y= avg), xlab= "interval", ylab= "
            average number of steps")
g+geom_line()+ggtitle("Average number of steps per interval")
dev.off()
```

Maximum number of steps per interval.
``` {r, echo= TRUE}
maximum <- max(interval_data$avg)
interval_data[interval_data$avg==maximum,1]
```

Code to describe and show a strategy for imputing missing data.


```{r, echo=TRUE}
steps_by_interval <- aggregate(steps ~ interval, data, mean)
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))
```

Histogram to show the difference
```{r, echo= TRUE}
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
png("plot3.png", width=480, height=480)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
hist(sum1$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)
dev.off()
```

Calculating mean and median
```{r, echo= TRUE}
mean2 <- mean(steps_by_day_i$steps)
median2 <- median(steps_by_day_i$steps)
mean2
median2
```

Check the difference 
```{r, echo= TRUE}
mean2 - mean1
median2 - median1
sum(steps_by_day_i$steps) - sum(sum1$steps)
```

Activity patterns in weekdays and weekends

```{r, echo= TRUE}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)
library(lattice)
png("plot4.png", width=480, height=480)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
dev.off()
```


