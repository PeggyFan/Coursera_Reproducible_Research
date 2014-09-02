#download and unzip data
setwd("/Users/peggyfan/Downloads/R_data/Reproducible_research")

temp <- tempfile(fileext = ".zip")
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = temp, method = "curl")
data <- read.csv(unzip(temp))

library(plyr)
day <-ddply(data, c("date"),summarize,tot=sum(steps), na.rm=TRUE)

library(date)
day$date <- as.Date(day$date, format="%Y-%m-%d") 

hist(day$tot, axes=F, ylim=c(0,35))
axis(1)
axis(side=2,)

##
cdata <- ddply(data, .(date), summarise,
               sum_steps = sum(steps, na.rm=TRUE),
               mean = mean(steps, na.rm=TRUE),
               median = median(steps, na.rm=TRUE) )
library(xtable)
xt <- xtable(cdata)
print(xt)
(cdata)

idata <- ddply(data, c("interval"), summarise, mean= mean(steps, na.rm=TRUE))
graphics::plot.default(
  x = idata$interval,
  y = idata$mean,
  type = "l",
  xlab = "Intervals",
  ylab = "Average",
  main = "Average Steps By Intervals"
)

x<- which.max( idata [,2] )
idata$interval[x]

summary(data[!complete.cases(data$steps),])

## Using average of intervals across all days for imputed means
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
dat2 <- ddply(data, ~ interval, transform, steps = impute.mean(steps))
sort(dat2$date)

day2 <-ddply(dat2, c("date"),summarize,tot=sum(steps), na.rm=TRUE)
hist(day2$tot, axes=F)
axis(1)
axis(2, at=seq(0,35, by=5), labels=seq(0,35, by=5))
# 

cdata2 <- ddply(dat2, .(date), summarise,
               sum_steps  = sum(steps, na.rm=TRUE),
               mean = mean(steps, na.rm=TRUE),
               median = median(steps, na.rm=TRUE) )
(cdata2)
hist(cdata2$sum_steps)

## 
library(timeDate)
dat2$dayofweek <- weekdays(as.Date(dat2$date))

library(car)
dat2$day_week <- recode(dat2$dayofweek, 
                "c('Saturday', 'Sunday')='Weekend' ; 
                c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')='Weekday'")
day_week <- factor(dat2$day_week, labels = c("Weekday", "Weekend"))

#library(plyr)
idat2 <- ddply(data, c("interval", "day_week"), summarise, mean= mean(steps, na.rm=TRUE))
     
library(ggplot2)     
ggplot(idat2, aes(x=interval, y=mean)) +
    geom_line (data = idat2) +
    facet_wrap(~day_week, ncol = 1) +
    ggtitle("Average number of steps by intervals") +
    theme(legend.position="none") 

## alternative
library(lattice)
xyplot(steps ~ interval | factor(dayClass), type = "l",
       data = dayClassData,
       main = "Mean of total steps taken each day on weekends vs. weekdays",
       xlab = "Number of steps",
       ylab = "Interval",
       layout = c(1, 2))

## Plotting each day on the x-axis, histogram
g <- ggplot(stepsperdayfull, aes(factor(date), total_steps))
g <- g + geom_histogram(stat = "identity",fill="#C0D9AF", colour="black")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, colour="black"))
g + labs(y = "steps taken", x = "Day", title = "steps taken per day (full data)")