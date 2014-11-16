# Reproducible Research
# Peer Assignment #1
# 2014.11.15

setwd("C:/Users/hmiyake/Documents/RepData_PeerAssignmen1")
df <- read.csv("activity.csv")

cat("Total Records: ", nrow(dt), "\n", sep="")

df.hist <- data.frame(tapply(df$steps, df$date, FUN=sum, na.rm=TRUE))
colnames(df.hist) <- "steps"
hist(df.hist$steps,
     main="Histogram of total number of steps taken each day", 
     xlab="Number of steps taken each day",
     col="cyan"
)

cat("Mean: ", mean(df.hist$steps, na.rm=TRUE), "\n",
    "Median: ", median(df.hist$steps, na.rm=TRUE), sep="")

df.pattern <- data.frame(tapply(df$steps, df$interval, FUN=mean, na.rm=TRUE))
colnames(df.pattern) <- "steps"
plot(row.names(df.pattern), df.pattern$steps, type="l", col="blue",
     main="Average number of steps taken at each 5-min interval",
     xlab="5-min interval",
     ylab="Average across all days"
)

max_interval <- which.max(df.pattern$steps)
names(df.pattern[max_interval,])

cat("Total NA: ", sum(is.na(df$steps)), "\n", sep="")

df2 <- df
for (i in 1:nrow(df2)) {
    if (is.na(df2$steps[i])) {
        df2$steps[i] <- as.integer(df.pattern$steps[which(row.names(df.pattern) == df2$interval[i])])
    }
}
cat("Total NA: ", sum(is.na(df2$steps)), sep="")

df2.hist <- data.frame(tapply(df2$steps, df2$date, FUN=sum, na.rm=FALSE))
colnames(df2.hist) <- "steps"
hist(df2.hist$steps,
     main="Histogram of total number of steps taken each day", 
     xlab="Number of steps taken each day",
     col="green"
)

cat("Mean: ", mean(df2.hist$steps), "\n",
    "Median: ", median(df2.hist$steps), "\n", sep="")

df2.dayofweek <- data.frame(factor(weekdays(as.Date(df2$date))))
colnames(df2.dayofweek) <- "weekday"
df2.dayofweek2 <- data.frame(factor(weekdays(as.Date(df2$date)),
                                   levels=c("Sunday","Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday"),
                                   labels=c("Weekend","Weekday","Weekday","Weekday","Weekday","Weekday","Weekend")))
colnames(df2.dayofweek2) <- "weekday2"

df2 <- cbind(df2,df2.dayofweek, df2.dayofweek2)

df2.wday <- subset(df2, df2$weekday2=="Weekday")
df2.wend <- subset(df2, df2$weekday2=="Weekend")

df2.wday.pattern <- data.frame(tapply(df2.wday$steps, df2.wday$interval, FUN=sum, na.rm=FALSE))
colnames(df2.wday.pattern) <- "steps"
df2.wend.pattern <- data.frame(tapply(df2.wend$steps, df2.wend$interval, FUN=sum, na.rm=FALSE))
colnames(df2.wend.pattern) <- "steps"

par(mfrow=c(2,1),title(main="title"))

plot(row.names(df2.wend.pattern), df2.wend.pattern$steps, type="l", col="blue",
     main="Weekend",
     xlab="5-min interval",
     ylab="Average across weekends"
)
plot(row.names(df2.wday.pattern), df2.wday.pattern$steps, type="l", col="blue",
     main="Weekday",
     xlab="5-min interval",
     ylab="Average across weekdays"
)
title("Average number of steps taken at each 5-min interval", outer=TRUE)

