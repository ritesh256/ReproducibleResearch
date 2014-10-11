##
readData <- read.csv("./data/activity.csv")

str(readData)

subdata <- aggregate(readData$steps, by=list(date=readData$date), FUN=sum, na.rm=T)
names(subdata) <- c("date", "total")

ggplot(subdata, aes(date, total)) + geom_bar(stat = "identity", fill = "orange", color="black") + theme(axis.text.x = element_text(angle=90, size=6, color="blue"))
+ ylab("Total Steps")

mean(subdata$total)

median(subdata$total)

intData <- aggregate(readData$steps, by=list(readData$interval), FUN=mean, na.rm=T)

ggplot(intData, aes(interval, avg)) + geom_line(stat = "identity", fill = "orange", color="red") 
+ theme(axis.text.x = element_text(size=10, color="blue")) + ylab("Avg Steps") 
+ geom_vline(xintercept = intData$interval[intData$avg ==max(intData$avg)])

sum(is.na(readData$steps))

for (i in 1:nrow(moddata)) 
{
  if(is.na(moddata$steps[i])){
  moddata$steps[i] <- intData[intData$interval==moddata$interval[i],]$avg
}
}





moddata[weekdays(as.Date(moddata$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),]$day <- "weekday"
moddata[weekdays(as.Date(moddata$date)) %in% c("Saturday", "Sunday"),]$day <- "weekend"

avgData <- aggregate(moddata$steps, by=list(moddata$interval, moddata$day), FUN=mean, na.rm=T)
names(avgData) <- c("interval", "day", "avg")


ggplot(avgData, aes(interval, avg)) + geom_line(stat = "identity", fill = "orange", color="red") + theme(axis.text.x = element_text(size=10, color="blue")) + ylab("Avg Steps") + facet_grid(day~.)