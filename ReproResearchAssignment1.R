library(knitr)
library(ggplot2)
library(dplyr)

#1) First, we must load the data from the working directory. The "activity.csv" file must be in your working directory for the data to load properly.
data <- read.csv("./activity.csv", header = TRUE)
data$date <- as.Date(data$date)
summary(data)

#2) Now that the data is loaded, we can procede with our analysis. First, we will create a histogram of the number of steps taken each day by the subject. To do this, we can group the data by "date" and summarize by "steps." Next, we create a histogram using the grouped data.

#Group data
stepsByday <- data %>% filter(!is.na(steps)) %>% group_by(date) %>% summarize(steps = sum(steps)) %>% print
#Generate plot
ggplot(stepsByday, aes(x = steps)) + geom_histogram(fill = "navy blue", binwidth = 2000) + labs(title = "Histogram: Steps per day", x = "Steps per day", y = "Frequency")

#3) We can use the same grouped data to provide a mean and median number of steps taken each day.
stepsMean <- mean(stepsByday$x, na.rm=TRUE)
stepsMedian <- median(stepsByday$x, na.rm=TRUE)

#4) To see the average daily activity pattern for the subject, we can create a time series plot. Instead of grouping our data by "data" as we did earlier, we can group by "interval" and summarize by the mean number of "steps" for each interval.

#group and summarize data
interval <- data %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarize(steps = mean(steps)) %>% print
#generate plot
ggplot(interval, aes(x=interval, y=steps)) + geom_line(color = "navy blue")

#5) Next, we can also show the 5 minute interval that contains the most steps on average.

interval[which.max(interval$steps),]

#6) The data currently contains several missing values. In order to impute these missing values, we can calculate the mean steps by interval, and then place this value in each missing value.

allData <- data
nas <- is.na(allData$steps)
meanInterval <- tapply(allData$steps, allData$interval, mean, na.rm=TRUE, simplify=TRUE)
allData$steps[nas] <- meanInterval[as.character(allData$interval[nas])]
sum(is.na(data$steps))
sum(is.na(allData$steps))

#7) Next, we can incorporate the imputed values into a new histogram. To do this, we must use the new imputed data to group values by date and summarize the data by steps taken. After this, we can plot the grouped data.

#group data
allSteps <- allData %>% filter(!is.na(steps)) %>% group_by(date) %>% summarize(steps = sum(steps)) %>% print
#generate plot
ggplot(allSteps, aes(x = steps)) + geom_histogram(fill = "navy blue", binwidth = 2000) + labs(title = "Histogram: Steps per day with NA values", x = "Steps per day", y = "Frequency")

#The new Mean and Median of the data with imputed values:
meanAllSteps <- mean(allSteps$steps, na.rm = TRUE)
medianAllSteps <- median(allSteps$steps, na.rm = TRUE)

#8) Using this new imputed data, we can create a new factor, "weektype," that segments our data by values collected on weekdays and values collected on the weekend.
weekData <- mutate(allData, weektype = ifelse(weekdays(allData$date) == "Saturday" | weekdays(allData$date) == "Sunday", "weekend", "weekday"))
weekData$weektype <- as.factor(weekData$weektype)
#The new data and variable can be seen in the data header below.
head(weekData)

# Using this new variable, we can create a new time series chart that shows the difference between steps taken during the work week and during the weekend. 

#group data
allInterval <- weekData %>% group_by(interval, weektype) %>% summarise(steps = mean(steps))
#generage plot
graph <- ggplot(allInterval, aes(x=interval, y=steps, color = weektype)) + geom_line() + facet_wrap(~weektype, ncol = 1, nrow=2)
print(graph)


    

