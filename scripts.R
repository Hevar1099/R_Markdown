df <- read.csv('activity.csv')
df$date <- as.Date(df$date)
str(df)
colSums(is.na(df))

step_sum <- tapply(df$steps, df$date, sum, na.rm = T)

sum_data <- as.data.frame(step_sum)
sum_data$dates <- rownames(sum_data)
head(sum_data)

colors <- colorRampPalette(c('red', 'blue'))
color <- colors(5)
hist(sum_data$step_sum,col = color, xlab = 'Sum of the steps', main = 'Total Steps per Day')
rug(sum_data$step_sum, lwd = 2, lty = 1, col = 'orange')

mean_value <- mean(sum_data$step_sum, na.rm = T)
median_value <- median(sum_data$step_sum, na.rm = T)
print(paste('The mean value of the total steps is :' ,round(mean_value, 2)))
print(paste('The median value of the total steps is :' ,median_value))


# second set of questions 
df
second_df <- df
interval_avg_steps <- aggregate(steps ~ interval, data = second_df, FUN = mean, na.rm = TRUE)
interval_avg_steps

plot(interval_avg_steps$interval, interval_avg_steps$steps,
        type = "l", 
        xlab = "5-Minute Interval",
        ylab = "Average Steps Across All Days",
        main = "Average Steps by 5-Minute Interval",
        col = "lightblue", lwd = 2)

print(paste('The maxium number of total steps is in interval:',
        interval_avg_steps[max(interval_avg_steps$steps),]$interval))

# Third Set of Questions 
df
table(rowSums(is.na(df))) # one means the row contains NA values
third_df <- df

mean_values <- aggregate(third_df$steps~third_df$interval, FUN = mean, na.rm = T)
colnames(mean_values) <- c('interval','means')

clean_data <- merge(third_df, mean_values, by.x= 'interval', by.y = 'interval')
clean_data$steps <- ifelse(is.na(clean_data$steps), 
        clean_data$means, clean_data$steps)
clean_data$means<- NULL
clean_data$steps <- round(clean_data$steps, 2)

total_steps_clean <- tapply(clean_data$steps, clean_data$date, sum)
total_steps_clean <- as.data.frame(total_steps_clean)

par(mfcol = c(1,2), mar = c(4,4,3,2))
hist(total_steps_clean$total_steps_clean, col = color, 
        xlab = 'total steps per day', main = 'Total steps taken each day 
        (clean data)')
hist(sum_data$step_sum,col = color, xlab = 'Sum of the steps', main = 'Total Steps per Day
        (Data With NA)')

clean_mean <- mean(total_steps_clean$total_steps_clean)
clean_median <- median(total_steps_clean$total_steps_clean)

print(paste('The mean value after cleaning the data is:' ,
        round(clean_mean,2), 'before it was:', round(mean_value,2)))
print(paste('The median value after cleaning the data is:' ,
        round(clean_median,2), 'before it was:',round(median_value,2)))

# Fourth Set Of Questions 
clean_data
clean_data$weekday <- wday(clean_data$date, label = T)

clean_data$weekday <- ifelse(clean_data$weekday == 'Sat' 
        | clean_data$weekday == 'Sun', 'weekends', 'weekdays')


clean_data$weekday <- as.factor(clean_data$weekday)
Average_steps <- aggregate(clean_data$steps~clean_data$interval+clean_data$weekday, FUN = mean)
colnames(Average_steps) <- c('interval', 'weekday', 'Average_steps')
par(mfrow = c(2,1), mar = c(4,4,4,4))
with(subset(Average_steps, weekday == 'weekdays'), plot( interval, Average_steps,
        col = 'lightblue', type = 'l',
        xlab = 'interval',
        ylab = 'Average Steps',
        main = 'Average Steps in the weekdays'))
with(subset(Average_steps, weekday == 'weekends'), plot( interval, Average_steps,
        col = 'lightblue', type = 'l',
        xlab = 'interval',
        ylab = 'Average Steps',
        main = 'Average Steps in the weekends '))







