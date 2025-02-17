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








