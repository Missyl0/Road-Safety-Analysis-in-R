# Create a new column for the day of the week
data$DayOfWeek <- weekdays(as.Date(data$CrashDate))

#Table counting accidents by day of week
DayOfWeek_AccidentCount <- data %>%
  group_by(DayOfWeek) %>%
  summarise(accident_count = n()) %>%
  arrange(desc(accident_count))

# Table counting accidents by hour of the day
accidents_by_hour <- data %>%
  group_by(CrashTime) %>%  # Replace with the column for hour of the day
  summarise(accident_count = n()) %>%
  arrange(desc(accident_count))

#Create line chart
ggplot(accidents_by_hour, aes(x = CrashTime, y = accident_count, group = 1)) +
  geom_line() +
  labs(title = "Accidents by Hour of Day", x = "Hour", y = "Count")

