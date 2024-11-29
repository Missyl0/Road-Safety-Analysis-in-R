#Create an intersection column combining StreetnameA and StreetnameB
data <- data %>%
  mutate(intersection = paste(AStreetName, BStreetName, sep = " & "))

#Create a Table to count accidents at intersections in desc order
intersection_accidents <- data %>%
  group_by(intersection) %>%  # Group by intersection
  summarise(accident_count = n()) %>%  # Count the number of accidents
  arrange(desc(accident_count))  # Sort in descending order

