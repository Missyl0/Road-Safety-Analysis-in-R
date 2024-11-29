#Load data
data <- read.csv("/Users/TamaraNdi/Downloads/SanJoseCrashData2022.csv")

# Load necessary libraries
library(dplyr)
library(lubridate)

# Define the list of major holidays you provided
holidays <- c(
    "2022-01-01", "2023-01-01", "2024-01-01", "2024-01-02",
     "2022-02-14", "2023-02-14", "2024-02-14",
     "2022-02-01", "2023-01-22", "2024-02-10",
     "2022-03-17", "2023-03-17", "2024-03-17",
     "2022-03-31", "2023-03-31", "2024-03-31",
     "2022-04-17", "2023-04-09",
     "2022-05-05", "2023-05-05",
     "2022-05-30", "2023-05-29",
     "2022-06-19", "2023-06-19",
     "2022-06-19", "2023-06-18",
     "2022-07-04", "2023-07-04",
     "2022-09-05", "2023-09-04",
     "2022-10-31", "2023-10-31",
     "2022-11-11", "2023-11-11",
     "2022-11-24", "2023-11-23",
     "2022-12-24", "2022-12-25", "2023-12-24", "2023-12-25",
     "2022-12-31", "2023-12-31",
     "2022-10-24", "2023-11-12",
     "2022-12-18", "2023-12-07"
 )

# Convert holidays to Date format
  holidays <- as.Date(holidays)

# Convert CrashDate from character (MM/DD/YY) to Date format (YYYY-MM-DD)
  data <- data %>%
  mutate(CrashDate = as.Date(CrashDate, format = "%m/%d/%y"))  # Adjusted for MM/DD/YY format

# Add the HolidayFlag column to data
  data <- data %>%
  mutate(HolidayFlag = ifelse(CrashDate %in% holidays, TRUE, FALSE))

# Define the holiday names for the corresponding dates
  holiday_names <- c(
             "2022-01-01" = "New Year's Day", "2023-01-01" = "New Year's Day", "2024-01-01" = "New Year's Day", "2024-01-02" = "New Year's Day",
             "2022-02-14" = "Valentine's Day", "2023-02-14" = "Valentine's Day", "2024-02-14" = "Valentine's Day",
             "2022-02-01" = "Lunar New Year", "2023-01-22" = "Lunar New Year", "2024-02-10" = "Lunar New Year",
             "2022-03-17" = "St. Patrick's Day", "2023-03-17" = "St. Patrick's Day", "2024-03-17" = "St. Patrick's Day",
             "2022-03-31" = "Cesar Chavez Day", "2023-03-31" = "Cesar Chavez Day", "2024-03-31" = "Cesar Chavez Day",
             "2022-04-17" = "Easter Sunday", "2023-04-09" = "Easter Sunday",
             "2022-05-05" = "Cinco de Mayo", "2023-05-05" = "Cinco de Mayo",
             "2022-05-30" = "Memorial Day", "2023-05-29" = "Memorial Day",
             "2022-06-19" = "Juneteenth", "2023-06-19" = "Juneteenth",
             "2022-06-19" = "Father's Day", "2023-06-18" = "Father's Day",
             "2022-07-04" = "Independence Day", "2023-07-04" = "Independence Day",
             "2022-09-05" = "Labor Day", "2023-09-04" = "Labor Day",
             "2022-10-31" = "Halloween", "2023-10-31" = "Halloween",
             "2022-11-11" = "Veterans Day", "2023-11-11" = "Veterans Day",
             "2022-11-24" = "Thanksgiving Day", "2023-11-23" = "Thanksgiving Day",
             "2022-12-24" = "Christmas Eve", "2022-12-25" = "Christmas Day", "2023-12-24" = "Christmas Eve", "2023-12-25" = "Christmas Day",
             "2022-12-31" = "New Year's Eve", "2023-12-31" = "New Year's Eve",
             "2022-10-24" = "Diwali", "2023-11-12" = "Diwali",
             "2022-12-18" = "Hanukkah", "2023-12-07" = "Hanukkah"
           )

# Convert holiday names into a vector that matches the CrashDate
  data <- data %>%
  mutate(HolidayName = holiday_names[as.character(CrashDate)])

# Create a summary table with holiday names, crash dates, and accident counts
  holiday_summary <- data %>%
  group_by(HolidayName, CrashDate) %>%
  summarise(AccidentCount = n(), .groups = 'drop')  # Count accidents for each holiday and crash date

# Create a summary table with holiday names, crash dates, and accident counts
  holiday_summary <- data %>%
  filter(!is.na(HolidayName)) %>%  # Ensure only rows with holidays are considered
  group_by(HolidayName, CrashDate) %>%
  summarise(AccidentCount = n(), .groups = 'drop')  # Count accidents for each holiday and crash date

# Using table() to count accidents on holidays vs not on holidays
  accident_count_summary <- table(data$HolidayFlag)

# Print the result
print(accident_count_summary)

library(ggplot2)
# Convert the table to a data frame for ggplot
  accident_count_df <- as.data.frame(accident_count_summary)
  colnames(accident_count_df) <- c("HolidayFlag", "AccidentCount")

# Create the bar plot
  ggplot(accident_count_df, aes(x = HolidayFlag, y = AccidentCount, fill = HolidayFlag)) +
       geom_bar(stat = "identity", show.legend = FALSE) +
       labs(title = "Accidents on Holidays vs Non-Holidays",
                       x = "Holiday",
                       y = "Number of Accidents") +
       scale_x_discrete(labels = c("Non-Holiday", "Holiday")) +
       theme_minimal()



