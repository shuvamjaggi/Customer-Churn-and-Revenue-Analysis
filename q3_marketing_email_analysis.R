# Install required packages 
install.packages("tidyverse")
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")

# Load the installed packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

# Import data set
emails.df <- read.csv("C:\\Users\\TANVI AJMERA\\OneDrive\\Documents\\clean_anonymized_emails.csv")
marketing_emails.df <- read.csv("C:\\Users\\TANVI AJMERA\\OneDrive\\Documents\\clean_anonymized_marketing.csv")

## Part A 

# Convert send_date to Date type if it's in character format
marketing_emails.df$send_date <- as.Date(marketing_emails.df$send_date, format = "%d-%b-%y")

# Add a new column for day of the week
marketing_emails.df$day_of_week <- weekdays(marketing_emails.df$send_date)

# Calculate open rate with check for division by zero
marketing_emails.df <- marketing_emails.df %>%
  mutate(open_rate = ifelse((total_recipients - failed) == 0, NA, (opened / (total_recipients - failed)) * 100))

# Calculate the average open rate by day
open_rate_by_day <- marketing_emails.df %>%
  group_by(day_of_week) %>%
  summarise(avg_open_rate = mean(open_rate, na.rm = TRUE))

# Manually specify the desired order of the days (starting from Sunday)
day_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# Convert day_of_week to a factor with the custom order
open_rate_by_day$day_of_week <- factor(open_rate_by_day$day_of_week, levels = day_order)

# Plot the bar chart with the custom order
ggplot(open_rate_by_day, aes(x = day_of_week, y = avg_open_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Open Rate by Day of the Week",
       x = "Day of the Week",
       y = "Average Open Rate (%)") +
  theme_minimal() +
  coord_cartesian(ylim = c(80,95))


## Part B

# Remove unwanted row 
emails.df <- emails.df[-1, ]

# Check for strings of the data set
str(emails.df)

# Convert string type of "creation_day" 
# From chr to date
emails.df$creation_day <- as.Date(emails.df$creation_day, format = "%d-%b-%y")

# Create new column for days 
emails.df$day <- weekdays(emails.df$creation_day)

# Check the updated data set
head(emails.df)

# Remove records with 'Failed' in status column
emails.df <- emails.df[emails.df$status != "Failed", ]

# Check the updated data set
head(emails.df, 20)

# Convert status column into binary 
emails.df <- emails.df %>%
  mutate(status = recode(status, 
                         Opened = 1, 
                         Unopened = 0))

# Count the number of emails sent to each customer
customer_email_count <- emails.df %>%
  group_by(customer_name) %>%
  summarise(total_emails = n())

# Merge this count back to the original data frame
emails.df <- emails.df %>%
  left_join(customer_email_count, by = "customer_name")

# Categorize customers into single or multiple email recipients
emails.df <- emails.df %>%
  mutate(email_type = ifelse(total_emails == 1, "Single Email", "Multiple Emails"))

# Calculate the open rate for each category
open_rate_by_email_type <- emails.df %>%
  group_by(email_type) %>%
  summarise(
    Total_Sent = n(),  # Total emails sent for each category
    Total_Opened = sum(status == "1"),  # Total opened emails for each category
    Open_Rate = (Total_Opened / Total_Sent) * 100  # Open rate for each category
  )

# Calculate percentage for each email type
open_rate_by_email_type <- open_rate_by_email_type %>%
  mutate(percentage = Open_Rate / sum(Open_Rate) * 100)

# Print the open rate comparison
print(open_rate_by_email_type)


ggplot(open_rate_by_email_type, aes(x = "", y = Open_Rate, fill = email_type)) +
  geom_bar(stat = "identity", width = 1) +   # Minimal bar chart
  coord_polar("y", start = 0) +              # Directly transform to pie chart
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "black") +  # Add percentage labels
  labs(title = "Open Rate: Single vs. Multiple Emails", x = NULL, y = NULL) +
  theme_void() +                             # Clean look by removing axes
  theme(legend.title = element_blank()) +    # Remove legend title
  scale_fill_manual(values = c("lightblue", "mediumblue")) # Selecting the color palette


