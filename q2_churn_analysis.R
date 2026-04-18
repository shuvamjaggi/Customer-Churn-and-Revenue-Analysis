# install the required packages
install.packages(dpylr)
install.packages(readr)
install.packages(ggplot2)
install.packages(forecast)

# load the required packages
library(dplyr)
library(readr)
library(ggplot2)
library(forecast)

#load the data
df <- read.csv("C:/Users/shuva/OneDrive/Desktop/Capstone Project/Data/anonymized_memberships.csv")

# view the head of data
head(df)

# view the column names
names(df)

# view the structure and summary of data
str(df)
summary(df)

# remove the unnecessary columns
df <- df %>% select(-Last.Renewal.Date, -Cancellation.Reason, -Cancellation.Note, -Termination.Date, -Conversion.Type, -Converted.to, -Hold.period)

# view df
View(df)

# change the column names to snake_case convention
colnames(df) <- c("name", "membership", "purchase_date", "start_date", "end_date", "auto_renew", "cancellation_date", "last_bill_amount", "status")

# check the number of duplicate rows
sum(duplicated(df))
View(df[duplicated(df), ])
# remove the duplicate rows
df <- unique(df)

# convert status column to factor
df$status <- as.factor(df$status)
summary(df$status)

# check for missing values
colSums(is.na(df))
colSums(df == "")
View(df[df$start_date == "", ])

# replace the missing values in start_date and cancellation_date columns
df <- df %>% mutate(start_date = ifelse(start_date == "", purchase_date, start_date))

View(df[df$cancellation_date == "", ])
nrow(df[df$status == "Expired" & df$cancellation_date == "", ])
View(df[df$status == "Expired" & df$cancellation_date != "", ])
nrow(df[df$status == "Active" & df$cancellation_date == "", ])
df <- df %>% mutate(cancellation_date = ifelse(status %in% c("Active", "Active/On Hold", "Not Activated"), NA, ifelse(status == "Expired", end_date, cancellation_date)))

# confirm the mising values are replaced 
colSums(df == "" & !is.na(df))

# convert the required columns to date format 
df$purchase_date <- as.Date(df$purchase_date, format = "%d-%b-%y")
df$start_date <- as.Date(df$start_date, format = "%d-%b-%y")
df$end_date <- as.Date(df$end_date, format = "%d-%b-%y")
df$cancellation_date <- as.Date(df$cancellation_date, format = "%d-%b-%y")

# plot bar graph of status
ggplot(df, aes(x = status)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Status", x = "Status", y = "Count") +
  scale_y_continuous(breaks = seq(0, max(table(df$status)), by = 100)) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, color = "black") +
  theme_minimal()

# define a function to calculate the number of total students during the specified year
calculate_total_students <- function(year) {
  year_start <- as.Date(paste0(year, "-01-01"))
  year_end <- as.Date(paste0(year, "-12-31"))
  
  #  new dataframe containing new students in the given year
  new_students <- df %>% filter(start_date >= year_start & start_date <= year_end)
  
  # new dataframe containing continuing students in the given year
  continuing_students <- df %>% 
    filter(start_date < year_start & 
             (is.na(cancellation_date) | cancellation_date > year_start))
  
  # calculate total students in the given year
  total_students <- nrow(new_students) + nrow(continuing_students)
  
  return(data.frame(Year = year, Total_Students = total_students))
}

# define function to calculate the churned students
calculate_churned_students <- function(year) {
  year_start <- as.Date(paste0(year, "-01-01"))
  year_end <- as.Date(paste0(year, "-12-31"))
  
  # filter churn students in new dataframe
  churn_students <- df %>% 
    filter(cancellation_date >= year_start & 
             cancellation_date <= year_end)
  
  return(data.frame(Year = year, No_of_Churned_Students = nrow(churn_students)))
}

# select the required years
years <- 2016:2024

# create total students table
total_students_table <- do.call(rbind, lapply(years, calculate_total_students))
# print the table 
print(total_students_table)

# create churned students table
churned_students_table <- do.call(rbind, lapply(years, calculate_churned_students))
# print the table
print(churned_students_table)

# merge the required tables
combined_table <- merge(total_students_table, churned_students_table, by = "Year")
View(combined_table)

# plot line graph for total students and churned students
ggplot() +
  geom_line(data = combined_table, aes(x = Year, y = Total_Students, color = "Total Students"), linewidth = 1) + 
  geom_point(data = combined_table, aes(x = Year, y = Total_Students, color = "Total Students"), size = 2) +
  geom_line(data = combined_table, aes(x = Year, y = No_of_Churned_Students, color = "Churned Students"), linewidth = 1) + 
  geom_point(data = combined_table, aes(x = Year, y = No_of_Churned_Students, color = "Churned Students"), size = 2) + 
  labs(title = "Total vs Churned Students Over the Years", x = "Year", y = "Number of Students") +
  scale_x_continuous(breaks = seq(2016, 2024, by = 1)) +
  scale_y_continuous(breaks = seq(0, 700, by = 100)) +
  scale_color_manual(values = c("Total Students" = "blue", "Churned Students" = "red")) +
  theme_minimal()

# select the required years
years <- 2017:2023

# create total students table
total_students_table <- do.call(rbind, lapply(years, calculate_total_students))
print(total_students_table)

# create churned students table
churned_students_table <- do.call(rbind, lapply(years, calculate_churned_students))
print(churned_students_table)

# define function to calculate churn rate
calculate_churn <- function(year) {
  churn <- churned_students_table$No_of_Churned_Students[churned_students_table$Year == year] / total_students_table$Total_Students[total_students_table$Year == year]
  
  return(data.frame(Year = year, Churn_Rate = churn))
}

# create churn rate table  
churn_table <- do.call(rbind, lapply(years, calculate_churn))
# print table 
print(churn_table)

# plot line graph for churn rate across the given years
ggplot(churn_table, aes(x = Year, y = Churn_Rate)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  scale_x_continuous(breaks = churn_table$Year) +  
  labs(title = "Yearly Churn Rate (2017-2023)", x = "Year", y = "Churn Rate") +
  theme_minimal()

# convert churn table to time-series 
churn_ts <- ts(churn_table$Churn_Rate, start = 2017, frequency = 1)

# ARIMA Model
fit_arima <- auto.arima(churn_ts)
forecast_arima <- forecast(fit_arima, h = 3)

# store forecast values from ARIMA model in a dataframe
arima_forecast_values <- data.frame(Year = 2024:2026, Churn_Rate_ARIMA = forecast_arima$mean)

#ETS Model
fit_ets <- ets(churn_ts)
forecast_ets <- forecast(fit_ets, h = 3)

# store forecast values from ETS model in a dataframe
ets_forecast_values <- data.frame(Year = 2024:2026, Churn_Rate_ETS = forecast_ets$mean)

# merge both models' forecast value in a single dataframe
combined_forecast_values <- merge(arima_forecast_values, ets_forecast_values, by = "Year")

# plot actual vs ARIMA predicted values
autoplot(forecast_arima) +
  autolayer(fitted(fit_arima), series = "Fitted ARIMA") +
  geom_point(data = churn_table, aes(x = Year, y = Churn_Rate), color = "red", size = 2) +
  geom_point(data = combined_forecast_values, aes(x = Year, y = Churn_Rate_ARIMA), color = "red", size = 2) +
  geom_text(data = churn_table, aes(x = Year, y = Churn_Rate, label = round(Churn_Rate, 3)), vjust = 0, hjust = -0.5, color = "black", size = 3) +
  geom_text(data = combined_forecast_values, aes(x = Year, y = Churn_Rate_ARIMA, label = round(Churn_Rate_ARIMA, 3)), vjust = -1, hjust = 0, color = "black", size = 3) +
  scale_x_continuous(breaks = seq(2017, 2026, by = 1)) +
  labs(title = "(ARIMA) Forecasted and Actual Churn Rates (2017-2026)", x = "Year", y = "Churn Rate") +
  theme_minimal()

# plot actual vs ETS predicted values
autoplot(forecast_ets) +
  autolayer(fitted(fit_ets), series = "Fitted ETS") +
  geom_point(data = churn_table, aes(x = Year, y = Churn_Rate), color = "red", size = 2) +
  geom_point(data = combined_forecast_values, aes(x = Year, y = Churn_Rate_ETS), color = "red", size = 2) +
  geom_text(data = churn_table, aes(x = Year, y = Churn_Rate, label = round(Churn_Rate, 3)), vjust = 0, hjust = -0.5, color = "black", size = 3) +
  geom_text(data = combined_forecast_values, aes(x = Year, y = Churn_Rate_ETS, label = round(Churn_Rate_ETS, 3)), vjust = -1, hjust = 0, color = "black", size = 3) +
  scale_x_continuous(breaks = seq(2017, 2026, by = 1)) +
  labs(title = "(ETS) Forecasted and Actual Churn Rates (2017-2026)", x = "Year", y = "Churn Rate") +
  theme_minimal()

# create a dataframe containing fitted values for ARIMA and ETS model
fitted_arima <- data.frame(Year = 2017:2023, Fitted_ARIMA = as.numeric(fitted(fit_arima)))
fitted_ets <- data.frame(Year = 2017:2023, Fitted_ETS = as.numeric(fitted(fit_ets)))
combined_fitted_values <- merge(fitted_arima, fitted_ets, by = "Year")
View(combined_fitted_values)

# create new column to capture membership length
df <- df %>% 
  mutate(membership_length = ifelse(is.na(cancellation_date), 
                                    as.numeric(difftime(end_date, start_date, units = "days")),
                                    as.numeric(difftime(cancellation_date, start_date, units = "days"))))

# view rows for which membership length is negative
View(df %>% filter(membership_length < 0))

# correction for negative membership length
df <- df %>% 
  mutate(membership_length = ifelse(membership_length < 0, 0, membership_length))

# view the summary 
summary(df$membership_length)

# plot boxplot for membership length
ggplot(df, aes(y = membership_length)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.color = "red", outlier.size = 2) +
  labs(title = "Box Plot of Membership Length", y = "Membership Length (Days)") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, max(df$membership_length, na.rm = TRUE), by = 50)) +
  annotate("text", x = 0, y = quantile(df$membership_length, 0.25, na.rm = TRUE), label = paste("Q1:", quantile(df$membership_length, 0.25, na.rm = TRUE)), vjust = 1.25) +  
  annotate("text", x = 0, y = median(df$membership_length, na.rm = TRUE), label = paste("Median:", median(df$membership_length, na.rm = TRUE)), vjust = -0.5) +  
  annotate("text", x = 0, y = quantile(df$membership_length, 0.75, na.rm = TRUE), label = paste("Q3:", quantile(df$membership_length, 0.75, na.rm = TRUE)), vjust = 1.5) 

# plot histogram for membership length
ggplot(df, aes(x = membership_length)) +
  geom_histogram(binwidth = 30, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Membership Length", x = "Membership length (Days)", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, max(df$membership_length, na.rm = TRUE), by = 30)) +
  scale_y_continuous(breaks = seq(0, 850, by = 50)) +
  theme_minimal()