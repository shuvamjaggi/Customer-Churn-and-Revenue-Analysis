# Install required packages
install.packages("dplyr")
install.packages("readr")
install.packages("tidyverse")

# Load required packages
library(dplyr)
library(readr)
library(tidyverse)

# Load the CSV file into a DataFrame 'df'
df <- read.csv("C:/Users/shuva/OneDrive/Desktop/Capstone Project/Data/anonymized_sales_amended.csv") 

# Display the first few rows of 'df' to preview the data
head(df)

# Show column names of 'df'
names(df)

# Show the structure of 'df' including data types of each column
str(df)

# Display summary statistics of 'df'
summary(df)

# Select specific columns and remove 'Location', 'Discount.Code', and 'Discount.Amount'
df <- df %>% select(-Location, -Discount.Code, -Discount.Amount)

# Open the 'df' data frame in a new view window
View(df)

# Rename columns according to snake_case convention
colnames(df) <- c("transaction_id", "name", "revenue_category", "qty", "item", "total_sales", "frequency")

# Check for duplicate rows in 'df' and return the count
sum(duplicated(df))

# Check for missing values in each column and return the column-wise sum
colSums(is.na(df))
colSums(df == "")

# Create a new DataFrame 'df_missing' for rows where 'revenue_category' is empty
df_missing <- df[df$revenue_category == "", ]
View(df_missing)

# Replace empty 'revenue_category' entries with "Uncategorized"
df[df == ""] <- "Uncategorized"

# Confirm the count of 'revenue_category' entries that are empty (should now be zero)
sum(df$revenue_category == "")

# Remove any parentheses from 'total_sales' values, replacing with a minus sign for negative values
df$total_sales <- gsub("\\(([^)]+)\\)", "-\\1", df$total_sales)
# Remove any dollar signs and commas from 'total_sales'
df$total_sales <- gsub("[$,]", "", df$total_sales)
# Convert 'total_sales' to a numeric data type
df$total_sales <- as.numeric(df$total_sales)
# Check the structure of 'total_sales' to confirm the data type conversion
str(df$total_sales)

# Display rows where 'qty' equals -1 for inspection
View(df[df$qty == -1, ])
# Display rows where 'name' is "Tucker Rogers" for inspection
View(df[df$name == "Tucker Rogers", ])

# Replace 'qty' values of -1 with 1
df <- df %>% mutate(qty = ifelse(qty == -1, 1, qty))

# View rows where 'qty' is 0 for inspection
View(df[df$qty == 0, ])

# Count rows where 'qty' is 0
nrow(df[df$qty == 0, ])

# Count rows where 'qty' is 0 and 'total_sales' is negative
nrow(df %>% filter(qty == 0 & total_sales < 0))

# Count rows where 'total_sales' is negative
nrow(df %>% filter(total_sales < 0))

# View rows where 'total_sales' is negative and 'qty' is not 0 for inspection
View(df %>% filter(total_sales < 0 & qty != 0))

# Create a new DataFrame 'df_assumed' with negative 'total_sales' converted to positive if 'qty' is not 0
# Assumption: The observations with 'qty' is 0 are refunds, the rest negative 'total_sales' values are due to pending payments
df_assumed <- df %>% 
  mutate(total_sales = ifelse(total_sales < 0 & qty != 0, 
                              abs(total_sales), 
                              total_sales))

# Count rows where 'total_sales' is still negative and 'qty' is not 0 in 'df_assumed' to confirm changes
nrow(df_assumed %>% filter(total_sales < 0 & qty != 0))

# View the updated 'df_assumed' DataFrame
View(df_assumed)

# View rows where 'revenue_category' has multiple category values 
View(df_assumed[df_assumed$revenue_category == "Music Together Mixed Age, Music Together Mixed Age", ])
View(df_assumed[df_assumed$revenue_category == "M30 - Music Lessons, M45 - Music Lessons", ])
View(df_assumed[df_assumed$revenue_category == "M45 - Music Lessons, M30 - Music Lessons", ])
View(df_assumed[df_assumed$revenue_category == "M45 - Music Lessons, M60 - Music Lessons", ])

# Modify 'revenue_category' by changing "Music Together Mixed Age, Music Together Mixed Age" category to "Music Together Mixed Age"
df_assumed <- df_assumed %>%
  mutate(revenue_category = ifelse(revenue_category == "Music Together Mixed Age, Music Together Mixed Age",
                                   "Music Together Mixed Age", revenue_category))

# Separate multiple revenue categories within each row into new rows
df_assumed <- df_assumed %>% 
  separate_rows(revenue_category, sep = ", ")

# Adjust 'qty' and 'total_sales' for rows with duplicated 'transaction_id' by splitting their values in half
df_assumed <- df_assumed %>% 
  mutate(qty = ifelse(duplicated(transaction_id) | duplicated(transaction_id, fromLast = TRUE), qty/2, qty), 
         total_sales = ifelse(duplicated(transaction_id) | duplicated(transaction_id, fromLast = TRUE), total_sales/2, total_sales))

# Convert 'revenue_category' to a factor variable for categorical analysis
df_assumed$revenue_category <- as.factor(df_assumed$revenue_category)

# Change the levels of 'revenue category' to shorter names for better graph readability 
levels(df_assumed$revenue_category) <- c("Group Classes", "M30", "M45", "M60", "In-School", "Mixed Age", "Off-Site", "SUMMER - M30", "SUMMER - M60", "Uncategorized")

# Display the levels of the 'revenue_category' factor variable
levels(df_assumed$revenue_category)

# Group 'df_assumed' by 'revenue_category' and calculate total quantity per category
df_qty_grouped <- df_assumed %>% group_by(revenue_category) %>% dplyr::summarize(total_qty = sum(qty))

# Group 'df_assumed' by 'revenue_category' and calculate total sales per category
df_sales_grouped <- df_assumed %>% group_by(revenue_category) %>% dplyr::summarize(total_sales = sum(total_sales))

# View the grouped quantity data
View(df_qty_grouped)

# View the grouped sales data
View(df_sales_grouped)

# Merge the grouped quantity and sales data into a single DataFrame 'df_grouped'
df_grouped <- merge(df_qty_grouped, df_sales_grouped)

# Create new column to capture avg. sale value per customer for each revenue category
df_grouped <- df_grouped %>% 
  mutate(avg_sales_per_membership = total_sales / total_qty)

# View the merged DataFrame with both quantity and sales data by 'revenue_category'
View(df_grouped)

ggplot(df_qty_grouped, aes(x = reorder(revenue_category, -total_qty), y = total_qty)) + 
  geom_col(fill = "blue", alpha = 0.5) + 
  labs(title = "Total Qty by Revenue Category", x = "Revenue Category", y = "Total Qty") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"))

ggplot(df_sales_grouped, aes(x = reorder(revenue_category, -total_sales), y = total_sales)) +
  geom_col(fill = "blue", alpha = 0.5) +
  labs(title = "Total Sales by Revenue Category", x = "Revenue Category", y = "Total Sales") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"))

ggplot(df_grouped, aes(x = reorder(revenue_category, -avg_sales_per_membership), y = avg_sales_per_membership)) +
  geom_col(fill = "blue", alpha = 0.5) +
  labs(title = "Avg. Sales per Membership by Revenue Category", x = "Revenue Category", y = "Avg. Sales per Membership") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"))
