## Data Cleaning and feature engineering
#getwd()
#setwd("/Users/badrinathsanagavaram/Desktop/R Project/")
#install.packages("tidyverse")
#install.packages("dplyr")
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
data = read_csv("50000 Sales Records.csv")
View(data)
head(data)
colnames(data)
# Checking for null values in each column
null_count <- colSums(is.na(data))
# columns with null values and their counts
print(null_count)
#boxplots for necessary columns
boxplot_total_revenue <- ggplot(data, aes(y = `Total Revenue`)) +
  geom_boxplot() +
  labs(title = "Boxplot of Total Revenue") +
  theme_minimal()

boxplot_units_sold <- ggplot(data, aes(y = `Units Sold`)) +
  geom_boxplot() +
  labs(title = "Boxplot of Units Sold") +
  theme_minimal()

boxplot_unit_price <- ggplot(data, aes(y = `Unit Price`)) +
  geom_boxplot() +
  labs(title = "Boxplot of Unit Price") +
  theme_minimal()

boxplot_total_profit <- ggplot(data, aes(y = `Total Profit`)) +
  geom_boxplot() +
  labs(title = "Boxplot of Total Profit") +
  theme_minimal()

# printing boxplots and bar charts
print(boxplot_total_revenue) ## has outliers
print(boxplot_units_sold)
print(boxplot_unit_price)
print(boxplot_total_profit) ## has outliers

# Subset the data for the specified columns
selected_columns <- c("Sales Channel","Region","Order Priority","Item Type")
selected_data <- data %>% select(all_of(selected_columns))

# Melting the data to long format for visualization
melted_data <- selected_data %>%
  tidyr::gather(key = "Variable", value = "Value")  # Reshape to long format

# Plotting bar charts
bar_chart_1 <- ggplot(melted_data, aes(x = as.factor(Value))) +
  geom_bar(fill = "skyblue", position = "dodge") +
  facet_wrap(~Variable, scales = "free_x") +
  labs(title = "Count of Categories in Selected Columns", x = "Categories", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Displaying bar chart
print(bar_chart_1)

## Droping Values
# Detect outliers in 'Total Profit' and 'Total Revenue' columns
outliers_profit <- boxplot.stats(data$`Total Profit`)$out
outliers_revenue <- boxplot.stats(data$`Total Revenue`)$out

# Filter out rows without outliers
cleaned_data <- data %>%
  filter(!(`Total Profit` %in% outliers_profit) & !(`Total Revenue` %in% outliers_revenue))

boxplot_profit_after <- ggplot(cleaned_data, aes(y = `Total Profit`)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Total Profit (After)", y = "Total Profit") +
  theme_minimal()

# Boxplot for 'Total Revenue' after removing outliers
boxplot_revenue_after <- ggplot(cleaned_data, aes(y = `Total Revenue`)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Total Revenue (After)", y = "Total Revenue") +
  theme_minimal()
boxplot_profit_before <- ggplot(data, aes(y = `Total Profit`)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Total Profit (Before)", y = "Total Profit") +
  theme_minimal()

# Boxplot for 'Total Revenue' before removing outliers
boxplot_revenue_before <- ggplot(data, aes(y = `Total Revenue`)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Total Revenue (Before)", y = "Total Revenue") +
  theme_minimal()

print(boxplot_profit_before)
print(boxplot_revenue_before)
print(boxplot_profit_after)
print(boxplot_revenue_after)

# Filtering rows where Order Priority does not contain 'C'
cleaned_data <- data %>% filter(!grepl("C", `Order Priority`))

# Save cleaned data to a new CSV file
write.csv(cleaned_data, file = "Cleaned_data.csv", row.names = FALSE)

# Calculating value counts for Order Priority
order_priority_counts <- table(cleaned_data$`Order Priority`)

# Creating a bar plot
barplot(order_priority_counts, 
        main = "Order Priority Counts",
        xlab = "Order Priority",
        ylab = "Count")

# Writing the cleaned data to a CSV file
write.csv(cleaned_data, file = "yours_data1.csv", row.names = FALSE)






