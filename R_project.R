#getwd()
#setwd("/Users/badrinathsanagavaram/Desktop/R Project/")
#install.packages("tidyverse")
#install.packages("dplyr")
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
data = read_csv("yours_data1.csv")
#View(data)
summary(data)
# Grouping the data storing the data in another variable
product_profit <-data %>%
  group_by(`Item Type`) %>%
  summarise(total_profit = median(`Total Profit`))
  view(product_profit)
# Identifying products with negative profits (losses)
loss_products <- product_profit %>%
  filter(total_profit < 0)
  view(loss_products)
# Identifying products with low profits   
  # Calculate summary statistics for Total Profit
profit_summary <- summary(data$`Total Profit`)
profit_summary
# Calculate the overall median profit
overall_median <- median(data$`Total Profit`)
# setting the threshold as 25% below the overall median
threshold <- overall_median * 0.75
low_profit_products <- product_profit %>%
  filter(total_profit < threshold)
View(low_profit_products)

# Sorting product_profit in ascending order
product_profit_sorted <- product_profit %>%
  arrange(total_profit)
# Selecting the three items with the lowest profits
lowest_three <- product_profit_sorted %>%
  head(3)


ggplot(product_profit_sorted, aes(x = reorder(product_profit_sorted$`Item Type`, product_profit_sorted$total_profit), y = product_profit_sorted$total_profit)) +
  geom_bar(stat = "identity", fill = ifelse(product_profit_sorted$`Item Type` %in% lowest_three$`Item Type`, "lightpink", "skyblue")) +
  labs(title = "Median Profits by Item Type",
       x = "Item Type",
       y = "Median Profit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Converting 'Order date' to Date format
data$`Order Date` <- as.Date(data$`Order Date`, format = "%m/%d/%Y")

# Extracting Year and Month from 'Order date'
data$Year <- format(data$`Order Date`, "%Y")
data$Month <- format(data$`Order Date`, "%m")

# Grouping data by Year and Month, calculate median revenue for each month within each year
median_revenue <-data %>%
  group_by(`Item Type`,Year, Month) %>%
  summarise(Median_Revenue = median(`Total Revenue`))

# Viewing the resulting data
#View(median_revenue)
unique(median_revenue$`Item Type`)

item_profit <- data %>%
  group_by(`Item Type`) %>%
  summarise(Total_Profit = sum(`Total Profit`))

item_profit <- item_profit %>%
  arrange(desc(Total_Profit))

ggplot(item_profit, aes(x = "", y = Total_Profit, fill = `Item Type`)) +
  geom_bar(stat = "identity", width = 1, color = "grey") +
  coord_polar("y", start = 0) +  # Convert to a pie chart
  labs(title = "Total Profit by Item Type",
       fill = "Item Type") +
  theme_void() +
  scale_fill_brewer(palette = "Set3") 


## Creating a liner model for checking correlation for total profit and total revenue
correlation <- cor(data$`Total Profit`, data$`Total Revenue`)

print(correlation)

linear_model <- lm(`Total Revenue` ~ `Total Profit`, data =data)

ggplot(data, aes(x = data$`Total Profit`, y = data$`Total Revenue`)) +
  geom_point() +  # Plot points
  geom_abline(intercept = coef(linear_model)[1], slope = coef(linear_model)[2], color = "Red") +  
  labs(title = "Correlation between Total Profit and Total Revenue",
       x = "Total Profit",
       y = "Total Revenue")


# Loading required libraries
#install.packages("randomForest")
library(randomForest)

# Renameing columns and remove spaces or special characters
colnames(data) <- make.names(colnames(data))

# Ensuring correct column names and data access
selected_data <- data %>%
  select(Total.Revenue, Total.Cost, Units.Sold, Total.Profit)

# Fiting random forest model
random_forest_model <- randomForest(Total.Revenue ~ ., data = selected_data, importance = TRUE)

# Getting variable importance
var_imp <- importance(random_forest_model)
print(var_imp)  

# Ploting variable importance
varImpPlot(random_forest_model)

# Getting variable importance for Total Revenue
var_imp_revenue <- importance(random_forest_model)
print("Variable Importance for Total Revenue:")
print(var_imp_revenue)

# dataframe with %IncMSE values
var_imp_df <- data.frame(
  Variable = c("Unit.Price", "Units.Sold", "Order.Date", "Total.Revenue"),
  IncMSE = c(64.775508, 51.866241, 1.050369, 43.821804)
)

# Plotting bar graph
ggplot(var_imp_df, aes(x = Variable, y = IncMSE, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Variable Importance based on %IncMSE",
       x = "Variable",
       y = "%IncMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Checking for correlation between 'Units Sold' and 'Unit Price'
correlation <- cor(data$Units.Sold, data$Unit.Price)
print(paste("Correlation between Units Sold and Unit Price:", correlation))

# Performing regression analysis
regression_model <- lm(data$Units.Sold ~ data$Unit.Price , data = data)

# Summary of the regression model
summary(regression_model)

# Plotting regression line
ggplot(data, aes(x = data$Unit.Price, y = data$Units.Sold )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regression Analysis: Units Sold vs Unit Price",
       x = "Unit Price",
       y = "Units Sold")
