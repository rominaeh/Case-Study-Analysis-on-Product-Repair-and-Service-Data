# Project: Case Study Analysis on Product Repair and Service Data

install.packages("utf8")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("arules")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(arules)
library(caret)

# 1. Load the CSV file into a DataFrame
samdata <- read.csv("Samdata.csv")

# 2. Initial Data Exploration
head(samdata)
names(samdata)
str(samdata)
summary(samdata)

# 3. Rename the column 'Repaire_Action_Desc' to 'Action'
samdata <- samdata %>% rename(Action = Repair_Action_Desc)

# 4. Check for missing values in the 'Product_Date' column
missing_product_date <- sum(is.na(samdata$Product_Date))
mean_missing_product_date <- mean(is.na(samdata$Product_Date))

# Display missing values and mean
cat("Number of missing values in 'Product_Date':", missing_product_date, "\n")
cat("Mean of missing values in 'Product_Date':", mean_missing_product_date, "\n")

# If more than 50% values are missing, drop the column
if (mean_missing_product_date > 0.5) {
  samdata <- samdata %>% select(-Product_Date)
}

# 5. Calculate average service speed in columns 'TAT01' and 'TAT02'
avg_TAT01 <- mean(samdata$TAT01, na.rm = TRUE)
avg_TAT02 <- mean(samdata$TAT02, na.rm = TRUE)

cat("Average TAT01:", avg_TAT01, "\n")
cat("Average TAT02:", avg_TAT02, "\n")

# 6. Display the number of duplicate entries (products accepted more than once)
duplicated_data <- samdata %>% 
  group_by(Serial_No) %>%
  filter(n() > 1) %>%
  summarise(duplicate_count = n())

duplicate_rate <- nrow(duplicated_data) / nrow(samdata)

cat("Number of duplicate products:", nrow(duplicated_data), "\n")
cat("Duplicate rate:", duplicate_rate, "\n")

# 7. Visualize the warranty status in the 'Cost_Type' column
install.packages("farver")
ggplot(samdata, aes(x = Cost_Type)) +
  geom_bar() +
  labs(title = "Warranty Status Distribution",
       x = "Cost Type",
       y = "Count")

# 8. Linear relationship analysis
# a. Between 'Total_Invoice_Amount' and other variables
correlation_matrix <- cor(samdata %>% select_if(is.numeric), use = "complete.obs")
print(correlation_matrix)

# b. Between 'Cost_Type' and 'Total_Invoice_Amount'
# Convert 'Cost_Type' to a factor and 'Total_Invoice_Amount' to numeric
samdata$Cost_Type <- as.factor(samdata$Cost_Type)
total_invoice_model <- lm(Total_Invoice_Amount ~ Cost_Type, data = samdata)
summary(total_invoice_model)
cat("R-squared value for the relationship:", summary(total_invoice_model)$r.squared, "\n")

# 9. Association Rule Mining
# Convert relevant columns to factors
samdata$Cost_Type <- as.factor(samdata$Cost_Type)
samdata$Product_Group <- as.factor(samdata$Product_Group)
samdata$Action <- as.factor(samdata$Action)
samdata$City <- as.factor(samdata$City)

# Prepare data for association rule mining
transaction_data <- as(samdata %>% select(Cost_Type, Product_Group, Action, City), "transactions")

# Perform association rule mining
rules <- apriori(transaction_data, parameter = list(supp = 0.01, conf = 0.8))

# Inspect the rules
inspect(rules)

# 10. Data Segmentation
# Apply K-means clustering for segmentation
set.seed(123)
samdata_clean <- samdata %>% select_if(is.numeric) %>% na.omit()
kmeans_model <- kmeans(samdata_clean, centers = 3)

# Add cluster results to the original dataset
samdata$Cluster <- as.factor(kmeans_model$cluster)

# Visualize clusters
ggplot(samdata, aes(x = Total_Invoice_Amount, y = TAT02, color = Cluster)) +
  geom_point() +
  labs(title = "Customer Segmentation based on Clustering",
       x = "Total Invoice Amount",
       y = "TAT02")
