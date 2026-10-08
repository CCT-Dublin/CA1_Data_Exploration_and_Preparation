# ============================================================
# DATA EXPLORATION AND PREPARATION - CA1
# ============================================================
# Student: Caio Jacob
# Student ID: 2023168
# Dataset: MEC04 - Large Energy Users Metered Electricity Consumption
# Domain: Energy
# ============================================================

# ============================================================
# 1. LOAD REQUIRED LIBRARIES
# ============================================================

# Install and load the tidyverse package.
# It provides functions for data manipulation and visualisation.
install.packages("tidyverse")
library(tidyverse)

# ============================================================
# 2. LOAD DATASET
# ============================================================

# Import the original MEC04 dataset from the raw data folder.
# The dataset is stored in a data frame called mec04.
mec04 <- read.csv(
  
  # Specify the path to the original CSV file.
  "data/raw/MEC04.20261007190104.csv",
  
  # Keep text variables as character values during import.
  stringsAsFactors = FALSE
)

# ============================================================
# 3. INITIAL DATA INSPECTION
# ============================================================

# Display the first six rows of the dataset.
# This provides an initial view of the imported data.
head(mec04)

# Display the number of rows and columns in the dataset.
# The first value represents the number of rows (observations).
# The second value represents the number of columns (variables).
dim(mec04)

# Display the names of all variables in the dataset.
# This helps us identify the variables that will be used during the analysis.
names(mec04)

# Display the structure of the dataset.
# This shows the data type of each variable and gives
# an overview of how the dataset is organised.
str(mec04)

# Generate summary statistics for all variables in the dataset.
# This provides information such as minimum, maximum, median,
# mean and the distribution of the variables where applicable.
summary(mec04)

# Count the number of missing values in each variable.
# This allows us to identify which variables contain missing data.
colSums(is.na(mec04))

# Calculate the percentage of missing values in the VALUE variable.
# This shows how large the missing-value problem is compared with
# the total number of observations in the dataset.
mean(is.na(mec04$VALUE)) * 100

# Display the rows where the VALUE variable contains missing values.
# This allows us to examine the missing observations before deciding
# how they should be treated.
mec04[is.na(mec04$VALUE), ]

# Display the unique years where missing VALUE observations occur.
# This helps us identify which years contain the missing observations.
unique(mec04$Year[is.na(mec04$VALUE)])

# Display the unique days associated with missing VALUE observations.
# This helps us understand the date pattern of the missing values.
unique(mec04$Day[is.na(mec04$VALUE)])

# Display the VALUE observations for 29 February in 2024.
# This allows us to compare the leap-year date with the missing observations.
mec04[mec04$Year == 2024 & mec04$Day == "29 February", ]

# Check whether VALUE contains any negative values.
# This helps identify values that may be outside the expected range
# for electricity consumption.
min(mec04$VALUE, na.rm = TRUE)

# Check the maximum value of VALUE.
# This helps identify the highest electricity consumption value
# in the dataset.
max(mec04$VALUE, na.rm = TRUE)

# Calculate the quartiles of the VALUE variable.
# Quartiles help describe the distribution of the electricity consumption
# values and are used when identifying potential outliers.
quantile(mec04$VALUE, na.rm = TRUE)

# Create a boxplot for the VALUE variable.
# The boxplot helps identify the distribution of the data
# and possible outliers.
boxplot(mec04$VALUE,
        main = "Boxplot of Electricity Consumption",
        ylab = "Electricity Consumption (MWh)")

# Calculate the interquartile range (IQR) of VALUE.
# The IQR measures the range between the first and third quartiles.
IQR(mec04$VALUE, na.rm = TRUE)

# Calculate the lower limit for identifying potential outliers.
# Values below this limit may be considered potential outliers.
lower_limit <- quantile(mec04$VALUE, 0.25, na.rm = TRUE) - 
  1.5 * IQR(mec04$VALUE, na.rm = TRUE)

# Calculate the upper limit for identifying potential outliers.
# Values above this limit may be considered potential outliers.
upper_limit <- quantile(mec04$VALUE, 0.75, na.rm = TRUE) + 
  1.5 * IQR(mec04$VALUE, na.rm = TRUE)

# Display the lower and upper limits.
# These values define the range used to identify potential outliers.
lower_limit
upper_limit

# Count the number of observations for each year.
# This shows how the dataset is distributed across the available years.
table(mec04$Year)

# Check the number of unique values in selected variables.
# This helps identify variables that contain different categories
# and variables that contain only one repeated value.

# Count the unique values in the day code variable.
length(unique(mec04$C03635V04375))

# Count the unique values in the hourly code variable.
length(unique(mec04$C04349V05129))

# Count the unique values in the electricity user code variable.
length(unique(mec04$C04540V05324))

# Count the unique values in the Electricity.Users variable.
length(unique(mec04$Electricity.Users))

# Count the unique values in the UNIT variable.
length(unique(mec04$UNIT))

# ============================================================
# 4. DATA PREPARATION AND CLEANING
# ============================================================

# Create a copy of the original dataset for data preparation.
# The original mec04 dataset is kept unchanged so that the
# imported data can still be referenced if required.
mec04_clean <- mec04



# ============================================================
# 5. MISSING VALUES
# ============================================================

# Remove the rows where VALUE is missing.
# The missing values occur on 29 February in 2023 and 2025.
# These years are not leap years, so 29 February is not a valid date.
# The rows are therefore removed instead of estimating or imputing
# electricity consumption values that should not exist.
mec04_clean <- mec04_clean[!is.na(mec04_clean$VALUE), ]

# Check the number of missing values after cleaning.
# This confirms that the missing VALUE observations were removed.
colSums(is.na(mec04_clean))

# Check the new number of rows and columns.
# The dataset should now contain 48 fewer rows and still have 12 columns.
dim(mec04_clean)

# ============================================================
# 6. OUTLIER ANALYSIS
# ============================================================



# ============================================================
# 7. DATA TRANSFORMATION
# ============================================================



# ============================================================
# 8. EXPLORATORY DATA ANALYSIS (EDA)
# ============================================================



# ============================================================
# 9. CATEGORICAL ENCODING
# ============================================================



# ============================================================
# 10. PRINCIPAL COMPONENT ANALYSIS (PCA)
# ============================================================



# ============================================================
# 11. FINAL RESULTS
# ============================================================



# ============================================================
# END OF ANALYSIS
# ============================================================
