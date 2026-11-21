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

# ------------------------------------------------------------
# 6.1 Global Outlier Check (1.5 x IQR Rule)
# ------------------------------------------------------------

# Calculate the first quartile (Q1), third quartile (Q3) and
# interquartile range (IQR) for VALUE in the cleaned dataset.
global_q1 <- quantile(mec04_clean$VALUE, 0.25)
global_q3 <- quantile(mec04_clean$VALUE, 0.75)
global_iqr <- IQR(mec04_clean$VALUE)

# Calculate the global lower and upper bounds for potential outliers
# using the standard 1.5 x IQR rule.
global_lower <- global_q1 - 1.5 * global_iqr
global_upper <- global_q3 + 1.5 * global_iqr

# Count the number of observations below the lower bound and above
# the upper bound across the entire cleaned dataset.
global_below <- sum(mec04_clean$VALUE < global_lower)
global_above <- sum(mec04_clean$VALUE > global_upper)
global_outliers_total <- global_below + global_above

# Display the global IQR limits and outlier counts.
# Across all three years combined, no global outliers are detected.
global_lower
global_upper
global_below
global_above
global_outliers_total

# ------------------------------------------------------------
# 6.2 Grouped Outlier Analysis (By Year, Month and Hour)
# ------------------------------------------------------------

# Define a helper function to compute summary statistics, IQR bounds
# and potential outlier counts for any numeric vector.
calculate_iqr_outliers <- function(x) {
  q1 <- unname(quantile(x, 0.25))
  q3 <- unname(quantile(x, 0.75))
  iqr_val <- IQR(x)
  lower <- q1 - 1.5 * iqr_val
  upper <- q3 + 1.5 * iqr_val
  below <- sum(x < lower)
  above <- sum(x > upper)

  c(
    N = length(x),
    Min = min(x),
    Q1 = q1,
    Median = median(x),
    Mean = round(mean(x), 2),
    Q3 = q3,
    Max = max(x),
    IQR = iqr_val,
    Lower_Limit = lower,
    Upper_Limit = upper,
    Below_Lower = below,
    Above_Upper = above,
    Total_Outliers = below + above
  )
}

# Apply the 1.5 x IQR rule separately to each year (2023, 2024, 2025).
# Separating by year accounts for changes in the overall consumption
# level across years.
outliers_by_year <- as.data.frame(
  do.call(
    rbind,
    lapply(split(mec04_clean$VALUE, mec04_clean$Year), calculate_iqr_outliers)
  )
)

# Display the yearly outlier summary table and the combined count.
outliers_by_year
sum(outliers_by_year$Total_Outliers)

# Calculate the percentage of observations flagged as potential
# within-year outliers across the cleaned dataset.
round((sum(outliers_by_year$Total_Outliers) / nrow(mec04_clean)) * 100, 2)

# Create a boxplot of electricity consumption grouped by year.
# This visualises both the upward shift in consumption from 2023 to 2025
# and the potential outliers identified within each individual year.
boxplot(
  VALUE ~ Year,
  data = mec04_clean,
  main = "Electricity Consumption by Year (Within-Year Outlier Check)",
  xlab = "Year",
  ylab = "Electricity Consumption (MWh)",
  col = c("lightblue", "lightgreen", "lightcoral")
)

# Extract month names temporarily from the Day variable (e.g., "01 January" -> "January")
# to check for potential outliers grouped by month across all years.
# Permanent date and month features will be created in Section 7.
month_temp <- sub("^[0-9]+ ", "", mec04_clean$Day)

# Apply the 1.5 x IQR rule separately to each calendar month.
outliers_by_month <- as.data.frame(
  do.call(
    rbind,
    lapply(split(mec04_clean$VALUE, month_temp), calculate_iqr_outliers)
  )
)

# Display the monthly outlier counts and total.
outliers_by_month[, c("N", "Lower_Limit", "Upper_Limit", "Below_Lower", "Above_Upper", "Total_Outliers")]
sum(outliers_by_month$Total_Outliers)

# Apply the 1.5 x IQR rule separately to each hourly consumption period.
outliers_by_hour <- as.data.frame(
  do.call(
    rbind,
    lapply(split(mec04_clean$VALUE, mec04_clean$Hourly.Consumption), calculate_iqr_outliers)
  )
)

# Display the hourly outlier counts and total.
outliers_by_hour[, c("N", "Lower_Limit", "Upper_Limit", "Below_Lower", "Above_Upper", "Total_Outliers")]
sum(outliers_by_hour$Total_Outliers)

# Build a summary comparison table showing how outlier detection
# depends on the grouping structure used during analysis.
outlier_comparison <- data.frame(
  Grouping = c("Full Dataset (Global)", "By Year", "By Month", "By Hour"),
  Below_Lower = c(
    global_below,
    sum(outliers_by_year$Below_Lower),
    sum(outliers_by_month$Below_Lower),
    sum(outliers_by_hour$Below_Lower)
  ),
  Above_Upper = c(
    global_above,
    sum(outliers_by_year$Above_Upper),
    sum(outliers_by_month$Above_Upper),
    sum(outliers_by_hour$Above_Upper)
  ),
  Total_Potential_Outliers = c(
    global_outliers_total,
    sum(outliers_by_year$Total_Outliers),
    sum(outliers_by_month$Total_Outliers),
    sum(outliers_by_hour$Total_Outliers)
  )
)

# Display the comparison table across all four grouping levels.
outlier_comparison

# ------------------------------------------------------------
# 6.3 Inspection of Within-Year Potential Outliers
# ------------------------------------------------------------

# Extract all observations flagged as potential outliers within
# their respective year so they can be examined before making a decision.
yearly_outliers_list <- lapply(sort(unique(mec04_clean$Year)), function(yr) {
  sub_df <- mec04_clean[mec04_clean$Year == yr, ]
  q1 <- unname(quantile(sub_df$VALUE, 0.25))
  q3 <- unname(quantile(sub_df$VALUE, 0.75))
  iqr_val <- IQR(sub_df$VALUE)
  lower <- q1 - 1.5 * iqr_val
  upper <- q3 + 1.5 * iqr_val

  flagged <- sub_df[sub_df$VALUE < lower | sub_df$VALUE > upper,
                    c("Year", "Day", "Hourly.Consumption", "VALUE")]

  if (nrow(flagged) > 0) {
    flagged$Lower_Limit <- lower
    flagged$Upper_Limit <- upper
    flagged$Outlier_Type <- ifelse(flagged$VALUE < lower, "Below Lower", "Above Upper")
  }
  flagged
})

# Combine the flagged observations from all three years into one data frame.
yearly_outliers <- do.call(rbind, yearly_outliers_list)

# Check the total number of flagged observations (expected: 59).
nrow(yearly_outliers)

# Display the count of flagged observations by year and outlier type.
table(yearly_outliers$Year, yearly_outliers$Outlier_Type)

# Display which dates contain the within-year potential outliers in each year.
# This helps check whether flagged values cluster around specific calendar dates
# (such as early January holiday periods or summer peaks).
table(yearly_outliers$Day, yearly_outliers$Year)

# Display the first ten flagged observations and the high outliers in 2025.
head(yearly_outliers, 10)
yearly_outliers[yearly_outliers$Outlier_Type == "Above Upper", ]

# ------------------------------------------------------------
# 6.4 Outlier Handling Decision
# ------------------------------------------------------------

# Decision: Retain all 59 within-year potential outliers in mec04_clean.
#
# Justification:
# 1. Global vs. within-group effect: No outliers appear globally, by month,
#    or by hour because the upward trend across 2023-2025 widens the combined
#    IQR (143 MWh) compared with individual years (51-59 MWh).
# 2. Domain validity: All 59 values fall within a realistic physical range
#    (802 to 1,248 MWh) with no evidence of negative values, zeros, or
#    metering errors.
# 3. Calendar context: Low within-year outliers occur mainly in early January
#    (e.g., 01-02 January) and early in each year when baseline consumption is
#    lower, while the two high outliers on 21 July 2025 reflect peak demand.
# 4. Preserving real demand variation: Removing valid observations would
#    artificially reduce genuine seasonal and demand-peak variability.

# Confirm that mec04_clean retains all 26,304 valid observations and 12 columns.
dim(mec04_clean)



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
