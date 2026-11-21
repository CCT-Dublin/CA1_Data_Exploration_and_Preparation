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

# In this section, a dedicated analysis dataset (mec04_eda) is created
# from mec04_clean. The original dataset (mec04) and the cleaned
# dataset (mec04_clean, with 26,304 rows and 12 columns) remain
# unchanged so that all source metadata and codes are preserved.
#
# Time zone note: According to CSO documentation, the MEC04 hourly
# intervals are recorded in Coordinated Universal Time (UTC). The
# hourly timestamps are intentionally kept in UTC without converting
# to Irish local time (IST/BST) so that every day retains exactly
# 24 hourly observations without daylight saving time distortions.

# ------------------------------------------------------------
# 7.1 Construct Date and Calendar Variables
# ------------------------------------------------------------

# Extract the numeric day of the month (1 to 31) from the Day text
# variable (for example, "01 January" becomes 1).
day_num <- as.integer(sub(" .*", "", mec04_clean$Day))

# Extract the English month name from the Day text variable
# (for example, "01 January" becomes "January").
month_text <- sub("^[0-9]+ ", "", mec04_clean$Day)

# Convert the English month names to numeric values (1 to 12) using
# R's built-in month.name vector. This approach is independent of
# the operating system language or locale settings.
month_num <- match(month_text, month.name)

# Combine Year, month_num and day_num into a proper Date object
# formatted as YYYY-MM-DD.
date_full <- as.Date(sprintf("%d-%02d-%02d", mec04_clean$Year, month_num, day_num))

# Verify that no invalid or missing dates were produced during conversion.
sum(is.na(date_full))

# Create an ordered factor for calendar months from January to December.
month_factor <- factor(month_text, levels = month.name, ordered = TRUE)

# Calculate the sequential day of the year from the Date object.
# This automatically respects leap years: 1 to 365 in 2023 and 2025,
# and 1 to 366 in 2024 (where 29 February is day 60).
day_of_year <- as.integer(format(date_full, "%j"))

# ------------------------------------------------------------
# 7.2 Create Day-of-Week, Day-Type and Season Variables
# ------------------------------------------------------------

# Extract the numeric weekday from POSIXlt (0 = Sunday, 1 = Monday,
# ..., 6 = Saturday) and map it to explicit English weekday names.
# This guarantees consistent English labels regardless of system locale.
weekday_labels <- c(
  "Sunday", "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday"
)
day_of_week_text <- weekday_labels[as.POSIXlt(date_full)$wday + 1]

# Convert Day_of_Week into an ordered factor starting on Monday.
day_of_week <- factor(
  day_of_week_text,
  levels = c(
    "Monday", "Tuesday", "Wednesday", "Thursday",
    "Friday", "Saturday", "Sunday"
  ),
  ordered = TRUE
)

# Classify each observation as a Weekday (Monday to Friday) or
# Weekend (Saturday and Sunday).
day_type <- factor(
  ifelse(day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday"),
  levels = c("Weekday", "Weekend")
)

# Assign Irish meteorological seasons (Met Eireann convention):
# Winter = Dec, Jan, Feb | Spring = Mar, Apr, May |
# Summer = Jun, Jul, Aug | Autumn = Sep, Oct, Nov.
# Meteorological seasons align cleanly with full calendar months.
season_text <- ifelse(
  month_num %in% c(12, 1, 2), "Winter",
  ifelse(
    month_num %in% c(3, 4, 5), "Spring",
    ifelse(month_num %in% c(6, 7, 8), "Summer", "Autumn")
  )
)

# Convert Season into an ordered factor following the seasonal cycle.
season_factor <- factor(
  season_text,
  levels = c("Winter", "Spring", "Summer", "Autumn"),
  ordered = TRUE
)

# ------------------------------------------------------------
# 7.3 Extract Hour and Time-of-Day Period (UTC)
# ------------------------------------------------------------

# Extract the starting hour (0 to 23, UTC) from the Hourly.Consumption
# interval string (for example, "00:00 < 01:00" becomes 0 and
# "23:00 < 00:00" becomes 23).
hour_start <- as.integer(substr(mec04_clean$Hourly.Consumption, 1, 2))

# Group the 24 hourly intervals (UTC) into four equal 6-hour periods:
# Night     = 00:00 to 05:59 (hours 0-5)
# Morning   = 06:00 to 11:59 (hours 6-11)
# Afternoon = 12:00 to 17:59 (hours 12-17)
# Evening   = 18:00 to 23:59 (hours 18-23)
time_period_text <- ifelse(
  hour_start < 6, "Night",
  ifelse(
    hour_start < 12, "Morning",
    ifelse(hour_start < 18, "Afternoon", "Evening")
  )
)

# Convert Time_Period into an ordered factor following the daily cycle.
time_period <- factor(
  time_period_text,
  levels = c("Night", "Morning", "Afternoon", "Evening"),
  ordered = TRUE
)

# ------------------------------------------------------------
# 7.4 Assemble and Validate the mec04_eda Dataset
# ------------------------------------------------------------

# Build the dedicated EDA dataset with all engineered temporal
# variables and electricity consumption in Megawatt Hours (MWh).
mec04_eda <- data.frame(
  Year = mec04_clean$Year,
  Date = date_full,
  Month = month_factor,
  Month_Num = month_num,
  Day_of_Year = day_of_year,
  Day_of_Week = day_of_week,
  Day_Type = day_type,
  Season = season_factor,
  Hour = hour_start,
  Time_Period = time_period,
  Consumption_MWh = mec04_clean$VALUE
)

# Confirm that mec04_clean remains unchanged (26,304 rows x 12 columns)
# and inspect the dimensions of mec04_eda (26,304 rows x 11 columns).
dim(mec04_clean)
dim(mec04_eda)

# Verify that no missing values were introduced during data transformation.
colSums(is.na(mec04_eda))

# Inspect the structure, first six rows and summary statistics of mec04_eda.
str(mec04_eda)
head(mec04_eda)
summary(mec04_eda)

# Validate the date range (expected: 2023-01-01 to 2025-12-31).
range(mec04_eda$Date)

# Validate that Day_of_Year respects leap years
# (expected max: 365 for 2023, 366 for 2024, 365 for 2025).
tapply(mec04_eda$Day_of_Year, mec04_eda$Year, max)

# Validate the hourly range (expected: 0 to 23) and check balanced
# observation counts across Day_Type, Season and Time_Period.
range(mec04_eda$Hour)
table(mec04_eda$Day_Type)
table(mec04_eda$Season)
table(mec04_eda$Time_Period)
table(mec04_eda$Day_of_Week)



# ============================================================
# 8. EXPLORATORY DATA ANALYSIS (EDA)
# ============================================================

# Ensure the figures output directory exists before saving plots.
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 8.1 RQ1: How does electricity consumption vary over time?
# ------------------------------------------------------------

# Note on time-series structure:
# The 26,304 rows represent sequential hourly observations rather
# than independent random samples. Any upward pattern across
# 2023-2025 describes temporal change over the three-year window
# and does not establish a causal effect of calendar year alone.

# Calculate annual summary statistics and year-over-year percentage
# change in mean hourly electricity consumption (MWh).
yearly_summary <- mec04_eda %>%
  group_by(Year) %>%
  summarise(
    Observations = n(),
    Mean_MWh = round(mean(Consumption_MWh), 2),
    Median_MWh = median(Consumption_MWh),
    SD_MWh = round(sd(Consumption_MWh), 2),
    IQR_MWh = IQR(Consumption_MWh),
    Min_MWh = min(Consumption_MWh),
    Max_MWh = max(Consumption_MWh),
    Total_GWh = round(sum(Consumption_MWh) / 1000, 2),
    .groups = "drop"
  ) %>%
  mutate(
    YoY_Mean_Change_Pct = round((Mean_MWh / lag(Mean_MWh) - 1) * 100, 2)
  )

# Display the annual summary table.
yearly_summary

# Calculate monthly mean, median and standard deviation for each
# year-month combination from January 2023 to December 2025.
monthly_summary <- mec04_eda %>%
  group_by(Year, Month_Num, Month) %>%
  summarise(
    Mean_MWh = round(mean(Consumption_MWh), 2),
    Median_MWh = median(Consumption_MWh),
    SD_MWh = round(sd(Consumption_MWh), 2),
    .groups = "drop"
  ) %>%
  mutate(
    Year_Month = as.Date(sprintf("%d-%02d-01", Year, Month_Num))
  )

# Display the first twelve rows of the monthly summary table.
head(monthly_summary, 12)

# Assign a meteorological Season_Year in a derived table so that
# December is grouped with January and February of the following
# calendar year (e.g., Winter 2024 = Dec 2023 + Jan 2024 + Feb 2024).
# This keeps mec04_eda and mec04_clean unchanged while ensuring
# meteorological winter is analysed accurately across calendar years.
mec04_seasonal <- mec04_eda %>%
  mutate(
    Season_Year = ifelse(Month_Num == 12, Year + 1L, Year)
  )

# Calculate seasonal summary statistics grouped by Season_Year and
# Irish meteorological season, flagging complete 3-month seasons.
# Note: Winter 2023 (Jan-Feb 2023 only, missing Dec 2022) and
# Winter 2026 (Dec 2025 only, missing Jan-Feb 2026) are incomplete.
seasonal_summary <- mec04_seasonal %>%
  group_by(Season_Year, Season) %>%
  summarise(
    Months_Included = n_distinct(Month_Num),
    Complete_Season = (Months_Included == 3),
    Observations = n(),
    Mean_MWh = round(mean(Consumption_MWh), 2),
    Median_MWh = median(Consumption_MWh),
    SD_MWh = round(sd(Consumption_MWh), 2),
    IQR_MWh = IQR(Consumption_MWh),
    .groups = "drop"
  )

# Display the seasonal summary table (including completeness flag).
seasonal_summary

# Plot 1 (RQ1): Monthly average electricity consumption time series (2023-2025).
plot_rq1_monthly <- ggplot(
  monthly_summary,
  aes(x = Year_Month, y = Mean_MWh, colour = factor(Year), group = 1)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title = "Monthly Average Electricity Consumption of Large Energy Users (2023-2025)",
    subtitle = "Ireland CSO MEC04 metered hourly electricity data (UTC), aggregated to monthly means",
    x = "Month and Year",
    y = "Mean Hourly Consumption (MWh)",
    colour = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

# Display and save Plot 1 to output/figures/.
plot_rq1_monthly
ggsave(
  filename = "output/figures/rq1_monthly_trend_2023_2025.png",
  plot = plot_rq1_monthly,
  width = 10,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# Plot 2 (RQ1): Distribution of hourly electricity consumption by year.
# This boxplot illustrates both the annual level shift and the
# within-year potential outliers examined in Section 6.
plot_rq1_year_box <- ggplot(
  mec04_eda,
  aes(x = factor(Year), y = Consumption_MWh, fill = factor(Year))
) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.6) +
  labs(
    title = "Distribution of Hourly Electricity Consumption by Year (2023-2025)",
    subtitle = "Comparison of median, interquartile range and within-year extreme observations",
    x = "Year",
    y = "Hourly Electricity Consumption (MWh)",
    fill = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

# Display and save Plot 2 to output/figures/.
plot_rq1_year_box
ggsave(
  filename = "output/figures/rq1_consumption_by_year_boxplot.png",
  plot = plot_rq1_year_box,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# Plot 3 (RQ1): Seasonal electricity consumption across complete
# meteorological seasons (Season_Year 2023, 2024 and 2025).
# Incomplete boundary winters (Winter 2023 without Dec 2022 and
# Winter 2026 without Jan-Feb 2026) are excluded from the comparison
# to avoid distorting seasonal distributions.
mec04_seasonal_complete <- mec04_seasonal %>%
  filter(
    !(Season == "Winter" & Season_Year %in% c(2023L, 2026L))
  )

plot_rq1_season_box <- ggplot(
  mec04_seasonal_complete,
  aes(x = Season, y = Consumption_MWh, fill = factor(Season_Year))
) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.4, position = position_dodge(preserve = "single")) +
  labs(
    title = "Seasonal Electricity Consumption by Meteorological Season Year",
    subtitle = "Complete 3-month seasons only (Dec assigned to next Winter; incomplete Winter 2023 & 2026 excluded)",
    x = "Meteorological Season",
    y = "Hourly Electricity Consumption (MWh)",
    fill = "Season Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

# Display and save Plot 3 to output/figures/.
plot_rq1_season_box
ggsave(
  filename = "output/figures/rq1_seasonal_comparison_by_year.png",
  plot = plot_rq1_season_box,
  width = 10,
  height = 5.5,
  dpi = 300,
  bg = "white"
)



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
