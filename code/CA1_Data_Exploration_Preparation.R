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

# Load the tidyverse package.
# It provides functions for data manipulation and visualisation.
# Package installation instructions are documented in README.md.
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

# ------------------------------------------------------------
# 8.2 RQ2: How does electricity consumption vary across days
#          and hourly consumption periods?
# ------------------------------------------------------------

# Note on time zone:
# All hourly intervals and time-of-day periods are analysed in UTC,
# matching the original CSO MEC04 reporting convention.

# Calculate summary statistics by day of the week (Monday to Sunday).
dow_summary <- mec04_eda %>%
  group_by(Day_of_Week, Day_Type) %>%
  summarise(
    Observations = n(),
    Mean_MWh = round(mean(Consumption_MWh), 2),
    Median_MWh = median(Consumption_MWh),
    SD_MWh = round(sd(Consumption_MWh), 2),
    IQR_MWh = IQR(Consumption_MWh),
    .groups = "drop"
  )

# Display the day-of-week summary table.
dow_summary

# Compare overall consumption between Weekdays and Weekends.
daytype_summary <- mec04_eda %>%
  group_by(Day_Type) %>%
  summarise(
    Observations = n(),
    Mean_MWh = round(mean(Consumption_MWh), 2),
    Median_MWh = median(Consumption_MWh),
    SD_MWh = round(sd(Consumption_MWh), 2),
    IQR_MWh = IQR(Consumption_MWh),
    .groups = "drop"
  )

# Display the Weekday vs. Weekend summary table.
daytype_summary

# Check whether the Weekday vs. Weekend difference is consistent
# across each individual year (2023, 2024 and 2025).
daytype_year_summary <- mec04_eda %>%
  group_by(Year, Day_Type) %>%
  summarise(
    Mean_MWh = round(mean(Consumption_MWh), 2),
    Median_MWh = median(Consumption_MWh),
    SD_MWh = round(sd(Consumption_MWh), 2),
    .groups = "drop"
  )

# Display the yearly Weekday vs. Weekend summary.
daytype_year_summary

# Calculate mean hourly electricity consumption (0-23 UTC) split
# by Weekday and Weekend to construct the daily load profile.
hourly_daytype_summary <- mec04_eda %>%
  group_by(Day_Type, Hour) %>%
  summarise(
    Mean_MWh = round(mean(Consumption_MWh), 2),
    Median_MWh = median(Consumption_MWh),
    SD_MWh = round(sd(Consumption_MWh), 2),
    .groups = "drop"
  )

# Display the first twelve rows of the hourly profile summary.
head(hourly_daytype_summary, 12)

# Calculate overall hourly summary across all days to identify
# peak and minimum consumption hours (UTC).
hourly_overall_summary <- mec04_eda %>%
  group_by(Hour) %>%
  summarise(
    Mean_MWh = round(mean(Consumption_MWh), 2),
    Median_MWh = median(Consumption_MWh),
    SD_MWh = round(sd(Consumption_MWh), 2),
    .groups = "drop"
  )

# Display the overall 24-hour summary table.
hourly_overall_summary

# Calculate summary statistics by four 6-hour periods of the day (UTC).
period_summary <- mec04_eda %>%
  group_by(Time_Period) %>%
  summarise(
    Observations = n(),
    Mean_MWh = round(mean(Consumption_MWh), 2),
    Median_MWh = median(Consumption_MWh),
    SD_MWh = round(sd(Consumption_MWh), 2),
    IQR_MWh = IQR(Consumption_MWh),
    .groups = "drop"
  )

# Display the time-of-day period summary table.
period_summary

# Calculate mean consumption by Time_Period and Year to check whether
# intra-day differences remain stable as overall demand grows.
period_year_summary <- mec04_eda %>%
  group_by(Year, Time_Period) %>%
  summarise(
    Mean_MWh = round(mean(Consumption_MWh), 2),
    Median_MWh = median(Consumption_MWh),
    SD_MWh = round(sd(Consumption_MWh), 2),
    .groups = "drop"
  )

# Display the yearly time-period summary table.
period_year_summary

# Plot 1 (RQ2): Average hourly consumption profile (UTC) for Weekdays vs. Weekends.
plot_rq2_hourly <- ggplot(
  hourly_daytype_summary,
  aes(x = Hour, y = Mean_MWh, colour = Day_Type, group = Day_Type)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(
    title = "Average Hourly Electricity Consumption Profile: Weekday vs. Weekend",
    subtitle = "Mean hourly consumption across 2023-2025 (UTC timestamps, CSO MEC04)",
    x = "Hour of Day (UTC)",
    y = "Mean Hourly Consumption (MWh)",
    colour = "Day Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

# Display and save RQ2 Plot 1 to output/figures/.
plot_rq2_hourly
ggsave(
  filename = "output/figures/rq2_hourly_profile_weekday_weekend.png",
  plot = plot_rq2_hourly,
  width = 10,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# Plot 2 (RQ2): Mean electricity consumption by day of the week.
# A point plot is used instead of a truncated bar chart so that small
# day-to-day differences are shown clearly without exaggerating magnitude.
plot_rq2_dow <- ggplot(
  dow_summary,
  aes(x = Day_of_Week, y = Mean_MWh, colour = Day_Type)
) +
  geom_line(aes(group = 1), colour = "grey75", linewidth = 0.7) +
  geom_point(size = 4.2) +
  geom_text(
    aes(label = sprintf("%.1f", Mean_MWh)),
    vjust = -1.1,
    size = 3.6,
    fontface = "bold",
    show.legend = FALSE
  ) +
  coord_cartesian(ylim = c(1022, 1054)) +
  labs(
    title = "Mean Hourly Electricity Consumption by Day of the Week (2023-2025)",
    subtitle = "Observed mean consumption across 2023-2025 (UTC); point plot avoids truncated-bar distortion",
    x = "Day of the Week",
    y = "Mean Hourly Consumption (MWh)",
    colour = "Day Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

# Display and save RQ2 Plot 2 to output/figures/.
plot_rq2_dow
ggsave(
  filename = "output/figures/rq2_day_of_week_consumption.png",
  plot = plot_rq2_dow,
  width = 9.5,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# Plot 3 (RQ2): Mean electricity consumption by time-of-day period across years.
# Grouped points connected by year highlight both the consistent intra-day
# ordering and the much larger multi-year shift without truncated bars.
plot_rq2_period_year <- ggplot(
  period_year_summary,
  aes(x = Time_Period, y = Mean_MWh, colour = factor(Year), group = factor(Year))
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3.8) +
  geom_text(
    aes(label = sprintf("%.0f", Mean_MWh)),
    vjust = -1.1,
    size = 3.4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  coord_cartesian(ylim = c(925, 1170)) +
  labs(
    title = "Mean Electricity Consumption by Time-of-Day Period and Year (UTC)",
    subtitle = "Night (00-05), Morning (06-11), Afternoon (12-17), Evening (18-23); point plot avoids bar distortion",
    x = "Time-of-Day Period (UTC)",
    y = "Mean Hourly Consumption (MWh)",
    colour = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

# Display and save RQ2 Plot 3 to output/figures/.
plot_rq2_period_year
ggsave(
  filename = "output/figures/rq2_time_period_by_year.png",
  plot = plot_rq2_period_year,
  width = 10,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# 8.3 RQ3: What unusual patterns or variations can be
#          identified in electricity consumption?
# ------------------------------------------------------------

# Flag each hourly observation against its own year's 1.5 x IQR
# bounds (matching Section 6) in a derived data frame so mec04_eda
# remains unchanged.
mec04_rq3_hourly <- mec04_eda %>%
  group_by(Year) %>%
  mutate(
    Year_Q1 = quantile(Consumption_MWh, 0.25),
    Year_Q3 = quantile(Consumption_MWh, 0.75),
    Year_IQR = IQR(Consumption_MWh),
    Year_Lower = Year_Q1 - 1.5 * Year_IQR,
    Year_Upper = Year_Q3 + 1.5 * Year_IQR,
    Hourly_Flag = case_when(
      Consumption_MWh < Year_Lower ~ "Below Lower",
      Consumption_MWh > Year_Upper ~ "Above Upper",
      TRUE ~ "Normal"
    )
  ) %>%
  ungroup()

# Verify that the total number of flagged hourly observations
# matches Section 6 (57 Below Lower and 2 Above Upper = 59 total).
table(mec04_rq3_hourly$Year, mec04_rq3_hourly$Hourly_Flag)

# Aggregate hourly observations to the daily level (1,096 days) and
# identify calendar days that contain at least one within-year
# flagged hourly observation.
# Note: Highlighting a day indicates that one or more hourly readings
# on that date fell outside the annual 1.5 x IQR bounds; it does not
# classify the entire daily mean itself as a statistical outlier.
daily_summary <- mec04_rq3_hourly %>%
  group_by(Year, Date, Month, Day_Type) %>%
  summarise(
    Daily_Mean_MWh = round(mean(Consumption_MWh), 2),
    Daily_Min_MWh = min(Consumption_MWh),
    Daily_Max_MWh = max(Consumption_MWh),
    Daily_SD_MWh = round(sd(Consumption_MWh), 2),
    Daily_Range_MWh = max(Consumption_MWh) - min(Consumption_MWh),
    Low_Outlier_Hours = sum(Hourly_Flag == "Below Lower"),
    High_Outlier_Hours = sum(Hourly_Flag == "Above Upper"),
    Total_Outlier_Hours = Low_Outlier_Hours + High_Outlier_Hours,
    .groups = "drop"
  ) %>%
  mutate(
    Day_Flag = case_when(
      High_Outlier_Hours > 0 ~ "Day with High Hourly Outlier(s)",
      Low_Outlier_Hours > 0 ~ "Day with Low Hourly Outlier(s)",
      TRUE ~ "No Flagged Hourly Outliers"
    )
  )

# Display the table of calendar days containing flagged hourly outliers.
flagged_days_summary <- daily_summary %>%
  filter(Total_Outlier_Hours > 0) %>%
  select(
    Date, Year, Day_Type, Daily_Mean_MWh, Daily_Min_MWh,
    Daily_Max_MWh, Low_Outlier_Hours, High_Outlier_Hours
  )

flagged_days_summary

# Identify the ten days with the largest intra-day consumption range
# (Daily_Max_MWh - Daily_Min_MWh) to inspect unusual short-term swings.
top_volatile_days <- daily_summary %>%
  arrange(desc(Daily_Range_MWh)) %>%
  select(Date, Year, Day_Type, Daily_Mean_MWh, Daily_Min_MWh, Daily_Max_MWh, Daily_Range_MWh) %>%
  head(10)

top_volatile_days

# Extract only the flagged days to overlay them clearly on the
# daily mean consumption time series.
daily_flagged_points <- daily_summary %>%
  filter(Total_Outlier_Hours > 0)

# Plot 1 (RQ3): Daily mean electricity consumption (2023-2025) with
# days containing within-year flagged hourly outliers highlighted.
plot_rq3_daily_anomalies <- ggplot(
  daily_summary,
  aes(x = Date, y = Daily_Mean_MWh)
) +
  geom_line(colour = "steelblue4", linewidth = 0.55, alpha = 0.9) +
  geom_point(
    data = daily_flagged_points,
    aes(colour = Day_Flag, shape = Day_Flag),
    size = 3.0
  ) +
  scale_colour_manual(
    values = c(
      "Day with Low Hourly Outlier(s)" = "#D95F02",
      "Day with High Hourly Outlier(s)" = "#D7191C"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Day with Low Hourly Outlier(s)" = 16,
      "Day with High Hourly Outlier(s)" = 17
    )
  ) +
  scale_x_date(
    limits = range(daily_summary$Date),
    date_breaks = "3 months",
    date_labels = "%b %Y",
    expand = c(0, 0)
  ) +
  labs(
    title = "Daily Mean Electricity Consumption and Days with Flagged Hourly Outliers",
    subtitle = "Markers identify dates containing >=1 within-year hourly IQR outlier (59 hours across 16 dates; UTC)",
    x = "Date",
    y = "Daily Mean Consumption (MWh)",
    colour = "Flagged Date Category",
    shape = "Flagged Date Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

# Display and save RQ3 Plot 1 to output/figures/.
plot_rq3_daily_anomalies
ggsave(
  filename = "output/figures/rq3_daily_consumption_anomalies.png",
  plot = plot_rq3_daily_anomalies,
  width = 11,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# Calculate mean electricity consumption by Year, Month and Hour (UTC)
# to construct a faceted Hour x Month heatmap.
# Separating the heatmap into yearly facets prevents the 2023-2025
# growth trend from obscuring intra-year monthly and hourly patterns.
hour_month_year_summary <- mec04_eda %>%
  group_by(Year, Month_Num, Hour) %>%
  summarise(
    Mean_MWh = round(mean(Consumption_MWh), 2),
    .groups = "drop"
  ) %>%
  mutate(
    Month_Short = factor(month.abb[Month_Num], levels = month.abb, ordered = TRUE)
  )

# Plot 2 (RQ3): Faceted Hour (UTC) x Month heatmap of mean consumption by year.
plot_rq3_heatmap <- ggplot(
  hour_month_year_summary,
  aes(x = Month_Short, y = Hour, fill = Mean_MWh)
) +
  geom_tile() +
  facet_wrap(~ Year, ncol = 3) +
  scale_y_continuous(breaks = seq(0, 23, by = 3)) +
  scale_fill_viridis_c(option = "viridis", name = "Mean (MWh)") +
  labs(
    title = "Hourly Electricity Consumption Heatmap by Month and Year (2023-2025)",
    subtitle = "Mean metered consumption by Hour (UTC) and Month, faceted by Year to separate annual growth",
    x = "Month",
    y = "Hour of Day (UTC)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right"
  )

# Display and save RQ3 Plot 2 to output/figures/.
plot_rq3_heatmap
ggsave(
  filename = "output/figures/rq3_hour_month_heatmap_by_year.png",
  plot = plot_rq3_heatmap,
  width = 12,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# 8.4 RQ4 (EDA): Relationships Between Features and
#                Correlation Analysis
# ------------------------------------------------------------

# Before building the correlation matrix, check for redundant
# calendar variables. Month_Num and Day_of_Year capture nearly
# identical within-year calendar position.
round(cor(mec04_eda$Month_Num, mec04_eda$Day_of_Year), 4)

# Because Month_Num and Day_of_Year are almost perfectly collinear
# (r > 0.99), only Day_of_Year is retained in the correlation matrix
# alongside Year, Is_Weekend (binary indicator: 0 = Weekday,
# 1 = Weekend), Hour (UTC) and Consumption_MWh.
# This avoids inflating the matrix with duplicate calendar measures
# while keeping mec04_eda unchanged.
mec04_corr_data <- mec04_eda %>%
  transmute(
    Consumption_MWh = Consumption_MWh,
    Year = as.numeric(Year),
    Day_of_Year = as.numeric(Day_of_Year),
    Is_Weekend = ifelse(Day_Type == "Weekend", 1, 0),
    Hour = as.numeric(Hour)
  )

# Compute Pearson (linear) and Spearman (monotonic) correlation
# matrices across the five selected non-redundant variables.
# Methodological notes:
# 1. Correlation measures association, not causation.
# 2. Because hourly measurements are sequentially autocorrelated,
#    correlations are interpreted descriptively without assuming
#    independent observations.
# 3. Linear correlation with Hour is expected to be weak even though
#    RQ2 showed a curved intra-day profile peaking at 14:00 UTC.
corr_pearson <- round(cor(mec04_corr_data, method = "pearson"), 3)
corr_spearman <- round(cor(mec04_corr_data, method = "spearman"), 3)

# Display both correlation matrices.
corr_pearson
corr_spearman

# Build a focused summary table comparing each temporal feature's
# association with Consumption_MWh (Pearson r and Spearman rho).
consumption_correlations <- data.frame(
  Feature = c("Year", "Day_of_Year", "Is_Weekend", "Hour"),
  Pearson_r = unname(corr_pearson["Consumption_MWh", c("Year", "Day_of_Year", "Is_Weekend", "Hour")]),
  Spearman_rho = unname(corr_spearman["Consumption_MWh", c("Year", "Day_of_Year", "Is_Weekend", "Hour")])
) %>%
  arrange(desc(abs(Pearson_r)))

# Display the ranked feature correlations with electricity consumption.
consumption_correlations

# Prepare the Pearson correlation matrix in long format for ggplot2
# using clean display labels.
corr_labels <- c(
  Consumption_MWh = "Consumption (MWh)",
  Year = "Year",
  Day_of_Year = "Day of Year",
  Is_Weekend = "Is Weekend",
  Hour = "Hour (UTC)"
)

corr_long <- as.data.frame(corr_pearson) %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "Correlation"
  ) %>%
  mutate(
    Var1 = factor(corr_labels[Var1], levels = rev(unname(corr_labels))),
    Var2 = factor(corr_labels[Var2], levels = unname(corr_labels))
  )

# Plot 1 (RQ4): Pearson correlation matrix heatmap of non-redundant features.
plot_rq4_corr <- ggplot(
  corr_long,
  aes(x = Var2, y = Var1, fill = Correlation)
) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(
    aes(
      label = ifelse(abs(Correlation) < 0.005, "0.00", sprintf("%.2f", Correlation)),
      colour = abs(Correlation) > 0.55
    ),
    size = 4.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "white")) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Pearson r"
  ) +
  coord_fixed() +
  labs(
    title = "Pearson Correlation Matrix of Non-Redundant Features",
    subtitle = "Month_Num excluded due to near-collinearity with Day_of_Year (r = 0.997); descriptive only",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 30, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank()
  )

# Display and save RQ4 Plot 1 to output/figures/.
plot_rq4_corr
ggsave(
  filename = "output/figures/rq4_correlation_matrix.png",
  plot = plot_rq4_corr,
  width = 9.2,
  height = 6.5,
  dpi = 300,
  bg = "white"
)



# ============================================================
# 9. CATEGORICAL ENCODING
# ============================================================

# In this section, key categorical variables from mec04_eda are
# converted into numeric dummy variables using Reference-Coded Dummy
# Encoding via base R model.matrix() (k - 1 dummy columns per feature).
#
# Methodological justification:
# 1. Reference-coded dummy encoding avoids imposing arbitrary numeric
#    distances between categories (unlike simple label encoding).
# 2. Using k - 1 columns prevents perfect multicollinearity (the dummy
#    variable trap), where the sum of all k indicators equals 1.
# 3. Reference categories are explicitly fixed as:
#    - Day_Type    : Weekday  (1 dummy: Day_Type_Weekend)
#    - Season      : Winter   (3 dummies: Spring, Summer, Autumn)
#    - Time_Period : Night    (3 dummies: Morning, Afternoon, Evening)
# 4. Because Season and Time_Period were stored as ordered factors in
#    mec04_eda for plotting, they are converted to unordered factors in
#    a temporary table here so model.matrix() produces binary 0/1
#    indicators rather than orthogonal polynomial contrasts.
# 5. mec04_clean and mec04_eda remain completely unchanged.

# ------------------------------------------------------------
# 9.1 Prepare Unordered Factors and Generate Dummy Variables
# ------------------------------------------------------------

# Create a temporary data frame with unordered factors and explicit
# reference levels (first level = reference category).
encoding_factors <- data.frame(
  Day_Type = factor(
    as.character(mec04_eda$Day_Type),
    levels = c("Weekday", "Weekend")
  ),
  Season = factor(
    as.character(mec04_eda$Season),
    levels = c("Winter", "Spring", "Summer", "Autumn")
  ),
  Time_Period = factor(
    as.character(mec04_eda$Time_Period),
    levels = c("Night", "Morning", "Afternoon", "Evening")
  )
)

# Generate the k - 1 dummy variable matrix using base R model.matrix()
# and remove the intercept column (column 1).
dummy_matrix <- model.matrix(
  ~ Day_Type + Season + Time_Period,
  data = encoding_factors
)[, -1, drop = FALSE]

# Assign clean, descriptive column names to the dummy variables.
colnames(dummy_matrix) <- c(
  "Day_Type_Weekend",
  "Season_Spring",
  "Season_Summer",
  "Season_Autumn",
  "Time_Period_Morning",
  "Time_Period_Afternoon",
  "Time_Period_Evening"
)

# ------------------------------------------------------------
# 9.2 Assemble and Validate the Encoded Dataset (mec04_encoded)
# ------------------------------------------------------------

# Combine the numeric variables from mec04_eda with the 7 binary
# dummy indicators into a dedicated numeric dataset (mec04_encoded).
# Note: mec04_encoded retains both numeric temporal variables
# (Day_of_Year, Hour) and encoded categorical features (Season,
# Time_Period) for completeness. Section 10 will build a separate,
# non-redundant input matrix for PCA so overlapping representations
# are not entered simultaneously.
mec04_encoded <- cbind(
  data.frame(
    Year = as.numeric(mec04_eda$Year),
    Day_of_Year = as.numeric(mec04_eda$Day_of_Year),
    Hour = as.numeric(mec04_eda$Hour),
    Consumption_MWh = as.numeric(mec04_eda$Consumption_MWh)
  ),
  as.data.frame(dummy_matrix)
)

# Verify dimensions (expected: 26,304 rows x 11 numeric columns)
# and confirm that mec04_eda remains unchanged (26,304 x 11).
dim(mec04_encoded)
dim(mec04_eda)

# Confirm that all columns in mec04_encoded are numeric and contain no NAs.
str(mec04_encoded)
colSums(is.na(mec04_encoded))

# Verify that every dummy column strictly contains only binary 0/1 values.
all(apply(dummy_matrix, 2, function(x) all(x %in% c(0, 1))))

# Summarise the count and percentage of observations equal to 1 for
# each dummy variable to cross-check against Section 7 tables.
encoding_summary <- data.frame(
  Dummy_Variable = colnames(dummy_matrix),
  Reference_Category = c(
    "Weekday",
    "Winter", "Winter", "Winter",
    "Night", "Night", "Night"
  ),
  Count_1 = colSums(dummy_matrix),
  Proportion_Pct = round(colMeans(dummy_matrix) * 100, 2),
  row.names = NULL
)

# Display the encoding validation summary and first six encoded rows.
encoding_summary
head(mec04_encoded)



# ============================================================
# 10. PRINCIPAL COMPONENT ANALYSIS (PCA)
# ============================================================

# Methodological notes:
# 1. Feature selection vs. dimensionality reduction (LO2):
#    - Feature selection occurs in Step 10.1, where nine non-redundant
#      variables are selected from mec04_encoded (excluding Day_of_Year
#      and Hour to avoid duplicating Season and Time_Period, and avoiding
#      linear wrap-around distortion between 23:00 and 00:00 UTC).
#    - Dimensionality reduction occurs in Step 10.3, where PCA combines
#      the nine standardised variables into orthogonal principal
#      components that capture the main directions of dataset variance.
# 2. Mixed numeric and dummy variables:
#    Binary k - 1 dummy variables are not continuous variables and depend
#    on the chosen reference categories (Weekday, Winter, Night). While
#    methods such as MCA or FAMD exist for purely categorical or mixed
#    data, standardised PCA is applied here as an exploratory
#    dimensionality-reduction technique aligned with the project aim.
# 3. Standardisation:
#    center = TRUE and scale. = TRUE are used in prcomp() so that
#    Consumption_MWh, Year and binary 0/1 dummy indicators are evaluated
#    on a comparable unit-variance scale.

# ------------------------------------------------------------
# 10.1 Prepare Dedicated PCA Input Matrix (mec04_pca_input)
# ------------------------------------------------------------

# Select the nine agreed non-redundant analytical variables from
# mec04_encoded (leaving mec04_clean, mec04_eda and mec04_encoded unchanged).
pca_feature_names <- c(
  "Consumption_MWh",
  "Year",
  "Day_Type_Weekend",
  "Season_Spring",
  "Season_Summer",
  "Season_Autumn",
  "Time_Period_Morning",
  "Time_Period_Afternoon",
  "Time_Period_Evening"
)

mec04_pca_input <- mec04_encoded[, pca_feature_names]

# ------------------------------------------------------------
# 10.2 Validate the PCA Input Matrix
# ------------------------------------------------------------

# Confirm dimensions (expected: 26,304 rows x 9 columns).
dim(mec04_pca_input)

# Confirm that all selected variables are numeric with zero missing values.
all(sapply(mec04_pca_input, is.numeric))
colSums(is.na(mec04_pca_input))

# Verify that every variable has non-zero standard deviation before scaling.
round(apply(mec04_pca_input, 2, sd), 4)

# ------------------------------------------------------------
# 10.3 Execute Standardised PCA using Base R prcomp()
# ------------------------------------------------------------

# Run Principal Component Analysis with centering and unit-variance scaling.
pca_result <- prcomp(
  mec04_pca_input,
  center = TRUE,
  scale. = TRUE
)

# Display the standard base R PCA summary (standard deviations,
# proportion of variance and cumulative proportion).
summary(pca_result)

# ------------------------------------------------------------
# 10.4 Eigenvalues, Variance Explained and Scree Plot
# ------------------------------------------------------------

# Calculate eigenvalues (squared standard deviations), individual
# variance explained (%) and cumulative variance explained (%).
pca_eigenvalues <- pca_result$sdev^2
pca_var_pct <- (pca_eigenvalues / sum(pca_eigenvalues)) * 100
pca_cum_pct <- cumsum(pca_var_pct)

# Build a structured summary table including the Kaiser criterion
# (eigenvalue > 1).
pca_variance_table <- data.frame(
  Component = factor(
    paste0("PC", seq_along(pca_eigenvalues)),
    levels = paste0("PC", seq_along(pca_eigenvalues)),
    ordered = TRUE
  ),
  Eigenvalue = round(pca_eigenvalues, 3),
  Variance_Pct = round(pca_var_pct, 2),
  Cumulative_Variance_Pct = round(pca_cum_pct, 2),
  Kaiser_Above_1 = pca_eigenvalues > 1
)

# Display the eigenvalues and variance-explained table.
pca_variance_table

# Plot 1 (PCA): Scree plot of individual and cumulative variance explained.
# Bars are coloured by the Kaiser criterion (eigenvalue > 1) so both
# component selection rules (Kaiser and cumulative variance) are visible.
plot_pca_scree <- ggplot(pca_variance_table, aes(x = Component)) +
  geom_col(
    aes(y = Variance_Pct, fill = Kaiser_Above_1),
    width = 0.65,
    alpha = 0.85
  ) +
  geom_text(
    aes(
      y = Variance_Pct,
      label = sprintf("%.1f%%", Variance_Pct)
    ),
    vjust = -0.8,
    size = 3.5,
    fontface = "bold"
  ) +
  geom_line(
    aes(y = Cumulative_Variance_Pct, group = 1),
    colour = "#1B4F72",
    linewidth = 1
  ) +
  geom_point(
    aes(y = Cumulative_Variance_Pct),
    colour = "#1B4F72",
    size = 2.8
  ) +
  geom_text(
    aes(
      y = Cumulative_Variance_Pct,
      label = ifelse(
        Component == "PC1",
        "",
        sprintf("%.1f%%", Cumulative_Variance_Pct)
      )
    ),
    vjust = -1.0,
    size = 3.2,
    colour = "#1B4F72"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#2E86C1", "FALSE" = "#BDC3C7"),
    labels = c("TRUE" = "Eigenvalue > 1 (Kaiser)", "FALSE" = "Eigenvalue <= 1")
  ) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) +
  labs(
    title = "PCA Scree Plot: Individual and Cumulative Variance Explained",
    subtitle = "Standardised PCA on 9 features (PC1-PC5 meet Kaiser > 1: 80.7%; PC1-PC6 reach 91.8%)",
    x = "Principal Component",
    y = "Percentage of Variance Explained (%)",
    fill = "Selection Criterion"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

# Display and save PCA Plot 1 to output/figures/.
plot_pca_scree
ggsave(
  filename = "output/figures/pca_scree_variance.png",
  plot = plot_pca_scree,
  width = 10,
  height = 5.8,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# 10.5 Component Loadings, Variable Contributions and Interpretation
# ------------------------------------------------------------

# Extract component loadings (eigenvectors / rotation matrix) rounded
# to three decimal places for interpretation.
pca_loadings <- round(pca_result$rotation, 3)
pca_loadings

# Calculate percentage contribution of each variable to each component
# (squared loadings x 100, summing to 100% within every component).
pca_contributions_pct <- round((pca_result$rotation^2) * 100, 1)
pca_contributions_pct

# Prepare loadings for PC1 to PC6 (the five Kaiser > 1 components plus
# borderline PC6 representing Day_Type_Weekend, capturing 91.83% of
# total variance) in long format for visual inspection.
pca_feature_labels <- c(
  Consumption_MWh = "Consumption (MWh)",
  Year = "Year",
  Day_Type_Weekend = "Day Type: Weekend",
  Season_Spring = "Season: Spring",
  Season_Summer = "Season: Summer",
  Season_Autumn = "Season: Autumn",
  Time_Period_Morning = "Time Period: Morning",
  Time_Period_Afternoon = "Time Period: Afternoon",
  Time_Period_Evening = "Time Period: Evening"
)

loadings_long <- as.data.frame(pca_result$rotation[, 1:6]) %>%
  rownames_to_column(var = "Feature") %>%
  pivot_longer(
    cols = -Feature,
    names_to = "Component",
    values_to = "Loading"
  ) %>%
  mutate(
    Feature = factor(
      pca_feature_labels[Feature],
      levels = rev(unname(pca_feature_labels))
    ),
    Component = factor(Component, levels = paste0("PC", 1:6))
  )

# Plot 2 (PCA): Loadings heatmap for PC1 to PC6.
plot_pca_loadings <- ggplot(
  loadings_long,
  aes(x = Component, y = Feature, fill = Loading)
) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(
    aes(
      label = ifelse(abs(Loading) < 0.005, "0.00", sprintf("%.2f", Loading)),
      colour = abs(Loading) > 0.55
    ),
    size = 3.8,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "white")) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Loading"
  ) +
  labs(
    title = "PCA Variable Loadings Across Principal Components (PC1-PC6)",
    subtitle = "Reference categories: Weekday, Winter, Night; PC1-PC6 capture 91.83% of total variance",
    x = "Principal Component",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank()
  )

# Display and save PCA Plot 2 to output/figures/.
plot_pca_loadings
ggsave(
  filename = "output/figures/pca_loadings_heatmap.png",
  plot = plot_pca_loadings,
  width = 9.5,
  height = 6.0,
  dpi = 300,
  bg = "white"
)

# Summary of PCA interpretation (RQ4):
# - PC1 (21.71%, eigenvalue = 1.954): Dominated by Consumption_MWh
#   (+0.698; 48.7%) and Year (+0.659; 43.4%), capturing the multi-year
#   upward demand shift from 2023 to 2025 as the single largest shared
#   dimension of variation.
# - PC2 (14.84%, eigenvalue = 1.336) and PC5 (14.54%, eigenvalue = 1.309):
#   Capture orthogonal seasonal contrasts among the reference-coded Season
#   dummies (Summer vs. Spring/Autumn on PC2; Autumn vs. Spring on PC5).
# - PC3 (14.81%, eigenvalue = 1.333) and PC4 (14.81%, eigenvalue = 1.333):
#   Capture intra-day contrasts among the Time_Period dummies (Afternoon
#   vs. Evening on PC3; Afternoon vs. Morning on PC4).
# - PC6 (11.11%, eigenvalue = 1.000): Almost entirely driven by
#   Day_Type_Weekend (+0.998; 99.6%), reflecting the weekly cycle as an
#   independent dimension from annual, seasonal and intra-day patterns.
# - Five components strictly exceed the Kaiser threshold (unrounded
#   eigenvalue > 1) and explain 80.72% of variance; PC6 displays a
#   rounded eigenvalue of 1.000 (just below 1 unrounded), and retaining
#   six components includes the independent Weekend dimension,
#   explaining 91.83% of total variance.
# - PC9 (0.65%, eigenvalue = 0.059) represents a very small residual
#   contrast between Consumption_MWh (+0.695) and Year (-0.665). Its
#   contribution to total variance is minimal, so the main shared
#   variation between consumption and year is better represented by PC1.
# - Overall, this PCA is exploratory: the components summarise main
#   dimensions of variation in the dataset and do not establish causal
#   relationships.



# ============================================================
# 11. FINAL RESULTS
# ============================================================

# Ensure processed data and results output directories exist
# before exporting CSV files.
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/results", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 11.1 Export Prepared Datasets to data/processed/
# ------------------------------------------------------------

# Export all four prepared datasets created across the workflow
# so each stage of preparation is reproducible and auditable.
write.csv(
  mec04_clean,
  file = "data/processed/mec04_clean.csv",
  row.names = FALSE
)

write.csv(
  mec04_eda,
  file = "data/processed/mec04_eda.csv",
  row.names = FALSE
)

write.csv(
  mec04_encoded,
  file = "data/processed/mec04_encoded.csv",
  row.names = FALSE
)

write.csv(
  mec04_pca_input,
  file = "data/processed/mec04_pca_input.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 11.2 Export Key Analytical Summary Tables to output/results/
# ------------------------------------------------------------

# Prepare data frames that store row names as explicit columns
# before writing clean CSV result tables.
outliers_by_year_export <- data.frame(
  Year = rownames(outliers_by_year),
  outliers_by_year,
  row.names = NULL
)

pca_loadings_export <- data.frame(
  Feature = rownames(pca_loadings),
  pca_loadings,
  row.names = NULL
)

pca_contributions_export <- data.frame(
  Feature = rownames(pca_contributions_pct),
  pca_contributions_pct,
  row.names = NULL
)

# Export outlier analysis tables (Section 6).
write.csv(
  outlier_comparison,
  file = "output/results/outlier_comparison.csv",
  row.names = FALSE
)

write.csv(
  outliers_by_year_export,
  file = "output/results/outliers_by_year.csv",
  row.names = FALSE
)

# Export EDA summary tables (Section 8: RQ1-RQ4).
write.csv(
  yearly_summary,
  file = "output/results/yearly_summary.csv",
  row.names = FALSE
)

write.csv(
  seasonal_summary,
  file = "output/results/seasonal_summary.csv",
  row.names = FALSE
)

write.csv(
  dow_summary,
  file = "output/results/day_of_week_summary.csv",
  row.names = FALSE
)

write.csv(
  period_year_summary,
  file = "output/results/time_period_by_year_summary.csv",
  row.names = FALSE
)

write.csv(
  flagged_days_summary,
  file = "output/results/flagged_days_summary.csv",
  row.names = FALSE
)

write.csv(
  consumption_correlations,
  file = "output/results/correlation_with_consumption.csv",
  row.names = FALSE
)

# Export categorical encoding and PCA tables (Sections 9-10).
write.csv(
  encoding_summary,
  file = "output/results/encoding_summary.csv",
  row.names = FALSE
)

write.csv(
  pca_variance_table,
  file = "output/results/pca_variance_explained.csv",
  row.names = FALSE
)

write.csv(
  pca_loadings_export,
  file = "output/results/pca_loadings.csv",
  row.names = FALSE
)

write.csv(
  pca_contributions_export,
  file = "output/results/pca_contributions_pct.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 11.3 Final Pipeline Verification Summary
# ------------------------------------------------------------

# Build and display an overview table confirming dimensions and
# missing-value counts across all datasets in the analysis pipeline.
pipeline_summary <- data.frame(
  Dataset = c(
    "mec04 (Raw)",
    "mec04_clean (Cleaned)",
    "mec04_eda (EDA Features)",
    "mec04_encoded (Dummy Encoded)",
    "mec04_pca_input (PCA Matrix)"
  ),
  Rows = c(
    nrow(mec04),
    nrow(mec04_clean),
    nrow(mec04_eda),
    nrow(mec04_encoded),
    nrow(mec04_pca_input)
  ),
  Columns = c(
    ncol(mec04),
    ncol(mec04_clean),
    ncol(mec04_eda),
    ncol(mec04_encoded),
    ncol(mec04_pca_input)
  ),
  Missing_Values = c(
    sum(is.na(mec04)),
    sum(is.na(mec04_clean)),
    sum(is.na(mec04_eda)),
    sum(is.na(mec04_encoded)),
    sum(is.na(mec04_pca_input))
  )
)

# Display and export the pipeline verification summary.
pipeline_summary
write.csv(
  pipeline_summary,
  file = "output/results/pipeline_summary.csv",
  row.names = FALSE
)



# ============================================================
# END OF ANALYSIS
# ============================================================
