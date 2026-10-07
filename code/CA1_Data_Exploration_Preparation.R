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
# ============================================================
# 4. DATA PREPARATION AND CLEANING
# ============================================================



# ============================================================
# 5. MISSING VALUES
# ============================================================



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
