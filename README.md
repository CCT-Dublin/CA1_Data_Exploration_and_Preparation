# CA1_Data_Exploration_and_Preparation

## Project Title

Exploring Large Energy Users' Electricity Consumption in Ireland

- **Student:** Caio Jacob (Student ID: 2023168)
- **Programme:** BSc (Hons) in Computing in IT (4th Year, Semester 7) — CCT College Dublin
- **Module:** Data Exploration & Preparation (CA1)
- **Domain:** Energy

## Dataset

- **Dataset:** MEC04 — *Large Energy Users Metered Electricity Consumption* (2023–2025)
- **Source:** Central Statistics Office (CSO), Ireland
- **Official Source Links:**
  - CSO PxStat Table: https://data.cso.ie/table/MEC04
  - Ireland Open Data Portal: https://data.gov.ie/dataset/mec04-large-energy-users-metered-electricity-consumption
- **Local Raw File:** `data/raw/MEC04.20261007190104.csv` (`26,352` raw rows × `12` columns; `26,304` valid hourly rows × `12` columns after removing 48 invalid non-leap-year placeholder rows for 29 February 2023 and 2025).

## Problem Statement

Electricity consumption by large energy users is an important part of Ireland's overall energy demand. Analysing how electricity consumption varies over time and across different consumption categories can help identify patterns, variations and unusual observations. This project explores and prepares the MEC04 dataset to investigate electricity consumption patterns among large energy users in Ireland.

## Aim

The aim of this project is to explore and prepare a real-world dataset on metered electricity consumption by large energy users in Ireland, identify meaningful patterns and relationships in the data, and apply Principal Component Analysis (PCA) to investigate the main dimensions of variation within the dataset.

## Research Questions

1. **RQ1:** How does electricity consumption among large energy users vary over time?
2. **RQ2:** How does electricity consumption vary across different days and hourly consumption periods?
3. **RQ3:** What unusual patterns or variations can be identified in electricity consumption among large energy users?
4. **RQ4:** Which features contribute most to the variation observed in the electricity consumption data?

## Project Objectives

- Inspect and prepare the selected real-world electricity consumption dataset.
- Identify and handle missing values, data-quality issues and potential outliers.
- Examine the structure, attributes and distributions of the dataset.
- Explore variations and relationships between relevant features.
- Identify unusual patterns using Exploratory Data Analysis (EDA).
- Apply an appropriate categorical encoding technique (*Reference-Coded Dummy Encoding*).
- Apply Principal Component Analysis (PCA) for dimensionality reduction.
- Interpret the PCA results and identify the features contributing most to the observed variation.
- Present the key findings through a PowerPoint poster and a short video presentation (maximum 4 minutes).

## Repository Structure

```text
CA1_Data_Exploration_and_Preparation/
├── code/
│   └── CA1_Data_Exploration_Preparation.R   # Complete 11-section R analysis pipeline
├── data/
│   ├── raw/
│   │   └── MEC04.20261007190104.csv         # Original CSO MEC04 dataset
│   └── processed/
│       ├── mec04_clean.csv                  # Cleaned dataset (26,304 x 12)
│       ├── mec04_eda.csv                    # Engineered temporal features (26,304 x 11)
│       ├── mec04_encoded.csv                # Reference-coded dummy dataset (26,304 x 11)
│       └── mec04_pca_input.csv              # Selected PCA input matrix (26,304 x 9)
├── output/
│   ├── figures/                             # 11 high-resolution (300 dpi) ggplot2 figures
│   └── results/                             # 13 exported analytical CSV summary tables
├── poster/
│   └── CA1_Poster_Caio_Jacob_2023168.pptx   # Final PowerPoint poster presentation
├── video/
│   └── video_presentation_script.md         # Timed video presentation script (<= 4 min)
├── .gitignore
└── README.md
```

## How to Run the Analysis

### Prerequisites
- **R** (tested on R 4.6.1) and **RStudio** (recommended).
- Required package: `tidyverse` (provides `dplyr`, `tidyr`, `ggplot2` and `tibble`; PCA and dummy encoding use base R `prcomp()` and `model.matrix()`).

### Step-by-Step Instructions
1. Clone this repository (or download the ZIP from GitHub) and open RStudio:
   ```bash
   git clone https://github.com/CCT-Dublin/CA1_Data_Exploration_and_Preparation.git
   ```
2. Set the R working directory to the repository root folder (`CA1_Data_Exploration_and_Preparation`):
   ```r
   setwd("path/to/CA1_Data_Exploration_and_Preparation")
   ```
3. Install `tidyverse` once (if not already installed):
   ```r
   install.packages("tidyverse")
   ```
4. Open and run `code/CA1_Data_Exploration_Preparation.R` (or execute `source("code/CA1_Data_Exploration_Preparation.R")`).
5. The script automatically generates and saves:
   - 4 prepared CSV datasets in `data/processed/`
   - 13 summary CSV tables in `output/results/`
   - 11 visualisations (`300 dpi` PNGs) in `output/figures/`

## Summary of Key Findings

- **Data Preparation & Missing Values:** Removed exactly **48** missing `VALUE` rows corresponding to non-existent leap-year dates (29 February 2023 and 29 February 2025), yielding **26,304** valid hourly observations across 2023 (`8,760`), leap year 2024 (`8,784`) and 2025 (`8,760`), with zero missing values across all prepared datasets.
- **Outlier Analysis (1.5 × IQR):** No outliers were detected using global, monthly or hour-of-day grouping; however, a separate within-year analysis flagged **59** hourly readings (`0.22%`: `23` in 2023, `3` in 2024, `33` in 2025) across **16** calendar dates (`57` low, `2` high on `21 July 2025`). All 59 observations were retained as physically plausible readings (`802`–`1,248` MWh).
- **RQ1 (Variation Over Time):** Mean hourly electricity consumption grew steadily across the three-year window: **948.83 MWh** (`2023`) → **1,034.78 MWh** (`2024`, `+9.06%`) → **1,137.66 MWh** (`2025`, `+9.94%`), an overall annual-mean increase of **+19.90%** from 2023 to 2025. Aligning December to the following meteorological `Season_Year` confirmed consistent seasonal growth with tight dispersion (`SD = 21.00–26.25` MWh in complete Winters 2024–2025).
- **RQ2 (Daily & Hourly Periods, UTC):** The aggregated electricity consumption exhibits a relatively stable 24-hour demand profile (`1,030.20` to `1,053.10` MWh; `~2.2%` intra-day swing), peaking at `14:00` UTC. An observed Weekday–Weekend difference of **+11.13 MWh** (`1,043.60` vs. `1,032.47` MWh; `+1.08%`) persists across all three years, and the period ranking (`Afternoon > Morning > Evening > Night`) is unchanged each year, though multi-year growth is far larger than intra-day variation.
- **RQ3 (Unusual Patterns):** The 59 within-year flagged hours cluster mainly in early January (`01–02 January` and early-year lows) plus isolated dips (e.g., `14 May 2024`) and two high hourly peaks on `21 July 2025` (`1,241` and `1,248` MWh). Daily mean demand peaked on `27 November 2025` (`1,210.29` MWh) without any flagged hourly outlier, illustrating why dates containing flagged hourly readings are distinguished from daily-mean outliers.
- **RQ4, Encoding & PCA:** After excluding `Month_Num` due to near-collinearity with `Day_of_Year` (`r = 0.9965`), Pearson correlations with `Consumption_MWh` were strongest for `Year` (`r = +0.900`) and `Day_of_Year` (`r = +0.348`), weak for `Is_Weekend` (`r = -0.059`) and near-zero linearly for `Hour` (`r = +0.015`, reflecting a non-linear daily curve). Using **Reference-Coded Dummy Encoding** (references: `Weekday`, `Winter`, `Night`) and standardised PCA on 9 non-redundant features (`26,304 × 9`):
  - **PC1 (`21.71%`, λ = `1.954`)** is dominated by `Consumption_MWh` (`+0.698`, `48.7%`) and `Year` (`+0.659`, `43.4%`), capturing the multi-year growth dimension.
  - **PC2–PC5 (`14.54%–14.84%` each)** separate orthogonal seasonal (`PC2`, `PC5`) and intra-day period (`PC3`, `PC4`) contrasts; together, the **5 components exceeding the Kaiser criterion (`λ > 1`)** explain **80.72%** of total variance.
  - Including borderline **PC6 (`11.11%`, rounded `λ = 1.000`, `Day_Type_Weekend = +0.998`)** brings cumulative variance explained to **91.83%**.

## Methodological Limitations and Potential Bias

- **Selection Bias:** MEC04 covers only Large Energy Users (`DG10` and `TCON` demand categories) and does not represent residential or total national electricity consumers in Ireland.
- **Aggregation Bias:** Metered consumption is aggregated across all large energy users; therefore, a stable aggregate 24-hour curve does not imply that every individual facility operates continuously at constant load.
- **Temporal Scope & Autocorrelation:** The dataset spans three calendar years (`2023–2025`) of sequential hourly observations, which are temporally autocorrelated rather than statistically independent; associations describe observed patterns and do not establish causality.
- **Boundary Seasons & Missing Dates:** Removing 48 non-leap-year 29 February placeholders preserves calendar integrity; Winter 2023 (missing Dec 2022) and Winter 2026 (missing Jan–Feb 2026) are incomplete meteorological winters and are excluded from complete-season comparisons.
- **UTC Time Zone:** All hourly timestamps are retained in UTC as published by the CSO (without Irish daylight saving time conversion) to preserve a uniform 24-hour daily structure.
- **Exploratory PCA on Mixed Data:** Reference-coded binary dummy variables depend on their reference categories (`Weekday`, `Winter`, `Night`); PCA results are therefore interpreted as exploratory dimensionality reduction rather than causal inference.

## References (Harvard Style)

- Central Statistics Office (CSO) (2026) *MEC04: Large Energy Users Metered Electricity Consumption*. Cork: Central Statistics Office Ireland. Available at: https://data.cso.ie/table/MEC04 (Accessed: 7 October 2026).
- Data.gov.ie (2026) *MEC04 – Large Energy Users Metered Electricity Consumption*. Government of Ireland Open Data Portal. Available at: https://data.gov.ie/dataset/mec04-large-energy-users-metered-electricity-consumption (Accessed: 21 November 2026).
- Jolliffe, I.T. and Cadima, J. (2016) 'Principal component analysis: a review and recent developments', *Philosophical Transactions of the Royal Society A: Mathematical, Physical and Engineering Sciences*, 374(2065), 20150202.
- Kaiser, H.F. (1960) 'The application of electronic computers to factor analysis', *Educational and Psychological Measurement*, 20(1), pp. 141–151.
- Met Éireann (2026) *Climate of Ireland – Seasons*. Dublin: The Irish Meteorological Service. Available at: https://www.met.ie/ (Accessed: 21 November 2026).
- R Core Team (2026) *R: A Language and Environment for Statistical Computing*. Vienna: R Foundation for Statistical Computing. Available at: https://www.R-project.org/ (Accessed: 21 November 2026).
- Tukey, J.W. (1977) *Exploratory Data Analysis*. Reading, MA: Addison-Wesley.
- Wickham, H. et al. (2019) 'Welcome to the tidyverse', *Journal of Open Source Software*, 4(43), p. 1686.
