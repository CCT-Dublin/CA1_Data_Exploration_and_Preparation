# CA1 – Video Presentation Script

- **Student:** Caio Jacob
- **Student ID:** 2023168
- **Module:** Data Exploration and Preparation (CA1)
- **Target Duration:** ~3 minutes 35 seconds (strict maximum: 4 minutes 00 seconds)
- **Recording Note:** Record using your own natural voice (no synthetic/AI voice) and upload to Google Drive with access granted to Dr. Muhammad Iqbal.

---

## 0:00–0:35 | Introduction
*[Screen cue: Show full PowerPoint poster, zooming briefly on the title banner and Column 1.]*

Hello, my name is Caio Jacob, and this is my Data Exploration and Preparation project.

My project focuses on electricity consumption among large energy users in Ireland. I used the authentic MEC04 dataset published by the Central Statistics Office, which contains hourly metered electricity consumption in UTC from 2023 to 2025.

The main objective was to inspect and prepare the dataset, investigate temporal patterns and unusual observations through Exploratory Data Analysis, and apply categorical encoding and Principal Component Analysis to understand the main dimensions of variation.

---

## 0:35–1:20 | Code Overview & Data Preparation
*[Screen cue: Switch briefly to RStudio showing Sections 1–7 of `CA1_Data_Exploration_Preparation.R`, scrolling smoothly past Cleaning, Outliers and Data Transformation.]*

I carried out the entire analysis in R and RStudio using one structured and thoroughly commented script organised into eleven sections.

The raw dataset contained 26,352 observations and 12 columns. During initial inspection, I identified 48 missing consumption values on the 29th of February in 2023 and 2025. Because neither year is a leap year, I removed these invalid calendar rows rather than imputing non-existent dates, leaving 26,304 valid records.

In Section 7, I created temporal features including a locale-independent date, leap-year day of the year, day of the week, Irish meteorological season, and UTC time period—keeping the original and cleaned datasets unchanged while building dedicated tables for EDA, encoding and PCA.

---

## 1:20–2:30 | Exploratory Data Analysis (RQ1–RQ4)
*[Screen cue: Switch back to the poster, focusing on Columns 2 and 3 (Figures 1 to 5).]*

The exploratory analysis in Section 8 was organised around four research questions using `ggplot2`.

First, electricity consumption grew steadily across the three-year window: annual mean hourly demand increased by 19.9 per cent between 2023 and 2025, from 948.8 to 1,137.7 megawatt hours. Aligning December to the next meteorological season year confirmed consistent growth across every season.

Second, aggregated consumption showed a relatively stable 24-hour profile in UTC, with only a 2.2 per cent range from lowest to highest average hour and a peak at 14:00 UTC. Weekday demand was slightly higher than weekends by 1.08 per cent, though multi-year growth was far larger than intra-day variation.

Third, using the 1.5 IQR rule, no outliers appeared under global, monthly or hour-of-day grouping, but analysing each year separately flagged 59 hourly readings across 16 dates—mainly early-January lows and two high peaks on the 21st of July 2025. All 59 were retained as plausible demand readings rather than removed.

Fourth, excluding month number due to near-collinearity with day of the year, consumption correlated strongly with year at 0.90 and moderately with day of the year at 0.35—describing strong temporal association without implying causation.

---

## 2:30–3:25 | Encoding & Principal Component Analysis
*[Screen cue: Briefly show Sections 9–11 in RStudio (`model.matrix()`, `prcomp()`, CSV exports), then show Column 4 of the poster (Figures 6 and 7).]*

In Section 9, I applied reference-coded dummy encoding using base R `model.matrix()`, converting day type, season and time period into seven binary indicators using Weekday, Winter and Night as reference categories.

In Section 10, I separated feature selection—choosing nine non-redundant variables—from dimensionality reduction using standardised `prcomp()`.

Using the Kaiser criterion of eigenvalues above 1, the first five components explained 80.72 per cent of total variance, and including the sixth component—which captures the weekend dimension—raised this to 91.83 per cent. PC1 alone explained 21.71 per cent and was dominated by electricity consumption and year, while PC2 to PC5 captured seasonal and intra-day contrasts. Finally, Section 11 exports all processed datasets and summary tables automatically.

---

## 3:25–3:50 | Conclusion & Limitations
*[Screen cue: Zoom on Column 4 bottom (Conclusions, Limitations & Harvard References).]*

To conclude, multi-year demand growth was the dominant driver of variation among Ireland's large energy users, alongside much smaller daily and weekly cycles.

Key limitations include selection and aggregation bias—as MEC04 covers aggregated large energy users only—the three-year autocorrelated time window, UTC timestamps without daylight saving conversion, and the exploratory nature of PCA on reference-coded dummies.

Thank you for watching my presentation.
