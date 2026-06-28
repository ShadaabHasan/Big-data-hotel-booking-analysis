# Hotel Booking Cancellation Risk Analysis

Predicting hotel booking cancellations and quantifying the revenue at risk, using
classification modelling, threshold tuning, model interpretability, and time-series
forecasting in R.

## Overview

Booking cancellations are a major source of lost revenue and operational uncertainty
for hotels. This project builds a model to predict which bookings are likely to cancel,
explains *why* the model flags them, and translates those predictions into a concrete
revenue figure a revenue manager can act on. A separate time-series model forecasts
overall monthly booking demand.

## Dataset

Hotel Booking Demand dataset (Antonio, Almeida & Nunes, 2019): **119,390 bookings**
across two hotels (city and resort), spanning July 2015 to August 2017, with 8 market
segments.

The dataset is not included in this repository. Download `hotel_bookings.csv` and place
it in a `data/` folder at the project root before running. Source:
[Hotel Booking Demand (Kaggle)](https://www.kaggle.com/datasets/jessemostipak/hotel-booking-demand).

## Results

### Cancellation model

| Model | AUC | Accuracy |
|---|---|---|
| Random Forest | **0.85** | 79% |
| Logistic Regression (baseline) | 0.75 | 72% |

The Random Forest outperforms the logistic baseline by ~10 AUC points, justifying the
use of a non-linear model.

**Threshold tuning.** At the default 0.50 cutoff the model favoured precision over recall
(catching only 53% of cancellations) due to 63/37 class imbalance. Lowering the decision
threshold to 0.25 raised **recall from 53% to 74%**, prioritising the catching of at-risk
bookings. A sensible trade for a cancellation tool, where a missed cancellation typically
costs more than a false alarm. (AUC is unchanged by thresholding; only the decision cutoff
moves.)

### Key findings (model interpretability)

Partial dependence analysis was used to explain the model's behaviour:

- **Lead time** is the strongest signal: predicted cancellation risk climbs from ~10% for
  last-minute bookings to ~60% for bookings made 300+ days ahead, then plateaus.
- **Market segment** matters: **Group bookings** carry the highest learned risk (~58%),
  while **Direct** bookings are lowest (~20%).

### Business impact

Linking predictions to booking value (cleaned ADR × nights) on the held-out set, the model
flags bookings representing roughly **€3.7M of at-risk revenue** for proactive intervention,
capturing about **73% of revenue** that ultimately cancelled.

*This is an illustrative estimate of revenue exposure, not a guaranteed saving — it assumes
a cancelled booking loses its full value, and does not account for rebooking, deposits, or
resold rooms.*

### Demand forecast

An ARIMA model (`auto.arima`) forecasts monthly booking demand. Given the dataset's limited
span (~26 months, ~2 seasonal cycles), the forecast is presented with that constraint noted.

## Tech stack

R — `tidyverse`, `caret`, `randomForest`, `pROC`, `pdp` (interpretability), `forecast` (ARIMA).

## Repository structure

```
.
├── scripts/
│   ├── cancellation_model.R   # EDA, Random Forest, logistic baseline,
│   │                          # threshold tuning, partial dependence, revenue impact
│   └── booking_forecast.R     # ARIMA monthly demand forecast
├── data/                      # place hotel_bookings.csv here (not tracked)
├── Big-data-hotel-booking-analysis.Rproj
└── README.md
```

## Running the project

1. Open `Big-data-hotel-booking-analysis.Rproj` in RStudio (this sets the working directory).
2. Place `hotel_bookings.csv` in the `data/` folder.
3. Run the scripts:
   ```r
   source("scripts/cancellation_model.R")
   source("scripts/booking_forecast.R")
   ```
   Note: the partial dependence plots in `cancellation_model.R` take a minute or two to compute.

## Contact

**Shadaab Hasan**
[Email](mailto:shadaabhasan7@gmail.com) | [LinkedIn](https://www.linkedin.com/in/shadaab-hasan-4a9b92271/)
