# Purpose: Simulate a reproducible sample of daily prices for four fictional
#          assets, used by the "Visualizing Financial Data" lesson
#          (Financial-Data-Visualization-Instructor.qmd) and its companion
#          Shiny app (app.R). Keeping the simulation in one script ensures
#          both consumers use exactly the same data.
#
# Output: Data/DataVizDemoData.rds (relative to the project root), containing
#         a list with two elements:
#           - asset_data:           long-format simulated daily prices,
#                                    returns, and indexed prices
#           - asset_specifications: per-asset simulation parameters
#
# Usage: source() this script (or run it directly) whenever the simulated
#        data needs to be generated or regenerated. The lesson document and
#        app.R both check for Data/DataVizDemoData.rds and source this
#        script automatically if it is missing.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(here)
})

set.seed(20260901)

trading_dates <- seq.Date(
  from = as.Date("2024-01-02"),
  to = as.Date("2025-12-31"),
  by = "day"
)

trading_dates <- trading_dates[
  !weekdays(trading_dates) %in% c("Saturday", "Sunday")
]

asset_specifications <- tribble(
  ~ticker,  ~asset_name,          ~start_price, ~annual_drift, ~annual_volatility, ~market_beta,
  "TECH",   "Technology Fund",             80,         0.16,               0.28,         1.20,
  "BANK",   "Banking Fund",                55,         0.09,               0.20,         0.95,
  "ENERGY", "Energy Fund",                 70,         0.07,               0.25,         0.80,
  "BOND",   "Bond Fund",                  100,         0.03,               0.07,        -0.10
)

market_factor <- rnorm(
  n = length(trading_dates),
  mean = 0.00015,
  sd = 0.006
)

asset_data <- crossing(
  asset_specifications,
  date = trading_dates
) %>%
  group_by(ticker) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    observation = row_number(),
    daily_drift = annual_drift / 252,
    daily_idiosyncratic_volatility =
      annual_volatility / sqrt(252) - abs(market_beta) * 0.002,
    common_component = market_beta * market_factor[match(date, trading_dates)],
    cyclical_component = case_when(
      ticker == "ENERGY" ~ 0.0015 * sin(2 * pi * observation / 125),
      TRUE ~ 0
    ),
    event_component = case_when(
      ticker == "BANK" &
        date >= as.Date("2025-02-03") &
        date <= as.Date("2025-02-14") ~ -0.012,
      ticker == "BANK" &
        date >= as.Date("2025-02-17") &
        date <= as.Date("2025-03-07") ~ 0.004,
      TRUE ~ 0
    ),
    idiosyncratic_component = rnorm(
      n(),
      mean = 0,
      sd = pmax(daily_idiosyncratic_volatility, 0.002)
    ),
    log_return =
      daily_drift +
      common_component +
      cyclical_component +
      event_component +
      idiosyncratic_component,
    daily_return = exp(log_return) - 1,
    price = start_price * exp(cumsum(log_return)),
    indexed_price = 100 * price / first(price)
  ) %>%
  ungroup() %>%
  select(
    date,
    ticker,
    asset_name,
    price,
    daily_return,
    indexed_price
  ) %>%
  arrange(ticker, date)

data_path <- here("Data", "DataVizDemoData.rds")
dir.create(dirname(data_path), showWarnings = FALSE, recursive = TRUE)

saveRDS(
  list(
    asset_data = asset_data,
    asset_specifications = asset_specifications
  ),
  data_path
)

message("Saved simulated data to: ", data_path)
