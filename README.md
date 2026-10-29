# Assignment 6 – Technical Analysis using R, Visualization Phase

| | |
|---|---|
| **Name** | Anthony Dibor |
| **Course** | BDA400 – Data Science Tools and Techniques, CDI College |
| **Assignment** | Assignment 6 – Technical Analysis using R, Visualization Phase (15%) |
| **Repository** | https://github.com/Andib77/AnthonyDibor_BDA400_A06 |

## Project description

An interactive portfolio dashboard built with **R Shiny**. It downloads historical stock prices from Yahoo Finance, charts them as a candlestick, OHLC bar, line or area chart on a daily, weekly or monthly time frame, and overlays technical indicators (SMA, EMA, Bollinger Bands, RSI and MACD), each of which can be switched on and off. A Moving Average Crossover trading rule, with optional RSI and MACD confirmation filters and adjustable periods, generates Buy / Sell / Hold signals that are annotated directly on the price chart. The dashboard also shows a signal table, a simple back-test against buy-and-hold, and a portfolio tab comparing the growth of every ticker in the list.

## How to run

1. Open `app.R` in RStudio (R 4.x).
2. Click **Run App**, or run `shiny::runApp("app.R")` in the console.
   Missing packages (`shiny`, `ggplot2`, `quantmod`, `TTR`, `xts`, `zoo`, `scales`) are installed automatically on first run.
3. Type your tickers, pick a date range and time frame, and press **Load / refresh data**.

## How the code is organised (`app.R`)

| Section in the script | Assignment step |
|---|---|
| `STEP 1 – DATA COLLECTION AND SETUP` | Data source, package install/load, `fetch_stock_data()` with error handling and fallback |
| `HELPER FUNCTIONS` | Time-frame conversion, indicator calculation, trading rules, back-test |
| `STEP 2 – SHINY APP SKELETON: USER INTERFACE` | `fluidPage` with date range, time frame, chart type and ticker widgets |
| `SERVER LOGIC` → main chart | Step 2 chart types, Step 3 indicator layers, Step 4 signal annotations |

## Features mapped to the rubric

- **Data collection** – Yahoo Finance via `quantmod::getSymbols()`; Tiingo and CSV upload as alternative sources; `tryCatch` error handling with readable messages; missing prices filled with `na.locf()`; automatic warm-up history so indicators are valid from the first visible bar.
- **Visualizing stock data** – Shiny skeleton, `dateRangeInput`, `selectInput` time frame (Daily / Weekly / Monthly via `to.weekly()` / `to.monthly()`), four chart types built in `ggplot2`, KPI tiles.
- **Technical indicators** – SMA (short and long), EMA, Bollinger Bands on the price panel; RSI and MACD in aligned panels below; each toggled from a checkbox group, with adjustable periods.
- **Trading rules and annotations** – MA crossover rule (Buy when the short MA crosses above the long MA, Sell when it crosses below, Hold otherwise), optional RSI overbought/oversold and MACD filters, Buy/Sell markers and labels on the bars, bullish/bearish shading for Hold periods, signal table and back-test.

## Trading rule

```r
# Buy  : short SMA crosses above long SMA   (prev short <= prev long AND short > long)
# Sell : short SMA crosses below long SMA   (prev short >= prev long AND short < long)
# Hold : every other bar
```

The signals are for learning purposes only and are not investment advice.
