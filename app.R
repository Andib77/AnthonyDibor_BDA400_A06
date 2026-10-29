# =============================================================================
#  BDA400 - Data Science Tools and Techniques
#  Assignment 6 - Technical Analysis using R, Visualization Phase
#  Author : Anthony Dibor
#
#  Portfolio dashboard built with R Shiny. It:
#    1. Fetches historical stock data (Yahoo Finance by default, with error
#       handling and fallback sources).
#    2. Visualizes the data as a line, area, candlestick or OHLC bar chart,
#       on a daily, weekly or monthly time frame.
#    3. Overlays technical indicators (SMA, EMA, Bollinger Bands, RSI, MACD)
#       that can each be switched on and off.
#    4. Applies a Moving Average Crossover trading rule (with optional RSI and
#       MACD filters and adjustable parameters) and annotates the chart with
#       Buy / Sell / Hold signals.
#
#  Run it from R / RStudio with:   shiny::runApp("app.R")
# =============================================================================


# -----------------------------------------------------------------------------
# STEP 1 - DATA COLLECTION AND SETUP
# -----------------------------------------------------------------------------

# 1a. Data source ---------------------------------------------------------------
# Yahoo Finance is the primary data source (as required by the assignment).
# Two other sources are offered so the app still works if Yahoo is down:
#   * Tiingo      - free API key from https://www.tiingo.com
#   * CSV upload  - any file with Date, Open, High, Low, Close, Volume columns

# 1b. Install (only if missing) and load the necessary packages ---------------
required_packages <- c("shiny", "ggplot2", "quantmod", "TTR", "xts", "zoo", "scales")
missing_packages  <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) install.packages(missing_packages)

library(shiny)     # web application framework
library(ggplot2)   # charting
library(quantmod)  # getSymbols(), Cl(), to.weekly(), ...
library(TTR)       # SMA(), EMA(), RSI(), MACD(), BBands()
library(xts)       # time-series objects
library(zoo)       # na.locf() for filling missing values
library(scales)    # axis label formatting

# 1c. Default parameters (same structure as the assignment brief) -------------
stock_symbol <- "AAPL"                        # default ticker
start_date   <- Sys.Date() - 365              # one year of history by default
end_date     <- Sys.Date()

# Indicators need "warm-up" history (e.g. a 50-day SMA needs 50 bars before
# its first value). We download extra calendar days before the chosen start
# date so indicators are fully formed on the first visible bar. The amount
# depends on the time frame: 50 monthly bars need ~4 years of history.
warmup_days <- function(long_n, time_frame) {
  days_per_bar <- c(Daily = 1.5, Weekly = 7, Monthly = 31)[[time_frame]]
  ceiling(max(long_n, 35) * days_per_bar) + 30
}

# 1d. Data-fetching helpers -----------------------------------------------------

# Clean an OHLC xts object: standard column names, remove empty rows,
# fill isolated missing prices with the previous value.
clean_ohlc <- function(x) {
  x <- x[, 1:min(ncol(x), 6)]
  colnames(x) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")[seq_len(ncol(x))]
  x <- x[rowSums(is.na(x[, 1:4])) < 4, ]       # drop rows where every price is missing
  x <- na.locf(x, na.rm = FALSE)                # carry last price forward over gaps
  x <- x[complete.cases(x[, 1:4]), ]            # drop any leading NAs that remain
  if (nrow(x) == 0) stop("No usable price rows after cleaning.")
  x
}

# Source 1: Yahoo Finance via quantmod (primary)
fetch_yahoo <- function(symbol, from, to) {
  getSymbols(symbol, src = "yahoo", from = from, to = to + 1, auto.assign = FALSE)
}

# Source 2: Tiingo via quantmod (needs a free API key)
fetch_tiingo <- function(symbol, from, to, api_key) {
  if (!nzchar(api_key)) stop("A Tiingo API key is required for this source.")
  getSymbols(symbol, src = "tiingo", from = from, to = to,
             api.key = api_key, auto.assign = FALSE)
}

# Source 3: uploaded CSV
fetch_csv <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  need <- c("date", "open", "high", "low", "close")
  if (!all(need %in% names(df)))
    stop("CSV must contain the columns: Date, Open, High, Low, Close (Volume optional).")
  if (!"volume" %in% names(df)) df$volume <- NA
  xts(df[, c("open", "high", "low", "close", "volume")], order.by = as.Date(df$date))
}

# Wrapper with exception handling: tries the chosen source, falls back to
# Yahoo if another source fails, and always returns a list with either
# $data or a readable $error message (the app never crashes on bad input).
fetch_stock_data <- function(symbol, from, to, source = "yahoo",
                             api_key = "", csv_path = NULL) {
  attempt <- function(fun) tryCatch(clean_ohlc(fun()), error = function(e) e)

  result <- switch(source,
    yahoo  = attempt(function() fetch_yahoo(symbol, from, to)),
    tiingo = attempt(function() fetch_tiingo(symbol, from, to, api_key)),
    csv    = attempt(function() {
      if (is.null(csv_path)) stop("Please upload a CSV file.")
      fetch_csv(csv_path)
    })
  )
  used <- source

  # Fallback: if Tiingo failed, try Yahoo before giving up
  if (inherits(result, "error") && source == "tiingo") {
    first_error <- conditionMessage(result)
    result <- attempt(function() fetch_yahoo(symbol, from, to))
    used   <- "yahoo (fallback)"
    if (inherits(result, "error"))
      result <- simpleError(paste0(first_error, " | Yahoo fallback: ", conditionMessage(result)))
  }

  if (inherits(result, "error"))
    return(list(data = NULL, source = used, error = conditionMessage(result)))
  list(data = result, source = used, error = NULL)
}

# Example from the assignment brief (kept for reference; the app itself
# calls fetch_stock_data() reactively from the UI):
#   stock_data <- getSymbols(stock_symbol, src = "yahoo", from = start_date,
#                            to = end_date, auto.assign = FALSE)


# -----------------------------------------------------------------------------
# HELPER FUNCTIONS - time frames, indicators, trading rules
# -----------------------------------------------------------------------------

# Convert daily bars into weekly or monthly bars
apply_time_frame <- function(x, time_frame) {
  switch(time_frame,
    Daily   = x,
    Weekly  = to.weekly(x,  indexAt = "endof", OHLC = TRUE, name = NULL),
    Monthly = to.monthly(x, indexAt = "endof", OHLC = TRUE, name = NULL)
  )
}

# STEP 3 helper: compute every indicator on the full (warm-up) history
compute_indicators <- function(x, p) {
  close <- Cl(x)
  n     <- nrow(x)
  safe  <- function(expr) tryCatch(expr, error = function(e) xts(rep(NA_real_, n), index(x)))

  ind <- data.frame(
    date       = as.Date(index(x)),
    open       = as.numeric(Op(x)),
    high       = as.numeric(Hi(x)),
    low        = as.numeric(Lo(x)),
    close      = as.numeric(close),
    sma_short  = as.numeric(safe(SMA(close, n = p$short_n))),
    sma_long   = as.numeric(safe(SMA(close, n = p$long_n))),
    ema        = as.numeric(safe(EMA(close, n = p$ema_n))),
    rsi        = as.numeric(safe(RSI(close, n = p$rsi_n)))
  )

  bb <- safe(BBands(close, n = 20, sd = 2))
  ind$bb_up <- if (ncol(bb) >= 3) as.numeric(bb[, "up"]) else NA_real_
  ind$bb_dn <- if (ncol(bb) >= 3) as.numeric(bb[, "dn"]) else NA_real_

  macd <- safe(MACD(close, nFast = 12, nSlow = 26, nSig = 9, percent = FALSE))
  ind$macd        <- if (ncol(macd) == 2) as.numeric(macd[, "macd"])   else NA_real_
  ind$macd_signal <- if (ncol(macd) == 2) as.numeric(macd[, "signal"]) else NA_real_
  ind$macd_hist   <- ind$macd - ind$macd_signal
  ind
}

# STEP 4 helper: trading rules
# ---------------------------------------------------------------------------
# Moving Average Crossover rule (from the brief):
#   * short MA crosses ABOVE long MA  -> "Buy"
#   * short MA crosses BELOW long MA  -> "Sell"
#   * otherwise                        -> "Hold"
# The brief's one-liner (ifelse(short > long, "Buy", "Sell")) labels every
# bar by trend direction. A real crossover rule only fires on the bar where
# the lines actually cross, so this version marks the crossing bars and
# reports the prevailing trend ("Bullish"/"Bearish") for every other bar.
#
# Optional confirmation filters (user can switch on/off):
#   * RSI filter : ignore Buys when RSI > overbought, Sells when RSI < oversold
#   * MACD filter: Buy only if MACD > signal line, Sell only if MACD < signal
generate_signals <- function(ind, p) {
  short <- ind$sma_short
  long  <- ind$sma_long
  prev_short <- c(NA, head(short, -1))
  prev_long  <- c(NA, head(long,  -1))

  cross_up   <- !is.na(prev_short) & !is.na(prev_long) & prev_short <= prev_long & short > long
  cross_down <- !is.na(prev_short) & !is.na(prev_long) & prev_short >= prev_long & short < long

  if (p$use_rsi_filter) {
    cross_up   <- cross_up   & !is.na(ind$rsi) & ind$rsi < p$rsi_overbought
    cross_down <- cross_down & !is.na(ind$rsi) & ind$rsi > p$rsi_oversold
  }
  if (p$use_macd_filter) {
    cross_up   <- cross_up   & !is.na(ind$macd_hist) & ind$macd_hist > 0
    cross_down <- cross_down & !is.na(ind$macd_hist) & ind$macd_hist < 0
  }

  ind$Signal <- ifelse(cross_up, "Buy", ifelse(cross_down, "Sell", "Hold"))
  ind$Trend  <- ifelse(is.na(short) | is.na(long), NA,
                       ifelse(short > long, "Bullish", "Bearish"))
  ind
}

# Simple back-test: start in cash, buy the whole position on a Buy signal,
# sell on a Sell signal. Returns summary numbers shown in the dashboard.
backtest <- function(df) {
  in_pos <- FALSE; entry <- NA; trades <- c()
  for (i in seq_len(nrow(df))) {
    if (!in_pos && df$Signal[i] == "Buy")  { in_pos <- TRUE;  entry <- df$close[i] }
    else if (in_pos && df$Signal[i] == "Sell") {
      in_pos <- FALSE; trades <- c(trades, df$close[i] / entry - 1)
    }
  }
  open_trade <- if (in_pos) tail(df$close, 1) / entry - 1 else NA
  all_trades <- c(trades, if (in_pos) open_trade)
  list(
    n_trades      = length(trades) + in_pos,
    win_rate      = if (length(trades)) mean(trades > 0) else NA,
    strategy_ret  = if (length(all_trades)) prod(1 + all_trades) - 1 else 0,
    buy_hold_ret  = tail(df$close, 1) / head(df$close, 1) - 1,
    open_position = in_pos
  )
}


# Null-coalescing helper (built into R >= 4.4, defined here for older R)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Small helper so the "About rules" tab does not depend on an external file
about_rules_html <- function() {
  HTML("
  <h4>Moving Average Crossover</h4>
  <p><b>Buy</b> when the short SMA crosses above the long SMA.
     <b>Sell</b> when it crosses below. Every other bar is a <b>Hold</b>;
     the shading shows whether the prevailing trend is bullish (green) or bearish (red).</p>
  <h4>Optional filters</h4>
  <ul>
    <li><b>RSI filter</b> - skip a Buy if RSI is above the overbought level, skip a Sell if RSI is below the oversold level.</li>
    <li><b>MACD filter</b> - only Buy when the MACD line is above its signal line, only Sell when it is below.</li>
  </ul>
  <p>The back-test assumes one full position, no fees, and trades at the closing price of the signal bar.
     It is for learning only, not investment advice.</p>")
}

# -----------------------------------------------------------------------------
# STEP 2 - SHINY APP SKELETON: USER INTERFACE
# -----------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#f6f7f9; }
    .well { background:#ffffff; border:1px solid #e3e6ea; box-shadow:none; }
    h2 { font-weight:600; }
    .kpi { background:#fff; border:1px solid #e3e6ea; border-radius:6px;
           padding:10px 14px; margin-bottom:10px; }
    .kpi .lbl { font-size:12px; color:#6b7280; }
    .kpi .val { font-size:20px; font-weight:600; }
    .err { color:#b91c1c; font-weight:600; }
  "))),

  titlePanel("Portfolio Dashboard - Technical Analysis"),

  sidebarLayout(
    sidebarPanel(width = 3,

      # --- Data selection widgets ---
      h4("1. Data"),
      selectInput("source", "Data source:",
                  choices = c("Yahoo Finance" = "yahoo",
                              "Tiingo (API key)" = "tiingo",
                              "Upload CSV" = "csv")),
      conditionalPanel("input.source == 'tiingo'",
        passwordInput("tiingo_key", "Tiingo API key:")),
      conditionalPanel("input.source == 'csv'",
        fileInput("csv_file", "CSV (Date, Open, High, Low, Close, Volume):", accept = ".csv")),
      textInput("symbols", "Portfolio tickers (comma separated):",
                value = "AAPL, MSFT, GOOGL"),
      uiOutput("symbol_picker"),
      dateRangeInput("date_range", "Select Date Range:",
                     start = start_date, end = end_date, max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      actionButton("load", "Load / refresh data", class = "btn-primary"),

      hr(),
      # --- Chart widgets ---
      h4("2. Chart"),
      radioButtons("chart_type", "Chart type:",
                   choices = c("Candlestick", "Line", "Area", "OHLC Bars"),
                   inline = TRUE),

      hr(),
      # --- Indicator toggles (each one can be turned on/off) ---
      h4("3. Technical indicators"),
      checkboxGroupInput("technical_indicators", NULL,
                         choices = c("Moving Averages", "EMA", "Bollinger Bands", "RSI", "MACD"),
                         selected = c("Moving Averages", "RSI", "MACD")),
      fluidRow(
        column(6, numericInput("short_n", "Short MA", 20, min = 2, max = 100)),
        column(6, numericInput("long_n",  "Long MA",  50, min = 5, max = 250))
      ),
      fluidRow(
        column(6, numericInput("ema_n", "EMA period", 12, min = 2, max = 200)),
        column(6, numericInput("rsi_n", "RSI period", 14, min = 2, max = 50))
      ),

      hr(),
      # --- Trading rule widgets ---
      h4("4. Trading rules"),
      checkboxInput("show_signals", "Annotate Buy / Sell signals on chart", TRUE),
      checkboxInput("show_trend",   "Shade bullish / bearish trend (Hold periods)", TRUE),
      checkboxInput("use_rsi_filter",  "RSI filter (avoid buying overbought / selling oversold)", FALSE),
      fluidRow(
        column(6, numericInput("rsi_oversold",   "Oversold",   30, min = 5,  max = 50)),
        column(6, numericInput("rsi_overbought", "Overbought", 70, min = 50, max = 95))
      ),
      checkboxInput("use_macd_filter", "MACD confirmation filter", FALSE)
    ),

    mainPanel(width = 9,
      uiOutput("status"),
      uiOutput("kpis"),
      tabsetPanel(
        tabPanel("Chart",       plotOutput("stock_chart", height = "720px")),
        tabPanel("Signals",     br(), tableOutput("signal_table")),
        tabPanel("Portfolio",   br(), plotOutput("portfolio_chart", height = "420px"),
                                br(), tableOutput("portfolio_table")),
        tabPanel("About rules", br(), about_rules_html())
      )
    )
  )
)



# -----------------------------------------------------------------------------
# SERVER LOGIC
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # Parse the ticker list typed by the user
  tickers <- reactive({
    s <- toupper(trimws(unlist(strsplit(input$symbols, ","))))
    unique(s[nzchar(s)])
  })

  output$symbol_picker <- renderUI({
    selectInput("stock_symbol", "Ticker to chart:", choices = tickers(),
                selected = isolate(input$stock_symbol) %||% tickers()[1])
  })

  # ---- STEP 1: fetch data (re-runs when "Load" is pressed) -----------------
  raw_data <- eventReactive(
    list(input$load, input$stock_symbol, input$source, input$csv_file,
         input$time_frame, input$long_n), {
    req(input$stock_symbol, input$date_range, input$time_frame)
    from <- as.Date(input$date_range[1]) - warmup_days(input$long_n %||% 50, input$time_frame)
    to   <- as.Date(input$date_range[2])
    withProgress(message = paste("Fetching", input$stock_symbol), {
      fetch_stock_data(input$stock_symbol, from, to,
                       source   = input$source,
                       api_key  = input$tiingo_key %||% "",
                       csv_path = input$csv_file$datapath)
    })
  }, ignoreNULL = FALSE)

  output$status <- renderUI({
    r <- raw_data()
    if (!is.null(r$error))
      div(class = "err", paste("Could not load data:", r$error))
    else
      p(sprintf("Source: %s  |  %d daily rows loaded (including indicator warm-up)",
                r$source, nrow(r$data)))
  })

  # Collect the user-adjustable parameters in one list
  params <- reactive({
    list(short_n = max(2, input$short_n), long_n = max(input$short_n + 1, input$long_n),
         ema_n = input$ema_n, rsi_n = input$rsi_n,
         use_rsi_filter = input$use_rsi_filter, use_macd_filter = input$use_macd_filter,
         rsi_oversold = input$rsi_oversold, rsi_overbought = input$rsi_overbought)
  })

  # ---- STEPS 2-4: time frame -> indicators -> signals -> filter to range ---
  chart_data <- reactive({
    r <- raw_data()
    validate(need(is.null(r$error), "No data to display - see the message above."))
    x   <- apply_time_frame(r$data, input$time_frame)
    ind <- compute_indicators(x, params())
    ind <- generate_signals(ind, params())

    # Filter on the selected date range (after indicators were calculated
    # on the warm-up history so they are valid from the first visible bar)
    filtered_data <- subset(ind, date >= input$date_range[1] & date <= input$date_range[2])
    validate(need(nrow(filtered_data) > 1,
                  "Not enough bars in this date range / time frame. Widen the range."))
    filtered_data
  })

  # ---- Main chart -----------------------------------------------------------
  output$stock_chart <- renderPlot({
    filtered_data <- chart_data()
    ti  <- input$technical_indicators
    sym <- input$stock_symbol

    # Bar width so candles look right on any time frame
    bar_w <- as.numeric(median(diff(filtered_data$date))) * 0.7

    # Panels: price is always shown; RSI and MACD get their own panels when
    # switched on. Using one ggplot with facets keeps the x-axis aligned.
    panels <- c("Price", if ("RSI" %in% ti) "RSI", if ("MACD" %in% ti) "MACD")
    price  <- transform(filtered_data, panel = factor("Price", levels = panels))

    # Create ggplot based on user input
    p <- ggplot(price, aes(x = date)) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold", hjust = 0)) +
      labs(title = paste(sym, "-", input$time_frame, input$chart_type, "chart"),
           x = NULL, y = NULL, colour = NULL, fill = NULL)

    # Trend shading = the Hold state between signals (Bullish / Bearish)
    if (input$show_trend) {
      trend <- price[!is.na(price$Trend), ]
      # each shaded block runs from one bar to the next, so there are no gaps
      trend$next_date <- c(tail(trend$date, -1), tail(trend$date, 1) + 1)
      if (nrow(trend)) p <- p + geom_rect(
        data = trend,
        aes(xmin = date, xmax = next_date, ymin = -Inf, ymax = Inf, fill = Trend),
        alpha = 0.08, inherit.aes = FALSE) +
        scale_fill_manual(values = c(Bullish = "#16a34a", Bearish = "#dc2626"))
    }

    # Add various chart types based on user input
    up <- price$close >= price$open
    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = close, colour = "Close"), linewidth = 0.8)
    } else if (input$chart_type == "Area") {
      p <- p + geom_ribbon(aes(ymin = min(low) * 0.98, ymax = close), fill = "#2563eb", alpha = 0.18) +
               geom_line(aes(y = close, colour = "Close"), linewidth = 0.8)
    } else if (input$chart_type == "Candlestick") {
      p <- p +
        geom_segment(aes(xend = date, y = low, yend = high),
                     colour = ifelse(up, "#16a34a", "#dc2626")) +
        geom_rect(aes(xmin = date - bar_w / 2, xmax = date + bar_w / 2,
                      ymin = pmin(open, close), ymax = pmax(open, close)),
                  fill = ifelse(up, "#16a34a", "#dc2626"), colour = NA)
    } else {  # OHLC Bars
      p <- p +
        geom_segment(aes(xend = date, y = low, yend = high), colour = ifelse(up, "#16a34a", "#dc2626")) +
        geom_segment(aes(x = date - bar_w / 2, xend = date, y = open, yend = open),
                     colour = ifelse(up, "#16a34a", "#dc2626")) +
        geom_segment(aes(x = date, xend = date + bar_w / 2, y = close, yend = close),
                     colour = ifelse(up, "#16a34a", "#dc2626"))
    }

    # ---- STEP 3: overlay technical indicators as additional layers --------
    if ("Moving Averages" %in% ti) {
      p <- p +
        geom_line(aes(y = sma_short, colour = paste0("SMA ", params()$short_n)), linewidth = 0.8, na.rm = TRUE) +
        geom_line(aes(y = sma_long,  colour = paste0("SMA ", params()$long_n)),  linewidth = 0.8, na.rm = TRUE)
    }
    if ("EMA" %in% ti) {
      p <- p + geom_line(aes(y = ema, colour = paste0("EMA ", params()$ema_n)),
                         linewidth = 0.7, linetype = "dashed", na.rm = TRUE)
    }
    if ("Bollinger Bands" %in% ti) {
      p <- p + geom_ribbon(aes(ymin = bb_dn, ymax = bb_up), fill = "#9333ea", alpha = 0.08, na.rm = TRUE) +
               geom_line(aes(y = bb_up, colour = "Bollinger (20, 2)"), linewidth = 0.4, na.rm = TRUE) +
               geom_line(aes(y = bb_dn, colour = "Bollinger (20, 2)"), linewidth = 0.4, na.rm = TRUE)
    }
    if ("RSI" %in% ti) {
      rsi_df <- transform(filtered_data, panel = factor("RSI", levels = panels))
      p <- p +
        geom_line(data = rsi_df, aes(y = rsi, colour = "RSI"), linewidth = 0.7, na.rm = TRUE) +
        geom_hline(data = data.frame(panel = factor("RSI", levels = panels),
                                     y = c(input$rsi_oversold, input$rsi_overbought)),
                   aes(yintercept = y), linetype = "dotted", colour = "grey40")
    }
    if ("MACD" %in% ti) {
      macd_df <- transform(filtered_data, panel = factor("MACD", levels = panels))
      p <- p +
        geom_col(data = macd_df, aes(y = macd_hist), width = bar_w,
                 fill = ifelse(!is.na(macd_df$macd_hist) & macd_df$macd_hist >= 0, "#16a34a", "#dc2626"),
                 alpha = 0.5, na.rm = TRUE) +
        geom_line(data = macd_df, aes(y = macd,        colour = "MACD"),        linewidth = 0.7, na.rm = TRUE) +
        geom_line(data = macd_df, aes(y = macd_signal, colour = "MACD signal"), linewidth = 0.7, na.rm = TRUE)
    }

    # ---- STEP 4: annotate the bars with the trading signals ----------------
    if (input$show_signals) {
      sig <- price[price$Signal != "Hold", ]
      if (nrow(sig)) {
        offset <- diff(range(price$low, price$high)) * 0.06
        sig$y  <- ifelse(sig$Signal == "Buy", sig$low - offset, sig$high + offset)
        sig$label_y <- ifelse(sig$Signal == "Buy", sig$y - offset, sig$y + offset)
        p <- p +
          geom_point(data = sig, aes(x = date, y = y, shape = Signal),
                     size = 4, fill = ifelse(sig$Signal == "Buy", "#16a34a", "#dc2626"),
                     colour = "white", inherit.aes = FALSE) +
          geom_label(data = sig, aes(x = date, y = label_y, label = Signal), size = 3.2,
                     colour = ifelse(sig$Signal == "Buy", "#166534", "#991b1b"),
                     label.size = 0.2, inherit.aes = FALSE) +
          scale_shape_manual(values = c(Buy = 24, Sell = 25), guide = "none") +
          # invisible points widen the price axis so labels are never cut off
          geom_blank(data = transform(sig, y = ifelse(Signal == "Buy", label_y - offset, label_y + offset)),
                     aes(x = date, y = y), inherit.aes = FALSE)
      }
    }

    # Panels share the date axis; heights favour the price panel
    p <- p + facet_grid(panel ~ ., scales = "free_y", switch = "y",
                        space = "fixed") +
      scale_x_date(labels = date_format("%b %Y"), expand = expansion(mult = 0.02)) +
      scale_y_continuous(position = "right") +
      scale_colour_manual(values = c(
        "Close" = "#1f2937", "RSI" = "#7c3aed", "MACD" = "#2563eb", "MACD signal" = "#f59e0b",
        "Bollinger (20, 2)" = "#9333ea",
        setNames("#2563eb", paste0("SMA ", params()$short_n)),
        setNames("#f59e0b", paste0("SMA ", params()$long_n)),
        setNames("#0891b2", paste0("EMA ", params()$ema_n))))

    # Return the ggplot object
    print(p)
  })

  # ---- KPI tiles ------------------------------------------------------------
  output$kpis <- renderUI({
    d  <- chart_data(); bt <- backtest(d)
    last <- tail(d, 1)
    tile <- function(lbl, val) column(2, div(class = "kpi", div(class = "lbl", lbl), div(class = "val", val)))
    fluidRow(
      tile("Last close",       dollar(last$close)),
      tile("Period change",    percent(bt$buy_hold_ret, accuracy = 0.1)),
      tile("Current trend",    last$Trend %||% "-"),
      tile("Signals (trades)", bt$n_trades),
      tile("Strategy return",  percent(bt$strategy_ret, accuracy = 0.1)),
      tile("RSI",              round(last$rsi, 1))
    )
  })

  # ---- Signal table -------------------------------------------------------
  output$signal_table <- renderTable({
    d <- chart_data()
    s <- d[d$Signal != "Hold", c("date", "Signal", "close", "sma_short", "sma_long", "rsi")]
    if (!nrow(s)) return(data.frame(Message = "No Buy/Sell crossovers in this range - every bar is a Hold."))
    s$date <- format(s$date)
    names(s) <- c("Date", "Signal", "Close",
                  paste0("SMA ", params()$short_n), paste0("SMA ", params()$long_n), "RSI")
    s
  }, digits = 2)

  # ---- Portfolio tab: compare every ticker in the list ------------------
  portfolio <- eventReactive(list(input$load, tickers()), {
    req(input$date_range)
    out <- lapply(tickers(), function(sym) {
      r <- fetch_stock_data(sym, as.Date(input$date_range[1]), as.Date(input$date_range[2]),
                            source = if (input$source == "csv") "yahoo" else input$source,
                            api_key = input$tiingo_key %||% "")
      if (!is.null(r$error)) return(data.frame(symbol = sym, date = as.Date(NA), growth = NA, error = r$error))
      cl <- as.numeric(Cl(r$data))
      data.frame(symbol = sym, date = as.Date(index(r$data)), growth = cl / cl[1] * 100, error = NA)
    })
    do.call(rbind, out)
  }, ignoreNULL = FALSE)

  output$portfolio_chart <- renderPlot({
    d <- portfolio(); d <- d[!is.na(d$growth), ]
    validate(need(nrow(d) > 0, "No portfolio data could be loaded."))
    ggplot(d, aes(date, growth, colour = symbol)) +
      geom_hline(yintercept = 100, colour = "grey60") +
      geom_line(linewidth = 0.9) +
      theme_minimal(base_size = 13) + theme(legend.position = "top") +
      labs(title = "Growth of $100 invested at the start of the range", x = NULL, y = NULL, colour = NULL)
  })

  output$portfolio_table <- renderTable({
    d <- portfolio()
    do.call(rbind, lapply(split(d, d$symbol), function(s) {
      if (all(is.na(s$growth))) return(data.frame(Ticker = s$symbol[1], Return = NA, `Max drawdown` = NA,
                                                  Status = s$error[1], check.names = FALSE))
      g <- s$growth
      data.frame(Ticker = s$symbol[1],
                 Return = percent(tail(g, 1) / 100 - 1, accuracy = 0.1),
                 `Max drawdown` = percent(min(g / cummax(g) - 1), accuracy = 0.1),
                 Status = "OK", check.names = FALSE)
    }))
  })
}

shinyApp(ui, server)
