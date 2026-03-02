# Alpha Vantage API – fetch functions for daily market tool
# Requires: httr, jsonlite. Load .env for ALPHAVANTAGE_API_KEY.
# Usage: source("scripts/load_env.R"); source("scripts/fetch_alphavantage.R")

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})
`%||%` <- function(x, y) if (is.null(x)) y else x

# Load API key from .env if not already set (app_market.R usually loads it first)
if (!nzchar(Sys.getenv("ALPHAVANTAGE_API_KEY"))) {
  tryCatch({
    env_path <- ".env"
    for (p in c(".env", file.path(getwd(), ".env"), file.path(getwd(), "..", ".env"))) {
      if (file.exists(p)) { env_path <- p; break }
    }
    if (exists("load_env")) load_env(env_path)
  }, error = function(e) NULL)
}

base_url <- "https://www.alphavantage.co/query"

av_get <- function(params) {
  key <- Sys.getenv("ALPHAVANTAGE_API_KEY")
  if (!nzchar(key)) stop("Set ALPHAVANTAGE_API_KEY in .env or environment.")
  params$apikey <- key
  r <- GET(base_url, query = params, user_agent("R-Shiny-Market-Tool"))
  if (http_error(r)) stop("Alpha Vantage request failed: ", status_code(r))
  txt <- content(r, as = "text", encoding = "UTF-8")
  out <- tryCatch(fromJSON(txt), error = function(e) NULL)
  if (is.null(out)) stop("Invalid JSON response.")
  if (is.list(out) && !is.null(out$`Error Message`)) stop(out$`Error Message`)
  if (is.list(out) && !is.null(out$`Note`)) stop("Rate limit: ", out$`Note`)
  out
}

# ---- 1. Stock Daily (TIME_SERIES_DAILY) ----
#' @param symbol e.g. "AAPL", "IBM"
av_stock_daily <- function(symbol = "AAPL") {
  out <- av_get(list(function = "TIME_SERIES_DAILY", symbol = symbol))
  ts <- out[["Time Series (Daily)"]]
  if (is.null(ts)) return(data.frame())
  df <- as.data.frame(ts, stringsAsFactors = FALSE)
  df <- data.frame(
    date = as.Date(rownames(df)),
    open = as.numeric(df[["1. open"]]),
    high = as.numeric(df[["2. high"]]),
    low = as.numeric(df[["3. low"]]),
    close = as.numeric(df[["4. close"]]),
    volume = as.numeric(df[["5. volume"]]),
    stringsAsFactors = FALSE
  )
  df[order(df$date, decreasing = TRUE), ]
}

# ---- 2. Top Gainers / Losers / Most Active ----
av_top_gainers_losers <- function() {
  out <- av_get(list(function = "TOP_GAINERS_LOSERS"))
  list(
    metadata = out$metadata,
    top_gainers = as.data.frame(out$top_gainers %||% list()),
    top_losers = as.data.frame(out$top_losers %||% list()),
    top_activated = as.data.frame(out$most_actively_traded %||% list())
  )
}

# ---- 3. News & Sentiment ----
#' @param tickers optional, e.g. "AAPL,MSFT"
#' @param limit max 50 free tier
av_news_sentiment <- function(tickers = "", limit = 50) {
  params <- list(function = "NEWS_SENTIMENT", limit = limit, sort = "LATEST")
  if (nzchar(tickers)) params$tickers <- tickers
  out <- av_get(params)
  feed <- out$feed
  if (is.null(feed) || length(feed) == 0) return(data.frame())
  df <- do.call(rbind, lapply(feed, function(x) {
    data.frame(
      title = as.character(x$title %||% NA),
      url = as.character(x$url %||% NA),
      time_published = as.character(x$time_published %||% NA),
      summary = as.character(x$summary %||% NA),
      overall_sentiment_score = as.numeric(x$overall_sentiment_score %||% NA),
      overall_sentiment_label = as.character(x$overall_sentiment_label %||% NA),
      source = as.character(x$source %||% NA),
      stringsAsFactors = FALSE
    )
  }))
  if (nrow(df) > 0) df else data.frame()
}

# ---- 4. Forex Daily (FX_DAILY) ----
#' @param from_symbol e.g. "EUR", "GBP"
#' @param to_symbol e.g. "USD"
av_fx_daily <- function(from_symbol = "EUR", to_symbol = "USD") {
  out <- av_get(list(
    function = "FX_DAILY",
    from_symbol = from_symbol,
    to_symbol = to_symbol
  ))
  key <- paste0("Time Series FX (Daily)")
  ts <- out[[key]]
  if (is.null(ts)) return(data.frame())
  df <- as.data.frame(ts, stringsAsFactors = FALSE)
  df <- data.frame(
    date = as.Date(rownames(df)),
    open = as.numeric(df[["1. open"]]),
    high = as.numeric(df[["2. high"]]),
    low = as.numeric(df[["3. low"]]),
    close = as.numeric(df[["4. close"]]),
    stringsAsFactors = FALSE
  )
  df[order(df$date, decreasing = TRUE), ]
}

# ---- 5. Commodities (WHEAT, CORN, WTI, etc.) ----
#' @param commodity "WHEAT", "CORN", "WTI", "BRENT", "NATURAL_GAS", "COPPER", "COFFEE", "GOLD", "SILVER" etc.
#' @param interval "daily", "weekly", "monthly" (some support quarterly/annual)
av_commodity <- function(commodity = "WHEAT", interval = "monthly") {
  out <- av_get(list(function = commodity, interval = interval))
  nm <- names(out)
  ts_key <- nm[grepl("data|time|series", nm, ignore.case = TRUE)][1]
  if (is.null(ts_key)) ts_key <- nm[!nm %in% c("name", "interval", "unit")][1]
  ts <- out[[ts_key]]
  if (is.null(ts)) return(data.frame())
  if (is.data.frame(ts)) {
    df <- ts
    if ("date" %in% names(df)) df$date <- as.Date(df$date)
    if ("value" %in% names(df)) df$value <- as.numeric(df$value)
    return(df)
  }
  if (is.list(ts) && !is.data.frame(ts) && length(ts) > 0) {
    df <- do.call(rbind, lapply(ts, function(x) data.frame(
      date = as.character(x$date %||% x$Date %||% NA),
      value = as.numeric(x$value %||% x$Value %||% NA),
      stringsAsFactors = FALSE
    )))
    df$date <- as.Date(df$date)
    return(df[order(df$date, decreasing = TRUE), ])
  }
  df <- as.data.frame(ts, stringsAsFactors = FALSE)
  df <- data.frame(
    date = as.Date(rownames(df)),
    value = as.numeric(unlist(df[, 1])),
    stringsAsFactors = FALSE
  )
  df[order(df$date, decreasing = TRUE), ]
}

# ---- 6. Economic Indicators ----
#' @param indicator "CPI", "INFLATION", "UNEMPLOYMENT", "FEDERAL_FUNDS_RATE", "TREASURY_YIELD", "REAL_GDP", "RETAIL_SALES", "DURABLES", "NONFARM_PAYROLL"
#' @param extra list of extra params e.g. list(interval="monthly", maturity="10year")
av_economic <- function(indicator = "CPI", interval = "monthly", ...) {
  params <- list(function = indicator, ...)
  if (indicator %in% c("CPI", "REAL_GDP", "TREASURY_YIELD", "FEDERAL_FUNDS_RATE") && nzchar(interval))
    params$interval <- interval
  out <- av_get(params)
  nm <- names(out)
  ts_key <- nm[grepl("data|time|series", nm, ignore.case = TRUE)][1]
  if (is.null(ts_key)) ts_key <- nm[!nm %in% c("name", "interval", "unit", "information")][1]
  ts <- out[[ts_key]]
  if (is.null(ts)) return(data.frame())
  if (is.data.frame(ts)) {
    df <- ts
    if ("date" %in% names(df)) df$date <- as.Date(df$date)
    return(df)
  }
  if (is.list(ts) && !is.data.frame(ts) && length(ts) > 0) {
    df <- do.call(rbind, lapply(ts, function(x) data.frame(
      date = as.character(x$date %||% x$Date %||% NA),
      value = as.numeric(x$value %||% x$Value %||% NA),
      stringsAsFactors = FALSE
    )))
    df$date <- as.Date(df$date)
    return(df[order(df$date, decreasing = TRUE), ])
  }
  df <- as.data.frame(ts, stringsAsFactors = FALSE)
  df <- data.frame(
    date = as.Date(rownames(df)),
    value = as.numeric(unlist(df[, 1])),
    stringsAsFactors = FALSE
  )
  df[order(df$date, decreasing = TRUE), ]
}

