# Daily Market Analysis Tool – Alpha Vantage
# Based on 06_alphavantage_ai_report.R: readRenviron(.env), API_KEY / ALPHAVANTAGE_API_KEY, httr2.
# Run: setwd("tool 1"); shiny::runApp("app_market.R")  or  source("run_market.R")
# Requires: httr2, jsonlite, shiny, ggplot2, DT, dplyr

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(jsonlite)
# Use httr2 like reference script (fallback to httr if httr2 not installed)
use_httr2 <- suppressWarnings(requireNamespace("httr2", quietly = TRUE))
if (use_httr2) library(httr2) else suppressPackageStartupMessages(library(httr))

# ---- Load .env (same pattern as 06_alphavantage_ai_report.R: readRenviron) ----
env_path <- NULL
if (file.exists(".env")) {
  env_path <- ".env"
} else if (file.exists("tool 1/.env")) {
  env_path <- "tool 1/.env"
} else if (file.exists("../.env")) {
  env_path <- "../.env"
} else if (file.exists(file.path(getwd(), ".env"))) {
  env_path <- file.path(getwd(), ".env")
} else if (file.exists(file.path(getwd(), "tool 1", ".env"))) {
  env_path <- file.path(getwd(), "tool 1", ".env")
}
if (!is.null(env_path)) readRenviron(env_path)

# API key: support both names (reference uses API_KEY, this project uses ALPHAVANTAGE_API_KEY)
API_KEY <- Sys.getenv("ALPHAVANTAGE_API_KEY")
if (!nzchar(API_KEY)) API_KEY <- Sys.getenv("API_KEY")

# ---- Alpha Vantage API (httr2 like reference, else httr) ----
`%||%` <- function(x, y) if (is.null(x)) y else x
base_url <- "https://www.alphavantage.co/query"

av_get <- function(params) {
  if (!nzchar(API_KEY)) stop("API key not set. Put API_KEY or ALPHAVANTAGE_API_KEY in .env in the app folder.")
  params$apikey <- API_KEY
  if (use_httr2) {
    req <- request(base_url)
    req <- do.call(req_url_query, c(list(req), params))
    req <- req_method(req, "GET")
    resp <- req_perform(req)
    if (resp_status(resp) != 200L) stop("Alpha Vantage request failed. Check API key or URL.")
    out <- resp_body_json(resp)
  } else {
    r <- GET(base_url, query = params, user_agent("R-Shiny-Market-Tool"))
    if (http_error(r)) stop("Alpha Vantage request failed: ", status_code(r))
    out <- content(r, as = "parsed", type = "application/json")
  }
  if (is.list(out) && !is.null(out$`Error Message`)) stop(out$`Error Message`)
  if (is.list(out) && !is.null(out$`Note`)) {
    msg <- out$`Note`
    stop("API 调用限额（Rate limit）: ", msg,
         " | 免费密钥：每分钟 5 次、每天约 25 次。请稍后再试。")
  }
  out
}

# TIME_SERIES_DAILY: daily OHLCV. Build rows explicitly to avoid date/format issues (like 06_alphavantage_ai_report.R).
av_stock_daily <- function(symbol = "AAPL", outputsize = "compact") {
  out <- av_get(list(`function` = "TIME_SERIES_DAILY", symbol = symbol, outputsize = outputsize))
  ts <- out[["Time Series (Daily)"]]
  if (is.null(ts) || length(ts) == 0L) return(data.frame())
  dates_char <- names(ts)
  if (is.null(dates_char) || length(dates_char) != length(ts)) return(data.frame())
  n <- length(dates_char)
  out_list <- vector("list", n)
  for (i in seq_len(n)) {
    d <- ts[[i]]
    out_list[[i]] <- data.frame(
      date = dates_char[i],
      open = as.numeric(d[["1. open"]] %||% NA),
      high = as.numeric(d[["2. high"]] %||% NA),
      low = as.numeric(d[["3. low"]] %||% NA),
      close = as.numeric(d[["4. close"]] %||% NA),
      volume = as.numeric(d[["5. volume"]] %||% NA),
      stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, out_list)
  df$date <- as.Date(df$date, format = "%Y-%m-%d")
  df <- df[order(df$date, decreasing = TRUE), ]
  rownames(df) <- NULL
  df
}

av_top_gainers_losers <- function() {
  out <- av_get(list(`function` = "TOP_GAINERS_LOSERS"))
  list(metadata = out$metadata, top_gainers = as.data.frame(out$top_gainers %||% list()),
    top_losers = as.data.frame(out$top_losers %||% list()), top_activated = as.data.frame(out$most_actively_traded %||% list()))
}

av_news_sentiment <- function(tickers = "", limit = 50) {
  params <- list(`function` = "NEWS_SENTIMENT", limit = limit, sort = "LATEST")
  if (nzchar(tickers)) params$tickers <- tickers
  out <- av_get(params)
  feed <- out$feed
  if (is.null(feed) || length(feed) == 0) return(data.frame())
  df <- do.call(rbind, lapply(feed, function(x) data.frame(title = as.character(x$title %||% NA), url = as.character(x$url %||% NA),
    time_published = as.character(x$time_published %||% NA), summary = as.character(x$summary %||% NA),
    overall_sentiment_score = as.numeric(x$overall_sentiment_score %||% NA), overall_sentiment_label = as.character(x$overall_sentiment_label %||% NA),
    source = as.character(x$source %||% NA), stringsAsFactors = FALSE)))
  if (nrow(df) > 0) df else data.frame()
}

# FX_DAILY: same explicit row-by-row parsing to avoid date/format and orientation issues.
av_fx_daily <- function(from_symbol = "EUR", to_symbol = "USD") {
  out <- av_get(list(`function` = "FX_DAILY", from_symbol = from_symbol, to_symbol = to_symbol))
  ts <- out[["Time Series FX (Daily)"]]
  if (is.null(ts) || length(ts) == 0L) return(data.frame())
  dates_char <- names(ts)
  if (is.null(dates_char) || length(dates_char) != length(ts)) return(data.frame())
  n <- length(dates_char)
  out_list <- vector("list", n)
  for (i in seq_len(n)) {
    d <- ts[[i]]
    out_list[[i]] <- data.frame(
      date = dates_char[i],
      open = as.numeric(d[["1. open"]] %||% NA),
      high = as.numeric(d[["2. high"]] %||% NA),
      low = as.numeric(d[["3. low"]] %||% NA),
      close = as.numeric(d[["4. close"]] %||% NA),
      stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, out_list)
  df$date <- as.Date(df$date, format = "%Y-%m-%d")
  df <- df[order(df$date, decreasing = TRUE), ]
  rownames(df) <- NULL
  df
}

# Realtime/latest exchange rate. CURRENCY_EXCHANGE_RATE is premium-only; free keys fall back to FX_DAILY (forex only).
av_currency_exchange_rate <- function(from_currency = "USD", to_currency = "EUR") {
  from_currency <- toupper(trimws(from_currency))
  to_currency <- toupper(trimws(to_currency))
  # Try premium endpoint first (works with premium API key)
  tryCatch({
    out <- av_get(list(
      `function` = "CURRENCY_EXCHANGE_RATE",
      from_currency = from_currency,
      to_currency = to_currency
    ))
    block <- out[["Realtime Currency Exchange Rate"]]
    if (is.null(block)) stop("No rate in response")
    return(data.frame(
      from_code = as.character(block[["1. From_Currency Code"]] %||% NA),
      from_name = as.character(block[["2. From_Currency Name"]] %||% NA),
      to_code = as.character(block[["3. To_Currency Code"]] %||% NA),
      to_name = as.character(block[["4. To_Currency Name"]] %||% NA),
      exchange_rate = as.numeric(block[["5. Exchange Rate"]] %||% NA),
      last_refreshed = as.character(block[["6. Last Refreshed"]] %||% NA),
      time_zone = as.character(block[["7. Time Zone"]] %||% NA),
      bid_price = as.numeric(block[["8. Bid Price"]] %||% NA),
      ask_price = as.numeric(block[["9. Ask Price"]] %||% NA),
      note = "",
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    msg <- conditionMessage(e)
    # CURRENCY_EXCHANGE_RATE is premium-only; for forex pairs use FX_DAILY (free) as fallback
    if (!grepl("Invalid API call|premium|Premium", msg, ignore.case = TRUE)) stop(e)
    # Fallback: FX_DAILY supports physical currency pairs only (e.g. EUR, USD, GBP, JPY)
    crypto <- c("BTC", "ETH", "XRP", "LTC", "BCH", "ADA", "DOGE", "SOL", "USDT", "USDC")
    is_crypto <- function(c) toupper(c) %in% crypto
    if (is_crypto(from_currency) || is_crypto(to_currency)) {
      stop("CURRENCY_EXCHANGE_RATE (including crypto) requires a premium Alpha Vantage key. Use the 'Forex' section for physical currency pairs (e.g. EUR/USD).")
    }
    fx <- tryCatch(av_fx_daily(from_symbol = from_currency, to_symbol = to_currency), error = function(e2) NULL)
    if (is.null(fx) || nrow(fx) == 0L) {
      stop("This pair is not available with a free key. CURRENCY_EXCHANGE_RATE requires premium. Try the 'Forex' section for pairs like EUR/USD.")
    }
    latest <- fx[1L, ]
    data.frame(
      from_code = from_currency,
      from_name = from_currency,
      to_code = to_currency,
      to_name = to_currency,
      exchange_rate = as.numeric(latest$close),
      last_refreshed = as.character(latest$date),
      time_zone = "",
      bid_price = NA_real_,
      ask_price = NA_real_,
      note = "Latest close from FX_DAILY (free tier; not realtime)",
      stringsAsFactors = FALSE
    )
  })
}

av_commodity <- function(commodity = "WHEAT", interval = "monthly") {
  out <- av_get(list(`function` = commodity, interval = interval))
  nm <- names(out)
  ts_key <- nm[grepl("data|time|series", nm, ignore.case = TRUE)][1]
  if (is.null(ts_key)) ts_key <- nm[!nm %in% c("name", "interval", "unit")][1]
  ts <- out[[ts_key]]
  if (is.null(ts)) return(data.frame())
  if (is.data.frame(ts)) {
    if ("date" %in% names(ts)) ts$date <- as.Date(ts$date)
    if ("value" %in% names(ts)) ts$value <- as.numeric(ts$value)
    return(ts)
  }
  if (is.list(ts) && !is.data.frame(ts) && length(ts) > 0) {
    df <- do.call(rbind, lapply(ts, function(x) data.frame(date = as.character(x$date %||% x$Date %||% NA), value = as.numeric(x$value %||% x$Value %||% NA), stringsAsFactors = FALSE)))
    df$date <- as.Date(df$date)
    return(df[order(df$date, decreasing = TRUE), ])
  }
  df <- data.frame(date = as.Date(rownames(as.data.frame(ts))), value = as.numeric(unlist(as.data.frame(ts)[, 1])), stringsAsFactors = FALSE)
  df[order(df$date, decreasing = TRUE), ]
}

av_economic <- function(indicator = "CPI", interval = "monthly", ...) {
  params <- list(`function` = indicator, ...)
  if (indicator %in% c("CPI", "REAL_GDP", "TREASURY_YIELD", "FEDERAL_FUNDS_RATE") && nzchar(interval)) params$interval <- interval
  out <- av_get(params)
  nm <- names(out)
  ts_key <- nm[grepl("data|time|series", nm, ignore.case = TRUE)][1]
  if (is.null(ts_key)) ts_key <- nm[!nm %in% c("name", "interval", "unit", "information")][1]
  ts <- out[[ts_key]]
  if (is.null(ts)) return(data.frame())
  if (is.data.frame(ts)) {
    if ("date" %in% names(ts)) ts$date <- as.Date(ts$date)
    return(ts)
  }
  if (is.list(ts) && !is.data.frame(ts) && length(ts) > 0) {
    df <- do.call(rbind, lapply(ts, function(x) data.frame(date = as.character(x$date %||% x$Date %||% NA), value = as.numeric(x$value %||% x$Value %||% NA), stringsAsFactors = FALSE)))
    df$date <- as.Date(df$date)
    return(df[order(df$date, decreasing = TRUE), ])
  }
  df <- data.frame(date = as.Date(rownames(as.data.frame(ts))), value = as.numeric(unlist(as.data.frame(ts)[, 1])), stringsAsFactors = FALSE)
  df[order(df$date, decreasing = TRUE), ]
}

ui <- fluidPage(
  titlePanel("Daily Market Analysis"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "section",
        "Select section",
        choices = c(
          "Stock Daily" = "stock",
          "Top Gainers/Losers" = "gainers",
          "News & Sentiment" = "news",
          "Forex" = "forex",
          "Currency Exchange Rate" = "currency_rate",
          "Commodities" = "commodity",
          "Economic Indicators" = "economic"
        ),
        selected = "stock"
      ),
      conditionalPanel(
        "input.section == 'stock'",
        textInput("stock_symbol", "Symbol", value = "AAPL"),
        actionButton("fetch_stock", "Fetch daily", class = "btn-primary")
      ),
      conditionalPanel(
        "input.section == 'gainers'",
        actionButton("fetch_gainers", "Fetch movers", class = "btn-primary")
      ),
      conditionalPanel(
        "input.section == 'news'",
        textInput("news_tickers", "Tickers (optional, comma-separated)", placeholder = "AAPL, MSFT"),
        numericInput("news_limit", "Limit", value = 20, min = 1, max = 50),
        actionButton("fetch_news", "Fetch news", class = "btn-primary")
      ),
      conditionalPanel(
        "input.section == 'forex'",
        textInput("fx_from", "From", value = "EUR"),
        textInput("fx_to", "To", value = "USD"),
        actionButton("fetch_fx", "Fetch rates", class = "btn-primary")
      ),
      conditionalPanel(
        "input.section == 'currency_rate'",
        textInput("curr_from", "From currency", value = "USD", placeholder = "USD, EUR, GBP, ..."),
        textInput("curr_to", "To currency", value = "EUR", placeholder = "EUR, JPY, GBP (crypto needs premium key)"),
        actionButton("fetch_currency_rate", "Get rate", class = "btn-primary")
      ),
      conditionalPanel(
        "input.section == 'commodity'",
        selectInput("commodity", "Commodity",
                    choices = c("WHEAT" = "WHEAT", "CORN" = "CORN", "WTI" = "WTI", "BRENT" = "BRENT",
                                "NATURAL_GAS" = "NATURAL_GAS", "COPPER" = "COPPER", "COFFEE" = "COFFEE")),
        selectInput("commodity_interval", "Interval", choices = c("daily", "weekly", "monthly"), selected = "monthly"),
        actionButton("fetch_commodity", "Fetch data", class = "btn-primary")
      ),
      conditionalPanel(
        "input.section == 'economic'",
        selectInput("economic_indicator", "Indicator",
                    choices = c("CPI" = "CPI", "INFLATION" = "INFLATION", "UNEMPLOYMENT" = "UNEMPLOYMENT",
                                "FEDERAL_FUNDS_RATE" = "FEDERAL_FUNDS_RATE", "TREASURY_YIELD" = "TREASURY_YIELD",
                                "REAL_GDP" = "REAL_GDP", "RETAIL_SALES" = "RETAIL_SALES", "NONFARM_PAYROLL" = "NONFARM_PAYROLL")),
        selectInput("economic_interval", "Interval", choices = c("monthly", "quarterly", "annual", "daily"), selected = "monthly"),
        actionButton("fetch_economic", "Fetch data", class = "btn-primary")
      )
    ),
    mainPanel(
      width = 9,
      uiOutput("section_title"),
      p("Select a section above and click the Fetch button to load data.", style = "color: #666; margin-bottom: 0.25em;"),
      p("Free API key: 5 requests/min, ~25/day. If you see no data or an error, wait a minute or try again tomorrow.", style = "color: #999; font-size: 0.85em; margin-bottom: 1em;"),
      verbatimTextOutput("api_error"),
      conditionalPanel("input.section == 'stock'", plotOutput("plot_stock"), DT::dataTableOutput("table_stock")),
      conditionalPanel(
        "input.section == 'gainers'",
        uiOutput("ui_gainers_msg"),
        h4("Top Gainers"),
        DT::dataTableOutput("table_gainers"),
        h4("Top Losers"),
        DT::dataTableOutput("table_losers"),
        h4("Most Actively Traded"),
        DT::dataTableOutput("table_active")
      ),
      conditionalPanel("input.section == 'news'", DT::dataTableOutput("table_news")),
      conditionalPanel("input.section == 'forex'", plotOutput("plot_fx"), DT::dataTableOutput("table_fx")),
      conditionalPanel("input.section == 'currency_rate'", uiOutput("ui_currency_rate"), DT::dataTableOutput("table_currency_rate")),
      conditionalPanel("input.section == 'commodity'", plotOutput("plot_commodity"), DT::dataTableOutput("table_commodity")),
      conditionalPanel("input.section == 'economic'", plotOutput("plot_economic"), DT::dataTableOutput("table_economic"))
    )
  )
)

server <- function(input, output, session) {
  err_msg <- reactiveVal(NULL)
  stock_df <- reactiveVal(NULL)
  gainers_data <- reactiveVal(NULL)
  news_df <- reactiveVal(NULL)
  fx_df <- reactiveVal(NULL)
  currency_rate_df <- reactiveVal(NULL)
  commodity_df <- reactiveVal(NULL)
  economic_df <- reactiveVal(NULL)

  clear_err <- function() err_msg(NULL)
  set_err <- function(e) err_msg(paste0("Error: ", conditionMessage(e)))

  output$section_title <- renderUI({
    titles <- c(stock = "Stock Daily", gainers = "Top Gainers/Losers", news = "News & Sentiment",
                forex = "Forex", currency_rate = "Currency Exchange Rate", commodity = "Commodities", economic = "Economic Indicators")
    tit <- titles[input$section]
    if (is.null(tit) || length(tit) == 0L || (length(tit) == 1L && is.na(tit))) tit <- input$section
    h3(tit)
  })
  output$api_error <- renderPrint({
    err <- err_msg()
    if (!is.null(err) && length(err) > 0L && nzchar(err)) err else invisible(NULL)
  })

  # ---- Stock Daily ----
  observeEvent(input$fetch_stock, {
    clear_err()
    tryCatch({
      sym <- trimws(input$stock_symbol %||% "")
      if (length(sym) == 0L || !nzchar(sym)) { set_err(simpleError("Please enter a symbol.")); return() }
      stock_df(av_stock_daily(sym))
    }, error = set_err)
  })
  output$plot_stock <- renderPlot({
    df <- stock_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df <- head(df, 60)
    df <- df[!is.na(df$date) & is.finite(as.numeric(df$close)), ]
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = date, y = close)) +
      geom_line(color = "steelblue", linewidth = 0.8) +
      geom_point(color = "steelblue", size = 1) +
      labs(x = "Date", y = "Close", title = paste("Daily Close (last", nrow(df), "days)")) +
      theme_minimal()
  })
  output$table_stock <- renderDT({
    df <- stock_df()
    if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    DT::datatable(as.data.frame(df), options = list(pageLength = 15))
  })

  # ---- Top Gainers / Losers ----
  observeEvent(input$fetch_gainers, {
    clear_err()
    tryCatch({
      gainers_data(av_top_gainers_losers())
    }, error = set_err)
  })
  output$ui_gainers_msg <- renderUI({
    d <- gainers_data()
    if (is.null(d)) return(p("Click 'Fetch movers' to load data."))
    NULL
  })
  output$table_gainers <- renderDT({
    d <- gainers_data()
    if (is.null(d) || !is.data.frame(d$top_gainers) || nrow(d$top_gainers) == 0)
      return(DT::datatable(data.frame(), options = list(pageLength = 10)))
    DT::datatable(as.data.frame(d$top_gainers), options = list(pageLength = 10))
  })
  output$table_losers <- renderDT({
    d <- gainers_data()
    if (is.null(d) || !is.data.frame(d$top_losers) || nrow(d$top_losers) == 0)
      return(DT::datatable(data.frame(), options = list(pageLength = 10)))
    DT::datatable(as.data.frame(d$top_losers), options = list(pageLength = 10))
  })
  output$table_active <- renderDT({
    d <- gainers_data()
    if (is.null(d) || !is.data.frame(d$top_activated) || nrow(d$top_activated) == 0)
      return(DT::datatable(data.frame(), options = list(pageLength = 10)))
    DT::datatable(as.data.frame(d$top_activated), options = list(pageLength = 10))
  })

  # ---- News ----
  observeEvent(input$fetch_news, {
    clear_err()
    tryCatch({
      tickers <- trimws(input$news_tickers)
      news_df(av_news_sentiment(tickers = tickers, limit = as.integer(input$news_limit)))
    }, error = set_err)
  })
  output$table_news <- renderDT({
    df <- news_df()
    if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    cols <- intersect(c("title", "time_published", "overall_sentiment_label", "source", "url"), names(df))
    if (length(cols) == 0) cols <- names(df)
    DT::datatable(as.data.frame(df)[, cols], options = list(pageLength = 15), escape = FALSE)
  })

  # ---- Forex ----
  observeEvent(input$fetch_fx, {
    clear_err()
    tryCatch({
      from <- trimws(input$fx_from %||% "")
      to <- trimws(input$fx_to %||% "")
      if (length(from) == 0L || !nzchar(from) || length(to) == 0L || !nzchar(to)) { set_err(simpleError("Please enter From and To currencies.")); return() }
      fx_df(av_fx_daily(from_symbol = from, to_symbol = to))
    }, error = set_err)
  })
  output$plot_fx <- renderPlot({
    df <- fx_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df <- head(df, 90)
    df <- df[!is.na(df$date) & is.finite(as.numeric(df$close)), ]
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = date, y = close)) +
      geom_line(color = "darkgreen", linewidth = 0.8) +
      labs(x = "Date", y = "Close", title = paste(input$fx_from, "/", input$fx_to)) +
      theme_minimal()
  })
  output$table_fx <- renderDT({
    df <- fx_df()
    if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    DT::datatable(as.data.frame(df), options = list(pageLength = 15))
  })

  # ---- Currency Exchange Rate (realtime: crypto or physical) ----
  observeEvent(input$fetch_currency_rate, {
    clear_err()
    tryCatch({
      from <- trimws(input$curr_from %||% "")
      to <- trimws(input$curr_to %||% "")
      if (length(from) == 0L || !nzchar(from) || length(to) == 0L || !nzchar(to)) { set_err(simpleError("Please enter From and To currency codes (e.g. USD, BTC).")); return() }
      currency_rate_df(av_currency_exchange_rate(from_currency = from, to_currency = to))
    }, error = set_err)
  })
  output$ui_currency_rate <- renderUI({
    df <- currency_rate_df()
    if (is.null(df) || nrow(df) == 0) return(p("Click 'Get rate' to fetch the realtime exchange rate."))
    r <- df[1, ]
    rate <- if (is.numeric(r$exchange_rate)) format(r$exchange_rate, digits = 8, scientific = FALSE) else as.character(r$exchange_rate)
    tagList(
      div(style = "background: #f0f4f8; padding: 1rem; border-radius: 8px; margin-bottom: 1rem;",
        p(style = "margin: 0 0 0.25rem 0; font-size: 0.9rem; color: #555;", paste(r$from_name %||% r$from_code, "to", r$to_name %||% r$to_code)),
        p(style = "margin: 0; font-size: 1.5rem; font-weight: bold;", paste0("1 ", r$from_code, " = ", rate, " ", r$to_code)),
        if (length(lst <- (r$last_refreshed %||% "")) > 0L && nzchar(lst)) p(style = "margin: 0.5rem 0 0 0; font-size: 0.8rem; color: #666;", paste("Last refreshed:", r$last_refreshed)),
        if (length(note <- (r$note %||% "")) > 0L && nzchar(note)) p(style = "margin: 0.5rem 0 0 0; font-size: 0.8rem; color: #888; font-style: italic;", note)
      )
    )
  })
  output$table_currency_rate <- renderDT({
    df <- currency_rate_df()
    if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 10)))
    DT::datatable(as.data.frame(df), options = list(pageLength = 10))
  })

  # ---- Commodity ----
  observeEvent(input$fetch_commodity, {
    clear_err()
    tryCatch({
      commodity_df(av_commodity(commodity = input$commodity, interval = input$commodity_interval))
    }, error = set_err)
  })
  output$plot_commodity <- renderPlot({
    df <- commodity_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df <- head(df, 60)
    xcol <- if ("date" %in% names(df)) "date" else names(df)[1]
    ycol <- if ("value" %in% names(df)) "value" else names(df)[2]
    df[[ycol]] <- as.numeric(df[[ycol]])
    df <- df[!is.na(df[[xcol]]) & is.finite(df[[ycol]]), ]
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = .data[[xcol]], y = .data[[ycol]])) +
      geom_line(color = "brown", linewidth = 0.8) +
      labs(x = xcol, y = ycol, title = paste(input$commodity, input$commodity_interval)) +
      theme_minimal()
  })
  output$table_commodity <- renderDT({
    df <- commodity_df()
    if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    DT::datatable(as.data.frame(df), options = list(pageLength = 15))
  })

  # ---- Economic ----
  observeEvent(input$fetch_economic, {
    clear_err()
    tryCatch({
      ind <- input$economic_indicator
      int <- input$economic_interval
      if (ind == "TREASURY_YIELD") {
        economic_df(av_economic(indicator = ind, interval = int, maturity = "10year"))
      } else if (ind == "REAL_GDP") {
        economic_df(av_economic(indicator = ind, interval = if (int %in% c("quarterly", "annual")) int else "annual"))
      } else {
        economic_df(av_economic(indicator = ind, interval = int))
      }
    }, error = set_err)
  })
  output$plot_economic <- renderPlot({
    df <- economic_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df <- head(df, 60)
    xcol <- if ("date" %in% names(df)) "date" else names(df)[1]
    ycol <- if ("value" %in% names(df)) "value" else names(df)[2]
    df[[ycol]] <- as.numeric(df[[ycol]])
    df <- df[!is.na(df[[xcol]]) & is.finite(df[[ycol]]), ]
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = .data[[xcol]], y = .data[[ycol]])) +
      geom_line(color = "purple", linewidth = 0.8) +
      labs(x = xcol, y = ycol, title = paste(input$economic_indicator, input$economic_interval)) +
      theme_minimal()
  })
  output$table_economic <- renderDT({
    df <- economic_df()
    if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame(), options = list(pageLength = 15)))
    DT::datatable(as.data.frame(df), options = list(pageLength = 15))
  })
}

shinyApp(ui, server)
