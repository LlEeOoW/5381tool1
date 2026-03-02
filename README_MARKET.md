# Daily Market Analysis Tool (Alpha Vantage)

A Shiny app for daily market analysis, separate from the Crime Analysis project. Data is fetched from the Alpha Vantage API.

## Files

- **`.env`** – Store API keys here (do not commit; in `.gitignore`): `ALPHAVANTAGE_API_KEY` (required for market data), `OLLAMA_CLOUD_API_KEY` or `OPENAI_API_KEY` (optional, for AI Reporter only).
- **`scripts/load_env.R`** – Loads environment variables from `.env`.
- **`scripts/fetch_alphavantage.R`** – Optional; API logic is inlined in `app_market.R` (stock daily, gainers/losers, news, forex, commodities, economic indicators).
- **`app_market.R`** – Main Shiny app file.
- **`run_market.R`** – Launcher script: sets working directory and runs the app (recommended).

## Dependencies

Install in R:

```r
install.packages(c("shiny", "httr2", "jsonlite", "ggplot2", "DT", "dplyr"))
```

If `httr2` is not installed, the app falls back to `httr`.

## How to run

**Option 1 (recommended)** – From R or RStudio, with working directory set to the folder that contains `app_market.R` and `.env`:

```r
setwd("C:/Users/10543/OneDrive/Desktop/cornell_courses/5831/tool 1")  # adjust path as needed
source("run_market.R")
```

**Option 2** – Run the app file directly:

```r
setwd("C:/Users/10543/OneDrive/Desktop/cornell_courses/5831/tool 1")
shiny::runApp("app_market.R", launch.browser = TRUE)
```

**Option 3** – From the command line (from the project directory):

```bash
R -e "setwd('tool 1'); source('run_market.R')"
```

## Sections

| Section                  | API / Description                                      |
|--------------------------|--------------------------------------------------------|
| Stock Daily              | `TIME_SERIES_DAILY` – enter a symbol (e.g. IBM, AAPL)  |
| Top Gainers/Losers       | `TOP_GAINERS_LOSERS` – gainers, losers, most active    |
| News & Sentiment         | `NEWS_SENTIMENT` – optional ticker filter              |
| Forex                    | `FX_DAILY` – choose From/To currency pair              |
| Currency Exchange Rate   | Realtime rate (premium) or latest from FX_DAILY (free) |
| Commodities              | WHEAT, CORN, WTI, BRENT, etc. – choose interval        |
| Economic Indicators      | CPI, UNEMPLOYMENT, TREASURY_YIELD, etc.                 |
| AI Reporter              | Optional. Set `OLLAMA_CLOUD_API_KEY` or `OPENAI_API_KEY` in `.env` to generate AI market summaries from your data. |

Select a section and click **Fetch** to load data. View, filter, and Download use already-loaded data (no extra API calls). Use **AI Reporter** for narrative insights.

## API limits (free key)

- **5 requests per minute**
- **~25 requests per day**

If you see no data or a rate-limit error, wait a minute or try again the next day. The app displays a short reminder and clearer error messages when limits are hit.

## AI Reporter (optional)

The AI Reporter uses a separate key from Alpha Vantage and runs on **Ollama Cloud**.

- Get a key from [ollama.com/settings](https://ollama.com/settings). 
- Add to `.env`: `OLLAMA_CLOUD_API_KEY=your-key`.
