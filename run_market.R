# Run this file to start the Daily Market app (same pattern as 06_alphavantage_ai_report.R).
# In RStudio: open this file, then click "Source".
# In R console: setwd("path/to/tool 1"); source("run_market.R")

# Set working directory so .env and app_market.R are found (readRenviron(".env") looks in getwd())
if (file.exists("app_market.R") && file.exists(".env")) {
  # Already in app folder (tool 1)
} else if (file.exists("tool 1/app_market.R") && file.exists("tool 1/.env")) {
  setwd("tool 1")
} else if (file.exists(file.path("..", "app_market.R")) && file.exists(file.path("..", ".env"))) {
  setwd("..")
} else {
  app_path <- "C:/Users/10543/OneDrive/Desktop/cornell_courses/5831/tool 1"
  if (dir.exists(app_path)) setwd(app_path)
}

if (!file.exists("app_market.R")) stop("app_market.R not found. Set wd to the folder that contains app_market.R and .env")
if (!file.exists(".env")) message("Warning: .env not found. Create .env with ALPHAVANTAGE_API_KEY=yourkey")

message("Starting Daily Market app...")
shiny::runApp("app_market.R", launch.browser = TRUE)
