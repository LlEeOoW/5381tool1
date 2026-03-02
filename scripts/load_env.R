# Load .env file into Sys.env (no external package required)
# Call once at startup, e.g. in app_market.R or fetch_alphavantage.R
load_env <- function(path = ".env") {
  env_path <- path
  if (!file.exists(env_path)) {
    # Try relative to project root / script location
    env_path <- file.path("..", "..", path)
    if (!file.exists(env_path)) env_path <- file.path("..", path)
  }
  if (!file.exists(env_path)) return(invisible(NULL))
  lines <- readLines(env_path, warn = FALSE)
  for (line in lines) {
    line <- trimws(line)
    if (nzchar(line) && !startsWith(line, "#") && grepl("=", line, fixed = TRUE)) {
      idx <- regexpr("=", line, fixed = TRUE)[1L]
      key <- trimws(substring(line, 1L, idx - 1L))
      val <- trimws(substring(line, idx + 1L, nchar(line)))
      val <- sub("^['\"]|['\"]$", "", val)
      if (nzchar(key)) Sys.setenv(setNames(val, key))
    }
  }
  invisible(NULL)
}

get_av_key <- function() {
  key <- Sys.getenv("ALPHAVANTAGE_API_KEY")
  if (!nzchar(key)) load_env() 
  Sys.getenv("ALPHAVANTAGE_API_KEY")
}
