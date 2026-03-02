# Fetch NY county data: unemployment, income, crime.
# Two modes (no API key required for "open data" mode):
#   - With Census API key: multi-year 2015-2022 from Census ACS (tidycensus)
#   - Without key: single year from BLS + Census SAIPE + data.ny.gov (direct file downloads only)
#
# Open data sources (no key): BLS LAUS, Census SAIPE XLS, Census TIGER, data.ny.gov crime
# For SAIPE Excel parsing install: install.packages("readxl")

library(sf)
library(dplyr)
library(readr)
library(tidyr)

data_dir <- if (dir.exists("data")) "data" else file.path("..", "data")
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

# --- TIGER: NY county boundaries (always; no API) ---
tiger_zip <- NULL
tiger_remove_after <- FALSE
local_zips <- list.files(data_dir, pattern = "county\\.zip$", full.names = TRUE, ignore.case = TRUE)
for (z in local_zips) {
  if (file.exists(z) && file.info(z)$size > 1000) {
    tiger_zip <- normalizePath(z)
    message("Using local TIGER file: ", basename(tiger_zip))
    break
  }
}
if (is.null(tiger_zip)) {
  tiger_urls <- c(
    "https://www2.census.gov/geo/tiger/TIGER2023/COUNTY/tl_2023_36_county.zip",
    "https://www2.census.gov/geo/tiger/TIGER2024/COUNTY/tl_2024_36_county.zip",
    "https://www2.census.gov/geo/tiger/TIGER2023/COUNTY/tl_2023_us_county.zip"
  )
  tiger_zip <- file.path(data_dir, "tiger_county.zip")
  message("Downloading NY county boundaries (Census TIGER)...")
  for (tiger_url in tiger_urls) {
    tryCatch({
      suppressWarnings(download.file(tiger_url, tiger_zip, quiet = TRUE, mode = "wb"))
      if (file.exists(tiger_zip) && file.info(tiger_zip)$size > 1000) break
    }, error = function(e) NULL)
  }
  if (!file.exists(tiger_zip) || file.info(tiger_zip)$size < 1000) {
    stop("TIGER failed. Put a TIGER county zip in the 'data' folder.")
  }
  tiger_remove_after <- TRUE
}

tmp_dir <- file.path(data_dir, "tiger_tmp")
if (!dir.exists(tmp_dir)) dir.create(tmp_dir)
unzip(tiger_zip, exdir = tmp_dir, overwrite = TRUE)
shp_file <- list.files(tmp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
if (is.na(shp_file)) stop("No .shp in zip.")
us_geo <- st_read(shp_file, quiet = TRUE)
unlink(tmp_dir, recursive = TRUE)
if (tiger_remove_after && file.exists(tiger_zip)) file.remove(tiger_zip)

if ("STATEFP" %in% names(us_geo)) {
  ny_geo <- us_geo %>% filter(STATEFP == "36")
} else if (nrow(us_geo) <= 100) {
  ny_geo <- us_geo
} else {
  ny_geo <- us_geo %>% filter(substr(as.character(GEOID), 1, 2) == "36")
}
ny_geo <- ny_geo %>%
  mutate(
    county_name = gsub(" County$", "", NAME),
    GEOID = as.character(GEOID)
  ) %>%
  select(GEOID, NAME, county_name, geometry)

# --- Check Census API key (optional) ---
has_census_key <- FALSE
if (requireNamespace("tidycensus", quietly = TRUE)) {
  tryCatch({
    tidycensus::get_acs(geography = "county", state = "NY", variables = "B01003_001", year = 2022, survey = "acs5")
    has_census_key <- TRUE
  }, error = function(e) NULL)
}

if (has_census_key) {
  # ========== Mode 1: Census API (multi-year 2015-2022) ==========
  message("Using Census API (multi-year 2015-2022).")
  years <- 2015:2022
  census_vars <- c("B19013_001", "B23025_002", "B23025_004", "B01003_001")
  all_acs <- list()
  for (yr in years) {
    tryCatch({
      acs <- tidycensus::get_acs(geography = "county", variables = census_vars, state = "NY", year = yr, survey = "acs5", geometry = FALSE)
      acs$year <- yr
      all_acs[[as.character(yr)]] <- acs
      message("  ", yr, " OK")
    }, error = function(e) message("  ", yr, " skipped"))
  }
  if (length(all_acs) == 0) stop("No Census data. Check API key.")
  acs_wide <- bind_rows(all_acs) %>%
    select(GEOID, NAME, year, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate)
  ny_census <- acs_wide %>%
    mutate(
      county_name = gsub(", New York", "", NAME),
      unemployment_rate = if_else(B23025_002 > 0, 100 * B23025_004 / B23025_002, NA_real_),
      median_household_income = B19013_001,
      population = B01003_001
    ) %>%
    select(GEOID, NAME, county_name, year, median_household_income, unemployment_rate, population)
} else {
  # ========== Mode 2: Open data only (no API key) ==========
  message("No Census API key — using open data only (BLS, SAIPE, data.ny.gov).")

  # Single year for open-data mode (crime data often has lag; 2022 usually available)
  open_data_year <- 2022
  ny_census <- ny_geo %>%
    st_drop_geometry() %>%
    mutate(year = open_data_year)

  # 2a. BLS LAUS — unemployment (direct download, no API)
  bls_url <- "https://www.bls.gov/web/metro/laucntycur14.txt"
  bls_file <- file.path(data_dir, "laucntycur14.txt")
  message("Downloading unemployment (BLS LAUS)...")
  tryCatch({
    download.file(bls_url, bls_file, quiet = TRUE, mode = "wb")
  }, error = function(e) message("BLS download failed: ", e$message))
  bls <- NULL
  if (file.exists(bls_file) && file.info(bls_file)$size > 100) {
    tryCatch({
      bls_raw <- read.delim(bls_file, skip = 6, stringsAsFactors = FALSE, check.names = FALSE, sep = "\t", fill = TRUE)
      if (nrow(bls_raw) == 0) bls_raw <- read.table(bls_file, skip = 6, sep = "\t", fill = TRUE, stringsAsFactors = FALSE)
      cn <- tolower(names(bls_raw))
      ncn <- length(cn)
      # Typical: State FIPS (col 2), County FIPS (col 3), Unemployed Rate (last or named)
      st_idx <- which(grepl("state|fips", cn))[1]
      if (is.na(st_idx)) st_idx <- 2
      cty_idx <- which(grepl("county|fips", cn))
      cty_idx <- cty_idx[cty_idx != st_idx][1]
      if (is.na(cty_idx)) cty_idx <- 3
      rate_idx <- which(grepl("unemploy.*rate|rate", cn))[1]
      if (is.na(rate_idx)) rate_idx <- ncn
      st_vec <- as.character(bls_raw[[st_idx]])
      bls <- bls_raw %>%
        filter(st_vec == "36" | substr(st_vec, 1, 2) == "36") %>%
        mutate(
          GEOID = paste0("36", sprintf("%03d", suppressWarnings(as.integer(!!sym(names(bls_raw)[cty_idx]))))),
          unemployment_rate = as.numeric(gsub("[^0-9.]", "", as.character(!!sym(names(bls_raw)[rate_idx]))))
        ) %>%
        select(GEOID, unemployment_rate)
      if (nrow(bls) > 0) bls <- distinct(bls, GEOID, .keep_all = TRUE)
    }, error = function(e) message("BLS parse failed: ", e$message))
    file.remove(bls_file)
  }
  if (!is.null(bls)) {
    ny_census <- ny_census %>% select(-any_of("unemployment_rate")) %>% left_join(bls, by = "GEOID")
  } else {
    ny_census <- ny_census %>% mutate(unemployment_rate = NA_real_)
  }

  # 2b. Census SAIPE — median household income (direct XLS, no API)
  if (requireNamespace("readxl", quietly = TRUE)) {
    saipe_url <- "https://www2.census.gov/programs-surveys/saipe/datasets/2022/2022-state-and-county/est22all.xls"
    saipe_file <- file.path(data_dir, "est22all.xls")
    message("Downloading income (Census SAIPE)...")
    tryCatch({
      download.file(saipe_url, saipe_file, quiet = TRUE, mode = "wb")
    }, error = function(e) message("SAIPE download failed: ", e$message))
    if (file.exists(saipe_file) && file.info(saipe_file)$size > 100) {
      tryCatch({
        sx <- readxl::read_xls(saipe_file, sheet = 1)
        nms <- tolower(names(sx))
        st_col <- which(grepl("state|fips", nms))[1]
        if (is.na(st_col)) st_col <- 1
        cty_col <- setdiff(which(grepl("county|fips", nms)), st_col)[1]
        if (is.na(cty_col)) cty_col <- 2
        inc_col <- which(grepl("median|income|household", nms))[1]
        if (is.na(inc_col)) inc_col <- max(which(grepl("inc|dollar", nms))[1], 10, na.rm = TRUE)
        st_vals <- suppressWarnings(as.integer(sx[[names(sx)[st_col]]]))
        if (all(is.na(st_vals))) st_vals <- as.integer(gsub("[^0-9]", "", as.character(sx[[names(sx)[st_col]]])))
        saipe <- sx %>%
          filter(!is.na(st_vals) & st_vals == 36) %>%
          mutate(
            GEOID = paste0("36", sprintf("%03d", suppressWarnings(as.integer(!!sym(names(sx)[cty_col]))))),
            median_household_income = as.numeric(!!sym(names(sx)[inc_col]))
          ) %>%
          select(GEOID, median_household_income) %>%
          filter(GEOID != "36NA" & !is.na(median_household_income))
        if (nrow(saipe) > 0) {
          ny_census <- ny_census %>% select(-any_of("median_household_income")) %>% left_join(saipe, by = "GEOID")
        }
      }, error = function(e) message("SAIPE parse failed: ", e$message))
      file.remove(saipe_file)
    }
  }
  if (!"median_household_income" %in% names(ny_census)) ny_census <- ny_census %>% mutate(median_household_income = NA_real_)

  # 2c. Population (for crime rate) — try Census pop estimates CSV
  pop_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/totals/co-est2022-alldata.csv"
  pop_file <- file.path(data_dir, "co-est2022-alldata.csv")
  message("Downloading population (Census)...")
  tryCatch({
    download.file(pop_url, pop_file, quiet = TRUE, mode = "wb")
  }, error = function(e) message("Population download failed: ", e$message))
  pop_df <- NULL
  if (file.exists(pop_file) && file.info(pop_file)$size > 1000) {
    tryCatch({
      pr <- read_csv(pop_file, show_col_types = FALSE, n_max = 5)
      nc <- tolower(names(pr))
      sumlev <- which(grepl("sumlev", nc))[1]
      st <- which(grepl("^state$|state ", nc))[1]
      ct <- which(grepl("^county$|county ", nc))[1]
      popc <- which(grepl("popestimate2022|pop2022|popest", nc))[1]
      if (!is.na(sumlev) && !is.na(st)) {
        pop_full <- read_csv(pop_file, show_col_types = FALSE)
        pop_df <- pop_full %>%
          filter(!!sym(names(pop_full)[sumlev]) == "50", as.character(!!sym(names(pop_full)[st])) == "36") %>%
          mutate(
            GEOID = paste0(sprintf("%02d", as.integer(!!sym(names(pop_full)[st]))), sprintf("%03d", as.integer(!!sym(names(pop_full)[ct])))),
            population = as.numeric(!!sym(names(pop_full)[popc]))
          ) %>%
          select(GEOID, population)
      }
    }, error = function(e) message("Population parse failed: ", e$message))
    file.remove(pop_file)
  }
  if (!is.null(pop_df)) {
    ny_census <- ny_census %>% select(-any_of("population")) %>% left_join(pop_df, by = "GEOID")
  } else {
    ny_census <- ny_census %>% mutate(population = NA_real_)
  }
}

# --- Crime: data.ny.gov (no API key) ---
crime_url <- "https://data.ny.gov/api/views/55zc-sp6m/rows.csv?accessType=DOWNLOAD"
crime_file <- file.path(data_dir, "index_crimes_ny.csv")
message("Downloading crime (data.ny.gov)...")
tryCatch({
  download.file(crime_url, crime_file, quiet = TRUE, mode = "wb")
}, error = function(e) message("Crime download failed: ", e$message))

crime_by_year <- NULL
if (file.exists(crime_file)) {
  crimes <- read_csv(crime_file, show_col_types = FALSE)
  cn <- tolower(names(crimes))
  county_col <- which(grepl("county", cn))[1]
  year_col <- which(grepl("year", cn))[1]
  total_col <- which(grepl("index|total|count", cn))[1]
  if (is.na(county_col)) county_col <- 1
  if (is.na(year_col)) year_col <- 2
  if (is.na(total_col)) total_col <- ncol(crimes)
  county_name_cr <- names(crimes)[county_col]
  year_name <- names(crimes)[year_col]
  total_name <- names(crimes)[total_col]
  crimes[[year_name]] <- as.integer(crimes[[year_name]])
  crime_by_year <- crimes %>%
    group_by(!!sym(county_name_cr), !!sym(year_name)) %>%
    summarise(total_index_crimes = sum(as.numeric(!!sym(total_name)), na.rm = TRUE), .groups = "drop") %>%
    mutate(county_key = tolower(gsub(" County$", "", !!sym(county_name_cr)))) %>%
    rename(year = !!sym(year_name)) %>%
    select(year, county_key, total_index_crimes)
  file.remove(crime_file)
}

# Join crime to census
ny_census <- ny_census %>%
  mutate(county_key = tolower(gsub(" County$", "", county_name)))
if (!is.null(crime_by_year)) {
  ny_census <- ny_census %>%
    left_join(crime_by_year, by = c("year", "county_key")) %>%
    select(-county_key)
} else {
  ny_census <- ny_census %>% select(-county_key) %>% mutate(total_index_crimes = NA_real_)
}
ny_census <- ny_census %>%
  mutate(
    crime_rate_per_100k = if_else(!is.na(population) & population > 0, 1e5 * total_index_crimes / population, NA_real_)
  )
if (!"total_index_crimes" %in% names(ny_census)) ny_census$total_index_crimes <- NA_real_
if (!"crime_rate_per_100k" %in% names(ny_census)) ny_census$crime_rate_per_100k <- NA_real_
if ("county_key" %in% names(ny_census)) ny_census <- ny_census %>% select(-county_key)

# --- Save ---
latest_year <- max(ny_census$year, na.rm = TRUE)
ny_latest <- ny_census %>% filter(year == latest_year)

# Long format (for app year selector when multi-year)
out_csv_long <- file.path(data_dir, "ny_counties_long.csv")
ny_export_long <- ny_census %>%
  select(GEOID, NAME, county_name, year, median_household_income, unemployment_rate, population, total_index_crimes, crime_rate_per_100k)
write_csv(ny_export_long, out_csv_long)
message("Wrote ", out_csv_long)

# Wide (latest year) + GeoJSON
ny_geo_join <- ny_geo %>%
  left_join(ny_latest %>% select(GEOID, median_household_income, unemployment_rate, population, total_index_crimes, crime_rate_per_100k), by = "GEOID")

out_csv <- file.path(data_dir, "ny_counties.csv")
ny_export <- ny_geo_join %>%
  st_drop_geometry() %>%
  select(GEOID, NAME, county_name, median_household_income, unemployment_rate, population, total_index_crimes, crime_rate_per_100k)
write_csv(ny_export, out_csv)
message("Wrote ", out_csv, " (year ", latest_year, ")")

out_geo <- file.path(data_dir, "ny_counties_geo.json")
ny_geo_wgs84 <- st_transform(ny_geo_join, 4326)
sf::st_write(ny_geo_wgs84, out_geo, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
message("Wrote ", out_geo)

message("Done. No API key used for open-data mode. Run app: setwd('tool 1'); shiny::runApp('.')")
