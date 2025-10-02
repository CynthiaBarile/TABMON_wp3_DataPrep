###########################################################
# Script to download any given audio file from NINA server
# Run with source("fullpath/download_audiofile.R") and provide necessary input
###########################################################

# Finding Desktop path for different OS
get_desktop <- function() {
  sysname <- Sys.info()["sysname"]
  if (sysname == "Windows") {
    # Use environment variable to get the real Desktop
    desktop <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
  } else {
    # Mac or Linux
    desktop <- file.path(Sys.getenv("HOME"), "Desktop")
  }
  if (!dir.exists(desktop)) {
    stop("❌ Could not find Desktop folder!")
  }
  return(desktop)
}

# Country codes
get_country_folder <- function(country) {
  country <- tolower(trimws(country))
  switch(country,
         "norway"      = "proj_tabmon_NINA",
         "spain"       = "proj_tabmon_NINA_ES",
         "france"      = "proj_tabmon_NINA_FR",
         "netherlands" = "proj_tabmon_NINA_NL",
         stop("❌ Unknown country. Please type: Norway, Spain, France or Netherlands.")
  )
}

# Build the full URL from country folder, device ID and filename
build_url <- function(country_folder, device_id, file_name) {
  base <- "https://tabmon.nina.no/data"
  paste0(
    base, "/", country_folder,
    "/bugg_RPiID-10000000", device_id,
    "/conf_20250218_TABMON/",
    file_name, ".mp3"
  )
}

# Download function
download_audio <- function(url,
                           username = "guest",
                           password = "ninaguest") {
  desktop <- get_desktop()
  dest_file <- file.path(desktop, basename(url))
  library(httr)
  res <- GET(url,
             authenticate(username, password, type = "basic"),
             write_disk(dest_file, overwrite = TRUE),
             timeout(60))  # one minute limit in case it buffers forever
  if (status_code(res) == 200) {
    message("✅ Downloaded successfully to: ", dest_file)
    return(dest_file)
  } else if (status_code(res) == 404) {
    stop("❌ File not found (404).")
  } else if (status_code(res) == 401) {
    stop("❌ Unauthorized (401).")
  } else {
    stop("❌ Download failed, status: ", status_code(res))
  }
}

# User inputs (country, device_id and filename)
cat("Select country (Norway / Spain / France / Netherlands):\n")
country <- readline()
country_folder <- get_country_folder(country)

cat("Enter the device ID (e.g. 8e7fead8; if only 7 digits, add a 0 before):\n")
device_id <- readline()

cat("Enter the filename WITHOUT .mp3 (e.g. 2025-02-19T20_58_48.960Z):\n")
file_name <- readline()

url <- build_url(country_folder, device_id, file_name)
cat("\nGenerated URL:\n", url, "\n\n")

download_audio(url)
