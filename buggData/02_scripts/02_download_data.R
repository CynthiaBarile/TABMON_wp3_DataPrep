## -----------------------------
## TABMON_dataprep.Rproject > buggData/
## 02_download_data.R - FROM NINA SERVER
## 💾🧹 Accesses, cleans, downloads parquet data
## -----------------------------

source("buggData/02_scripts/01_config.R")


# URLs need to be manually given (automatic listing from server not possible)
# e.g. "https://tabmon.nina.no/data/merged_predictions_light/country=France/device_id=688a86aa/2025-03_688a86aa.parquet"
# To automatise a minimum, access site info to extract country, deviceID.

# Download site_info.csv from the server if not already downloaded
if (!file.exists(site_info_file)) {
  res <- GET(site_info_url, authenticate(usr, psswd), write_disk(site_info_file, overwrite = TRUE))
  if (status_code(res) != 200) stop("Failed to download site_info.csv")
}

# Load and clean site_info (with janitor::make_clean_namesnames)
site_info <- read_csv(site_info_file, show_col_types = FALSE)
names(site_info) <- make_clean_names(names(site_info))

# Extract country and device ID from relevant columns
device_lookup <- site_info %>%
  select(country, device_id) %>%
  filter(!is.na(country), !is.na(device_id)) %>%
  filter(country == "Netherlands") %>% ## To download only Netherlands data
  distinct()

# Define months to consider
analysis_months <- c("2025-02", "2025-03", "2025-04", "2025-05", "2025-06",
                     "2025-07", "2025-08") # add more here later if needed

# Generate all possible URLs
candidate_urls <- crossing(month = analysis_months, device_lookup) %>%
  mutate(url = glue("https://tabmon.nina.no/data/merged_predictions_light/country={country}/device_id={device_id}/{month}_{device_id}.parquet"))

# Function to check which URLs actually exist
check_exists <- function(url) {
  res <- HEAD(url, authenticate(usr, psswd))
  res$status_code == 200
}

# Run function
candidate_urls$exists <- map_lgl(candidate_urls$url, possibly(check_exists, otherwise = FALSE))
valid_files <- candidate_urls %>% filter(exists)

# Add local paths
valid_files <- valid_files %>%
  mutate(local_path = file.path(download_dir, basename(url)))


# Download, clean, and save cleaned parquet files
# -------------------------------------------

walk2(valid_files$url, valid_files$local_path, ~ {                                    # walk2() iterates over two vectors in parallel, for each pair of elements .x and .y
  temp_file <- tempfile(fileext = ".parquet")                                         # Temporary location for download
  
  res <- GET(.x, authenticate(usr, psswd), write_disk(temp_file, overwrite = TRUE))   # Download with credentials
  if (status_code(res) != 200) {
    warning(paste("Download failed:", .x))
    return()
  }

  df <- read_parquet(temp_file) %>% as_tibble()                                       # Read file
  names(df) <- gsub(" ", "_", names(df))                                              # Normalize column names (replace spaces with underscores)
  device_id <- str_match(.y, "([0-9a-f]{7,8})\\.parquet$")[, 2]                       # Extract device ID from the local path (captures what is before .parquet)
  
  # Create time stamp + clip id + final filename
  df <- df %>%
    mutate(
      scientific_name = str_to_lower(scientific_name),
      device_id = device_id,
      chunk_index = as.integer(start_time) / 3,                                       # audio clip id in audio filenames (=start time/3)
      filename_cleaned = str_remove(filename, "\\.mp3$") %>%                          # remove ".mp3"
        paste(device_id, chunk_index, sep = "_"),                                     # add device id and index in filename
      recording_start_time = str_extract(filename_cleaned, "^[0-9T_\\.:-]+Z") %>%     # extract time from filename
        as.POSIXct(format = "%Y-%m-%dT%H_%M_%OSZ", tz = "UTC"),                       # format it properly
      detection_time_utc = recording_start_time + as.numeric(start_time),             # datetime column for detections
    )
  
  # Final columns to keep
  keep_cols <- c("filename_cleaned", "device_id", "deploymentID", "recording_start_time", 
                 "detection_time_utc", "scientific_name", "common_name", "confidence", "uncertainty")
  
  df_clean <- df %>%
    select(any_of(keep_cols)) %>%
    relocate(filename_cleaned) %>%  # optional: make filename first column
    arrange(filename_cleaned)       # orders by filename
  
  # Save cleaned file
  write_parquet(df_clean, .y)
})

# Quick check 
# read_parquet(valid_files$local_path[1]) %>% head()