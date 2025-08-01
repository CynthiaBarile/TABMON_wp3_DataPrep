## -----------------------------
## buggData/
## 02_download_data.R
## 💾🧹 Accesses, cleans, downloads parquet data
## -----------------------------

library(arrow)
library(glue)
library(here)
library(httr)
library(janitor)
library(purrr)
library(readr)
library(tidyverse)

source("buggData/02_scripts/01_config.R")


# URLs need to be manually given (automatic listing from server not possible atm)
# e.g. "https://tabmon.nina.no/data/parquet_results/country=France/device_id=688a86aa/2025-03_688a86aa.parquet"
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
analysis_months <- c("2025-02", "2025-03", "2025-04", "2025-05", "2025-06") # add more here later if needed

# Generate all possible URLs
candidate_urls <- crossing(month = analysis_months, device_lookup) %>%
  mutate(url = glue("https://tabmon.nina.no/data/parquet_results/country={country}/device_id={device_id}/{month}_{device_id}.parquet"))

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

# Download the files

## BUT! The full files have columns with acoustic indices, which a) I do not need,
## b) make the files much heavier.
## So I will download to a temporary location, read and clean them, then save to disk.

# purrr::walk2(); iterates over two vectors in parallel
# For each pair of elements (.x from valid_files$url, .y from valid_files$local_path),
# it runs the function provided in the {}.

# Download, clean, and save cleaned parquet files
# -------------------------------------------

walk2(valid_files$url, valid_files$local_path, ~ {
  # Temporary location for download
  temp_file <- tempfile(fileext = ".parquet")
  
  # Download with credentials
  res <- GET(.x, authenticate(usr, psswd), write_disk(temp_file, overwrite = TRUE))
  if (status_code(res) != 200) {
    warning(paste("Download failed:", .x))
    return()
  }
  
  # Read file
  df <- read_parquet(temp_file) %>% as_tibble()
  
  # Normalize column names (replace spaces with underscores)
  names(df) <- gsub(" ", "_", names(df))
  
  # Extract device ID from the local path (captures what is before .parquet)
  device_id <- str_match(.y, "([0-9a-f]{7,8})\\.parquet$")[, 2]
  
  # Create time stamp + clip id + final filename
  df <- df %>%
    mutate(
      common_name = str_to_lower(common_name),
      scientific_name = str_to_lower(scientific_name),
      device_id = device_id,
      chunk_index = as.integer(start_time) / 3, # audio clip id in audio filenames (=start time/3)
      filename_cleaned = str_remove(filename, "\\.mp3$") %>% # remove .mp3
        paste(device_id, chunk_index, sep = "_"), # add device id and index in filename
      recording_start_time = str_extract(filename_cleaned, "^[0-9T_\\.:-]+Z") %>% # extract time from filename
        as.POSIXct(format = "%Y-%m-%dT%H_%M_%OSZ", tz = "UTC"), # format it properly
      detection_time_utc = recording_start_time + as.numeric(start_time), # datetime column
    )
  
  # Final columns to keep
  keep_cols <- c("filename_cleaned", "device_id", "detection_time_utc",
                 "scientific_name", "common_name", "confidence")
  
  df_clean <- df %>%
    select(any_of(keep_cols)) %>%
    relocate(filename_cleaned) %>%  # optional: make filename first column
    arrange(filename_cleaned) # orders by filename
  
  # Save cleaned file
  write_parquet(df_clean, .y)
})

# Optional: loop version for reference/learning
# for (i in seq_len(nrow(valid_files))) {
#   url <- valid_files$url[i]
#   local_path <- valid_files$local_path[i]
#   temp_file <- tempfile(fileext = ".parquet")
#   res <- GET(url, authenticate(usr, psswd), write_disk(temp_file, overwrite = TRUE))
#   if (status_code(res) == 200) {
#     dt <- read_parquet(temp_file)
#     dt_filtered <- dt %>% select(any_of(keep_cols))
#     write_parquet(dt_filtered, local_path)
#   } else {
#     warning(paste("Download failed:", url))
#   }
# }





## To adapt some manually downloaded files (sent by Corentin)
library(fs)
manual_files_dir <- "C:/Users/cbarile/Desktop"
manual_files <- dir_ls(manual_files_dir, regexp = "\\.parquet$")
# Clean and write them
walk(manual_files, function(file_path) {
  # Read and normalize column names
  df <- read_parquet(file_path) %>%
    as_tibble()
  names(df) <- gsub(" ", "_", names(df))
  # Extract device ID from filename
  device_id <- str_match(path_file(file_path), "([0-9a-f]{7,8})\\.parquet$")[, 2]
  # Construct chunk index and cleaned filename
  df <- df %>%
    mutate(
      common_name = str_to_lower(common_name),
      scientific_name = str_to_lower(scientific_name),
      device_id = device_id,
      chunk_index = as.integer(start_time) / 3, # audio clip id in audio filenames (=start time/3)
      filename_cleaned = str_remove(filename, "\\.mp3$") %>% # remove .mp3
        paste(device_id, chunk_index, sep = "_"), # add device id and index in filename
      recording_start_time = str_extract(filename_cleaned, "^[0-9T_\\.:-]+Z") %>% # extract time from filename
        as.POSIXct(format = "%Y-%m-%dT%H_%M_%OSZ", tz = "UTC"), # format it properly
      detection_time_utc = recording_start_time + as.numeric(start_time), # datetime column
    )
  # Final columns to keep
  keep_cols <- c("filename_cleaned", "device_id", "detection_time_utc",
                 "scientific_name", "common_name", "confidence")
  df_clean <- df %>%
    select(any_of(keep_cols)) %>%
    relocate(filename_cleaned) %>%
    arrange(filename_cleaned)
  # Output path (use same name)
  out_file <- path(manual_files_dir, path_file(file_path))
  write_parquet(df_clean, out_file)
})
