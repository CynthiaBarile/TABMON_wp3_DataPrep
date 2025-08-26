## -----------------------------
## TABMON_dataprep.Rproject > buggData/
## 03_prepare_duckdb.R
## 🦆 Loads cleaned data into DuckDB
## 🔗 Creates SQL views (all_data, all_data_with_metadata)
## 🧬 Joins with site metadata
## -----------------------------

library(DBI)
library(dplyr)
library(duckdb)
library(janitor)
library(readr)

source("buggData/02_scripts/01_config.R")

# Connect to DuckDB (creates or opens existing DB)
db_connect <- dbConnect(duckdb(duckdb_file))

# List all cleaned .parquet files from the .parquet data directory
cleaned_files <- list.files(download_dir, pattern = "\\.parquet$", full.names = TRUE)

# Build SQL query that unions all .parquet files into a virtual table
parquet_union <- paste0(
  "SELECT * FROM read_parquet('", cleaned_files, "')",
  collapse = " UNION ALL ")

# Create or replace DuckDB View called 'all_data' based on the unioned parquet files
dbExecute(db_connect, paste0("CREATE OR REPLACE VIEW all_data AS ", parquet_union)) # should return 0

# Optional sanity check: see top rows
dbGetQuery(db_connect, "SELECT * FROM all_data LIMIT 10")

# -----------------------------
# JOIN METADATA
# -----------------------------

# Ensure site_info.csv exists
if (!file.exists(site_info_file)) {
  stop("site_info.csv not found. Run 02_download_data.R first.")
}

# Load the metadata file (site_info.csv)
site_info <- read_csv(site_info_file, show_col_types = FALSE)
# Clean column names (e.g., "X1 Country" becomes "x1_country")
names(site_info) <- make_clean_names(names(site_info))  

# Extract only needed metadata columns and rename them for clarity
site_metadata <- site_info %>%
  select(country, device_id, cluster, site, habitat = x12_habitat, latitude, longitude) %>%
  distinct() %>%
  mutate(across(where(is.character), tolower))

# Write metadata as DuckDB table so it can then be joined via SQL
dbWriteTable(db_connect, "site_metadata", site_metadata, overwrite = TRUE)

# Create 2nd DuckDB View joining all_data with site_metadata using device_id
# LEFT JOIN = all rows in all_data are kept, even if no match in site_metadata
# Uses SQL with table aliases `a` and `m` for readability

dbExecute(db_connect, "
  CREATE OR REPLACE VIEW all_data_with_metadata AS
  SELECT 
    a.*,                                  -- all data columns (filename, timestamp, etc.)
    m.country,                            -- join: add country from site_metadata
    m.cluster,                            -- join: add cluster from site_metadata
    m.site,                               -- join: add site from site_metadata
    m.habitat,                            -- join: add habitat from site_metadata
    m.latitude,                           -- join: add latitude from site_metadata
    m.longitude                           -- join: add longitude from site_metadata
  FROM all_data a                         -- alias `a` = all_data view
  LEFT JOIN site_metadata m               -- alias `m` = site_metadata table
    ON a.device_id = m.device_id          -- join condition: matching device_ID
")

# Optional: sanity check on joined data
df_preview <- dbGetQuery(db_connect, "SELECT * FROM all_data_with_metadata LIMIT 100")
View(df_preview)

# Disconnect from database
dbDisconnect(db_connect)

