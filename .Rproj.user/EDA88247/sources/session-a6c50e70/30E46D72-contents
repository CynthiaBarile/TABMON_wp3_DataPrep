## -----------------------------
## buggData/
## 01_config.R
## 🔧 Sets up the environment (path, credentials, constants)
## -----------------------------

library(here)

# Load credentials from R environment
usr <- Sys.getenv("TABMON_USR")
psswd <- Sys.getenv("TABMON_PSSWD")

# Stop with error if credentials are not set
if (usr == "" || psswd == "") {
  stop("Missing authentication credentials! Please set TABMON_USR and TABMON_PSSWD in your .Renviron file.")
}

# Define core file paths based on project root
project_root <- here::here()  # Automatically finds root of project (e.g., RStudio project or Git repo)
download_dir <- file.path(project_root, "buggData", "01_data", "parquet_clean")  # Folder to save downloaded .parquet files
duckdb_file <- file.path(project_root, "buggData", "01_data", "buggdata_db.duckdb")  # Path to DuckDB database file

# NOTE: site_info.csv is hosted remotely
site_info_url <- "https://tabmon.nina.no/data/site_info.csv"  # Remote CSV with deployments metadata
site_info_file <- file.path(project_root, "buggData", "01_data", "site_info.csv")  # Local dir to store it after download

# Create the download directory if it doesn't already exist
dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)

