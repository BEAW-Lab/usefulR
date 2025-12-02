#' Download ERA5 Point Data from the Copernicus Climate Data Store
#'
#' This function downloads ERA5 reanalysis data for one or more geographic
#' coordinates by requesting a small spatial window around each point  
#' (ERA5 does not support point queries directly). It retrieves hourly data
#' for a user-defined time range and list of climate variables using the
#' Copernicus Climate Data Store (CDS) API via the \pkg{ecmwfr} package.
#'
#' One NetCDF file is produced per coordinate.
#'
#' @param start_date Character or Date. Start of the time window
#'   (e.g., `"2020-01-01"`).
#' @param end_date Character or Date. End of the time window
#'   (e.g., `"2020-01-31"`).
#' @param coords A data frame containing point coordinates with columns:
#'   \describe{
#'     \item{lon}{Longitude in decimal degrees.}
#'     \item{lat}{Latitude in decimal degrees.}
#'   }
#' @param variables Character vector of ERA5 variable names to download.
#' @param token Character. Your CDS API token (passed to
#'   \code{ecmwfr::wf_set_key}).
#' @param temp_dir Character. Directory where NetCDF files will be stored.
#'   Defaults to `"era5_downloads"`. Created if missing.
#' @param force Logical. If \code{FALSE} (default), existing files are skipped.
#'   If \code{TRUE}, existing files are overwritten and re-downloaded.
#'
#' @return Invisibly returns \code{NULL}.  
#'   The function is called for its side effects: downloading NetCDF files.
#'
#' @details
#' ERA5 provides hourly climate data at ~0.25° resolution. Because the CDS API
#' does not support exact point extraction, the function automatically builds a
#' small bounding box (0.3° buffer) around each coordinate to ensure that the
#' relevant ERA5 grid cell is included.
#'
#' For large spatial or temporal requests, downloads may take time.  
#' To avoid unnecessary downloads, existing files are skipped unless
#' \code{force = TRUE}.
#'
#' @examples
#' \dontrun{
#' coords <- data.frame(
#'   lon = c(-3.70, -8.40),
#'   lat = c(40.42, 43.36)
#' )
#'
#' download_era5(
#'   start_date = "2021-01-01",
#'   end_date   = "2021-01-31",
#'   coords     = coords,
#'   variables  = c("2m_temperature"),
#'   token      = "your_cds_token_here"
#' )
#'
#' # Force overwriting existing NetCDF files
#' download_era5(
#'   start_date = "2021-01-01",
#'   end_date   = "2021-01-31",
#'   coords     = coords,
#'   variables  = "total_precipitation",
#'   token      = "your_cds_token_here",
#'   force      = TRUE
#' )
#' }
#'
#' @export
download_era5 <- function(start_date, 
                          end_date,
                          coords,
                          variables,
                          token,
                          temp_dir = "era5_downloads",
                          force = FALSE) {
  
  requireNamespace("ecmwfr", quietly = TRUE)
  requireNamespace("terra", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  
  # Authenticate
  ecmwfr::wf_set_key(key = token)
  
  # Create directory if needed
  if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
  
  # Bounding box helper
  make_bbox <- function(lat, lon, buffer = 0.3) {
    c(lat + buffer, lon - buffer, lat - buffer, lon + buffer)
  }
  
  # Date components
  dates  <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  years  <- unique(format(dates, "%Y"))
  months <- unique(format(dates, "%m"))
  days   <- unique(format(dates, "%d"))
  
  # Loop through coordinates
  purrr::map2_dfr(coords$lat, coords$lon, function(lat, lon) {
    
    out_file <- paste0("era5_",
                       gsub("\\.", "_", format(lat)), "_",
                       gsub("\\.", "_", format(lon)), ".nc")
    
    full_path <- file.path(temp_dir, out_file)
    
    # Check if file exists and force is FALSE → skip
    if (file.exists(full_path) && !force) {
      message("✔ File exists, skipping download: ", out_file)
      return(NULL)
    }
    
    # If force = TRUE, delete the old file first
    if (file.exists(full_path) && force) {
      message("⚠ Overwriting existing file: ", out_file)
      file.remove(full_path)
    }
    
    bbox <- make_bbox(lat, lon)
    
    request <- list(
      dataset_short_name = "reanalysis-era5-single-levels",
      product_type = "reanalysis",
      variable = variables,
      year = years,
      month = months,
      day = days,
      time = sprintf("%02d:00", 0:23),
      area = bbox,
      format = "netcdf",
      target = out_file
    )
    
    message("⤵ Downloading: ", out_file)
    
    ecmwfr::wf_request(
      request  = request,
      transfer = TRUE,
      path     = temp_dir,
      time_out = 36000
    )
    
    return(NULL)
  })
}
