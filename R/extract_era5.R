#' Download and Summarise ERA5 Point Data from the Copernicus Climate Data Store
#'
#' This function downloads ERA5 reanalysis data for one or more geographic
#' coordinates by requesting a small spatial window around each point  
#' (ERA5 does not support point queries directly). It retrieves hourly data
#' for a user-defined time range and list of climate variables using the
#' Copernicus Climate Data Store (CDS) API via the \pkg{ecmwfr} package.
#'
#' After downloading and extracting the nearest ERA5 grid cell for each point,
#' the function summarises all hourly values into one aggregated value per
#' variable, using a user-defined summary function (e.g., `mean`, `sum`,
#' `median`, `max`, custom functions, etc.). This is especially useful for
#' long time periods such as monthly or seasonal summaries.
#'
#' @param start_date Character or Date. Start of the time window
#'   (e.g. `"2020-01-01"`).
#' @param end_date Character or Date. End of the time window
#'   (e.g. `"2020-01-31"`).
#' @param coords A data frame containing point coordinates with columns:
#'   \describe{
#'     \item{lon}{Longitude in decimal degrees.}
#'     \item{lat}{Latitude in decimal degrees.}
#'   }
#' @param variables Character vector of ERA5 variable names to download.
#'   Defaults to `c("2m_temperature")`.
#' @param token Character. Your CDS API token.
#' @param summarise_fun A function used to summarise hourly values across the
#'   entire time window. Defaults to `mean`. Must accept a numeric vector and
#'   return a single numeric value. Can be base R functions or custom functions.
#' @param temp_dir Character. Directory where temporary NetCDF files will be
#'   stored. Defaults to `"era5_temp_downloads"`. Will be created if missing.
#'
#' @return A data frame with one row per coordinate and one column per
#'   summarised climate variable. Output columns include:
#'   \itemize{
#'     \item{`lat`, `lon` — original coordinate values}
#'     \item{One column per variable summarised over the time window}
#'   }
#'
#' @details
#' ERA5 provides hourly climate data at ~0.25° resolution. Because the CDS API
#' does not support point extraction directly, the function automatically builds
#' a small bounding box (0.3° buffer) around each coordinate to ensure that the
#' corresponding ERA5 grid cell is included. The nearest cell is extracted using
#' the \pkg{terra} package.
#'
#' For long time windows or many points, consider batching multiple coordinates
#' into a single request, or running the function in parallel.
#'
#' @examples
#' \dontrun{
#' coords <- data.frame(
#'   lon = c(-3.70, -8.40),
#'   lat = c(40.42, 43.36)
#' )
#'
#' # Monthly mean temperature and precipitation
#' df <- download_era5_points(
#'   start_date = "2021-01-01",
#'   end_date   = "2021-01-31",
#'   coords     = coords,
#'   variables  = c("2m_temperature", "total_precipitation"),
#'   user_id    = "123456",
#'   token      = "your_cds_token_here",
#'   summarise_fun = mean
#' )
#'
#' # Monthly precipitation sum
#' df2 <- download_era5_points(
#'   start_date = "2021-01-01",
#'   end_date   = "2021-01-31",
#'   coords     = coords,
#'   variables  = "total_precipitation",
#'   user_id    = "123456",
#'   token      = "your_cds_token_here",
#'   summarise_fun = sum
#' )
#' }
#'
#' @export
#' 
#' 
extract_era5 <- function(start_date, 
                         end_date,
                         coords,
                         variables,
                         token,
                         summarise_fun = mean,
                         temp_dir = "era5_downloads") {
  
  # Load required packages inside function (safe for R packages)
  requireNamespace("ecmwfr", quietly = TRUE)
  requireNamespace("terra", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  
  # Authenticate
  ecmwfr::wf_set_key(
    #user = user_id,
    key = token
  )
  
  # Create temporary download directory if needed
  if (!dir.exists(temp_dir)) dir.create(temp_dir)
  
  # Bounding box helper
  make_bbox <- function(lat, lon, buffer = 0.3) {
    c(lat + buffer, lon - buffer, lat - buffer, lon + buffer)  # N, W, S, E
  }
  
  # Sequence of dates
  dates  <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  years  <- unique(format(dates, "%Y"))
  months <- unique(format(dates, "%m"))
  days   <- unique(format(dates, "%d"))
  
  # Loop through coordinates
  results <- purrr::map2_dfr(coords$lat, coords$lon, function(lat, lon) {
    
    bbox <- make_bbox(lat, lon)
    out_file <- paste0("era5_", gsub("\\.", "_", format(lat)), "_", gsub("\\.", "_", format(lon)), ".nc")
    
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
      target = basename(out_file)
    )
    
    # Submit CDS request
    ecmwfr::wf_request(
      request  = request,
      transfer = TRUE,
      path   = temp_dir,
    )

    # expected path to the nc we asked for
    expected_nc <- paste0(temp_dir, "/", out_file)

    # read the (one or many) netcdf(s)
    r <- terra::rast(expected_nc)
    
    # extract values at the single point (returns 1 row)
    extracted <- terra::extract(r, data.frame(lon = lon, lat = lat))
    
    # drop ID column
    values <- extracted[, -1, drop = FALSE]
    summary_values <- apply(values, 1, summarise_fun, na.rm = TRUE) 
    
    # Build final output data.frame: one column per variable (group)
    out <- data.frame(
      start_date = start_date,
      end_date = end_date,
      lat = lat,
      lon = lon)
    
    out <- dplyr::tibble(out, !!variables := summary_values)
    rownames(out) <- NULL
    return(out)
  })
  
  # Convert temperature variables from Kelvin to Celsius
  kelvin_vars <- c("t2m", "2m_temperature")
  
  for (kv in kelvin_vars) {
    if (kv %in% names(results)) {
      results[[kv]] <- results[[kv]] - 273.00
    }
  }
  
  # final object
  return(results)
}
