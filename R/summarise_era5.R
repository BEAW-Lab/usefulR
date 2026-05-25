#' Summarise ERA5 point data from downloaded NetCDF files
#'
#' This function reads NetCDF files previously downloaded using
#' \code{download_era5()} and summarises each climate variable using a
#' user-defined function (e.g., \code{mean}, \code{sum}, \code{max}).
#'
#' @param path Character. Directory containing the NetCDF files.
#'   Defaults to \code{"era5_downloads"}.
#' @param variables Optional character vector specifying which ERA5 variables to
#'   summarise. If \code{NULL} (default), all variables present in each NetCDF
#'   file are summarised.
#' @param summarise_fun A function to apply to each variable time series.
#'   Defaults to \code{mean}.
#' @param ... Additional arguments passed to \code{summarise_fun}
#'   (e.g., \code{na.rm = TRUE}).
#'
#' @return A data frame with one row per NetCDF file and one column per
#'   summarised ERA5 variable, alongside the corresponding coordinates.
#' @export
summarise_era5 <- function(path = "era5_downloads",
                           variables = NULL,
                           summarise_fun = mean,
                           ...) {
  requireNamespace("terra", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  
  files <- list.files(path, pattern = "\\.nc$", full.names = TRUE)
  if (base::length(files) == 0) {
    base::stop("No NetCDF files found in directory: ", path, call. = FALSE)
  }
  
  parse_coords <- function(f) {
    base_name <- basename(f)
    cleaned <- gsub("era5_|\\.nc", "", base_name)
    parts <- strsplit(cleaned, "_")[[1]]
    
    if (length(parts) < 4L) {
      base::stop("Could not parse coordinates from file name: ", base_name, call. = FALSE)
    }
    
    lat_str <- paste(parts[1], parts[2], sep = ".")
    lon_str <- paste(parts[3], parts[4], sep = ".")
    
    base::data.frame(
      lat = as.numeric(lat_str),
      lon = as.numeric(lon_str)
    )
  }
  
  result_list <- purrr::map(files, function(f) {
    coords <- parse_coords(f)
    r <- terra::rast(f)
    available_vars <- names(r)
    
    vars_to_use <- if (base::is.null(variables)) {
      available_vars
    } else {
      missing_vars <- base::setdiff(variables, available_vars)
      if (base::length(missing_vars) > 0L) {
        base::stop(
          "Variables not found in ", basename(f), ": ",
          paste(missing_vars, collapse = ", "),
          call. = FALSE
        )
      }
      variables
    }
    
    extracted <- terra::extract(r[[vars_to_use]], base::data.frame(lon = coords$lon, lat = coords$lat))
    values <- extracted[, -1, drop = FALSE]
    
    summary_values <- lapply(values, function(x) summarise_fun(x, ...))
    summary_df <- base::as.data.frame(summary_values, check.names = FALSE)
    
    temp_like_vars <- base::intersect(base::c("t2m", "2m_temperature", "2m_temp"), base::names(summary_df))
    for (var_name in temp_like_vars) {
      summary_df[[var_name]] <- summary_df[[var_name]] - 273.15
    }
    
    dplyr::bind_cols(coords, summary_df)
  })
  
  dplyr::bind_rows(result_list)
}
