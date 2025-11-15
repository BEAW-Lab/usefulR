#' This function calculates artificial light at night around a set of coordinates
#' 
#' @description
#' Function to extract artificial light at night around a set of coordinates. The units of the output will
#' be determined by the ALAN units in the input raster.
#'
#' @param coords  data.frame or matrix with columns lon, lat (WGS84)
#' @param raster_path path to tif raster of your choice
#' @param radius_m numeric buffer radius in meters
#' @return data.frame with lon, lat, radius_m, alan in natural scale and log scale
#' @export
extract_alan <- function(coords, 
                         raster_path,
                         radius_m) {
  
  # access raster data
  rast <- terra::rast(raster_path)
  
  # Read raster (if path)
  if (!base::inherits(rast, "SpatRaster")) rast <- terra::rast(rast)
  
  # Build sf points (WGS84)
  pts <- sf::st_as_sf(base::as.data.frame(coords), coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  # Project to a metric CRS (EPSG:3857)
  pts_m <- sf::st_transform(pts, 3857)
  
  # Make buffers (in meters)
  buffers_m <- sf::st_buffer(pts_m, dist = radius_m)
  
  # Reproject buffers to raster CRS
  rast_crs <- terra::crs(rast, proj = TRUE)
  if (base::is.na(rast_crs) || rast_crs == "") {
    base::stop("Raster has no CRS defined.")
  }
  buffers_rastcrs <- sf::st_transform(buffers_m, crs = rast_crs)
  
  # Area-weighted summary with exactextractr
  results <- base::lapply(base::seq_len(base::nrow(buffers_rastcrs)), function(i) {
    poly <- buffers_rastcrs[i, ]
    
    ex <- exactextractr::exact_extract(rast, poly, include_cell = FALSE, progress = FALSE)[[1]]
    
    if (is.null(ex) || nrow(ex) == 0) {
      return(data.frame(
        alan = NA_real_,
        alan_log = NA_real_
      ))
    }
    
    alan <- sum(ex$value * ex$coverage_fraction, na.rm = TRUE) /
      sum(ex$coverage_fraction, na.rm = TRUE)
    
    # Area-weighted mean log10(ALAN + 1)
    alan_log <- sum(log10(ex$value + 1) * ex$coverage_fraction, na.rm = TRUE) /
      sum(ex$coverage_fraction, na.rm = TRUE)
    
    data.frame(
      alan = alan,
      alan_log = alan_log
    )
  })
  
  # --- Bind results ---
  results_df <- dplyr::bind_rows(results)
  
  # --- Final output ---
  out <- cbind(
    lon = coords$lon,
    lat = coords$lat,
    radius_m = radius_m,
    results_df
  )
  
  return(out)
}



