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
  coords <- base::as.data.frame(coords)
  required_cols <- c("lon", "lat")

  if (!base::all(required_cols %in% base::names(coords))) {
    base::stop(
      "`coords` must contain columns named `lon` and `lat`.",
      call. = FALSE
    )
  }

  coords$lon <- base::as.numeric(coords$lon)
  coords$lat <- base::as.numeric(coords$lat)

  if (base::any(!base::is.finite(coords$lon)) ||
      base::any(!base::is.finite(coords$lat))) {
    base::stop(
      "`coords$lon` and `coords$lat` must contain only finite numeric values.",
      call. = FALSE
    )
  }

  if (base::any(coords$lon < -180 | coords$lon > 180)) {
    base::stop(
      "`coords$lon` must be in decimal degrees within [-180, 180].",
      call. = FALSE
    )
  }

  if (base::any(coords$lat < -90 | coords$lat > 90)) {
    base::stop(
      "`coords$lat` must be in decimal degrees within [-90, 90].",
      call. = FALSE
    )
  }

  if (
    !base::is.numeric(radius_m) ||
      base::length(radius_m) != 1L ||
      base::is.na(radius_m) ||
      radius_m <= 0
  ) {
    base::stop("`radius_m` must be a single positive number.", call. = FALSE)
  }

  rast <- if (base::inherits(raster_path, "SpatRaster")) {
    raster_path
  } else {
    terra::rast(raster_path)
  }
  
  # Build sf points (WGS84)
  pts <- sf::st_as_sf(
    coords,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

  # Build the buffer geodesically from the original WGS84 coordinates so the
  # requested radius is preserved before matching polygons to raster cells.
  old_s2 <- sf::sf_use_s2()
  base::on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(TRUE)
  buffers_ll <- sf::st_buffer(pts, dist = radius_m)
  
  # Reproject buffers to raster CRS
  rast_crs <- terra::crs(rast, proj = TRUE)
  if (base::is.na(rast_crs) || rast_crs == "") {
    base::stop("Raster has no CRS defined.", call. = FALSE)
  }
  buffers_rastcrs <- sf::st_transform(buffers_ll, crs = rast_crs)
  
  # Area-weighted summary with exactextractr
  results <- base::lapply(base::seq_len(base::nrow(buffers_rastcrs)), function(i) {
    poly <- buffers_rastcrs[i, ]
    
    ex <- exactextractr::exact_extract(rast, poly, include_cell = FALSE, progress = FALSE)[[1]]
    
    if (base::is.null(ex) || base::nrow(ex) == 0) {
      return(base::data.frame(
        alan = NA_real_,
        alan_log = NA_real_
      ))
    }
    
    alan <- base::sum(ex$value * ex$coverage_fraction, na.rm = TRUE) /
      base::sum(ex$coverage_fraction, na.rm = TRUE)
    
    # Area-weighted mean log10(ALAN + 1)
    alan_log <- base::sum(log10(ex$value + 1) * ex$coverage_fraction, na.rm = TRUE) /
      base::sum(ex$coverage_fraction, na.rm = TRUE)
    
    base::data.frame(
      alan = alan,
      alan_log = alan_log
    )
  })
  
  # --- Bind results ---
  results_df <- dplyr::bind_rows(results)
  
  # --- Final output ---
  out <- base::cbind(
    lon = coords$lon,
    lat = coords$lat,
    radius_m = radius_m,
    results_df
  )
  
  return(out)
}


