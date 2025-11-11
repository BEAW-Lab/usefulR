#' This function calculate proportion of impervious surface (or any other habitat type) given a set of coordinates
#' 
#' @description
#' Function to extract the proportion of land cover around a given set of coordinates. 
#' It uses ESA WorldCover 2021 data. By default, the function loads and uses land cover data from Spain, but the function 
#' can take data from other geographical areas, passed to the 'rast' argument.
#' 
#' For more information on the land cover data use here, check: https://worldcover2021.esa.int/
#'
#' @param coords  data.frame or matrix with columns lon, lat (WGS84)
#' @param raster_lc terra raster
#' @param radius_m numeric buffer radius in meters
#' @param type    "categorical"
#' @param cat_vals integer vector of raster values to consider 'impervious' (only for categorical) (cat_vals = 50 for impervious surface)
#' @param value_scale numeric: if fractional values are 0-100 set to 100; default 1 (0-1)
#' @return data.frame with lon, lat, radius_m, percent_impervious (0-100)
#' @export
extract_land_cover <- function(coords, radius_m, 
                                cat_vals = c(50), # this is the value for impervious surface
                                value_scale = 100) {
  rast <- terra::rast("./data/LC_raster.tif")
  
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
    
    if (base::is.null(ex) || base::nrow(ex) == 0) {
      return(base::data.frame(percent_impervious = 'NA_real_'))
    }
    
    # actual calculation of %
      is_imp <- ex$value %in% cat_vals
      cov    <- ex$coverage_fraction
      percent <- base::sum(cov[is_imp], na.rm = TRUE) / base::sum(cov, na.rm = TRUE) * 100
      return(base::data.frame(percent_impervious = percent))
  })
  
  res_df <- dplyr::bind_rows(results)
  out <- base::cbind(
    sf::st_drop_geometry(pts)[, c("lon", "lat", 
                                  base::setdiff(base::names(sf::st_drop_geometry(pts)), c("lon", "lat")))],
    radius_m = radius_m,
    res_df
  )
  
  return(out)
}




