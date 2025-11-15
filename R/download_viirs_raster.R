#' Download VIIRS Nighttime Lights data
#' The downloaded file includes data for Europe in 2024.
#'
#' @description
#' This helper function downloads VIIRS Nighttime Lights data to use in the calculation of amount of artificial light at night.
#' It stores it locally, so it needs to be downloaded only once. Data for Europe in 2024 is downloaded and saved.
#' 
#' For more details on the dataset, go to: https://eogdata.mines.edu/products/vnl/
#'
#' @param local_dir Path to directory where the file should be saved.
#'   If NULL (default), a system cache directory is used.
#' @param file_name Desired name for the downloaded file (e.g. "VIIRS.tif").
#'   Must include the ".tif" extension. Defaults to "VIIRS.tif".
#' @param force Logical. If TRUE, re-downloads even if the file already exists.
#' @return The full path to the downloaded raster file.
#' @examples
#' \dontrun{
#' TBC
#' }
#' @export
download_viirs_raster <- function(local_dir = NULL,
                                  file_name = "VIIRS.tif",
                                  force = FALSE) {
  
  requireNamespace("utils")
  requireNamespace("rappdirs")
  requireNamespace("terra")
  
  # --- Ensure .tif extension ---
  if (!grepl("\\.tif$", file_name, ignore.case = TRUE)) {
    file_name <- paste0(file_name, ".tif")
  }
  
  # --- Increase timeout for large files ---
  options(timeout = max(6000, getOption("timeout")))
  
  # --- Remote source (Dropbox direct link) ---
  url <- "https://www.dropbox.com/scl/fi/s9yrh00vfspvy16ns671j/VNL_npp_2024_global_vcmslcfg_v2_c202502261200.europe.average.dat.tif?rlkey=8amvixia1rkk6w5mil8awphug&dl=1"
  
  # --- Resolve storage directory (user_cache_dir by default) ---
  if (is.null(local_dir)) {
    local_dir <- rappdirs::user_cache_dir("usefulR")
  }
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  
  local_file <- file.path(local_dir, file_name)
  
  # --- Temporary file for downloading large global raster ---
  tmp_file <- tempfile(fileext = ".tif")
  
  # --- Download logic ---
  if (!file.exists(local_file) || force) {
    message("Downloading VIIRS global raster (~450 MB). This will take a while...")
    
    tryCatch({
      utils::download.file(url,
                           destfile = tmp_file,
                           mode = "wb",
                           quiet = FALSE)
    },
    error = function(e) {
      stop("❌ Download failed: ", e$message, call. = FALSE)
    })
    
    message("✔ Download complete. Now cropping Europe...")
    
    # --- Load global raster ---
    r_global <- tryCatch(
      terra::rast(tmp_file),
      error = function(e) stop("❌ Could not read downloaded raster: ", e$message)
    )
    
    # --- Europe crop extent ---
    europe_ext <- terra::ext(-10, 40, 35, 72)  # WEST, EAST, SOUTH, NORTH
    
    # --- Crop to Europe ---
    r_eu <- tryCatch(
      terra::crop(r_global, europe_ext),
      error = function(e) stop("❌ Cropping failed: ", e$message)
    )
    
    # --- Save cropped raster ---
    message("💾 Saving Europe-only raster to: ", normalizePath(local_file))
    terra::writeRaster(r_eu, local_file, overwrite = TRUE)
    
    message("✔ Europe raster saved.")
  } else {
    message("File already exists at: ", normalizePath(local_file))
  }
  
  # --- File integrity check (for cropped Europe raster) ---
  size <- file.info(local_file)$size
  
  if (is.na(size) || size < 40e6) {  # Europe crop is 441 MB
    warning("⚠ Saved raster is unexpectedly small. Something may have gone wrong.")
  }
  
  return(local_file)
}
