#' Download ESA WorldCover 2021 data for package vignette
#'
#' @description
#' This helper function downloads a land cover raster from ESA WorldCover 2021 to use in the package vignette.
#'
#' @param local_dir Path to directory where the file should be saved.
#'   If NULL (default), a system cache directory is used.
#' @param file_name Desired name for the downloaded file (e.g. "LC_raster.tif").
#'   Must include the ".tif" extension. Defaults to "LC_raster.tif".
#' @param force Logical. If TRUE, re-downloads even if the file already exists.
#' @return The full path to the downloaded raster file.
#' @examples
#' \dontrun{
#' download_ESAWC21_raster_vignette(local_dir = "data_vignette/")
#' }
#' @export
download_ESAWC21_raster_vignette <- function(local_dir = './data_vignette',
                                             file_name = "LC_raster_example.tif",
                                             force = FALSE) {
  requireNamespace("utils")
  requireNamespace("rappdirs")
  
  # --- Safety: make sure filename has .tif extension ---
  if (!grepl("\\.tif$", file_name, ignore.case = TRUE)) {
    file_name <- paste0(file_name, ".tif")
  }
  
  # --- Increase timeout for large files ---
  options(timeout = max(600, getOption("timeout")))
  
  # --- Remote source (Dropbox direct link) ---
  url <- "https://www.dropbox.com/scl/fi/g2j8f5jwpnp124xlopfdj/ESA_WorldCover_10m_2021_V200_N39W006_Map.tif?rlkey=3obz9tvp6zds78kcbvq9mcbv5&dl=1"
  
  # --- Determine where to store file ---
  if (is.null(local_dir)) {
    local_dir <- rappdirs::user_cache_dir("usefulR")
  }
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  
  # --- Combine directory + file name ---
  local_file <- file.path(local_dir, file_name)
  
  # --- Download if missing or forced ---
  if (!file.exists(local_file) || force) {
    message("Downloading ", file_name, " (~100 MB)... please wait.")
    tryCatch({
      utils::download.file(url, destfile = local_file, mode = "wb", quiet = FALSE)
      message("✅ Download complete. File saved to: ", normalizePath(local_file))
    }, error = function(e) {
      stop("❌ Download failed: ", e$message, call. = FALSE)
    })
  } else {
    message("File already exists at: ", normalizePath(local_file))
  }
  
  # --- Verify download integrity ---
  size <- file.info(local_file)$size
  if (is.na(size) || size < 150e6) {
    warning("File may be incomplete (size < 100 MB). Try re-downloading with force = TRUE.")
  }
  
  return(local_file)
}
