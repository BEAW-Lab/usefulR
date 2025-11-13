#' Download ESA WorldCover 2021 data
#'
#' @description
#' This helper function downloads a land cover raster from ESA WorldCover 2021.
#' It stores it locally, so it needs to be downloaded only once.
#'
#' @param local_dir Path to directory where the file should be saved.
#'   If NULL (default), a system cache directory is used.
#' @param file_name Desired name for the downloaded file (e.g. "LC_raster.tif").
#'   Must include the ".tif" extension. Defaults to "LC_raster.tif".
#' @param force Logical. If TRUE, re-downloads even if the file already exists.
#' @return The full path to the downloaded raster file.
#' @examples
#' \dontrun{
#' # Default: saves in cache with default name
#' path <- download_LC_raster()
#'
#' # Save in custom folder
#' path <- download_LC_raster(local_dir = "data_raw/")
#'
#' # Save with custom name
#' path <- download_LC_raster(local_dir = "data_raw/", file_name = "Spain_LC_2021.tif")
#' }
#' @export
download_ESAWC21_raster <- function(local_dir = NULL,
                                    file_name = "LC_raster.tif",
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
  url <- "https://www.dropbox.com/scl/fi/100yws6j6v1bburss7t6b/LC_raster.tif?rlkey=yl8lyidmenr33lb6gh0ugajva&dl=1"
  
  # --- Determine where to store file ---
  if (is.null(local_dir)) {
    local_dir <- rappdirs::user_cache_dir("usefulR")
  }
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  
  # --- Combine directory + file name ---
  local_file <- file.path(local_dir, file_name)
  
  # --- Download if missing or forced ---
  if (!file.exists(local_file) || force) {
    message("Downloading ", file_name, " (~630 MB)... please wait.")
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
  if (is.na(size) || size < 500e6) {
    warning("File may be incomplete (size < 500 MB). Try re-downloading with force = TRUE.")
  }
  
  return(local_file)
}
