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
#' TBC
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
  url <- "https://www.dropbox.com/scl/fi/kt47lhz9eta3g77x03m20/ESA21_WLC_raster.tif?rlkey=4w3fzk50t966tk7a2klciz3o2&dl=1"
  
  # --- Determine where to store file ---
  if (is.null(local_dir)) {
    local_dir <- rappdirs::user_cache_dir("usefulR")
  }
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  
  # --- Combine directory + file name ---
  local_file <- file.path(local_dir, file_name)
  
  # --- Download if missing or forced ---
  if (!file.exists(local_file) || force) {
    message("Downloading ", file_name, " (~1.8 GB)... please wait.")
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
  if (is.na(size) || size < 1800e6) {
    warning("File may be incomplete (size < 1.8 GB). Try re-downloading with force = TRUE.")
  }
  
  return(local_file)
}
