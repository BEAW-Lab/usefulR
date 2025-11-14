#' Download VIIRS Nighttime Lights data (https://eogdata.mines.edu/products/vnl/) to use in the calculation of amount of artificial light at night.
#' The downloaded file includes global data from June 2025.
#'
#' @description
#' This helper function downloads VIIRS Nighttime Lights data from June 2025.
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
download_viirsJune25_raster <- function(local_dir = NULL,
                                        file_name = "VIIRS_June2025.tif",
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
  url <- "https://www.dropbox.com/scl/fi/kipt8s5hr23ozurhl416s/VNL_npp_2024_global_vcmslcfg_v2_c202502261200.average_masked.dat.tif?rlkey=g38yht4a8aur73pdftjk1vfva&dl=1"
  
  # --- Determine where to store file ---
  if (is.null(local_dir)) {
    local_dir <- rappdirs::user_cache_dir("usefulR")
  }
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  
  # --- Combine directory + file name ---
  local_file <- file.path(local_dir, file_name)
  
  # --- Download if missing or forced ---
  if (!file.exists(local_file) || force) {
    message("Downloading ", file_name, " (~12 GB)... please wait.")
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
  if (is.na(size) || size < 10000e6) {
    warning("File may be incomplete (size < 10 GB). Try re-downloading with force = TRUE.")
  }
  
  return(local_file)
}
