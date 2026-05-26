#' Download CORINE Land Cover 2018 raster data
#'
#' @description
#' This helper function downloads a CORINE Land Cover 2018 raster from a
#' Dropbox direct link. It stores the file locally, so it only needs to be
#' downloaded once.
#'
#' @param local_dir Path to directory where the file should be saved.
#'   If \code{NULL} (default), a system cache directory is used.
#' @param file_name Desired name for the downloaded file
#'   (e.g. \code{"CLC_CORINE_2018.tif"}). Must include the \code{".tif"}
#'   extension. Defaults to \code{"CLC_CORINE_2018.tif"}.
#' @param force Logical. If \code{TRUE}, re-downloads even if the file already
#'   exists.
#' @return The full path to the downloaded CORINE raster file.
#' @examples
#' \dontrun{
#' download_corine_raster(local_dir = "data")
#' }
#' @export
download_corine_raster <- function(
  local_dir = NULL,
  file_name = "CLC_CORINE_2018.tif",
  force = FALSE
) {
  requireNamespace("utils")
  requireNamespace("rappdirs")

  if (!grepl("\\.tif$", file_name, ignore.case = TRUE)) {
    file_name <- paste0(file_name, ".tif")
  }

  options(timeout = max(600, getOption("timeout")))

  url <- "https://www.dropbox.com/scl/fi/vk9lr402twwrkomrugdvs/U2018_CLC2018_V2020_20u1.tif?rlkey=j8bl5nd74spm3wj5u93llzye0&dl=1"

  if (!nzchar(url)) {
    stop(
      "`url` in `download_corine_raster()` must be a non-empty Dropbox direct-download link.",
      call. = FALSE
    )
  }

  if (is.null(local_dir)) {
    local_dir <- rappdirs::user_cache_dir("usefulR")
  }
  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE)
  }

  local_file <- file.path(local_dir, file_name)

  if (!file.exists(local_file) || force) {
    message("Downloading ", file_name, "... please wait.")
    tryCatch(
      {
        utils::download.file(
          url,
          destfile = local_file,
          mode = "wb",
          quiet = FALSE
        )
        message("Download complete. File saved to: ", normalizePath(local_file))
      },
      error = function(e) {
        stop("Download failed: ", e$message, call. = FALSE)
      }
    )
  } else {
    message("File already exists at: ", normalizePath(local_file))
  }

  if (!file.exists(local_file)) {
    stop("The CORINE raster file was not found after download.", call. = FALSE)
  }

  local_file
}
