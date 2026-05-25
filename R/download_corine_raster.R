#' Download CORINE Land Cover 2018 raster data
#'
#' @description
#' This helper function requests the official pre-packaged CORINE Land Cover
#' 2018 raster from the Copernicus Land Monitoring Service (CLMS) API, waits
#' until the download is ready, and saves the main Europe GeoTIFF locally.
#'
#' The function accepts either:
#' \itemize{
#'   \item a short-lived CLMS API bearer access token, or
#'   \item the full CLMS service-key JSON used to mint that access token.
#' }
#'
#' If a service-key JSON is supplied, the function automatically creates a JWT,
#' exchanges it at the CLMS \code{@@oauth2-token} endpoint, and then uses the
#' returned bearer token for the download request.
#'
#' Instructions to create a CLMS service key are available at
#' \url{https://eea.github.io/clms-api-docs/authentication.html}.
#'
#' @param token Character. Either a CLMS API bearer access token or the full
#'   CLMS service-key JSON string.
#' @param local_dir Path to directory where the file should be saved.
#'   If \code{NULL} (default), a system cache directory is used.
#' @param file_name Desired name for the downloaded file (e.g. \code{"CLC_CORINE_2018.tif"}).
#'   Must include the \code{".tif"} extension. Defaults to \code{"CLC_CORINE_2018.tif"}.
#' @param force Logical. If \code{TRUE}, re-downloads even if the file already exists.
#' @param poll_interval Numeric. Seconds between status checks while the CLMS API
#'   prepares the download. Defaults to 10.
#' @param timeout_sec Numeric. Maximum number of seconds to wait for the CLMS
#'   download job to finish. Defaults to 3600.
#' @param keep_zip Logical. If \code{TRUE}, keeps the downloaded ZIP archive in
#'   \code{local_dir}. Defaults to \code{FALSE}.
#' @return The full path to the downloaded CORINE raster file.
#' @examples
#' \dontrun{
#' download_corine_raster(
#'   token = Sys.getenv("CLMS_BEARER_TOKEN"),
#'   local_dir = "data"
#' )
#' }
#' @export
download_corine_raster <- function(token,
                                   local_dir = NULL,
                                   file_name = "CLC_CORINE_2018.tif",
                                   force = FALSE,
                                   poll_interval = 10,
                                   timeout_sec = 3600,
                                   keep_zip = FALSE) {
  requireNamespace("curl", quietly = TRUE)
  requireNamespace("jsonlite", quietly = TRUE)
  requireNamespace("jose", quietly = TRUE)
  requireNamespace("openssl", quietly = TRUE)
  requireNamespace("rappdirs", quietly = TRUE)
  requireNamespace("utils", quietly = TRUE)
  
  if (!is.character(token) || length(token) != 1L || is.na(token) || token == "") {
    stop(
      "`token` must be a single non-empty CLMS access token or service-key JSON. ",
      "See https://eea.github.io/clms-api-docs/authentication.html",
      call. = FALSE
    )
  }
  
  if (!grepl("\\.tif$", file_name, ignore.case = TRUE)) {
    file_name <- paste0(file_name, ".tif")
  }
  
  if (is.null(local_dir)) {
    local_dir <- rappdirs::user_cache_dir("usefulR")
  }
  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE)
  }
  
  local_file <- file.path(local_dir, file_name)
  zip_file <- file.path(local_dir, "CLC_CORINE_2018.zip")
  
  if (file.exists(local_file) && !force) {
    message("File already exists at: ", normalizePath(local_file))
    return(local_file)
  }
  
  clms_api_url <- "https://land.copernicus.eu/api"
  corine_dataset_id <- "0407d497d3c44bcd93ce8fd5bf78596a"
  corine_file_id <- "3d9e2413-46ca-4f6e-abf8-b6eb429a4e48"
  
  resolve_access_token <- function(token_value) {
    trimmed <- trimws(token_value)
    
    if (grepl("^\\{", trimmed)) {
      service_key <- jsonlite::fromJSON(trimmed, simplifyVector = TRUE)
      
      required_fields <- c("client_id", "private_key", "token_uri", "user_id")
      missing_fields <- setdiff(required_fields, names(service_key))
      if (length(missing_fields) > 0L) {
        stop(
          "The CLMS service-key JSON is missing required fields: ",
          paste(missing_fields, collapse = ", "),
          call. = FALSE
        )
      }
      
      now_unix <- as.integer(Sys.time())
      claim_set <- list(
        iss = service_key$client_id,
        sub = service_key$user_id,
        aud = service_key$token_uri,
        iat = now_unix,
        exp = now_unix + 3600L
      )
      
      private_key <- openssl::read_key(textConnection(service_key$private_key))
      jwt_assertion <- jose::jwt_encode_sig(claim = claim_set, key = private_key)
      
      form_handle <- curl::new_handle()
      curl::handle_setheaders(
        form_handle,
        Accept = "application/json",
        `Content-Type` = "application/x-www-form-urlencoded"
      )
      curl::handle_setopt(
        form_handle,
        post = TRUE,
        postfields = paste0(
          "grant_type=",
          utils::URLencode("urn:ietf:params:oauth:grant-type:jwt-bearer", reserved = TRUE),
          "&assertion=",
          utils::URLencode(jwt_assertion, reserved = TRUE)
        )
      )
      
      token_response <- curl::curl_fetch_memory(service_key$token_uri, handle = form_handle)
      token_content <- rawToChar(token_response$content)
      
      if (token_response$status_code >= 300) {
        stop(
          "CLMS token request failed [", token_response$status_code, "]: ",
          token_content,
          call. = FALSE
        )
      }
      
      token_json <- jsonlite::fromJSON(token_content, simplifyVector = TRUE)
      if (is.null(token_json$access_token) || !nzchar(token_json$access_token)) {
        stop("The CLMS token endpoint did not return an access token.", call. = FALSE)
      }
      
      return(token_json$access_token)
    }
    
    trimmed
  }
  
  access_token <- resolve_access_token(token)
  
  clms_request <- function(url, method = "GET", body = NULL) {
    handle <- curl::new_handle()
    curl::handle_setheaders(
      handle,
      Accept = "application/json",
      Authorization = paste("Bearer", access_token)
    )
    
    if (!is.null(body)) {
      curl::handle_setheaders(handle, `Content-Type` = "application/json")
      curl::handle_setopt(
        handle,
        customrequest = method,
        postfields = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
      )
    } else if (!identical(method, "GET")) {
      curl::handle_setopt(handle, customrequest = method)
    }
    
    response <- curl::curl_fetch_memory(url, handle = handle)
    content <- rawToChar(response$content)
    
    if (response$status_code >= 300) {
      stop(
        "CLMS API request failed [", response$status_code, "]: ",
        content,
        call. = FALSE
      )
    }
    
    jsonlite::fromJSON(content, simplifyVector = TRUE)
  }
  
  message("Submitting CORINE download request to the CLMS API...")
  request_body <- list(
    Datasets = list(
      list(
        DatasetID = corine_dataset_id,
        FileID = corine_file_id
      )
    )
  )
  
  request_response <- clms_request(
    url = paste0(clms_api_url, "/@datarequest_post"),
    method = "POST",
    body = request_body
  )
  
  task_id <- as.character(request_response$TaskIds$TaskID[1])
  if (length(task_id) == 0L || is.na(task_id) || task_id == "") {
    stop("The CLMS API did not return a valid task identifier.", call. = FALSE)
  }
  
  start_time <- Sys.time()
  download_url <- NULL
  
  repeat {
    status_response <- clms_request(
      url = paste0(clms_api_url, "/@datarequest_status_get?TaskID=", task_id)
    )
    
    status <- status_response$Status
    
    if (identical(status, "Finished_ok")) {
      download_url <- status_response$DownloadURL
      break
    }
    
    if (identical(status, "Finished_ko") || identical(status, "Canceled")) {
      stop("CLMS download request failed with status: ", status, call. = FALSE)
    }
    
    elapsed_sec <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed_sec > timeout_sec) {
      stop(
        "Timed out while waiting for the CLMS download to finish. ",
        "Last status was: ", status,
        call. = FALSE
      )
    }
    
    message("CLMS status: ", status, ". Waiting ", poll_interval, " seconds...")
    Sys.sleep(poll_interval)
  }
  
  if (!is.character(download_url) || length(download_url) != 1L || is.na(download_url) || download_url == "") {
    stop("The CLMS API did not provide a download URL for the CORINE archive.", call. = FALSE)
  }
  
  message("Downloading CORINE archive...")
  utils::download.file(download_url, destfile = zip_file, mode = "wb", quiet = FALSE)
  
  zip_listing <- utils::unzip(zip_file, list = TRUE)
  tif_candidates <- zip_listing$Name[grepl("\\.tif$", zip_listing$Name, ignore.case = TRUE)]
  target_tif <- tif_candidates[basename(tif_candidates) == "U2018_CLC2018_V2020_20u1.tif"]
  
  if (length(target_tif) == 0L) {
    target_tif <- tif_candidates[1]
  }
  if (length(target_tif) == 0L || is.na(target_tif)) {
    stop("No GeoTIFF file was found inside the downloaded CORINE archive.", call. = FALSE)
  }
  target_tif <- target_tif[1]
  
  extract_dir <- tempfile(pattern = "corine_extract_")
  dir.create(extract_dir, recursive = TRUE)
  utils::unzip(zip_file, files = target_tif, exdir = extract_dir)
  
  extracted_file <- file.path(extract_dir, target_tif)
  if (!file.exists(extracted_file)) {
    stop("The expected CORINE GeoTIFF could not be extracted from the archive.", call. = FALSE)
  }
  
  file.copy(extracted_file, local_file, overwrite = TRUE)
  unlink(extract_dir, recursive = TRUE)
  
  if (!keep_zip && file.exists(zip_file)) {
    unlink(zip_file)
  }
  
  message("CORINE raster saved to: ", normalizePath(local_file))
  local_file
}
