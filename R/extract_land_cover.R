#' Calculate land-cover proportions around coordinates
#'
#' @description
#' Function to extract the proportion of land cover around a given set of
#' coordinates. By default it uses ESA WorldCover 2021 class codes, but it can
#' also work with CORINE Land Cover rasters or any other categorical land-cover
#' raster provided by the user.
#'
#' For more information on the land cover datasets used here, check:
#' \itemize{
#'   \item ESA WorldCover 2021: https://worldcover2021.esa.int/
#'   \item CORINE Land Cover: https://land.copernicus.eu/en/products/corine-land-cover
#' }
#'
#' @param coords data.frame or matrix with columns lon, lat (WGS84)
#' @param raster_path Path to a raster file or a \code{terra::SpatRaster}.
#' @param radius_m Numeric buffer radius in meters.
#' @param dataset Character string indicating which land-cover classification is
#'   being used. Supported options are \code{"esawc21"}, \code{"corine"}, and
#'   \code{"custom"}.
#' @param cat_vals Integer vector of raster values to treat as the land-cover
#'   category of interest. If \code{NULL}, dataset-specific defaults are used:
#'   ESA WorldCover 2021 uses class \code{50} (built-up), and CORINE uses the
#'   artificial-surface classes \code{111, 112, 121, 122, 123, 124, 131, 132,
#'   133, 141, 142}.
#' @return data.frame with lon, lat, radius_m, dataset, and one output column.
#'   For \code{dataset = "esawc21"}, the output column is
#'   \code{percent_impervious}. For \code{dataset = "corine"} and
#'   \code{dataset = "custom"}, the output column is \code{percent_land_cover}.
#' @export
extract_land_cover <- function(
  coords,
  raster_path,
  radius_m,
  dataset = c("esawc21", "corine", "custom"),
  cat_vals = NULL
) {
  dataset <- base::match.arg(dataset)
  coords <- base::as.data.frame(coords)
  required_cols <- c("lon", "lat")

  if (!base::all(required_cols %in% base::names(coords))) {
    base::stop(
      "`coords` must contain columns named `lon` and `lat`.",
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

  if (base::is.null(cat_vals)) {
    cat_vals <- switch(
      dataset,
      esawc21 = 50L,
      corine = c(
        111L,
        112L,
        121L,
        122L,
        123L,
        124L,
        131L,
        132L,
        133L,
        141L,
        142L
      ),
      custom = base::stop(
        "When `dataset = \"custom\"`, you must supply `cat_vals`.",
        call. = FALSE
      )
    )
  }
  cat_vals <- base::as.integer(cat_vals)
  output_col <- switch(
    dataset,
    esawc21 = "percent_urban",
    corine = "percent_urban",
    custom = "percent_urban"
  )

  rast <- if (base::inherits(raster_path, "SpatRaster")) {
    raster_path
  } else {
    terra::rast(raster_path)
  }

  if (identical(dataset, "corine")) {
    source_path <- if (base::inherits(raster_path, "SpatRaster")) {
      terra::sources(rast)[1]
    } else {
      raster_path
    }
    vat_path <- paste0(source_path, ".vat.dbf")

    # CORINE rasters are often stored with compact internal values (1:48) and
    # the original CLC codes (111, 112, 121, ...) in the VAT sidecar table.
    if (base::all(cat_vals > 100L) && file.exists(vat_path)) {
      if (!requireNamespace("foreign", quietly = TRUE)) {
        base::stop(
          "Package `foreign` is required to read the CORINE VAT table.",
          call. = FALSE
        )
      }

      vat <- foreign::read.dbf(vat_path, as.is = TRUE)
      if (base::all(c("Value", "CODE_18") %in% base::names(vat))) {
        vat$Value <- base::as.integer(vat$Value)
        vat$CODE_18 <- base::as.integer(vat$CODE_18)
        mapped_vals <- vat$Value[vat$CODE_18 %in% cat_vals]
        if (base::length(mapped_vals) > 0L) {
          cat_vals <- base::sort(base::unique(mapped_vals))
        }
      }
    }

    # Fallback for the standard CORINE 2018 raster when the VAT sidecar is not
    # available next to the GeoTIFF.
    if (base::all(cat_vals > 100L)) {
      corine_default_map <- c(
        `111` = 1L,
        `112` = 2L,
        `121` = 3L,
        `122` = 4L,
        `123` = 5L,
        `124` = 6L,
        `131` = 7L,
        `132` = 8L,
        `133` = 9L,
        `141` = 10L,
        `142` = 11L
      )
      mapped_vals <- base::unname(corine_default_map[base::as.character(cat_vals)])
      mapped_vals <- mapped_vals[!base::is.na(mapped_vals)]
      if (base::length(mapped_vals) > 0L) {
        cat_vals <- base::sort(base::unique(base::as.integer(mapped_vals)))
      }
    }
  }

  pts <- sf::st_as_sf(
    coords,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
  pts_m <- sf::st_transform(pts, 3857)
  buffers_m <- sf::st_buffer(pts_m, dist = radius_m)

  rast_crs <- terra::crs(rast, proj = TRUE)
  if (base::is.na(rast_crs) || rast_crs == "") {
    base::stop("Raster has no CRS defined.", call. = FALSE)
  }
  buffers_rastcrs <- sf::st_transform(buffers_m, crs = rast_crs)

  results <- base::lapply(
    base::seq_len(base::nrow(buffers_rastcrs)),
    function(i) {
      poly <- buffers_rastcrs[i, ]
      ex <- exactextractr::exact_extract(
        rast,
        poly,
        include_cell = FALSE,
        progress = FALSE
      )[[1]]

      if (base::is.null(ex) || base::nrow(ex) == 0) {
        out_i <- base::data.frame(value = NA_real_)
        base::names(out_i) <- output_col
        return(out_i)
      }

      is_target <- ex$value %in% cat_vals
      cov <- ex$coverage_fraction
      percent <- base::sum(cov[is_target], na.rm = TRUE) /
        base::sum(cov, na.rm = TRUE) *
        100

      out_i <- base::data.frame(value = percent)
      base::names(out_i) <- output_col
      out_i
    }
  )

  res_df <- dplyr::bind_rows(results)

  out <- base::cbind(
    sf::st_drop_geometry(pts)[, c(
      "lon",
      "lat",
      base::setdiff(base::names(sf::st_drop_geometry(pts)), c("lon", "lat"))
    )],
    radius_m = radius_m,
    dataset = dataset,
    res_df
  )

  out
}
