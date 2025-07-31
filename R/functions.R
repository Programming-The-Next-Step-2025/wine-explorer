#' Loads wine data in a standardised form (project dataset or a given path)
#'
#' Returns only these standardised columns (in that order):
#' wine_id, wine_name, type, region, country, grape,
#' secondary_grapes, vintage, price, appellation, style, characteristics.
#'
#' Notes:
#' - Vintage is normalised: "NV" to NA; "2019/20" to 2019 (integer).
#' - Type is lower-cased and "rosé" is normalised to "rose").
#' - Region/Country are trimmed for stray spaces.
#'
#' @param path Path to the CSV. Defaults to the package file
#'   \code{inst/extdata/WineDataset.csv}.
#' @return A tibble with exactly the 12 standardised columns.
#' @examples
#' df <- vinexplorer::load_wine_data()
#' names(df); head(df)
#' @export
load_wine_data <- function(
    path = system.file("extdata", "WineDataset.csv", package = "vinexplorer")
) {
  if (is.null(path) || !file.exists(path)) {
    stop("Could not find wine data CSV. Put your file at inst/extdata/WineDataset.csv and reinstall the package.", call. = FALSE)
  }
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  nm <- names(df); tolow <- tolower(nm)
  find <- function(cands) {
    i <- match(tolower(cands), tolow, nomatch = 0)
    if (any(i > 0)) nm[i[i > 0][1]] else NULL
  }
  
  # Map headers to standardised schema
  col_wine_name <- find(c("Title"))
  col_type      <- find(c("Type"))
  col_region    <- find(c("Region"))
  col_country   <- find(c("Country"))
  col_grape     <- find(c("Grape"))
  col_secondary <- find(c("Secondary Grape Varieties"))
  col_vintage   <- find(c("Vintage"))
  col_price     <- find(c("Price"))
  col_appel     <- find(c("Appellation"))
  col_style     <- find(c("Style"))
  col_char      <- find(c("Characteristics"))
  
  out <- tibble::tibble(
    wine_id          = seq_len(nrow(df)),
    wine_name        = if (!is.null(col_wine_name)) df[[col_wine_name]] else NA_character_,
    type             = if (!is.null(col_type))      df[[col_type]]      else NA_character_,
    region           = if (!is.null(col_region))    df[[col_region]]    else NA_character_,
    country          = if (!is.null(col_country))   df[[col_country]]   else NA_character_,
    grape            = if (!is.null(col_grape))     df[[col_grape]]     else NA_character_,
    secondary_grapes = if (!is.null(col_secondary)) df[[col_secondary]] else NA_character_,
    vintage          = if (!is.null(col_vintage))   df[[col_vintage]]   else NA_character_,
    price            = if (!is.null(col_price))     df[[col_price]]     else NA_real_,
    appellation      = if (!is.null(col_appel))     df[[col_appel]]     else NA_character_,
    style            = if (!is.null(col_style))     df[[col_style]]     else NA_character_,
    characteristics  = if (!is.null(col_char))      df[[col_char]]      else NA_character_
  )
  
  # Normalisation
  out$type <- stringr::str_to_lower(stringr::str_trim(out$type))
  out$type <- dplyr::recode(out$type,
                            "rosé" = "rose", "rosé wine" = "rose",
                            "red wine" = "red", "white wine" = "white",
                            .default = out$type
  )
  
  if (is.character(out$price)) out$price <- readr::parse_number(out$price)
  
  out$vintage <- as.character(out$vintage)
  out$vintage[out$vintage %in% c("NV","nv","N/V")] <- NA_character_
  out$vintage <- sub("^([0-9]{4}).*$", "\\1", out$vintage)
  suppressWarnings(out$vintage <- as.integer(out$vintage))
  
  out$region  <- stringr::str_trim(out$region)
  out$country <- stringr::str_trim(out$country)
  
  out
}

#' Load region coordinates from inst/extdata/RegionCoordinates.csv
#'
#' Reads a CSV with columns \code{region, lat, lon} from inside the package.
#' Trims region names and ensures numeric lat/lon.
#'
#' @param path Optional path override. By default reads
#'   \code{inst/extdata/RegionCoordinates.csv}.
#' @return A tibble with columns \code{region, lat, lon}.
#' @examples
#' coords <- vinexplorer::load_region_coords()
#' head(coords)
#' @export
load_region_coords <- function(
    path = system.file("extdata", "RegionCoordinates.csv", package = "vinexplorer")
) {
  if (is.null(path) || !file.exists(path)) {
    stop("Region coordinates file not found. Expected inst/extdata/RegionCoordinates.csv.", call. = FALSE)
  }
  coords <- readr::read_csv(path, show_col_types = FALSE)
  names(coords) <- tolower(names(coords))
  req <- c("region","lat","lon")
  miss <- setdiff(req, names(coords))
  if (length(miss)) stop("Coordinates file missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  
  coords <- coords |>
    dplyr::transmute(
      region = stringr::str_trim(.data$region),
      lat = suppressWarnings(as.numeric(.data$lat)),
      lon = suppressWarnings(as.numeric(.data$lon))
    )
  coords
}

#' Filter wines by type, country, and/or region
#'
#' @param data A tibble from \code{load_wine_data()}.
#' @param types Optional character vector of types to keep.
#' @param country Optional single country to keep.
#' @param region Optional single region to keep.
#' @return A filtered tibble.
#' @examples
#' df <- vinexplorer::load_wine_data()
#' whites <- vinexplorer::filter_wines(df, types = "white")
#' reds_and_rose <- vinexplorer::filter_wines(df, types = c("red","rose"))
#' rioja_all <- vinexplorer::filter_wines(df, region = "Rioja")
#' @export
filter_wines <- function(data, types = NULL, country = NULL, region = NULL) {
  out <- data
  if (!is.null(types)) {
    types <- tolower(types)
    out <- dplyr::filter(out, .data$type %in% types)
  }
  if (!is.null(country)) out <- dplyr::filter(out, .data$country == country)
  if (!is.null(region))  out <- dplyr::filter(out, .data$region  == region)
  out
}

#' Load region coordinates from inst/extdata/RegionCoordinates.csv
#'
#' Reads a CSV with columns \code{region, lat, lon} from inside the package.
#' Trims region names and ensures numeric lat/lon.
#'
#' @param path Optional path override. By default reads
#'   \code{inst/extdata/RegionCoordinates.csv}.
#' @return A tibble with columns \code{region, lat, lon}.
#' @examples
#' coords <- vinexplorer::load_region_coords()
#' head(coords)
#' @export
load_region_coords <- function(
    path = system.file("extdata", "RegionCoordinates.csv", package = "vinexplorer")
) {
  if (is.null(path) || !file.exists(path)) {
    stop("Region coordinates file not found. Expected inst/extdata/RegionCoordinates.csv.", call. = FALSE)
  }
  coords <- readr::read_delim(path, delim = ";", show_col_types = FALSE)
  names(coords) <- tolower(names(coords))
  req <- c("region","lat","lon")
  miss <- setdiff(req, names(coords))
  if (length(miss)) stop("Coordinates file missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  
  coords <- coords |>
    dplyr::transmute(
      region = stringr::str_trim(.data$region),
      lat = suppressWarnings(as.numeric(.data$lat)),
      lon = suppressWarnings(as.numeric(.data$lon))
    )
  coords
}
