#' Function that computes shares of the energy types.
#'
#' @param path_to_data character string giving the path to the data
#' @param as_fractions logical; if \code{TRUE}, then shares are returned as
#'  fractions rather than levels
#' @return original dataset with additional variables:
#'   \itemize{
#'     \item{\code{CLEIB_share}:}{ share of coal}
#'     \item{\code{NGEIB_share}:}{ share of natural gas}
#'     \item{\code{PAEIB_share}:}{ share of petroleum}
#'     \item{\code{NUEGB_share}:}{ share of nuclear power}
#'     \item{\code{renewables_share}:}{ share of renewables}
#'   }
#'   where the latter may come in unaggregated form i.e. there are additional
#'   variables:
#'   \itemize{
#'     \item{\code{GEEGB_share}:}{ share of geothermal}
#'     \item{\code{SOEGB_share}:}{ share of soalr}
#'     \item{\code{WYEGB_share}:}{ share of ?}
#'     \item{\code{HYEGB_share}:}{ share of hydropower}
#'     \item{\code{WWEIB_share}:}{ share of hydropower}
#'    }
#' @export
generate_data_to_plot_shares <- function(path_to_data, as_fractions) {
  if (any(grepl("\\.dta", path_to_data))) {
    data_all <- haven::read_dta(path_to_data)
  } else if (any(grepl("\\.csv", path_to_data))) {
    data_all <- utils::read.csv(path_to_data)
  } else if (is.data.frame(path_to_data) || tibble::is_tibble(path_to_data)) {
    data_all <- path_to_data
  } else {
    stop("Could not read data.")
  }
  renewables <- c("GEEGB", "SOEGB", "WYEGB", "HYEGB", "WWEIB")
  names_esources_all <-  c("CLEIB", "PAEIB", "NGEIB", "NUEGB",
                           "renewables",
                           renewables)
  if (as_fractions) {
    names_esources_all <- paste0(names_esources_all, "_share")
    data_shares <- data_all %>%
      dplyr::select(.data$state, .data$year,
                    tidyselect::any_of(names_esources_all))
  } else {
    data_shares <- data_all %>%
      dplyr::select(.data$state, .data$year,
                    tidyselect::any_of(names_esources_all))
  }
  return(data_shares)
}
#' Function that returns the states, years and prices of the data argument.
#'
#' @param path_to_data character string giving the path to the data
#'
#' @return a data.frame/tibble that contains states, years and energy prices
#' @export
generate_data_to_plot_prices <- function(path_to_data) {
  if (any(grepl("\\.dta", path_to_data))) {
    data_all <- haven::read_dta(path_to_data)
  } else if (any(grepl("\\.csv", path_to_data))) {
    data_all <- utils::read.csv(path_to_data)
  } else if (is.data.frame(path_to_data) || tibble::is_tibble(path_to_data)) {
    data_all <- path_to_data
  } else {
    stop("Could not read data.")
  }
  data_prices <- data_all %>%
    dplyr::select(.data$state, .data$year,
                  .data$cleid, .data$dfeid, .data$ngeid)
  return(data_prices)
}
#' Function that returns the states, years and prices of the data argument.
#'
#' @param path_to_data character string giving the path to the data
#' @param as_fractions logical; if \code{TRUE}, then cumcaps are returened as
#'  fractions rather than levels
#'
#' @return a data.frame/tibble that contains states, years and cumulative
#'  generating capacities
#' @export
generate_data_to_plot_ccap <- function(path_to_data, as_fractions = NULL) {
  if (is.null(as_fractions)) {
    stop("Please specify is cumcap should be used as fractions.")
  }
  if (any(grepl("\\.dta", path_to_data))) {
    data_all <- haven::read_dta(path_to_data)
  } else if (any(grepl("\\.csv", path_to_data))) {
    data_all <- utils::read.csv(path_to_data)
  } else if (is.data.frame(path_to_data) || tibble::is_tibble(path_to_data)) {
    data_all <- path_to_data
  } else {
    stop("Could not read data.")
  }
  if (as_fractions) {
    data_ccap <- data_all %>%
      dplyr::select(.data$state, .data$year,
                   tidyselect::matches("cumcap_share$"))
  } else {
    data_ccap <- data_all %>%
      dplyr::select(.data$state, .data$year,
                    tidyselect::matches("cumcap$"))
  }
  return(data_ccap)
}
