#' Merging components of renewable sources into single category
#'
#' Merging components of renewable sources into single category called
#' 'renewables'.
#'
#' @param data a dataframe or tibble containing all resource categories
#' @param components_to_merge a character string of (variable) names for energy
#'  sources (shares) corresponding to col-names of data which will be merged
#'  into the new "renewables" category
#' @param cumcap_to_merge a character string of cumulative-capacity (variable)
#'   names corresponding to col-names of data which will be merged into the new
#'   "renewables" category
#
#'
#' @return a dataframe or tibble; same as argument data but with renewable
#'   sources grouped into a single category, removed columns of
#'   'components_to_merge', and adjusted cumulative generating capacities.
#' @export
merge_into_renewables <- function(data,
                                  components_to_merge = NULL,
                                  cumcap_to_merge = NULL) {
  if (!is.null(components_to_merge)) {
    data_cats_merge <- data %>%
      dplyr::select(tidyselect::all_of(components_to_merge))
  } else {
    data_cats_merge <- NULL
  }
  if (!is.null(cumcap_to_merge)) {
    data_ccap_merge <- data %>%
      dplyr::select(tidyselect::all_of(cumcap_to_merge))
  } else {
    data_ccap_merge <- NULL
  }
  out_data <- data

  out_data <- out_data %>%
    dplyr::select(-tidyselect::all_of(components_to_merge))
  out_data <- out_data %>%
    dplyr::select(-tidyselect::all_of(cumcap_to_merge))
  if ("renewables" %in% names(data)) {
    if (!is.null(components_to_merge)) {
      out_data$renewables <- out_data$renewables + rowSums(data_cats_merge,
                                                           na.rm = TRUE)
    }
    if (!is.null(cumcap_to_merge)) {
      out_data$renew_cumcap <- out_data$renew_cumcap + rowSums(data_ccap_merge,
                                                               na.rm = TRUE)
    }
  } else {
    if (!is.null(components_to_merge)) {
      out_data$renewables   <- rowSums(data_cats_merge, na.rm = TRUE)
    }
    if (!is.null(cumcap_to_merge)) {
      out_data$renew_cumcap <- rowSums(data_ccap_merge, na.rm = TRUE)
    }
  }
  out_data_final <- out_data %>%
    dplyr::select(-tidyselect::matches("cumcap"))
  out_data_final <- cbind(out_data_final, out_data %>%
                            dplyr::select(tidyselect::matches("cumcap")))
  return(out_data_final)
}
#' Generates data which is used for estimation.
#'
#' The main additions to the raw data are:
#' \itemize{
#' \item{1.}{ a constant is added for all states and years}
#' \item{2.}{ the \code{xxx_avail} variable is set}
#' }
#' The latter varies  for all cross sectional units (all US states), and all
#' energy types!
#'
#' @param data either character string of the path to input data or the data
#'   object itself in form of a tibble or data.frame
#' @param data_gdp either character string of the path to input data or the data
#'   object itself, containing GDP data, in form of a tibble or data.frame
#' @param avail_thresholds a double matrix of dimension "us_states" x
#'   "energy_types" i.e. it contains for every row (for every US state) as
#'   columns/variables the corresponding energy threshold where the
#'   \code{xxx_avail} variable is set from 0 to 1. So if the 3rd row and 2nd
#'   column entry of this matrix is 1000, then, for the third US state which is
#'   passed, and for the second energy type, the corresponding threshold is 1000
#'   meaning that the dummy becomes =1 if the raw counts exceeds 1000, but is
#'   zero otherwise.
#'
#' @return the resulting data with addtional variabls 'constant' the
#'   \code{xxx_avail}s
#' @export
generate_data_for_estimation <- function(data,
                                         data_gdp,
                                         avail_thresholds) {
  if (is.character(data)) {
    uspp_out_new <- haven::read_dta(data)
  } else if (dplyr::is.tbl(data) || is.data.frame(data)) {
    uspp_out_new <- data
  } else {
    err_msg <- paste0("Argument data is provided in non-standard form: ",
                      "either path to data as character string, or tibble or ",
                      "data.frame required!")
    stop(err_msg)
  }

  num_obs <- nrow(uspp_out_new)

  n_env <- new.env(parent = emptyenv()) # n for 'names', hence identifier n_env!
  get_names_vars(n_env)

  # id_esources_present    <- which(n_env$names_esources_all %in% names(data))
  # names_esources_present <- n_env$names_esources_all[id_esources_present]
  # names_avail_present    <- n_env$names_avail_all[id_esources_present]
  # num_esources_present   <- length(names_esources_present)

  uspp_out_new <- uspp_out_new %>%
    generate_data_neg_adjusted(n_env = n_env) %>%
    generate_avail_vars(n_env = n_env) %>%
    generate_shares(n_env = n_env) %>%
    generate_other_vars(data_gdp) %>%
    generate_constant() %>%
    get_final_selection(n_env = n_env)
  return(uspp_out_new)
}
#' Adjust negative values for observed (megawatt) counts.
#'
#' Some of the observed megawatts contain negative entries which is strange.
#' This is the case for \code{NUEGB} (nuclear power) and \code{renewables}. The
#' current workaround is to switch the sign, assuming that this is wrong data
#' entry and the value should actually be positive. When the sign is switched,
#' the corresponding availability value smust be set to 1 (instead of zero, as
#' it was before). An alternative would is to set them to zero with no
#' adjustment to the availability dummy (stays at zero).
#'
#'
#' @param data old data set with negative entries for raw megawatts
#'   (currently affected are onlyh \code{NUEGB} and \code{renewables})
#' @param n_env environment containing the names of variables of energy
#'   sources
#' @param change_sign_avail logical with default \code{FALSE}; if \code{TRUE},
#'   then the variables "xxx_avail" (e.g. "coal_avail") get their sign switched
#'   from zero to one, whenever there is a negative entry (since the negative
#'   value becomes positive via \code{abs()}, the corresponding availability
#'   dummy must be equal to 1, not zero)
#'
#' @return new data set which is the old but with the adjustments made as
#'   outlined in the 'Description'.
#' @export
generate_data_neg_adjusted <- function(data,
                                       n_env,
                                       change_sign_avail = FALSE) {


  data_adj_neg           <- data
  id_esources_present    <- n_env$names_esources_all %in% names(data_adj_neg)
  id_esources_present    <- which(id_esources_present)
  names_esources_present <- n_env$names_esources_all[id_esources_present]

  num_negs_to_adjs <- length(names_esources_present)
  for (i in 1:num_negs_to_adjs) {
    id_change <- which(data_adj_neg[[names_esources_present[i]]] < 0)
    new_vals <- rep(0, times = length(id_change))
    # new_vals  <- abs(data_adj_neg[id_change, names_esources_present[i]])
    data_adj_neg[id_change, names_esources_present[i]] <- new_vals
    if (change_sign_avail) {
      data_adj_neg[id_change, n_env$names_avail_present[i]] <- 1
    }
  }
  return(data_adj_neg)
}
#' Adds shares to the data as well as avail-dummies and cumulative capacities
#'
#' @param data the dataset for which to compute shares, and add avail dummies,
#'   cummulative capacities and avail x cummulative capacities
#' @param n_env environment containing the names of variables of energy
#'   sources
#' @return data with shares for energy sources, avail-dummies and cumulative
#'   capacities in the "correct" variable order
#'
#' @export
generate_shares <- function(data, n_env) {
  id_esources_present    <- which(n_env$names_esources_all %in% names(data))

  names_esources_present      <- n_env$names_esources_all[id_esources_present]
  names_avail_present         <- n_env$names_avail_all[id_esources_present]
  names_cumcap_present        <- n_env$names_cumcap_all[id_esources_present]
  names_shares_present        <- paste0(names_esources_present, "_share")
  names_cumcap_shares_present <- paste0(names_cumcap_present, "_share")
  names_availcumcap_present <- n_env$names_avail_cumcap_all[id_esources_present]

  num_esources_present   <- length(names_esources_present)

  if (any(grepl("cumcap", names(data)))) {
    for (i in 1:num_esources_present) {
      current_formula <- paste0(names_avail_present[i],
                                " * ",
                                names_cumcap_present[i])
      current_formula <- parse(text = current_formula)
      data <- data %>%
        dplyr::mutate("{names_availcumcap_present[i]}" := eval(current_formula))
    }
  }
  uspp_shares <- data
  divide_shr  <- numeric(nrow(uspp_shares))
  divide_ccap <- numeric(nrow(uspp_shares))
  for (j in 1:num_esources_present) {
    divide_shr  <- divide_shr + uspp_shares[[names_esources_present[j]]]
    divide_ccap <- divide_ccap + uspp_shares[[names_cumcap_present[j]]]
  }
  for (i in 1:num_esources_present) {
    divide_tot <- uspp_shares[[names_esources_present[i]]] / divide_shr
    uspp_shares[[names_shares_present[i]]] <- divide_tot
    divide_tot <- uspp_shares[[names_cumcap_present[i]]] / divide_ccap
    uspp_shares[[names_cumcap_shares_present[i]]] <- divide_tot
  }
  uspp_shares[["resource_share_sum"]] <- numeric(nrow(uspp_shares))
  uspp_shares[["cumcap_share_sum"]]   <- numeric(nrow(uspp_shares))
  add_me_shr  <- numeric(nrow(uspp_shares))
  add_me_ccap <- numeric(nrow(uspp_shares))
  for (i in 1:num_esources_present) {
    add_me_shr  <- add_me_shr  + uspp_shares[[names_shares_present[i]]]
    add_me_ccap <- add_me_ccap + uspp_shares[[names_cumcap_shares_present[i]]]
  }
  uspp_shares[["resource_share_sum"]] <- add_me_shr
  uspp_shares[["cumcap_share_sum"]]   <- add_me_ccap
  return(uspp_shares)
}
#' Adds other variables to the dataset
#'
#' @param data the current dataset where to add additional variables
#' @param ... additional datasets with cross sectional identifier \code{state}
#'   and year identifier \code{year} to be merged (\code{dplyr::left_join()}),
#'   and remaining columns being the variables to add to the \code{data} dataset
#' @return data with shares for energy sources, avail-dummies and cumulative
#'   capacities in the "correct" variable order
#'
#' @export
generate_other_vars <- function(data, ...) {
  data_additional <- list(...)
  num_dataset <- length(data_additional)
  data_out <- data
  for (i in 1:num_dataset) {
    data_out <- dplyr::left_join(data_out,
                                 data_additional[[i]],
                                 by = c("state", "year"))
  }
  return(data_out)
}
#' Function that generates availability dummy
#'
#' Availability dummy equals one if resource is >0, but is zero else
#'
#' @param data current dataset as dataframe or tibble
#' @param n_env environment containing the names of variables of energy sources
#'
#' @return dataset as provided by the argument \code{data} but with additionally
#'   the availability dummy variable
#' @export
generate_avail_vars <- function(data, n_env) {
  num_obs <- nrow(data)
  id_esources_present    <- which(n_env$names_esources_all %in% names(data))
  names_esources_present <- n_env$names_esources_all[id_esources_present]
  num_esources_present   <- length(names_esources_present)
  names_avail_present    <- n_env$names_avail_all[id_esources_present]

  state_names <- unique(data$state)
  num_states  <- length(state_names)
  for (i in 1:num_esources_present) {
    data[n_env$names_avail_all[i]] <- rep(0, times = num_obs)
  }
  # avail_thresholds <- avail_thresholds[, names_esources_present]
  for (i in 1:num_states) {
    for (d in 1:num_esources_present) {
      tmp_thrsh <- data[data$state == state_names[i],
                        names_esources_present[[d]]] > 1
      tmp_thrsh <- as.numeric(tmp_thrsh)
      data[data$state == state_names[i], names_avail_present[d]] <- tmp_thrsh
    }
  }
  return(data)
}
#' Adds a constant to the dataset
#'
#' @param data current dataset as dataframe or tibble
#'
#' @return dataset as before but with a column of "ones" (i.e. a constant)
#' @export
generate_constant <- function(data) {
  num_obs <- nrow(data)
  data$const <- rep(1, times = num_obs)
  return(data)
}
#' Does a final selection on the dataset to get only relevant variables
#'
#' @param data current dataset as dataframe or tibble
#' @param n_env environment containing the names of variables of energy sources
#'
#' @return dataset as provided by the argument \code{data} but with only
#'   relevant variables selected (see function body)
#' @export
get_final_selection <- function(data, n_env) {
  id_esources_present    <- which(n_env$names_esources_all %in% names(data))

  names_esources_present      <- n_env$names_esources_all[id_esources_present]
  names_shares_present        <- paste0(names_esources_present, "_share")
  names_avail_present         <- n_env$names_avail_all[id_esources_present]
  names_cumcap_present        <- n_env$names_cumcap_all[id_esources_present]
  names_shares_present        <- paste0(names_esources_present, "_share")
  names_cumcap_shares_present <- paste0(names_cumcap_present, "_share")
  names_availcumcap_present <- n_env$names_avail_cumcap_all[id_esources_present]

  num_esources_present   <- length(names_esources_present)

  names_all <- rbind(names_esources_present,
                     names_shares_present,
                     names_cumcap_present,
                     names_cumcap_shares_present,
                     names_avail_present,
                     names_availcumcap_present)
  names_all <- as.vector(names_all)

  data_out <- data %>%
    dplyr::select(.data$state, .data$year,
                  tidyselect::any_of(names_all),
                  .data$cleid, .data$dfeid, .data$ngeid,
                  .data$gdp, # partlylib, envir,
                  .data$const,
                  .data$resource_share_sum,
                  .data$cumcap_share_sum) %>%
    sjlabelled::remove_label()
  return(data_out)
}
#' Adjust negative values for observed (megawatt) counts.
#'
#' @param env environment containing the names of variables of energy
#'   sources
#' @return invisible; plain side effect function assigning variables to caller
#'  environment
#'
#' @export
get_names_vars <- function(env) {
  renewables <- c("GEEGB", "SOEGB", "WYEGB", "HYEGB", "WWEIB")
  names_esources_all <-  c("CLEIB", "PAEIB", "NGEIB", "NUEGB",
                           "renewables",
                           renewables)
  names_colloquial   <- c("coal",
                          "petroleum",
                          "naturalgas",
                          "nuclear",
                          "renew",
                          "geothermal", "solar",
                          "wind", "hydro", "wood_waste")
  names_avail_all        <- paste0(names_colloquial, "_avail")
  names_cumcap_all       <- paste0(names_colloquial, "_cumcap")
  names_avail_cumcap_all <- paste0(names_colloquial, "_avail_cumcap")

  env$renewables             <- renewables
  env$names_esources_all     <- names_esources_all
  env$names_colloquial       <- names_colloquial
  env$names_avail_all        <- names_avail_all
  env$names_cumcap_all       <- names_cumcap_all
  env$names_avail_cumcap_all <- names_avail_cumcap_all

  return(invisible(NULL))
}
