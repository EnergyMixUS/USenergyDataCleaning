#' Get data from the EIA webpage
#'
#' @param key key to access EIA web API via R (can be requested from the
#'   webpage
#'
#' @return a dataset
#' @export
get_data_eia <- function(key = NULL) {
  if (!is.null(key)) {
    eia::eia_set_key(key)
  } else {
    eia::eia_set_key("cd59d4adcef436d3f0b8841664f41b93")
  }

  info_paeib <- eia::eia_cats("40596")$childseries[, 1, drop = TRUE]

  state_codes <- stringr::str_match_all(info_paeib, "\\...\\..$") %>%
    stringr::str_extract(pattern = "\\...\\..$") %>%
    stringr::str_replace_all("\\.A$", "") %>%
    stringr::str_replace_all("\\.", "")

  resource_series_mat <- generate_resource_series(state_codes)
  price_series_mat    <- generate_price_series(state_codes)

  out_resources <- get_resource_consumption(series_mat = resource_series_mat,
                                            state_codes = state_codes)
  out_prices    <- get_resource_prices(series_mat = price_series_mat,
                                       state_codes = state_codes)

  out_resources_prices <- dplyr::left_join(out_resources,
                                           out_prices,
                                           by = c("state", "year"))

  return(out_resources_prices)

  # print(eia::eia_cats())
  # print(eia::eia_cats("40203"))
  # print(eia::eia_cats("40204"))
  # print(eia::eia_cats("40213"))
  # print(eia::eia_cats("40213")$childcategories)

  # print(eia::eia_cats("40596")) # PAEIB = oil
  # print(eia::eia_cats("40299")) # CLEIB = coal
  #
  # print(eia::eia_cats("40858")) # GEEGB = geothermal
  # print(eia::eia_cats("40821")) # NUEGB = nuclear
  # print(eia::eia_cats("40386")) # SOEGB = solar
  # print(eia::eia_cats("40325")) # WYEGB = wind
  #
  # print(eia::eia_cats("40826")) # HYEGB = hydropower
  # print(eia::eia_cats("40602")) # NGEIB = natural gas
  # print(eia::eia_cats("40343")) # WWEIB = wood and waste

  # print(eia::eia_cats("40302")) # CLEID
  # # Coal

  # print(eia::eia_cats("40697")) # DFEID
  # # Distillate Fuel Oil

  # print(eia::eia_cats("40693")) # PAEID
  # # All Petroleum Products

  # print(eia::eia_cats("40695")) # NGEID
  # # 40695 Natural Gas including Supplemental Gaseous Fuels

  # print(eia::eia_cats("40694")) # NUEGD
  # # Nuclear fuel price in the electric power sector, Arkansa

  # print(eia::eia_cats("40342")) # WWEID
  # # Wood and Waste
}
generate_resource_series <- function(state_codes) {
  renewables <- c("GEEGB", "SOEGB", "WYEGB", "HYEGB", "WWEIB")
  names_resources <-  c("CLEIB", "PAEIB", "NGEIB", "NUEGB", renewables)
  series_tmp <- paste0("SEDS.",
                       names_resources, ".")
  series_mat <- matrix(nrow = length(state_codes), ncol = length(series_tmp))
  colnames(series_mat) <- names_resources

  for (i in 1:length(series_tmp)) {
    series_mat[, i] <- paste0(series_tmp[i], state_codes, ".A")
  }
  return(series_mat)
}
generate_price_series <- function(state_codes) {
  names_prices <-  c("CLEID", "DFEID", "PAEID", "NGEID", "NUEGD", "WWEID")
  series_tmp <- paste0("SEDS.",
                       names_prices, ".")
  series_mat <- matrix(nrow = length(state_codes), ncol = length(series_tmp))
  colnames(series_mat) <- tolower(names_prices)

  for (i in 1:length(series_tmp)) {
    series_mat[, i] <- paste0(series_tmp[i], state_codes, ".A")
  }
  return(series_mat)
}
get_resource_consumption <- function(series_mat, state_codes) {
  names_resources <- colnames(series_mat)
  num_resources   <- length(names_resources)
  num_states <- length(state_codes)

  out_states   <- list(num_states)
  out_resource <- list(num_resources)
  for (j in 1:num_states) {
    print(paste0("Retrieve data on state ", state_codes[j], " for resource:"))
    for (i in 1:num_resources) {
      tmp_series_full_info <- eia::eia_series(series_mat[j, i])
      tmp_start  <- tmp_series_full_info$start
      tmp_end    <- tmp_series_full_info$end
      tmp_years  <- data.frame(year = seq(from = tmp_start,
                                          to = tmp_end,
                                          by = 1))
      tmp_series <- tmp_series_full_info$data[[1]] %>%
        dplyr::arrange(.data$year)
      tmp_series <- tmp_series[, 1]
      out_resource[[i]]  <- data.frame(tmp_years, tmp_series)
      print(names_resources[i])
    }
    out_states[[j]] <- purrr::reduce(out_resource,
                                     dplyr::inner_join,
                                     by = c("year"))
    out_states[[j]] <- cbind("state" = state_codes[j], out_states[[j]])
    colnames(out_states[[j]]) <- c("state", "year", names_resources)
  }
  out_final <- purrr::reduce(out_states,
                             dplyr::bind_rows)
  return(tibble::as_tibble(out_final))
}
get_resource_prices <- function(series_mat, state_codes) {
  names_prices <- colnames(series_mat)
  num_prices   <- length(names_prices)
  num_states <- length(state_codes)

  out_states    <- list(num_states)
  out_prices <- list(num_prices)
  for (j in 1:num_states) {
    print(paste0("Retrieve data on state ", state_codes[j], " for prices:"))
    for (i in 1:num_prices) {
      tmp_series_full_info <- eia::eia_series(series_mat[j, i])
      tmp_start  <- tmp_series_full_info$start
      tmp_end    <- tmp_series_full_info$end
      tmp_years  <- data.frame(year = seq(from = tmp_start,
                                          to = tmp_end,
                                          by = 1))
      tmp_series <- tmp_series_full_info$data[[1]] %>%
        dplyr::arrange(.data$year)
      tmp_series <- tmp_series[, 1]
      out_prices[[i]]  <- data.frame(tmp_years, tmp_series)
      print(names_prices[i])
    }
    out_states[[j]] <- purrr::reduce(out_prices,
                                     dplyr::inner_join,
                                     by = c("year"))
    out_states[[j]] <- cbind("state" = state_codes[j], out_states[[j]])
    colnames(out_states[[j]]) <- c("state", "year", names_prices)
  }
  out_final <- purrr::reduce(out_states,
                             dplyr::bind_rows)
  return(tibble::as_tibble(out_final))
}
#' Preleaning raw cumulative generating capacities data
#'
#' Preleaning raw cumulative generating capacities data
#'
#' @param path_to_data character string containing the path to the raw
#'  cumulative generating capacities data
#'
#' @return list of two; precleaned cumulative capacities data with first element
#'  being the operational power plant dataset and the second the one containing
#'  the retired power plants
#'
#' @export
get_cumcap_precleaned <- function(path_to_data) {
  data_ccap_op  <- readxl::read_xlsx(path = path_to_data,
                                     sheet = 1,
                                     range = cellranger::cell_cols(c(5, 35)))
  data_ccap_op  <- data_ccap_op %>% dplyr::select(.data$State,
                                                  .data$`Operating Year`,
                                                  .data$Technology,
                                                  .data$`Nameplate Capacity (MW)`)
  names(data_ccap_op) <- c("state", "year_op",
                           "technology", "nameplate_capacity")
  data_ccap_re  <- readxl::read_xlsx(path = path_to_data,
                                     sheet = 3,
                                     range = cellranger::cell_cols(c(5, 29)))
  data_ccap_re  <- data_ccap_re %>% dplyr::select(.data$State,
                                                  .data$`Operating Year`,
                                                  .data$`Retirement Year`,
                                                  .data$Technology,
                                                  .data$`Nameplate Capacity (MW)`)
  names(data_ccap_re) <- c("state", "year_op", "year_re",
                           "technology", "nameplate_capacity")

  data_ccap_op <- data_ccap_op %>%
    dplyr::filter(!is.na(.data$state),
                  !is.na(.data$year_op),
                  !is.na(.data$technology),
                  !is.na(.data$nameplate_capacity))
  data_ccap_re <- data_ccap_re %>%
    dplyr::filter(!is.na(.data$state),
                  !is.na(.data$year_op),
                  !is.na(.data$year_re),
                  !is.na(.data$technology),
                  !is.na(.data$nameplate_capacity))
  out <- list(data_ccap_op, data_ccap_re)
  return(out)
}
#' Transform from raw cumulative capacities dataset to state aggregated version
#'
#' Transform from raw cumulative capacities dataset to state aggregated version
#'
#' @param path_to_data character string containing the path to the raw
#'  cumulative generating capacities data'
#'
#' @param year_range range of years for the returned dataset; e.g. 1970 to 2018
#'  for the current energy application because we have energy consumption and
#'  fuel prices for that period
#'
#' @return a dataset of aggregated cumulative generating capacites on state
#'  level but with unaggregated technologies
#' @export
get_cumcap_data <- function(path_to_data,
                            year_range = 1970:2018) {
  data_ccap_raw             <- get_cumcap_precleaned(path_to_data)
  data_ccap_raw_operational <- data_ccap_raw[[1]]
  data_ccap_raw_retired     <- data_ccap_raw[[2]]

  year_range <- as.character(year_range)

  tech_avail_op <- unique(data_ccap_raw_operational$technology)
  tech_avail_re <- unique(data_ccap_raw_retired$technology)
  tech_avail_re <- tech_avail_re[!is.na(tech_avail_re)]
  if (!all(tech_avail_re %in% tech_avail_op)) {
    stop("ambigous technology naming in operational and retired datasets: I.")
  } else if (all(tech_avail_re %in% tech_avail_op)) {
    names_tech   <- tech_avail_op
  } else {
    stop("ambigous technology naming in operational and retired datasets: II.")
  }

  names_states <- unique(data_ccap_raw_operational$state)

  num_states <- length(names_states)
  num_tech   <- length(names_tech)
  num_years  <- length(year_range)

  data_out <- get_empty_out_data(names_states, names_tech, year_range,
                                 num_states, num_tech, num_years)

  year_range <- as.numeric(year_range)

  for (i in seq_len(num_states)) {
    state_current <- names_states[i]
    tmp_ccap   <- get_initial_ccaps(data_ccap_raw_operational,
                                    data_ccap_raw_retired,
                                    names_tech = names_tech,
                                    year = year_range[1],
                                    state = state_current)
    id_state <- which(data_out$state == state_current)
    id_rows  <- intersect(id_state,
                          which(data_out$year == year_range[1]))
    id_cols  <- which(names(data_out) %in% names(tmp_ccap))

    data_out[id_rows, id_cols] <- as.list(tmp_ccap)

    for (t in year_range[-1]) {
      tmp_ccap <- add_ccap_op(data_ccap_raw_operational,
                              data_ccap_raw_retired,
                              names_tech, t, state_current)
      id_rows  <- intersect(id_state,
                            which(data_out$year == t))
      id_cols  <- which(names(data_out) %in% names(tmp_ccap))
      data_out[id_rows, -c(1, 2)] <- data_out[id_rows - 1, -c(1, 2)]
      data_out[id_rows, id_cols]  <- data_out[id_rows, id_cols] + tmp_ccap

    }
    cat(paste0("Adding capacities for state ",
               crayon::green(state_current), "!\n"))
  }
  names_coal_ccap <- names(data_out)[grep("coal",
                                          names(data_out),
                                          ignore.case = TRUE)]
  names_ngas_ccap <- names(data_out)[grep("natural gas",
                                          names(data_out),
                                          ignore.case = TRUE)]
  names_nucl_ccap <- names(data_out)[grep("nuclear",
                                          names(data_out),
                                          ignore.case = TRUE)]
  names_hydr_ccap <- names(data_out)[grep("hydro",
                                          names(data_out),
                                          ignore.case = TRUE)][1]
  names_petr_ccap <- names(data_out)[grep("petroleum",
                                          names(data_out),
                                          ignore.case = TRUE)]
  pattern_current <- "(Wood|Solar|Waste|Wind|Geotherm|Fly)"
  names_renw_ccap <- names(data_out)[grep(pattern_current,
                                          names(data_out),
                                          ignore.case = TRUE)]
  names_ccap <- list(names_coal_ccap = names_coal_ccap,
                     names_ngas_ccap = names_ngas_ccap,
                     names_nucl_ccap = names_nucl_ccap,
                     names_hydr_ccap = names_hydr_ccap,
                     names_petr_ccap = names_petr_ccap,
                     names_renw_ccap = names_renw_ccap)
  nms <- combn(names(names_ccap) , 2 ,
               FUN = paste0,
               collapse = "" , simplify = FALSE )
  ll <- combn(nms , 2 , simplify = FALSE )
  intersction_check <- lapply(ll,
                              function(x){length(intersect(x[[1]] , x[[2]]))})
  if (sum(unlist(intersction_check)) != 0) {
    stop("Intersection in pairwise ccap category names!")
  }
  cat(paste0("Unassigned ccaps: ",
             crayon::red(setdiff(names_tech, unlist(names_ccap))), "!\n"))

  data_ccap_coal_tmp <- data_out %>%
    dplyr::select(tidyselect::any_of(names_coal_ccap)) %>%
    dplyr::mutate(coal_cumcap = rowSums(.)) %>%
    dplyr::pull(.data$coal_cumcap)

  data_ccap_ngas_tmp <- data_out %>%
    dplyr::select(tidyselect::any_of(names_ngas_ccap)) %>%
    dplyr::mutate(naturalgas_cumcap = rowSums(.)) %>%
    dplyr::pull(.data$naturalgas_cumcap)

  data_ccap_nucl_tmp <- data_out %>%
    dplyr::select(tidyselect::any_of(names_nucl_ccap)) %>%
    dplyr::mutate(nuclear_cumcap = rowSums(.)) %>%
    dplyr::pull(.data$nuclear_cumcap)

  data_ccap_hydro_tmp <- data_out %>%
    dplyr::select(tidyselect::any_of(names_hydr_ccap)) %>%
    dplyr::mutate(hydro_cumcap = rowSums(.)) %>%
    dplyr::pull(.data$hydro_cumcap)

  data_ccap_petr_tmp <- data_out %>%
    dplyr::select(tidyselect::any_of(names_petr_ccap)) %>%
    dplyr::mutate(oil_cumcap = rowSums(.)) %>%
    dplyr::pull(.data$oil_cumcap)

  data_ccap_renw_tmp <- data_out %>%
    dplyr::select(tidyselect::any_of(names_renw_ccap)) %>%
    dplyr::mutate(renew_cumcap = rowSums(.)) %>%
    dplyr::pull(.data$renew_cumcap)

  data_out <- data_out %>%
    dplyr::mutate(coal_cumcap = data_ccap_coal_tmp) %>%
    dplyr::mutate(naturalgas_cumcap = data_ccap_ngas_tmp) %>%
    dplyr::mutate(nuclear_cumcap = data_ccap_nucl_tmp) %>%
    dplyr::mutate(hydro_cumcap = data_ccap_hydro_tmp) %>%
    dplyr::mutate(petroleum_cumcap = data_ccap_petr_tmp) %>%
    dplyr::mutate(renew_cumcap = data_ccap_renw_tmp) %>%
    dplyr::select(.data$state, .data$year,
                  tidyselect::ends_with("cumcap"))
  return(data_out)
}
get_empty_out_data <- function(names_states, names_tech, year_range,
                               num_states, num_tech, num_years) {
  data_out   <- c(list(names_states),
                  rep(list(rep(0, times = num_states)),
                      times = num_years),
                  rep(list(rep(0, times = num_states)),
                      times  = num_tech))
  data_out   <- data.frame(data_out)
  names(data_out) <- c("state", year_range, names_tech)

  data_out <- tidyr::pivot_longer(data_out,
                                  cols = tidyselect::all_of(year_range),
                                  names_to = "year") %>%
    dplyr::select(.data$state, .data$year, dplyr::everything()) %>%
    dplyr::select(-c(.data$value)) %>%
    dplyr::mutate_at(c("year"), as.numeric)
  return(data_out)
}
get_initial_ccaps <- function(...,
                              names_tech, year, state) {
  data_ccap_raw <- list(...)
  num_datasets  <- length(data_ccap_raw)
  ccaps_adds    <- vector("list", num_datasets)
  for (i in 1:num_datasets) {
    tmp_data_ccap_op <- data_ccap_raw[[i]] %>%
      dplyr::filter(.data$year_op <= year, state == {{state}})
    tmp_ccap <- tmp_data_ccap_op %>%
      dplyr::group_by(.data$technology) %>%
      dplyr::summarise(ccap = sum(.data$nameplate_capacity))
    ccaps_adds[[i]] <- tmp_ccap
  }
  # if (!isTRUE(all.equal(sort(ccaps_adds[[1]]$technology), sort(ccaps_adds[[2]]$technology)))) {
  #   browser()
  # }
  tmp_ccap <- ccaps_adds[[1]]
  for (i in 2:(num_datasets)) {
    tmp_ccap <- dplyr::full_join(tmp_ccap,
                                 ccaps_adds[[i]],
                                 by = "technology")
  }
  tmp_ccap <- tmp_ccap[order(match(tmp_ccap$technology, names_tech)), ]
  tmp_ccap <- tmp_ccap %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(ccap_all = rowSums(.[2:(num_datasets + 1)]))
  out      <- tmp_ccap$ccap_all

  names(out) <- tmp_ccap$technology
  return(out)
}
add_ccap_op <- function(data_ccap_raw_operational,
                        data_ccap_raw_retired,
                        names_tech, year, state) {
  tmp_data_ccap_op <- data_ccap_raw_operational %>%
    dplyr::filter(.data$year_op == {{year}}, state == {{state}})
  tmp_data_ccap_re_add <- data_ccap_raw_retired %>%
    dplyr::filter(.data$year_op == {{year}}, state == {{state}})
  tmp_data_ccap_re_sub <- data_ccap_raw_retired %>%
    dplyr::filter(.data$year_re == {{year}}, .data$state == {{state}})

  # if (!(nrow(tmp_data_ccap_re_sub) == 0)) {
  #   browser()
  # }
  tmp_ccap_op <- tmp_data_ccap_op %>%
    dplyr::group_by(.data$technology) %>%
    dplyr::summarise(ccap_add = sum(.data$nameplate_capacity))
  tmp_ccap_re_add <- tmp_data_ccap_re_add %>%
    dplyr::group_by(.data$technology) %>%
    dplyr::summarise(ccap_add2 = sum(.data$nameplate_capacity))
  tmp_ccap_re_sub <- tmp_data_ccap_re_sub %>%
    dplyr::group_by(.data$technology) %>%
    dplyr::summarise(ccap_sub = sum(.data$nameplate_capacity))
  tmp_ccap_re_sub$ccap_sub <- tmp_ccap_re_sub$ccap_sub * (-1)
  tmp_ccap <- dplyr::full_join(tmp_ccap_op, tmp_ccap_re_add, by = "technology")
  tmp_ccap <- dplyr::full_join(tmp_ccap, tmp_ccap_re_sub, by = "technology")
  tmp_ccap <- tmp_ccap[order(match(tmp_ccap$technology, names_tech)), ]
  tmp_ccap <- tmp_ccap %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(ccap_all = rowSums(.[2:4]))

  out <- tmp_ccap$ccap_all
  names(out) <- tmp_ccap$technology

  return(out)
}
#' Generates tidy GDP data in long format from a raw (untidy) dataset
#'
#' @param path_to_data character string specifying path to GDP data without
#'   trailing '/'
#'
#' @return a tibble/dataframe of GDP data in long format
#' @export
get_clean_data_gdp <- function(path_to_data) {
  data_out <- readxl::read_excel(path_to_data) %>%
    dplyr::filter(!is.na(.data$Area)) %>%
    dplyr::select(3:60) %>%
    tidyr::pivot_longer(cols = -c(1), names_to = "year") %>%
    dplyr::arrange(.data$code)
  names(data_out)[c(1, 3)] <- c("state", "gdp")
  data_out[[2]] <- as.numeric(data_out[[2]])
  return(data_out)
}
