#' Return the problematic cases for raw or final data sets
#'
#' Return problematic cases for raw or final data sets for analysis purposes.
#' Helps to justify if shares should be merged (e.g. to merge hydropower into a
#' component of renewable sources).
#'
#' Possible problematic cases for raw or final data sets can be:
#' \itemize{
#'   \item{negatives: negative values for energy consumption not allowed}
#'   \item{zeros: zero values for energy consumption possible, but annoying}
#'   \item{missing values for raw consumption units}
#'   \item{missing values for shares of energy consumption (mix): occurs e.g. if
#'   divided by a sum of energy consumption which is zero (nothing produced,
#'   recorded)}
#'   \item{cases where cumulative capacities are zero, though the corresponding
#'   resource is consumed are returned}
#' }
#'
#' @param data a data frame or tibble with raw data
#' @param negatives logical; if \code{TRUE}, negative cases are returned
#' @param zeros logical; if \code{TRUE}, zero cases are returned
#' @param missing_raw logical; if \code{TRUE}, missing values for raw
#'   consumption units are returned
#' @param missing_shares logical; if \code{TRUE},  missing values for shares are
#'   returned
#' @param cumcap_wrong logical; if \code{TRUE}, cases where cumulative
#'   capacities are zero, though the corresponding resource is consumed are
#'   returned
#'
#' @return A named list with elements 'negatives', 'zeros', 'missing_raw',
#'   'missing_shares', 'cumcap_wrong'.
#' @export
view_problematic <- function(data,
                             negatives,
                             zeros,
                             missing_raw,
                             missing_shares,
                             cumcap_wrong) {
  if(cumcap_wrong) {
    zeros <- TRUE
  }
  problematic_cases_names <- c("negative values", "zero values",
                               "missing raw values",
                               "missing share values",
                               "cummulative capacities")
  # browser()
  n_env <- new.env(parent = emptyenv())
  USenergyDataCleaning::get_names_vars(n_env)
  id_esources_present    <- which(n_env$names_esources_all %in% names(data))
  names_esources_present <- n_env$names_esources_all[id_esources_present]
  names_cumcap_present   <- n_env$names_cumcap_all[id_esources_present]

  is_resource_negative <- function(data_resources) {
    return(lapply(data_resources,
                  function(x) {
                    which(x < 0)
                  }))
  }
  is_resource_zero <- function(data_resources) {
    return(lapply(data_resources,
                  function(x) {
                    which(x == 0)
                  }))
  }
  is_resource_na <- function(data_resources) {
    return(lapply(data_resources,
                  function(x) {
                    which(is.na(x))
                  }))
  }
  is_resource_cumcap_wrong <- function(data_resources, data_cumcap) {
    check_dims <- c(ncol(data_resources), ncol(data_cumcap))
    if (all.equal(check_dims[1], check_dims[2])) {
      num_sources <- check_dims[1]
      out <- vector("list", num_sources)
    } else {
      stop("Wrong data dimension: ncol differ for resources/cumcap datasets.")
    }
    data_resources_wrong <- lapply(data_resources, function(x) {which(x > 0)})
    data_cumcap_wrong <- lapply(data_cumcap, function(x) {which(x == 0)})
    for (i in 1:num_sources) {
      out[[i]] <- intersect(data_resources_wrong[[i]], data_cumcap_wrong[[i]])
    }
    return(out)
  }
  list_of_functions <- list(is_resource_negative,
                            is_resource_zero,
                            is_resource_na,
                            is_resource_na,
                            is_resource_cumcap_wrong)

  data_to_check_raw   <- NULL
  data_to_check_share <- NULL
  data_to_check_ccap  <- NULL
  if (missing_raw || zeros || negatives) {
    data_to_check_raw   <- data %>%
      dplyr::select(dplyr::all_of(names_esources_present))
  }
  if (missing_shares) {
    names_esources_present_shares <- paste0(names_esources_present, "_share")
    data_to_check_share <- data %>%
      dplyr::select(dplyr::all_of(names_esources_present_shares))
  }
  if (cumcap_wrong) {
    data_to_check_ccap <- data %>%
      dplyr::select(dplyr::all_of(names_cumcap_present))
  }
  list_of_arguments <- list(list(data_resources = data_to_check_raw),
                            list(data_resources = data_to_check_raw),
                            list(data_resources = data_to_check_raw),
                            list(data_resources = data_to_check_share),
                            list(data_resources = data_to_check_raw,
                                 data_cumcap = data_to_check_ccap))

  out <- list(negatives = NULL,
              zeros = NULL,
              missing_raw = NULL,
              missing_shares = NULL,
              cumcap_wrong = NULL)

  id_fct_to_use <- which(c(negatives,
                           zeros,
                           missing_raw,
                           missing_shares,
                           cumcap_wrong))
  for (i in id_fct_to_use) {
    id    <- numeric(0)
    dummy <- numeric(0)

    tmp <- do.call(list_of_functions[[i]], list_of_arguments[[i]])

    id    <- unlist(tmp)
    dummy <- rep(names_esources_present,
                 times = sapply(tmp, length))

    if (length(id) == 0) {
      msg <- "No problmeatic cases with '"
      msg <- paste0(msg, problematic_cases_names[i], "'", " in the data!\n")
      cat(crayon::green(msg))
    } else {
      out[[i]] <- cbind(data[id, ], dummy)
    }
  }
  return(out)
}
#' Constructs a summary of problematic cases
#'
#' Constructs a summary of problematic cases given the output of
#' \code{view_problematic()}.
#'
#' The summary is either grouped by state or energy source and has a maximum of
#' six components for the corresponding categories (already used in
#' \code{view_problematic()} ) describing what could possible go wrong. These
#' are:
#' \itemize{
#'   \item{negatives: negative values for energy consumption not allowed}
#'   \item{zeros: zero values for energy consumption possible, but annoying}
#'   \item{missing values for raw consumption units}
#'   \item{missing values for shares of energy consumption (mix): occurs e.g. if
#'   divided by a sum of energy consumption which is zero (nothing produced,
#'   recorded)}
#'   \item{cases where cumulative capacities are zero, though the corresponding
#'   resource is consumed are returned}
#' }
#'
#' @param list_of_problematic a list having the structure as the output from
#'   \code{view_problematic()}
#' @param len_ts length of the original time series (e.g. 45 if from 1970 to
#'   2014)
#'
#' @return A list of the same length (counting only non-NULL elements) as
#'   \code{list_of_problematic} containing the summary tables for each of the
#'   six categories of failures as described in the "Description".
#' @export
summarise_problematic <- function(list_of_problematic,
                                  len_ts) {
  id_to_show  <- which(!sapply(list_of_problematic, is.null))
  num_to_show <- length(id_to_show)
  out_source <- vector("list", num_to_show)

  for (i in id_to_show) {
    out_source[[i]] <- summarise_problematic_source(list_of_problematic[[i]],
                                                       len_ts = len_ts)
  }
  return(out_source)
}
summarise_problematic_states <- function(data_problematic, len_ts) {
  out <- data_problematic %>%
    dplyr::group_by(.data$state) %>%
    dplyr::summarize(number_fails = table(.data$dummy),
                     fraction_fails = table(.data$dummy) / len_ts,
                     category = unique(.data$dummy))
  return(out)
}
summarise_problematic_source <- function(data_problematic, len_ts) {
  my_years <- data_problematic %>%
    dplyr::group_by(source = .data$dummy, state = .data$state) %>%
    dplyr::summarize(years_to_get = get_consecutive_years(.data$year)) %>%
    dplyr::pull(.data$years_to_get)
    out <- data_problematic %>%
      dplyr::group_by(resource = .data$dummy) %>%
      dplyr::summarise(state_name = unique(.data$state),
                       num_fails = table(.data$state),
                       `frac_own_ts (in %)` = round(.data$num_fails / len_ts,
                                                         digits = 4) * 100,
                       `frac_fails (in %)` = round(.data$num_fails / dplyr::n(),
                                     digits = 4) * 100
                     )
    out[["frac_own_ts (in %)"]] <- as.numeric(out$`frac_own_ts (in %)`)
    out[["frac_fails (in %)"]] <- as.numeric(out$`frac_fails (in %)`)
      # frac_fails = fraction of total resource fails
  out <- dplyr::tibble(out, years = my_years)
  out$num_fails <- as.integer(out$num_fails)
  sum_row <- dplyr::tibble(resource = "SUM ALL:", state_name = NA_character_,
                           num_fails = sum(out$num_fails),
                           `frac_own_ts (in %)` = NA_real_,
                           `frac_fails (in %)` = NA_real_,
                           years = NA_character_)
  out <- dplyr::bind_rows(out, sum_row)
  return(out)
}
get_consecutive_years <- function(years) {
  if (length(years) == 1) {
    return(as.character(years))
  }
  years <- sort(unique(years))
  numeric_years <- as.numeric(years)
  out <- years[1]

  max_num_years <- length(years) - 1
  current_year_to <- NULL
  for (i in 1:max_num_years) {
    final_iter <- i == max_num_years
    check_me <- (numeric_years[i] + 1) == numeric_years[i + 1]
    if (check_me && !final_iter) {
      current_year_to <- numeric_years[i + 1]
      next
    } else if (check_me && final_iter) {
      out <- paste0(out, "-", numeric_years[i + 1])
    } else {
      if (!is.null(current_year_to)) {
        out <- paste0(out, "-", current_year_to, " ; ", years[i + 1])
        current_year_to <- NULL
      } else {
        out <- paste0(out, " ; ", years[i + 1])
      }
    }
  }
  return(out)
}
generate_single_plot <- function(data_ts,
                                 names_ts,
                                 col_seq,
                                 names_to_display,
                                 threshold,
                                 current_explode,
                                 add_intercept) {
  force(data_ts)
  force(names_ts)
  force(col_seq)
  force(names_to_display)
  force(threshold)
  force(current_explode)
  force(add_intercept)
  if (add_intercept) {
    current_plot <- ggplot2::ggplot(data_ts, ggplot2::aes(x = .data$year)) +
      ggplot2:: geom_line(ggplot2::aes_string(x = "year", y = names_ts),
                          col = col_seq) +
      ggplot2::ggtitle(names_to_display) +
      ggplot2::labs(x = "years", y = "counts") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = force(threshold),
                                       col = "red")) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = force(current_explode),
                                       col = "red"))
  } else {
    current_plot <- ggplot2::ggplot(data_ts, ggplot2::aes(x = .data$year)) +
      ggplot2:: geom_line(ggplot2::aes_string(x = "year", y = names_ts),
                          col = col_seq) +
      ggplot2::ggtitle(names_to_display) +
      ggplot2::labs(x = "years", y = "counts") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = force(threshold),
                                       col = "red"))
  }
  return(current_plot)
}
#' Function that generates a single time series plot.
#'
#' The time series can be either the energy types directly i.e. their shares or
#'  the raw counts, or, the energy prices.
#'
#' @param data_ts tibble/data.frame of energy types (i.e. shares and/or raw
#'   counts) or energy prices
#' @param name_state character string of the name of the state for which to plot
#' @param names_ts names of the time series variables to plot
#' @param names_to_display how the names should occur in the final plots (labels
#'  etc. typically energy type names or names of the prices)
#' @param thresholds numeric vector of dimension equal to the number of energy
#'   types giving the threshold values for each energy component where to
#'   position the dummy
#' @param explode_here point to explode
#' @param explode_offset magnitude of explode
#' @param check_me bool; if \code{TRUE}, then this sries is checked
#'
#' @return a plot of the data: either 3x2 (for shares or raw counts) and a plot
#' on top; for prices a 3x1 plot
#' @export
analyze_plot_ts <- function(data_ts, name_state, names_ts, names_to_display,
                            thresholds, explode_here, explode_offset,
                            check_me = FALSE) {
  num_ts  <- length(names_ts)
  data_ts <- data_ts %>%
    dplyr::filter(.data$state == name_state)
  col_seq <- RColorBrewer::brewer.pal(num_ts, "Dark2")
  single_plots <- rep(list(list()), times = num_ts)
  plots <- ggplot2::ggplot(data_ts, ggplot2::aes(x = .data$year))

  explode_here <- rep(min(data_ts$year), times = num_ts)
  explode_offset <- rep(0, times = num_ts)
  for (k in 1:num_ts) {
    energy_eval <- data_ts %>%
      dplyr::pull(names_ts[k])
    energy_eval <- as.numeric(energy_eval)
    energy_eval <- energy_eval > thresholds[k]
    explode_offset[k] <- which(energy_eval > 0)[1] - 1
    explode_here[k] <- explode_here[k] + explode_offset[k]
  }
  donot_explode_set <- 0:0

  for (d in 1:num_ts) {
    plots <- plots + ggplot2::geom_line(ggplot2::aes_string(x = "year",
                                                            y = names_ts[d]),
                                                            col = col_seq[d]) +
      ggplot2::labs(x = "years", y = "counts")

    add_intercept <- FALSE
    if (!(explode_offset[d] %in% donot_explode_set)) {
      add_intercept <- TRUE
    } else if (explode_offset[d] %in% setdiff(donot_explode_set, 0)) {
      stop("too early explode; better reset the thresholds")
    }
    single_plots[[d]] <- generate_single_plot(data_ts,
                                              names_ts[d],
                                              col_seq[d],
                                              names_to_display[d],
                                              thresholds[d],
                                              explode_here[d],
                                              add_intercept)
  }
  if (check_me) {
    for (i in seq_len(length(single_plots))) {
      plot(single_plots[[i]])
      print(explode_here[i])
    }
    browser()
  }
  # if (num_ts == 6) {
  #   final_layout <- matrix(c(1, 1, 2:7), ncol = 2, byrow = TRUE)
  # } else {
  #   stop("Can't specify layout for current number of ts-plots!")
  # }
  # plot_final <- gridExtra::arrangeGrob(grobs = c(list(plots),
  #                                                single_plots),
  #                                      layout_matrix = final_layout,
  #                                      top = name_state)
  # return(list(plot_final, single_plots))
  return(invisible(0))
}
#' Iterates over US states and plots the enrgy raw counts and thresholds.
#'
#' @param data data input
#' @param threshold_matrix the matrix of thresholds; numeric matrix of dimension
#'   equal to the number of states (rows) and number of energy types (cols)
#'   giving the threshold values for each energy component where to position the
#'   dummy
#' @param id_range range of ids
#' @param id_check ids to check
#' @param explode_here point to explode
#' @param explode_offset magnitude of explode
#' @return pure side effect functions with just plots
#' @export
analyze_plot_ts_all <- function(data, threshold_matrix,
                                id_range = NULL, id_check,
                                explode_here, explode_offset) {
  names_states <- unique(data$state)
  if (is.null(id_range)) {
    id_range   <- seq_len(length(names_states))
  }
  plot_list    <- rep(list(list()), times = length(id_range))

  renewables <- c("GEEGB", "SOEGB", "WYEGB", "HYEGB", "WWEIB")
  vars <-  c("CLEIB", "PAEIB", "NGEIB", "NUEGB", renewables)
  names_cumcap_all <- c("coal_cumcap", "petroleum_cumcap",
                        "naturalgas_cumcap", "nuclear_cumcap",
                        "geothermal_cumcap", "solar_cumcap", "wind_cumcap",
                        "hydro_cumcap", "wood_waste_cumcap", "renew_cumcap")
  vars_all <- c("CLEIB", "coal_cumcap",
                "PAEIB", "petroleum_cumcap",
                "NGEIB", "naturalgas_cumcap",
                "NUEGB", "petroleum_cumcap",
                "GEEGB", "geothermal_cumcap",
                "SOEGB", "solar_cumcap",
                "WYEGB", "wind_cumcap",
                "HYEGB", "hydro_cumcap",
                "WWEIB", "wood_waste_cumcap",
                "renewables", "renew_cumcap")
  var_names <- c("coal", "gas", "oil", "nuclear",
                 "geothermal", "solar", "wind",
                 "hydro", "wood_waste", "renew")
  current_data <- data %>% dplyr::select(.data$state, .data$year,
                                         dplyr::all_of(vars_all))
  for (i in id_range) {
    print(names_states[i])
    if (i %in% id_check) {
      check_me <- TRUE
    } else {
      check_me <- FALSE
    }
    plot_list[[i]] <- analyze_plot_ts(current_data,
                                      names_states[i],
                                      vars,
                                      var_names,
                                      threshold_matrix[i, ],
                                      explode_here = explode_here,
                                      explode_offset = explode_offset,
                                      check_me)
  }
}
