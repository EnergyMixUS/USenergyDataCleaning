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
#'
#' @return a plot of the data: either 3x2 (for shares or raw counts) and a plot
#' on top; for prices a 3x1 plot
#' @export
generate_plot_ts <- function(data_ts, name_state, names_ts, names_to_display) {
  data_ts <- data_ts %>%
    dplyr::filter(.data$state == name_state)

  name_state <- get_state_name_full(name_state)
  names_ts <- names_ts[[1]]
  num_ts   <- length(names_ts)

  # col_seq <- RColorBrewer::brewer.pal(num_ts, "Dark2")
  col_seq <- wesanderson::wes_palette(n = num_ts, name = "Zissou1")
  col_seq <- col_seq[5:1]
  single_plots <- rep(list(list()), times = num_ts)

  id_drop <- which(sapply(data_ts[-c(1,2)], function(x){all(x==0)}))
  if (length(id_drop) == 0) {
    names_to_display_longer <- names_to_display %>%
      stringr::str_sub(start = 1L, end = -7L)
    col_seq_longer <- col_seq
    data_ts_longer <- data_ts
  } else {
    names_to_display_longer <- names_to_display[-id_drop] %>%
      stringr::str_sub(start = 1L, end = -7L)
    col_seq_longer <- col_seq[-id_drop]
    data_ts_longer <- data_ts[-(id_drop + 2)]
  }

  data_ts_longer <- tidyr::pivot_longer(data_ts_longer, cols = -c(1,2),
                                        names_to = "source")
  factor_var <- factor(data_ts_longer$source,
                       colnames(data_ts[-c(1,2)]))
  data_ts_longer$source <- factor_var
  plots <- ggplot2::ggplot(data_ts_longer, ggplot2::aes(x = .data$year,
                                                        y = .data$value,
                                                        group = .data$source,
                                                        fill = .data$source)) +
    ggplot2::geom_area(position = "fill", alpha = 0.7) +
    ggplot2::scale_fill_manual(values = col_seq_longer,
                               labels = names_to_display_longer) +
    # ggplot2::labs(x = "years", y = "shares",
    #               title = name_state, subtitle = "All shares") +
    ggplot2::labs(x = "years", y = "shares") +
    ggplot2::ggtitle("All shares") +
    my_theme_settings() +
    my_guides_settings()

  # num_ts <- 1
  for (i in 1:num_ts) {
    y_max_current <- data_ts[[names_ts[i]]]
    single_plots[[i]] <- ggplot2::ggplot(data_ts,
                                         ggplot2::aes(x = .data$year)) +
      ggplot2:: geom_line(ggplot2::aes_string(x = "year", y = names_ts[i]),
                          col = col_seq[i]) +
      ggplot2::geom_ribbon(ggplot2::aes_(ymin = 0,
                                         ymax = y_max_current),
                           fill= col_seq[i], alpha = 0.8) +
      my_theme_settings() +
      ggplot2::ggtitle(names_to_display[i]) +
      ggplot2::labs(x = "years", y = "share")
  }
  if (num_ts == 6) {
    final_layout <- matrix(c(1, 1, 1, 2:7), ncol = 3, byrow = TRUE)
    # final_layout <- matrix(c(1, 1, 1, 1, 2:7), ncol = 4, byrow = TRUE)
  } else if (num_ts == 5) {
    final_layout <- matrix(1:6, ncol = 3, byrow = FALSE)
  } else  if (num_ts == 3) {
    final_layout <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4), ncol = 2, byrow = TRUE)
  } else if (num_ts == 1) {
    final_layout <- matrix(1, ncol = 1, nrow = 1)
  } else {
    stop("Can't specify layout for current number of ts-plots!")
  }

  plot_final <- gridExtra::arrangeGrob(grobs = c(list(plots),
                                                 single_plots),
                                       layout_matrix = final_layout,
                                       top = name_state)
  # plot_final <- plots
  return(plot_final)
}
#' Function that generates two time series plot.
#'
#' The time series are e.g. be the energy types directly i.e. their shares or
#' the raw counts, and the fuel prices.
#'
#' @param data_ts tibble/data.frame of energy types (i.e. shares and/or raw
#'   counts) or energy prices
#' @param name_state character string of the name of the state for which to plot
#' @param names_ts list: names of the time series variables to plot (list of
#'  two)
#' @param names_to_display how the names should occur in the final plots (labels
#'  etc. typically energy type names or names of the prices)
#'
#' @return a plot of the data in 3x2-form containing the shares or raw counts,
#' the corresponding time series of prices (if available) and a plot on top
#' @export
generate_plot_ts_src_prc <- function(data_ts,
                                     name_state,
                                     names_ts,
                                     names_to_display) {
  data_ts <- data_ts %>%
    dplyr::filter(.data$state == name_state)
  name_state <- get_state_name_full(name_state)

  names_ts1 <- names_ts[[1]]
  names_ts2 <- names_ts[[2]]
  num_ts1 <- length(names_ts1)
  num_ts2 <- length(names_ts2)

  id_renew <- which(grepl("renewable", names(data_ts))) - 2

  col_seq <- RColorBrewer::brewer.pal(num_ts1, "Dark2")
  single_plots <- rep(list(list()), times = num_ts1)

  sub_data_cor <- vector("list", 2)
  names_ts_all <- vector("list", 2)

  names_ts_id       <- grep("(^CLEIB|^PAEIB|^NGEIB|^renew)", names_ts1)
  names_ts_all[[1]] <- names_ts1[names_ts_id]
  sub_data_cor[[1]] <- data_ts[names_ts_all[[1]]]

  names_ts_id       <- grep("(cleid|dfeid|ngeid)", names_ts2)
  names_ts_all[[2]] <- names_ts2[names_ts_id]
  sub_data_cor[[2]] <- data_ts[names_ts_all[[2]]]

  sub_data_cor[[2]]$ngeid2 <- sub_data_cor[[2]][[3]]

  num_cor     <- ncol(sub_data_cor[[1]])
  cor_src_prc <- vector("numeric", num_cor)
  for (i in 1:num_cor) {
    cor_src_prc[i] <- stats::cor(sub_data_cor[[1]][[i]], sub_data_cor[[2]][[i]])
    cor_src_prc[i] <- round(cor_src_prc[i], digits = 4)
  }
  names_ts_all <- unlist(names_ts_all)
  num_ts_all   <- length(names_ts_all)

  scl_me  <- sapply(data_ts[names_ts_all], max, na.rm = TRUE)

  data_ts2 <- data_ts
  for (i in 1:num_ts_all) {
    data_ts2 <- data_ts2 %>%
      dplyr::mutate("{names_ts_all[i]}" := .data[[names_ts_all[i]]] / scl_me[i])
    list_repl_na <- stats::setNames(as.list(c(0)), names_ts_all[i])
    data_ts2 <- data_ts2 %>%
      tidyr::replace_na(list_repl_na)
  }
  plots <- ggplot2::ggplot(data_ts, ggplot2::aes(x = .data$year))
  for (i in 1:num_ts1) {
    plots <- plots + ggplot2::geom_line(ggplot2::aes_string(x = "year",
                                                            y = names_ts1[i]),
                                        col = col_seq[i]) +
      ggplot2::labs(x = "years", y = "share") +
      ggplot2::ggtitle("All") +
      my_theme_settings()

    if (i <= num_ts2 || i == id_renew) {

      if (i == id_renew) {
        id_price <- 3
        id_corr  <- 4
      } else {
        id_price <- i
        id_corr  <- i
      }

      single_plots[[i]] <- ggplot2::ggplot(data_ts2,
                                           ggplot2::aes(x = .data$year)) +
        ggplot2::geom_line(ggplot2::aes_string(x = "year",
                                               y = names_ts1[i]),
                           col = col_seq[i]) +
        my_theme_settings()

      single_plots[[i]] <- single_plots[[i]] +
        ggplot2::geom_line(ggplot2::aes_string(x = "year",
                                               y = names_ts2[id_price]),
                           col = "black") +

        ggplot2::ggtitle(names_to_display[i],
                         subtitle = paste0("correlation: ",
                                           cor_src_prc[id_corr])) +
        ggplot2::labs(x = "years", y = "share")
      if (!all(data_ts2[names_ts2[id_price]] == 0)) {
        my_y_scaling <- ggplot2::sec_axis(~. * 1, name = "price")
        single_plots[[i]] <- single_plots[[i]] +
          ggplot2::scale_y_continuous(sec.axis = my_y_scaling)
      }
    } else {
      single_plots[[i]] <- ggplot2::ggplot(data_ts,
                                           ggplot2::aes(x = .data$year)) +
        ggplot2::geom_line(ggplot2::aes_string(x = "year", y = names_ts1[i]),
                           col = col_seq[i]) +
        my_theme_settings()
      single_plots[[i]] <- single_plots[[i]] +
        ggplot2::ggtitle(names_to_display[i]) +
        ggplot2::labs(x = "years", y = "share")
    }
  }
  if (num_ts1 == 6) {
    final_layout <- matrix(c(1, 1, 1, 2:7), ncol = 3, byrow = TRUE)
  } else if (num_ts1 == 5) {
    final_layout <- matrix(1:6, ncol = 2, byrow = TRUE)
  } else  if (num_ts1 == 3) {
    final_layout <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4), ncol = 2, byrow = TRUE)
  } else {
    stop("Can't specify layout for current number of ts-plots!")
  }
  # gridExtra::grid.arrange(grobs = single_plots,
  #                         layout_matrix = matrix(1:6, ncol = 2))
  plot_final <- gridExtra::arrangeGrob(grobs = c(list(plots),
                                                 single_plots),
                                       layout_matrix = final_layout,
                                       top = name_state)
  return(plot_final)
}
#' Function that generates two time series plot.
#'
#' The time series are those of shares and cumulative capacities.
#'
#' @param data_ts tibble/data.frame of energy types (i.e. shares and/or raw
#'   counts) or energy prices
#' @param name_state character string of the name of the state for which to plot
#' @param names_ts list: names of the time series variables to plot (list of
#'  two)
#' @param names_to_display how the names should occur in the final plots (labels
#'  etc. typically energy type names or names of the prices)
#'
#' @return a plot of the data in 3x2-form containing the shares or raw counts,
#' the corresponding time series of prices (if available) and a plot on top
#' @export
generate_plot_ts_src_ccap <- function(data_ts,
                                      name_state,
                                      names_ts,
                                      names_to_display) {
  data_ts <- data_ts %>%
    dplyr::filter(.data$state == name_state)
  name_state <- get_state_name_full(name_state)
  names_ts1 <- names_ts[[1]]
  names_ts2 <- names_ts[[2]]
  num_ts <- length(names_ts1)


  col_seq <- RColorBrewer::brewer.pal(num_ts, "Dark2")
  single_plots <- rep(list(list()), times = num_ts)

  sub_data_cor <- vector("list", 2)
  names_ts_all <- vector("list", 2)

  names_ts_id       <- grep("(^CLEIB|^PAEIB|^NGEIB|^NUEGB|^renew|^HYEGB)",
                            names_ts1)
  names_ts_all[[1]] <- names_ts1[names_ts_id]
  sub_data_cor[[1]] <- data_ts[names_ts_all[[1]]]

  names_ts_id       <- grep("(^coal|^petrol|^natural|^nuclear|^renew|^hydro)",
                            names_ts2)
  names_ts_all[[2]] <- names_ts2[names_ts_id]
  sub_data_cor[[2]] <- data_ts[names_ts_all[[2]]]

  sub_data_cor[[2]]$ngeid2 <- sub_data_cor[[2]][[3]]

  num_cor      <- ncol(sub_data_cor[[1]])
  cor_src_ccap <- vector("numeric", num_cor)
  for (i in 1:num_cor) {
    cor_src_ccap[i] <- stats::cor(sub_data_cor[[1]][[i]],
                                  sub_data_cor[[2]][[i]])
    cor_src_ccap[i] <- round(cor_src_ccap[i], digits = 4)
  }
  names_ts_all <- unlist(names_ts_all)
  num_ts_all   <- length(names_ts_all)

  scl_me  <- sapply(data_ts[names_ts_all], max, na.rm = TRUE)

  data_ts2 <- data_ts
  for (i in 1:num_ts_all) {
    data_ts2 <- data_ts2 %>%
      dplyr::mutate("{names_ts_all[i]}" := .data[[names_ts_all[i]]] / scl_me[i])
    list_repl_na <- stats::setNames(as.list(c(0)), names_ts_all[i])
    data_ts2 <- data_ts2 %>%
      tidyr::replace_na(list_repl_na)
  }
  plots <- ggplot2::ggplot(data_ts, ggplot2::aes(x = .data$year))
  for (i in 1:num_ts) {
    plots <- plots + ggplot2::geom_line(ggplot2::aes_string(x = "year",
                                                            y = names_ts1[i]),
                                        col = col_seq[i]) +
      ggplot2::labs(x = "years", y = "share") +
      ggplot2::ggtitle("All") +
      my_theme_settings()

    single_plots[[i]] <- ggplot2::ggplot(data_ts2,
                                         ggplot2::aes(x = .data$year)) +
      ggplot2::geom_line(ggplot2::aes_string(x = "year",
                                             y = names_ts1[i]),
                         col = col_seq[i]) +
      ggplot2::geom_line(ggplot2::aes_string(x = "year",
                                             y = names_ts2[i]),
                         col = "black") +
      my_theme_settings() +
      ggplot2::ggtitle(names_to_display[i],
                       subtitle = paste0("correlation: ",
                                         cor_src_ccap[i])) +
      ggplot2::labs(x = "years", y = "share")

    if (!all(data_ts2[names_ts2[i]] == 0)) {
      my_y_scaling <- ggplot2::sec_axis(~. * 1, name = "price")
      single_plots[[i]] <- single_plots[[i]] +
        ggplot2::scale_y_continuous(sec.axis = my_y_scaling)
    }
  }
  if (num_ts == 6) {
    final_layout <- matrix(c(1, 1, 1, 2:7), ncol = 3, byrow = TRUE)
  } else if (num_ts == 5) {
    final_layout <- matrix(1:6, ncol = 2, byrow = TRUE)
  } else  if (num_ts == 3) {
    final_layout <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4), ncol = 2, byrow = TRUE)
  } else {
    stop("Can't specify layout for current number of ts-plots!")
  }
  # gridExtra::grid.arrange(grobs = single_plots,
  #                         layout_matrix = matrix(1:6, ncol = 2))
  plot_final <- gridExtra::arrangeGrob(grobs = c(list(plots),
                                                 single_plots),
                                       layout_matrix = final_layout,
                                       top = name_state)
  return(plot_final)
}
generate_plot_ts_all <- function(data,
                                 vars,
                                 var_names,
                                 which_plots) {
  names_states <- unique(data$state)
  num_states   <- length(names_states)
  plot_list    <- rep(list(list()), times = num_states)

  len_list_vars <- length(vars)
  if (len_list_vars == 1) {
    for (i in 1:num_states) {
      plot_list[[i]] <- generate_plot_ts(data,
                                         names_states[i],
                                         vars,
                                         var_names)
      # browser()
      print(paste0(round(i / num_states, digits = 4) * 100,
                   "%", " of current plots generated! (", which_plots, ")"))
    }
  } else if (len_list_vars == 2) {
    if (which_plots == "sources and prices") {
      for (i in 1:num_states) {
        plot_list[[i]] <- generate_plot_ts_src_prc(data,
                                                   names_states[i],
                                                   vars,
                                                   var_names)
        print(paste0(round(i / num_states, digits = 4) * 100,
                     "%", " of current plots generated! (", which_plots, ")"))
      }
    } else if (which_plots == "sources and cumcaps") {
      for (i in 1:num_states) {
        plot_list[[i]] <- generate_plot_ts_src_ccap(data,
                                                    names_states[i],
                                                    vars,
                                                    var_names)
        print(paste0(round(i / num_states, digits = 4) * 100,
                     "%", " of current plots generated! (", which_plots, ")"))
      }
    }
  } else {
    stop("Undefined behaviour for more than two time series; examine manually!")
  }
  return(plot_list)
}
save_plot_ts_all <- function(plot_list_to_save,
                             path_to_save,
                             names_to_save,
                             landscape = FALSE) {
  # browser()
  num_states <- length(plot_list_to_save)
  for (i in 1:num_states) {
    name <- paste0(path_to_save, names_to_save)
    if (isFALSE(landscape)) {
      ggplot2::ggsave(filename = name[i],
                      plot = plot_list_to_save[[i]],
                      width = 8.27, height = 11.69,
                      units = "in")
    } else if (isTRUE(landscape)) {
      ggplot2::ggsave(filename = name[i],
                      plot = plot_list_to_save[[i]],
                      width = 11.69, height = 8.27,
                      units = "in")
    }

    print(paste0(round(i / num_states, digits = 4) * 100,
                 "%", " of current plots saved!"))
  }
}
#' Wrapper functions to generate all plot types
#'
#' Wrapper functions to generate all plot types #'
#' @param data_sources data set of shares as generated by
#'   \code{generate_data_to_plot_shares}
#' @param save_data_sources logical; if \code{TRUE}, then resulting plots for
#'   \code{data_sources} are saved
#' @param data_prices data set of shares as generated by
#'   \code{generate_data_to_plot_price}
#' @param save_data_prices logical; if \code{TRUE}, then resulting plots for
#'   \code{data_prices} are saved
#' @param data_cumcap dataset of cumulative generating capacities as
#'   generated by \code{generate_data_to_plot_ccap}
#' @param save_data_cumcap logical; if \code{TRUE}, then resulting plots for
#'   \code{plot_src_prc} are saved
#' @param plot_src_prc logical; if \code{TRUE}, type '03'-plots will be
#'   returned that plot shares vs. prices with merged data via e.g.
#'   \code{dplyr::full_join(data_shaes, data_prices)}
#' @param save_src_prc logical; if \code{TRUE}, then resulting plots for
#'   \code{plot_src_prc} are saved
#' @param plot_src_ccap logical; if \code{TRUE}, type '04'-plots will be
#'   returned that plot shares vs. cumcap with merged data via e.g.
#'   \code{dplyr::full_join(data_shaes, data_cumcap)}
#' @param save_src_ccap logical; if \code{TRUE}, then resulting plots for
#'   \code{plot_src_prc} are saved
#' @param path_to_save a character string giving the path where to save above
#'   plots
# ' @param var_names_shares either \code{NULL} or the variable names used in the
# '   sublplot-functions of the overall plotting function
# '   \code{generate_all_plots()}
# ' @param var_names_prices either \code{NULL} or the variable names used in the
# '   sublplot-functions of the overall plotting function
# '   \code{generate_all_plots()}

#'
#' @return a list of 'ggplots' and, as a side effect, the resulting plots are
#'   save too, if the corresponding \code{save_data_xxx= TRUE}
#' @export
generate_all_plots <- function(data_sources  = NULL, save_data_sources = FALSE,
                               data_prices   = NULL, save_data_prices  = FALSE,
                               data_cumcap   = NULL, save_data_cumcap  = FALSE,
                               plot_src_prc  = TRUE, save_src_prc      = FALSE,
                               plot_src_ccap = TRUE, save_src_ccap     = FALSE,
                               path_to_save) {
  check_landscape <- if((ncol(data_sources) - 2) == 5) {
    landscape_set <- TRUE
  } else {
    landscape_set <- FALSE
  }
  plot_list_out <- vector("list", sum(c(!sapply(list(data_sources,
                                                     data_prices,
                                                     data_cumcap),
                                                is.null),
                                        plot_src_prc,
                                        plot_src_ccap)))
  num_entry <- 1
  # all energy shares jointly ---------------------------------------------
  if (!is.null(data_sources)) {
    vars_used         <- get_var_names_plain(data_sources)
    var_names_display <- get_var_names_display(vars_used)
    plot_list <- generate_plot_ts_all(data = data_sources,
                                      vars = vars_used,
                                      var_names =  var_names_display,
                                      which_plots = "sources")
    if (save_data_sources) {
      check_shares <- any(grepl("share", vars_used))
      if (check_shares) {
        name_identifier <- "_01_sources_shares.pdf"
      } else {
        name_identifier <- "_02_sources_levels.pdf"
      }
      save_plot_ts_all(plot_list_to_save = plot_list,
                       path_to_save = path_to_save,
                       names_to_save = paste0(unique(data_sources$state),
                                              name_identifier),
                       landscape = landscape_set)
    }
    plot_list_out[[num_entry]] <- plot_list
    num_entry <- num_entry + 1
  }
  # all energy prices jointly ---------------------------------------------
  if (!is.null(data_prices)) {
    vars_used         <- get_var_names_plain(data_prices)
    var_names_display <- get_var_names_display(vars_used)
    plot_list <- generate_plot_ts_all(data = data_prices,
                                      vars = vars_used,
                                      var_names =  var_names_display,
                                      which_plots = "prices")
    if (save_data_prices) {
      save_plot_ts_all(plot_list_to_save = plot_list,
                       path_to_save = path_to_save,
                       names_to_save = paste0(unique(data_prices$state),
                                              "_02_prices.pdf"))
    }
    plot_list_out[[num_entry]] <- plot_list
    num_entry <- num_entry + 1
  }
  # all cumulative capacities jointly -------------------------------------
  if (!is.null(data_cumcap)) {
    vars_used         <- get_var_names_plain(data_cumcap)
    var_names_display <- get_var_names_display(vars_used)
    plot_list <- generate_plot_ts_all(data = data_cumcap,
                                      vars = vars_used,
                                      var_names =  var_names_display,
                                      which_plots = "cumcap")
    if (save_data_cumcap) {
      check_shares <- any(grepl("share", vars_used))
      if (check_shares) {
        name_identifier <- "_03_cumcap_shares.pdf"
      } else {
        name_identifier <- "_03_cumcap_levels.pdf"
      }
      save_plot_ts_all(plot_list_to_save = plot_list,
                       path_to_save = path_to_save,
                       names_to_save = paste0(unique(data_cumcap$state),
                                              name_identifier),
                       landscape = landscape_set)
    }
    plot_list_out[[num_entry]] <- plot_list
    num_entry <- num_entry + 1
  }
  # all energy shares and coal, gas and oil prices jointly -----------------
  if (isTRUE(plot_src_prc)) {
    data_sources_prices <- dplyr::full_join(data_sources, data_prices)
    vars_used           <- get_var_names_plain(data_sources,
                                               data_prices)
    var_names_display   <- get_var_names_display(vars_used[[1]], vars_used[[2]])
    plot_list <- generate_plot_ts_all(data = data_sources_prices,
                                      vars = vars_used,
                                      var_names =  var_names_display,
                                      which_plots = "sources and prices")
    if (save_src_prc) {
      check_shares <- any(grepl("share", vars_used[[1]]))
      if (check_shares) {
        name_identifier <- "_04_src_shr_prc.pdf"
      } else {
        name_identifier <- "_04_src_lvl_prc.pdf"
      }
      save_plot_ts_all(plot_list_to_save = plot_list,
                       path_to_save = path_to_save,
                       names_to_save = paste0(unique(data_sources_prices$state),
                                              name_identifier),
                       landscape = landscape_set)
    }
    plot_list_out[[num_entry]] <- plot_list
    num_entry <- num_entry + 1
  }
  # all energy shares and their cumulative generating capacities jointly ----
  if (isTRUE(plot_src_ccap)) {
    data_sources_cumcap <- dplyr::full_join(data_sources, data_cumcap)
    vars_used           <- get_var_names_plain(data_sources,
                                               data_cumcap)
    var_names_display   <- get_var_names_display(vars_used[[1]], vars_used[[2]])
    plot_list <- generate_plot_ts_all(data = data_sources_cumcap,
                                      vars = vars_used,
                                      var_names =  var_names_display,
                                      which_plots = "sources and cumcaps")
    if (save_src_ccap) {
      check_shares <- any(grepl("share", vars_used[[1]]))
      if (check_shares) {
        name_identifier <- "_05_src_shr_ccap.pdf"
      } else {
        name_identifier <- "_05_src_lvl_ccap.pdf"
      }
      save_plot_ts_all(plot_list_to_save = plot_list,
                       path_to_save = path_to_save,
                       names_to_save = paste0(unique(data_sources_cumcap$state),
                                              name_identifier),
                       landscape = landscape_set)
    }
    plot_list_out[[num_entry]] <- plot_list
  }
  return(plot_list_out)
}
my_theme_settings <- function() {
  out <- ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(face = "bold", size = 9),
      axis.ticks = ggplot2::element_line(colour = "grey70", size = 0.2),
      panel.grid.major = ggplot2::element_line(colour = "grey70", size = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "white",
                                                size = 4,
                                                colour = "white"),
      legend.title = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 6),
      # legend.justification = c(1, 1),
      legend.position = "bottom")
  return(out)
}
my_guides_settings <- function() {
  ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 0.5)),
                  color = ggplot2::guide_legend(override.aes = list(size = 0.5)))
}
get_var_names_plain <- function(...) {
  num_datasets <- length(list(...))
  out <- vector("list", num_datasets)
  for (i in 1:num_datasets) {
    out[[i]] <- names(list(...)[[i]])
    out[[i]] <- setdiff(out[[i]], c("state", "year"))
  }
  return(out)
}
get_var_names_display <- function(...) {
  num_datasets <- length(list(...))
  out <- vector("list", num_datasets)
  for (i in 1:num_datasets) {

    var_names_plain <- unlist(list(...)[[i]])

    renewables <- c("GEEGB", "SOEGB", "WYEGB", "HYEGB", "WWEIB")
    names_esources_all    <-  c("CLEIB", "PAEIB", "NGEIB", "NUEGB",
                                "renewables",
                                renewables)
    names_esources_shares <- paste0(names_esources_all, "_share")

    names_cumcap_all      <- paste0(c("coal", "petroleum", "naturalgas",
                                      "nuclear", "renew", "geo", "solar",
                                      "wind",
                                      "hydro", "wood_waste"),
                                    "_cumcap")
    names_cumcap_shares   <- paste0(names_cumcap_all, "_share")
    names_prices_all      <- c("cleid", "paeid", "dfeid", "ngeid", "nueid")

    names_all <- c(names_esources_all, names_esources_shares,
                   names_cumcap_all, names_cumcap_shares,
                   names_prices_all)

    names_cq_esources <- c("coal",
                           "petroleum",
                           "naturalgas",
                           "nuclear",
                           "renewables",
                           "geothermal", "solar",
                           "wind", "hydro", "wood_waste")
    names_cq_esources_shares <- paste0(names_cq_esources, " share")
    names_cq_cumcap          <- paste0(names_cq_esources, " cumcap")
    names_cq_cumcap_shares   <- paste0(names_cq_esources, " cumcap share")
    names_cq_prices          <- c("coal price", "oil price (all)",
                                  "oil price (distillate)",
                                  "naturalgas price", "nuclear price")

    names_cq_all <- c(names_cq_esources, names_cq_esources_shares,
                      names_cq_cumcap, names_cq_cumcap_shares,
                      names_cq_prices)

    if (isFALSE(length(names_all) == length(names_cq_all))) {
      stop("Number of names in dataset differs from num. of colloquial names!")
    }

    id_check <- which(names_all %in% var_names_plain)
    out[[i]] <- names_cq_all[id_check]
  }
  if (num_datasets == 1) {
    out_final <- out[[1]]
  } else if (num_datasets == 2) {
    if (grepl("price", out[[1]]) || grepl("price", out[[2]])) {
      out_final <- out[[1]]
      out_final[1] <- paste0(out_final[1], " & ", out[[2]][1])
      out_final[2] <- paste0(out_final[2], " & ", out[[2]][2])
      out_final[3] <- paste0(out_final[3], " & ", out[[2]][3])
      id_renew <- which(grepl("renew", out_final))
      out_final[id_renew] <- paste0(out_final[id_renew], " & ", out[[2]][[3]])
    } else {
      out_final <- paste(out[[1]], out[[2]], sep = " & ")
    }
  }
  return(out_final)
}
get_state_name_full <- function(state_name_short) {
  look_up_table <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL",
                     "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA",
                     "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE",
                     "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI",
                     "SC", "SD", "TN", "TX", "US", "UT", "VA", "VT", "WA", "WI",
                     "WV", "WY")
  names(look_up_table) <- c("Alaska",
                            "Alabama",
                            "Arkansas",
                            "Arizona",
                            "California",
                            "Colorado",
                            "Connecticut",
                            "District of Columbia",
                            "Delaware",
                            "Florida",
                            "Georgia",
                            "Hawaii",
                            "Iowa",
                            "Idaho",
                            "Illinois",
                            "Indiana",
                            "Kansas",
                            "Kentucky",
                            "Louisiana",
                            "Massachusetts",
                            "Maryland",
                            "Maine",
                            "Michigan",
                            "Minnesota",
                            "Missouri",
                            "Mississippi",
                            "Montana",
                            "North Carolina",
                            "North Dakota",
                            "Nebraska",
                            "New Hampshire",
                            "New Jersey",
                            "New Mexico",
                            "Nevada",
                            "New York",
                            "Ohio",
                            "Oklahoma",
                            "Oregon",
                            "Pennsylvania",
                            "Rhode Island",
                            "South Carolina",
                            "South Dakota",
                            "Tennessee",
                            "Texas",
                            "USA",
                            "Utah",
                            "Virginia",
                            "Vermont",
                            "Washington",
                            "Wisconsin",
                            "West Virginia",
                            "Wyoming")
  id_find <- which(state_name_short == look_up_table)
  return(names(look_up_table)[id_find])
}
