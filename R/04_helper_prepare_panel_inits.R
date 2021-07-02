#' Helper function to obtain initial values from prior estimations.
#'
#' The output of the \code{pgas_xxx_R()} or \code{pgas_xxx_cpp()} is used to
#' obtain initial starting/initial values for parameters and latent states for
#' subsequent estimation. The last (particle) MCMC iteration is used.
#'
#' @param path_to_inits path to \code{.RData}-file of the output of
#'   \code{pgas_xxx_R()} or \code{pgas_xxx_cpp()}
#' @param dim_reg_z either \code{NULL} or the vector giving the z-type regressor
#'   dimensions (typically used when working with simulated data where there are
#'   no regressor names); if \code{NULL}, then \code{reg_names_z} must be
#'   specified!
#' @param dim_reg_u either \code{NULL} or the vector giving the u-type regressor
#'   dimensions (typically used when working with simulated data where there are
#'   no regressor names); if \code{NULL}, then \code{reg_names_} must be
#'   specified!
#' @param reg_names_Z either \code{NULL} or the list of character vectors
#'  giving the z-type regressor names (typically used when working with real
#'  data); if \code{NULL}, then \code{dim_reg_z} must be specified!
#' @param reg_names_U either \code{NULL} or the list of character vectors
#'  giving the u-type regressor names (typically used when working with real
#'  data); if \code{NULL}, then \code{dim_reg_u} must be specified!
#'
#' @return a list of two: \code{par_inits} and \code{states_init} with
#'   structures to be directly used as an argument to \code{pgas_xxx_R()} or
#'   \code{pgas_xxx_cpp()} type function
#' @export
get_inits_from_prior_output <- function(path_to_inits,
                                        dim_reg_z = NULL,
                                        dim_reg_u = NULL,
                                        reg_names_Z = NULL,
                                        reg_names_U = NULL) {
  load(path_to_inits)
  check_out_data <- grep("out", ls(), value = TRUE)
  if (length(check_out_data) > 1 || length(check_out_data) == 0) {
    stop("Ambiguity in output names: 'out' is found several times.")
  }
  my_out_data <- eval(parse(text = check_out_data))

  DD_old       <- dim(my_out_data$x)[2]
  num_mcmc_old <- dim(my_out_data$x)[3]
  NN_old       <- dim(my_out_data$x)[4]
  # 2. Initialization for the states ----------------------------------------
  states_init <- my_out_data$x[, , num_mcmc_old, ]
  # 3. Initialization for the parameters ------------------------------------
  init_sig_sq <- matrix(my_out_data$sig_sq_x[, num_mcmc_old],
                        nrow = DD_old, ncol = 1)
  init_phi    <- matrix(my_out_data$phi_x[, num_mcmc_old],
                        nrow = DD_old, ncol = 1)

  if (!is.null(reg_names_Z)) {
    dim_zet <- sapply(reg_names_Z, length)
  } else if (!is.null(dim_reg_z)) {
    dim_zet <- dim_reg_z
  } else {
    stop("No dimension information for Z-type regressors.")
  }
  if (!is.null(reg_names_U)) {
    dim_uet <- sapply(reg_names_U, length)
  } else if (!is.null(dim_reg_u)) {
    dim_uet <- dim_reg_u
  } else {
    stop("No dimension information for U-type regressors.")
  }

  init_bet_z     <- vector("list", DD_old)
  init_bet_u     <- vector("list", DD_old)
  init_vcm_bet_u <- vector("list", DD_old)

  dim_zet_id <- c(0, cumsum(dim_zet))
  dim_uet_id <- c(0, cumsum(dim_uet))

  for (d in 1:DD_old) {
    init_bet_z[[d]] <- my_out_data$bet_z[(dim_zet_id[d] + 1):(dim_zet_id[d + 1]),
                                       num_mcmc_old]
    init_bet_u[[d]] <- matrix(0, nrow = dim_uet[d], ncol = NN_old)
    for (n in 1:NN_old) {
      init_bet_u[[d]][, n] <- my_out_data$bet_u[(dim_uet_id[d] + 1):(dim_uet_id[d + 1]),
                                          num_mcmc_old, n]
    }
  par_inits <- list(init_sig_sq = init_sig_sq,
                    init_phi = init_phi,
                    init_bet_z = init_bet_z,
                    init_bet_u = init_bet_u)

    if (any(grepl("vcm_bet_u", names(my_out_data)))) {
      for (d in 1:DD_old) {
        init_vcm_bet_u[[d]] <- my_out_data$vcm_bet_u[[d]][, , num_mcmc_old]
      }
      par_inits$init_vcm_bet_u = init_vcm_bet_u
    }
  }
  return(list(par_inits = par_inits,
              states_init = states_init))
}
