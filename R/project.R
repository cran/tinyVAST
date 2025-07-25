
#' @title Conditional simulation from a GMRF
#'
#' @description
#' Generates samples from a Gaussian Markov random field (GMRF) conditional upon
#' fixed values for some elements.
#'
#' @param Q precision for a zero-centered GMRF.
#' @param observed_idx integer vector listing rows of \code{Q} corresponding to
#'        fixed measurements
#' @param x_obs numeric vector with fixed values for indices \code{observed_idx}
#' @param n_sims integer listing number of simulated values
#' @param what Whether to simulate from the conditional GMRF, or predict the mean
#'        and precision
#'
#' @return
#' A matrix with \code{n_sims} columns and a row for every row of \code{Q} not in
#' \code{observed_idx}, with simulations for those rows
#'
#' @export
conditional_gmrf <-
function( Q,
          observed_idx,
          x_obs,
          n_sims = 1,
          what = c("simulate","predict") ){

  # Required libraries
  #library(Matrix)
  what = match.arg(what)

  # Error checks
  if( !all(observed_idx %in% seq_len(nrow(Q))) ){
    stop("Check `observed_idx` in `conditional_gmrf")
  }
  if( length(observed_idx) != length(x_obs) ){
    stop("Check length of `observed_idx` and `x_obs`")
  }
  if( any(is.na(x_obs)) ){
    stop("`x_obs` cannot include NA values")
  }

  # Calculate conditional mean and variance
  predict_conditional_gmrf <- function(Q, observed_idx, x_obs) {
    all_idx <- seq_len(nrow(Q))
    unobserved_idx <- setdiff(all_idx, observed_idx)

    # Partition Q
    Q_oo <- Q[observed_idx, observed_idx, drop = FALSE]
    Q_ou <- Q[observed_idx, unobserved_idx, drop = FALSE]
    Q_uo <- Matrix::t(Q_ou)
    Q_uu <- Q[unobserved_idx, unobserved_idx, drop = FALSE]

    # Compute conditional mean and covariance
    #mu_cond <- -Q_uu_inv %*% Q_uo %*% x_obs
    mu_cond <- -1 * Matrix::solve(Q_uu, Q_uo %*% x_obs)

    out = list( mean = as.vector(mu_cond),
                Q_uu = Q_uu,
                unobserved_idx = unobserved_idx )
    return(out)
  }

  res <- predict_conditional_gmrf(Q, observed_idx, x_obs )
  if( what == "predict" ){
    return(res)
  }else{
    y = rmvnorm_prec( n = n_sims, mu = res$mean, prec = res$Q_uu )
    return(y)
  }
}


#' @title Project tinyVAST to future times (EXPERIMENTAL)
#'
#' @description
#' Projects a fitted model forward in time.
#'
#' @inheritParams predict.tinyVAST
#' @param object fitted model from \code{tinyVAST(.)}
#' @param extra_times a vector of extra times, matching values in \code{newdata}
#' @param newdata data frame including new values for \code{time_variable}
#' @param future_var logical indicating whether to simulate future process errors
#'        from GMRFs, or just compute the predictive mean
#' @param past_var logical indicating whether to re-simulate past process errors
#'        from predictive distribution of random effects, thus changing the boundary
#'        condition of the forecast
#' @param parm_var logical indicating whether to re-sample fixed effects from their
#'        predictive distribution, thus changing the GMRF for future process errors
#'
#' @return
#' A vector of values corresponding to rows in \code{newdata}
#'
#' @examples
#' # Convert to long-form
#' set.seed(123)
#' n_obs = 100
#' rho = 0.9
#' sigma_x = 0.2
#' sigma_y = 0.1
#' x = rnorm(n_obs, mean=0, sd = sigma_x)
#' for(i in 2:length(x)) x[i] = rho * x[i-1] + x[i]
#' y = x + rnorm( length(x), mean = 0, sd = sigma_y )
#' data = data.frame( "val" = y, "var" = "y", "time" = seq_along(y) )
#'
#' # Define AR2 time_term
#' time_term = "
#'   y -> y, 1, rho1
#'   y -> y, 2, rho2
#'   y <-> y, 0, sd
#' "
#'
#' # fit model
#' mytiny = tinyVAST(
#'   time_term = time_term,
#'   data = data,
#'   times = unique(data$t),
#'   variables = "y",
#'   formula = val ~ 1,
#'   control = tinyVASTcontrol( getJointPrecision = TRUE )
#' )
#'
#' # Deterministic projection
#' extra_times = length(x) + 1:100
#' n_sims = 10
#' newdata = data.frame( "time" = c(seq_along(x),extra_times), "var" = "y" )
#' Y = project(
#'   mytiny,
#'   newdata = newdata,
#'   extra_times = extra_times,
#'   future_var = FALSE
#' )
#' plot( x = seq_along(Y),
#'       y = Y,
#'       type = "l", lty = "solid", col = "black" )
#'
#' # Stochastic projection with future process errors
#' \dontrun{
#' extra_times = length(x) + 1:100
#' n_sims = 10
#' newdata = data.frame( "time" = c(seq_along(x),extra_times), "var" = "y" )
#' Y = NULL
#' for(i in seq_len(n_sims) ){
#'   tmp = project(
#'     mytiny,
#'     newdata = newdata,
#'     extra_times = extra_times,
#'     future_var = TRUE,
#'     past_var = TRUE,
#'     parm_var = TRUE
#'   )
#'   Y = cbind(Y, tmp)
#' }
#' matplot( x = row(Y),
#'          y = Y,
#'          type = "l", lty = "solid", col = "black" )
#' }
#'
#' @export
project <-
function( object,
          extra_times,
          newdata,
          what = "mu_g",
          future_var = TRUE,
          past_var = FALSE,
          parm_var = FALSE ){


  ##############
  # Step 1: Generate uncertainty from parm_var and past_var
  ##############

  if( isFALSE(parm_var) & isFALSE(past_var) ){
    parlist = object$internal$parlist
  }
  if( isTRUE(parm_var) & isFALSE(past_var) ){
    stop("option not available")
  }
  if( isFALSE(parm_var) & isTRUE(past_var) ){
    parvec = object$obj$env$last.par.best
    MC = object$obj$env$MC( keep=TRUE, n=1, antithetic=FALSE )
    parvec[object$obj$env$lrandom()] = attr(MC, "samples")
    parlist = object$obj$env$parList( par = parvec )
  }
  if( isTRUE(parm_var) & isTRUE(past_var) ){
    if(is.null(object$sdrep$jointPrecision)) stop("Rerun with `getJointPrecision=TRUE`")
    parvec = rmvnorm_prec( mu = object$obj$env$last.par.best,
                           prec = object$sdrep$jointPrecision )
    parlist = object$obj$env$parList( par = parvec )
  }

  ##############
  # Step 2: Augment objects
  ##############

  all_times = union( object$internal$times, extra_times )

  ##############
  # Step 3: Build object with padded bounds
  ##############

  new_control = object$internal$control
  new_control$run_model = TRUE
  new_control$nlminb_loops = 0
  new_control$newton_loops = 0
  new_control$getsd = FALSE
  new_control$calculate_deviance_explained = FALSE
  new_control$suppress_user_warnings = TRUE
  new_control$extra_reporting = TRUE

  newobj = tinyVAST(
    formula = object$formula,
    data = object$data,
    time_term = object$internal$time_term,
    space_term = object$internal$space_term,
    spacetime_term = object$internal$spacetime_term,
    family = object$internal$family,
    space_columns = object$internal$space_columns,
    spatial_domain = object$spatial_domain,
    time_column = object$internal$time_column,
    times = all_times,
    variable_column = object$internal$variable_column,
    variables = object$internal$variables,
    distribution_column = object$internal$distribution_column,
    delta_options = list( formula = object$internal$delta_formula,
                          space_term = object$internal$delta_space_term,
                          time_term = object$internal$delta_time_term,
                          spacetime_term = object$internal$delta_spacetime_term,
                          spatial_varying = object$internal$delta_spatial_varying ),
    spatial_varying = object$internal$spatially_varying,
    weights = object$internal$weights,
    control = new_control
  )

  #
  newpar = newobj$obj$env$last.par
  oldpar = object$obj$env$last.par.best
  tmb_fixed = setdiff(unique(names(newpar)), newobj$tmb_inputs$tmb_random)
  newpar[ (names(newpar) %in% tmb_fixed) ] = oldpar[ (names(oldpar) %in% tmb_fixed) ]
  newrep = newobj$obj$report( newpar )

  ##############
  # Step 4: Merge ParList and ParList1
  ##############

  augment_epsilon <-
  function( neweps_stc,
            eps_stc,
            #beta_z,
            #model,
            linpred ){

    if( prod(dim(eps_stc)) > 0 ){
    #if( length(beta_z) > 0 ){
      #
      #mats = dsem::make_matrices(
      #  beta_p = beta_z,
      #  model = model,
      #  variables = object$internal$variables,
      #  times = all_times
      #)
      #Q_kk = Matrix::t(mats$IminusP_kk) %*% solve(Matrix::t(mats$G_kk) %*% mats$G_kk) %*% mats$IminusP_kk
      if(linpred==1){
        IminusRho_hh = Matrix::Diagonal(n=nrow(newrep$Rho_hh)) - newrep$Rho_hh
        Q_kk = Matrix::t(IminusRho_hh) %*% Matrix::solve(Matrix::t(newrep$Gamma_hh) %*% newrep$Gamma_hh) %*% IminusRho_hh
      }else{
        IminusRho_hh = Matrix::Diagonal(n=nrow(newrep$Rho_hh)) - newrep$Rho2_hh
        Q_kk = Matrix::t(IminusRho_hh) %*% Matrix::solve(Matrix::t(newrep$Gamma2_hh) %*% newrep$Gamma2_hh) %*% IminusRho_hh
      }
      Q_hh = Matrix::kronecker( Q_kk, Q_ss )

      #
      grid = expand.grid( s = seq_len(dim(neweps_stc)[1]),
                          t = all_times,
                          c = object$internal$variables )
      grid$num = seq_len(prod(dim(neweps_stc)))
      observed_idx = subset( grid, t %in% object$internal$times )$num

      #
      tmp = conditional_gmrf(
        Q = Q_hh,
        observed_idx = observed_idx,
        x_obs = as.vector( eps_stc ),
        n_sims = 1,
        what = ifelse(future_var, "simulate", "predict")
      )
      if( isTRUE(future_var) ){
        simeps_h = tmp[,1]
      }else{
        simeps_h = tmp$mean
      }

      # Compile
      #missing_indices = as.matrix(subset( grid, t %in% extra_times )[,1:3])
      #neweps_stc[missing_indices] = simeps_stc[,1]
      tset = match( extra_times, all_times )
      neweps_stc[,tset,] = simeps_h
      #observed_indices = as.matrix(subset( grid, t %in% object$internal$times )[,1:3])
      #neweps_stc[observed_indices] = eps_stc[observed_indices]
      tset = match( object$internal$times, all_times )
      neweps_stc[,tset,] = eps_stc
    }
    return(neweps_stc)
  }
  augment_delta <-
  function( newdelta_tc,
            delta_tc,
            #nu_z,
            #model,
            linpred ){

    if( prod(dim(delta_tc)) > 0 ){
    #if( length(nu_z) > 0 ){
      #
      #mats = dsem::make_matrices(
      #  beta_p = nu_z,
      #  model = model,
      #  variables = object$internal$variables,
      #  times = all_times
      #)
      #Q_kk = Matrix::t(mats$IminusP_kk) %*% solve(Matrix::t(mats$G_kk) %*% mats$G_kk) %*% mats$IminusP_kk
      if(linpred==1){
        IminusRho_hh = Matrix::Diagonal(n=nrow(newrep$Rho_time_hh)) - newrep$Rho_time_hh
        Q_kk = Matrix::t(IminusRho_hh) %*% Matrix::solve(Matrix::t(newrep$Gamma_time_hh) %*% newrep$Gamma_time_hh) %*% IminusRho_hh
      }else{
        IminusRho_hh = Matrix::Diagonal(n=nrow(newrep$Rho2_time_hh)) - newrep$Rho2_time_hh
        Q_kk = Matrix::t(IminusRho_hh) %*% Matrix::solve(Matrix::t(newrep$Gamma2_time_hh) %*% newrep$Gamma2_time_hh) %*% IminusRho_hh
      }

      #
      grid = expand.grid( t = all_times,
                          c = object$internal$variables )
      grid$num = seq_len(prod(dim(newdelta_tc)))
      observed_idx = subset( grid, t %in% object$internal$times )$num

      #
      tmp = conditional_gmrf(
        Q = Q_kk,
        observed_idx = observed_idx,
        x_obs = as.vector( delta_tc ),
        n_sims = 1,
        what = ifelse(future_var, "simulate", "predict")
      )
      if( isTRUE(future_var) ){
        simdelta_k = tmp[,1]
      }else{
        simdelta_k = tmp$mean
      }

      # Compile
      #missing_indices = as.matrix(subset( grid, t %in% extra_times )[,1:2])
      #newdelta_tc[missing_indices] = simdelta_tc[,1]
      tset = match( extra_times, all_times )
      newdelta_tc[tset,] = simdelta_k
      #observed_indices = as.matrix(subset( grid, t %in% object$internal$times )[,1:2])
      #newdelta_tc[observed_indices] = delta_tc[observed_indices]
      tset = match( object$internal$times, all_times )
      newdelta_tc[tset,] = delta_tc
    }
    return(newdelta_tc)
  }

  #
  new_parlist = newobj$tmb_inputs$tmb_par
  #new_parlist = newobj$tmb_par
  Q_ss = object$rep$Q_ss

  # Replace epsilon
  new_parlist$epsilon_stc = augment_epsilon(
    #beta_z = parlist$beta_z,
    eps_stc = parlist$epsilon_stc,
    neweps_stc = new_parlist$epsilon_stc,
    #model = object$internal$spacetime_term_ram$output$model,
    linpred = 1
  )
  new_parlist$epsilon2_stc = augment_epsilon(
    #beta_z = parlist$beta2_z,
    eps_stc = parlist$epsilon2_stc,
    neweps_stc = new_parlist$epsilon2_stc,
    #model = object$internal$delta_spacetime_term_ram$output$model,
    linpred = 2
  )

  # Replace delta
  new_parlist$delta_tc = augment_delta(
    #nu_z = parlist$nu_z,
    delta_tc = parlist$delta_tc,
    newdelta_tc = new_parlist$delta_tc,
    #model = object$internal$time_term_ram$output$model,
    linpred = 1
  )
  new_parlist$delta2_tc = augment_delta(
    #nu_z = parlist$nu2_z,
    delta_tc = parlist$delta2_tc,
    newdelta_tc = new_parlist$delta2_tc,
    #model = object$internal$delta_time_term_ram$output$model,
    linpred = 2
  )

  # Replace other variables that are not changed
  same_vars = setdiff( names(new_parlist), c("epsilon_stc","epsilon2_stc","delta_tc","delta2_tc") )
  new_parlist[same_vars] = parlist[same_vars]

  ##############
  # Step 5: Re-build model
  ##############

  #new_control$run_model = TRUE
  new_control$tmb_par = new_parlist
  new_control$extra_reporting = FALSE
  #new_control$nlminb_loops = 0
  #new_control$newton_loops = 0
  #new_control$getsd = FALSE
  #new_control$calculate_deviance_explained = FALSE

  newobj = tinyVAST(
    formula = object$formula,
    data = object$data,
    time_term = object$internal$time_term,
    space_term = object$internal$space_term,
    spacetime_term = object$internal$spacetime_term,
    family = object$internal$family,
    space_columns = object$internal$space_columns,
    spatial_domain = object$spatial_domain,
    time_column = object$internal$time_column,
    times = all_times,
    variable_column = object$internal$variable_column,
    variables = object$internal$variables,
    distribution_column = object$internal$distribution_column,
    delta_options = list( formula = object$internal$delta_formula,
                          space_term = object$internal$delta_space_term,
                          time_term = object$internal$delta_time_term,
                          spacetime_term = object$internal$delta_spacetime_term,
                          spatial_varying = object$internal$delta_spatial_varying ),
    spatial_varying = object$internal$spatially_varying,
    weights = object$internal$weights,
    control = new_control
  )

  ##############
  # Step 6: simulate samples
  ##############

  pred = predict(
    object = newobj,
    newdata = newdata,
    what = what
  )
  return(pred)
}
