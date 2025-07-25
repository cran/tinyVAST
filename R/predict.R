#' @title Predict using vector autoregressive spatio-temporal model
#'
#' @description Predicts values given new covariates using a \pkg{tinyVAST} model
#'
#' @inheritParams add_predictions
#'
#' @param remove_origdata Whether to eliminate the original data
#'        from the TMB object, thereby speeding up the TMB object construction.  However, this
#'        also eliminates information about random-effect variance, and is not
#'        appropriate when requesting predictive standard errors or epsilon
#'        bias-correction.
#' @param what What REPORTed object to output, where
#'        \code{mu_g} is the inverse-linked transformed predictor including both linear components,
#'        \code{p_g} is the first linear predictor,
#'        \code{palpha_g} is the first predictor from fixed covariates in \code{formula},
#'        \code{pgamma_g} is the first predictor from random covariates in \code{formula} (e.g., splines),
#'        \code{pomega_g} is the first predictor from spatial variation,
#'        \code{pepsilon_g} is the first predictor from spatio-temporal variation,
#'        \code{pxi_g} is the first predictor from spatially varying coefficients,
#'        \code{p2_g} is the second linear predictor,
#'        \code{palpha2_g} is the second predictor from fixed covariates in \code{formula},
#'        \code{pgamma2_g} is the second predictor from random covariates in \code{formula} (e.g., splines),
#'        \code{pomega2_g} is the second predictor from spatial variation,
#'        \code{pepsilon2_g} is the second predictor from spatio-temporal variation, and
#'        \code{pxi2_g} is the second predictor from spatially varying coefficients.
#' @param se.fit Calculate standard errors?
#' @param bias.correct whether to epsilon bias-correct the predicted value
#' @param ... Not used.
#' @importFrom Matrix Matrix sparseMatrix
#'
#' @return
#' Either a vector with the prediction for each row of \code{newdata}, or a named list
#' with the prediction and standard error (when \code{se.fit = TRUE}).
#'
#' @method predict tinyVAST
#' @export
predict.tinyVAST <-
function( object,
          newdata,
          remove_origdata = FALSE,
          what = c("mu_g", "p_g", "palpha_g", "pgamma_g", "pepsilon_g", "pomega_g", "pdelta_g", "pxi_g",
                   "p2_g", "palpha2_g", "pgamma2_g", "pepsilon2_g", "pomega2_g", "pdelta2_g", "pxi2_g"),
          se.fit = FALSE,
          bias.correct = FALSE,
          ... ){

  # extract original X and Z
  check_tinyVAST_version( object$internal$packageVersion )
  if(missing(newdata)){
    newdata = object$data
  }
  if( isTRUE(bias.correct) & isFALSE(se.fit) ) stop("`bias.correct` can only be done if `se.fit=TRUE`")
  assertDataFrame(newdata)
  if(inherits(newdata,"tbl")) stop("`data` must be a data.frame and cannot be a tibble")
  what = match.arg(what)

  # Build new
  tmb_data2 = add_predictions( object = object,
                               newdata = newdata,
                               remove_origdata = remove_origdata )

  # Rebuild object
  newobj = MakeADFun( data = tmb_data2,
                      parameters = object$internal$parlist,
                      map = object$tmb_inputs$tmb_map,
                      random = object$tmb_inputs$tmb_random,
                      profile = object$internal$control$profile,
                      DLL = "tinyVAST" )
  newobj$env$beSilent()
  out = newobj$report()[[what]]

  # Add standard errors
  if( isTRUE(se.fit) ){
    if( !(what %in% "p_g") ) stop("se.fit=TRUE only works for what=`p_g`", call. = FALSE)
    if( remove_origdata==TRUE ) stop("se.fit=TRUE only works for remove_origdata=FALSE", call. = FALSE)
    newsd = sdreport( obj = newobj,
                      par.fixed = object$opt$par,
                      hessian.fixed = object$internal$Hess_fixed,
                      bias.correct = FALSE,
                      getReportCovariance = FALSE )
    SD = as.list( newsd, what="Std. Error", report=TRUE )
    out = list( fit = out,
                se.fit = SD[[what]] )
    if( isTRUE(bias.correct) ){
      bias.corrected = as.list( newsd, what="Est. (bias.correct)", report=TRUE )
      out$bias.corrected = bias.corrected[[what]]
    }
  }

  return( out )
}

#' @title Integration for target variable
#'
#' @description 
#' Calculates an estimator for a derived quantity by summing across multiple predictions.
#' This can be used to approximate an integral when estimating area-expanded abundance, 
#' abundance-weighting a covariate to calculate distribution shifts, 
#' and/or weighting one model variable by another.
#'
#' @inheritParams add_predictions
#'
#' @param newdata New data-frame of independent variables used to predict the response,
#'        where a total value is calculated by combining across these individual predictions.
#'        If these locations are randomly drawn from a specified spatial domain, then 
#'        `integrate_output` applies midpoint integration to approximate the
#'        total over that area.  If locations are drawn sysmatically from a domain,
#'        then `integrate_output` is applying a midpoint approximation to the integral.
#' @param area vector of values used for area-weighted expansion of 
#'        estimated density surface for each row of `newdata`
#'        with length of \code{nrow(newdata)}.
#' @param type Integer-vector indicating what type of expansion to apply to
#'        each row of `newdata`, with length of \code{nrow(newdata)}.  
#' \describe{
#'   \item{\code{type=1}}{Area-weighting: weight predictor by argument `area`}
#'   \item{\code{type=2}}{Abundance-weighted covariate: weight `covariate` by 
#'   proportion of total in each row of `newdata`}
#'   \item{\code{type=3}}{Abundance-weighted variable: weight predictor by 
#'   proportion of total in a prior row of `newdata`. 
#'   This option is used to weight a prediction for 
#'   one category based on predicted proportional density of another category, e.g.,
#'   to calculate abundance-weighted condition in a bivariate model.}
#'   \item{\code{type=4}}{Abundance-expanded variable: weight predictor by
#'   density in a prior row of `newdata`.
#'   This option is used to weight a prediction for
#'   one category based on predicted density of another category, e.g.,
#'   to calculate abundance-expanded consumption in a bivariate model.}
#'   \item{\code{type=0}}{Exclude from weighting: give weight of zero for
#'   a given row of `newdata`. Including a row of `newdata` with 
#'   \code{type=0} is useful, e.g., when calculating abundance at that 
#'   location, where the eventual index uses abundance as weighting term
#'   but without otherwise using the predicted density in calculating a total
#'   value.}
#' }
#' @param covariate numeric-vector used to provide a covariate
#'        that is used in expansion, e.g., to provide positional 
#'        coordinates when calculating the abundance-weighted centroid with respect 
#'        to that coordinate. Only used for when \code{type=2}.
#' @param weighting_index integer-vector used to indicate a previous row
#'        that is used to calculate a weighted average that is then 
#'        applied to the given row of `newdata`. Only used for when \code{type=3}.
#' @param getsd logical indicating whether to get the standard error, where
#'        \code{getsd=FALSE} is faster during initial exploration
#' @param bias.correct logical indicating if bias correction should be applied using
#'        standard methods in [TMB::sdreport()]
#' @param intern Do Laplace approximation on C++ side? Passed to [TMB::MakeADFun()].
#' @param apply.epsilon Apply epsilon bias correction using a manual calculation
#'        rather than using the conventional method in [TMB::sdreport]?  
#'        See details for more information.  
#'
#' @details
#' Analysts will often want to calculate some value by combining the predicted response at multiple
#' locations, and potentially from multiple variables in a multivariate analysis. 
#' This arises in a univariate model, e.g., when calculating the integral under a predicted
#' density function, which is approximated using a midpoint or Monte Carlo approximation
#' by calculating the linear predictors at each location `newdata`, 
#' applying the inverse-link-trainsformation,
#' and calling this predicted response `mu_g`.  Total abundance is then be approximated
#' by multiplying `mu_g` by the area associated with each midpoint or Monte Carlo 
#' approximation point (supplied by argument `area`), 
#' and summing across these area-expanded values.
#'
#' In more complicated cases, an analyst can then use `covariate` 
#' to calculate the weighted average
#' of a covariate for each midpoint location. For example, if the covariate is 
#' positional coordinates or depth/elevation, then \code{type=2}
#' measures shifts in the average habitat utilization with respect to that covariate.  
#' Alternatively, an analyst fitting a multivariate model might weight one variable
#' based on another using `weighting_index`, e.g., 
#' to calculate abundance-weighted average condition, or
#' predator-expanded stomach contents.
#'
#' In practice, spatial integration in a multivariate model requires two passes through the rows of
#' `newdata` when calculating a total value.  In the following, we
#' write equations using C++ indexing conventions such that indexing starts with 0, 
#' to match the way that `integrate_output` expects indices to be supplied.  
#' Given inverse-link-transformed predictor \eqn{ \mu_g }, 
#' function argument `type` as \eqn{ type_g }
#' function argument `area` as \eqn{ a_g },
#' function argument `covariate` as \eqn{ x_g }, 
#' function argument `weighting_index` as `\eqn{ h_g }`
#' function argument `weighting_index` as `\eqn{ h_g }`
#' the first pass calculates:
#'
#' \deqn{ \nu_g = \mu_g a_g }
#'
#' where the total value from this first pass is calculated as:
#'
#' \deqn{ \nu^* = \sum_{g=0}^{G-1} \nu_g }
#' 
#' The second pass then applies a further weighting, which depends upon \eqn{ type_g },
#' and potentially upon \eqn{ x_g } and \eqn{ h_g }.
#'
#' If \eqn{type_g = 0} then \eqn{\phi_g = 0}
#'  
#' If \eqn{type_g = 1} then \eqn{\phi_g = \nu_g}
#'  
#' If \eqn{type_g = 2} then \eqn{\phi_g = x_g \frac{\nu_g}{\nu^*} }
#'  
#' If \eqn{type_g = 3} then \eqn{\phi_g = \frac{\nu_{h_g}}{\nu^*} \mu_g }
#'  
#' If \eqn{type_g = 4} then \eqn{\phi_g = \nu_{h_g} \mu_g }
#'
#' Finally, the total value from this second pass is calculated as:
#'
#' \deqn{ \phi^* = \sum_{g=0}^{G-1} \phi_g }
#'
#' and \eqn{\phi^*} is outputted by `integrate_output`, 
#' along with a standard error and potentially using
#' the epsilon bias-correction estimator to correct for skewness and retransformation
#' bias.
#'
#' Standard bias-correction using \code{bias.correct=TRUE} can be slow, and in 
#' some cases it might be faster to do \code{apply.epsilon=TRUE} and \code{intern=TRUE}.
#' However, that option is somewhat experimental, and a user might want to confirm
#' that the two options give identical results. Similarly, using \code{bias.correct=TRUE}
#' will still calculate the standard-error, whereas using
#' \code{apply.epsilon=TRUE} and \code{intern=TRUE} will not.  
#'
#' @return
#' A vector containing the plug-in estimate, standard error, the epsilon bias-corrected
#' estimate if available, and the standard error for the bias-corrected estimator.
#' Depending upon settings, one or more of these will be \code{NA} values, and the
#' function can be repeatedly called to get multiple estimators and/or statistics.
#' 
#' @export
integrate_output <-
function( object,
          newdata,
          area,
          type = rep(1,nrow(newdata)),
          weighting_index,
          covariate,
          getsd = TRUE,
          bias.correct = TRUE,
          apply.epsilon = FALSE,
          intern = FALSE
  ){

  # extract original X and Z
  check_tinyVAST_version( object$internal$packageVersion )
  if(missing(newdata)) newdata = object$data
  # Build new .. object$data must be same as used for fitting to get SEs / skewness of random effects
  tmb_data2 = add_predictions( object = object,
                               newdata = newdata ) # ,
                               # remove_origdata = isFALSE(apply.epsilon) & isFALSE(bias.correct) )

  # Check for no random effects
  if( length(object$obj$env$random)==0 ){
    if( isTRUE(bias.correct) || isTRUE(apply.epsilon) ){
      stop("No random effects present, so set `bias.correct=FALSE` and `apply.epsilon=FALSE` in `integrate_output(.)`")
    }
  }


  # Expansion area
  if(missing(area)){
    area = rep(1, nrow(newdata))
  }else if(length(area)==1){
    area = rep(area, nrow(newdata))
  }  
  checkNumeric( area, lower=0, len=nrow(newdata), any.missing=FALSE )

  # Expansion type
  if(missing(type)){
    type = rep(1, nrow(newdata))
  }else if(length(type)==1){
    type = rep(type, nrow(type))
  }  
  checkInteger( type, lower=0, upper=4, len=nrow(newdata), any.missing=FALSE )
  
  # Index for variable-weighted value
  if(missing(weighting_index)){
    weighting_index = rep(0, nrow(newdata))
  }
  if( any(weighting_index>=seq_len(nrow(newdata))) ){
    stop("Invalid `weighting_index`")
  }
  checkInteger( weighting_index, lower=0, len=nrow(newdata), any.missing=FALSE )
  
  # 
  if(missing(covariate)){
    covariate = rep(0, nrow(newdata))
  }
  checkNumeric( covariate, len=nrow(newdata), any.missing=FALSE )
  
  # Bundle
  tmb_data2$V_gz = cbind( type, weighting_index )
  tmb_data2$W_gz = cbind( area, covariate )
  
  # Area-expanded sum
  #if(missing(W_gz)){
  #  # Default for area
  #  if(missing(area)){
  #    area = rep(1, nrow(newdata))
  #  }else if(length(area)==1){
  #    area = rep(area, nrow(newdata))
  #  }else if( length(area)!=nrow(newdata) ){
  #    stop("Check length of `area`")
  #  }
  #  tmb_data2$W_gz = cbind(area, 0)
  #}else{
  #  tmb_data2$W_gz = W_gz
  #}
  #if(missing(V_gz)){
  #  tmb_data2$V_gz = cbind( rep(1,nrow(newdata)), 0 )
  #}else{
  #  tmb_data2$V_gz = V_gz
  #}

  # Abundance-weighted z
  #tmb_data2$W_gz = cbind( 1, newdata$x )
  #tmb_data2$V_gz = matrix(1, nrow=nrow(newdata), ncol=2)

  #
  tmb_par2 = object$internal$parlist
  if( isTRUE(apply.epsilon) ){
    tmb_par2$eps = 0
    inner.control = list(sparse=TRUE, lowrank=TRUE, trace=FALSE)
  }else{
    inner.control = list(sparse=TRUE, lowrank=FALSE, trace=FALSE)
  }

  # Rebuild object
  newobj = MakeADFun( data = tmb_data2,
                      parameters = tmb_par2,
                      map = object$tmb_inputs$tmb_map,
                      random = object$tmb_inputs$tmb_random,
                      DLL = "tinyVAST",
                      intern = intern,
                      inner.control = inner.control,
                      profile = object$internal$control$profile )
  newobj$env$beSilent()

  # Run sdreport
  if( isTRUE(apply.epsilon) ){
    #Metric = newobj$report()$Metric
    grad = newobj$gr( newobj$par )[which(names(newobj$par)=="eps")]
    out = c( "Estimate"=NA, "Std. Error"=NA, "Est. (bias.correct)"=grad, "Std. (bias.correct)"=NA )
  }else if( isTRUE(getsd) | isTRUE(bias.correct) ){
    newsd = sdreport( obj = newobj,
                      par.fixed = object$opt$par,
                      hessian.fixed = object$internal$Hess_fixed,
                      bias.correct = bias.correct )
    out = summary(newsd, "report")['Metric',]
  }else{
    rep = newobj$report()
    out = c( "Estimate"=rep$Metric, "Std. Error"=NA, "Est. (bias.correct)"=NA, "Std. (bias.correct)"=NA )
  }
  
  # deal with memory internally
  remove("newobj")
  gc() 
  
  # return stuff
  return(out)
}

#' @title Add predictions to data-list
#'
#' @description Given user-provided `newdata`, expand the object `tmb_data`
#'    to include predictions corresponding to those new observations
#'
#' @param object Output from [tinyVAST()].
#' @param newdata New data-frame of independent variables used to predict the response.
#' @param remove_origdata Whether to remove original-data to allow faster evaluation.
#'        \code{remove_origdata=TRUE} eliminates information about the distribution
#'        for random effects, and cannot be combined with epsilon bias-correction.
#'        WARNING:  feature is experimental and subject to change.
#'
#' @return
#' the object \code{fit$tmb_inputs$tmb_data} representing data used during fitting,
#' but with updated values for slots associated with predictions, where this
#' updated object can be recompiled by TMB to provide predictions
#'
#' @export
add_predictions <-
function( object,
          newdata,
          remove_origdata = FALSE ){

  tmb_data = object$tmb_inputs$tmb_data
  origdata = object$data
  if(inherits(newdata,"tbl")) stop("`data` must be a data.frame and cannot be a tibble")

  # Check newdata for missing variables and/or factors
  pred_set = unique(unlist(sapply( object$internal[c('gam_setup','delta_gam_setup')],
                            FUN=\(x) all.vars(x$pred.formula) ) ))
  for( pred in pred_set ){
    if( !(pred %in% colnames(newdata)) ){
      stop("Missing ", pred, " in newdata")
    }
    #
    if( is.factor(origdata[,pred]) ){
      if( !all(unique(newdata[,pred]) %in% levels(origdata[,pred])) ){
        stop("`newdata` column ", pred, "has new levels not present in the fitted data, which is not permitted")
      }
    }
    # If predictor is a factor, make sure newdata has same level order
    if( is.factor(origdata[,pred]) ){
      newdata[,pred] = factor(newdata[,pred], levels=levels(origdata[,pred]))
    }
    # Check for NAs after factor(..., levels=levels(origdata)), which converts new levels to NAs
    if( any(is.na(newdata[,pred])) ){
      stop("`newdata` column ", pred, " has NAs, which is not permitted")
    }
  }

  make_covariates <-
  function( gam ){

    remove_s_t2_ti_t2 <- function(formula) {
      # FROM sdmTMB smoothers.R
      terms <- stats::terms(formula)
      terms_labels <- attr(terms, "term.labels")
      drop_s <- grep("s\\(", terms_labels)
      drop_t2 <- grep("t2\\(", terms_labels)
      drop_te <- grep("te\\(", terms_labels)
      drop_ti <- grep("ti\\(", terms_labels)
      drop_all = unique( c(drop_s, drop_t2, drop_te, drop_ti) )
      tdrop <- terms_labels[drop_all]
      for (i in seq_along(tdrop)) {
        formula <- stats::update(formula, paste("~ . -", tdrop[i]))
      }
      formula
    }

    # Assemble predZ
    Z_gk = lapply( seq_along(gam$smooth),
                    FUN = \(x) mgcv::PredictMat(gam$smooth[[x]], data=newdata) )
    Z_gk = do.call( cbind, Z_gk )
    #if(is.null(Z_gk)) Z_gk = matrix( 0, nrow=nrow(newdata), ncol=ncol(tmb_data$Z_ik) )
    if(is.null(Z_gk)) Z_gk = matrix( 0, nrow=nrow(newdata), ncol=0 )

    # Assemble predX
    formula_no_sm = remove_s_t2_ti_t2(gam$formula)
    mf1 = model.frame(formula_no_sm, origdata, drop.unused.levels=TRUE)
    #  drop.unused.levels necessary when using formula = ~ interaction(X,Y), which otherwise creates full-factorial combination of levels
    terms1 = attr(mf1, "terms")
    # X_ij = model.matrix(terms1, mf1)
    terms2 = stats::terms(formula_no_sm)
    attr(terms2, "predvars") = attr(terms1, "predvars")
    terms2 = stats::delete.response(terms2)
    mf2 = model.frame(terms2, newdata, xlev=.getXlevels(terms1,mf1))
    X_gj = model.matrix(terms2, mf2)
    offset_g = model.offset(mf2)
    if(is.null(offset_g)) offset_g = rep(0,nrow(newdata))

    # out
    list( "X_gj"=X_gj, "Z_gk"=Z_gk, "offset_g"=offset_g)
  }
  covariates = make_covariates( object$internal$gam_setup )
  covariates2 = make_covariates( object$internal$delta_gam_setup )
  
  make_spatial_varying <-
  function( spatial_varying ){
    if( is.null(spatial_varying) ){
      spatial_varying = ~ 0
    }
    W_gl = model.matrix( spatial_varying, data = newdata )
    return(W_gl)
  }
  W_gl = make_spatial_varying( object$internal$spatial_varying )
  W2_gl = make_spatial_varying( object$internal$delta_spatial_varying )

  # Assemble A_gs
  if( is(object$spatial_domain, "fm_mesh_2d") ){
    A_gs = fm_evaluator( object$spatial_domain, loc=as.matrix(newdata[,object$internal$space_columns]) )$proj$A
  }else if( is(object$spatial_domain, "igraph") ) {
    Match = match( newdata[,object$internal$space_columns], rownames(object$tmb_inputs$tmb_data$Adj) )
    if(any(is.na(Match))) stop("Check `spatial_domain` for SAR")
    A_gs = sparseMatrix( i=seq_len(nrow(newdata)), j=Match, x=rep(1,nrow(newdata)) )
  }else if( is(object$spatial_domain,"sfnetwork_mesh") ){      # if( !is.null(sem) )
    # stream network
    A_gs = sfnetwork_evaluator( stream = object$spatial_domain$stream,
                                loc = as.matrix(newdata[,object$internal$space_columns]) )
  }else if( is(object$spatial_domain, "sfc_GEOMETRY") ){
    sf_coords = st_as_sf( newdata,
                          coords = object$internal$space_columns,
                          crs = st_crs(object$spatial_domain) )
    s_i = as.integer(st_within( sf_coords, object$spatial_domain ))
    if(any(is.na(s_i))){
      stop("Some rows of `newdata` in `predict` are not within the SAR domain.
            Please exclude rows listed below or refit model with extended domain.\n",
            paste0(which(is.na(s_i)), collapse = ", ") )
    }
    A_gs = sparseMatrix( i = seq_along(s_i),
                         j = s_i,
                         x = 1,
                         dims = c(length(s_i),length(object$spatial_domain)) )
  }else{
    A_gs = matrix(1, nrow=nrow(newdata), ncol=1)    # dgCMatrix
    A_gs = as(Matrix(A_gs),"CsparseMatrix")
  }
  predAtriplet = Matrix::mat2triplet(A_gs)

  # Turn of t_i and c_i when times and variables are missing, so that delta_k isn't built
  if( length(object$internal$times) > 0 ){
    t_g = match( newdata[,object$internal$time_column], object$internal$times )
  }else{
    t_g = integer(0)
  }
  if( length(object$internal$variables) > 0 ){
    # Telescoping for a univariate model using var = "response" by default
    if( !(object$internal$variable_column %in% colnames(newdata)) ){
      newdata = data.frame( newdata, matrix(object$internal$variables[1], nrow=nrow(newdata), ncol=1, dimnames=list(NULL,object$internal$variable_column)) )
    }
    c_g = match( newdata[,object$internal$variable_column], object$internal$variables )
  }else{
    c_g = integer(0)
  }

  #
  AepsilonG_zz = cbind(predAtriplet$i, predAtriplet$j, t_g[predAtriplet$i], c_g[predAtriplet$i])
  which_Arows = which(apply( AepsilonG_zz, MARGIN=1, FUN=\(x) all(!is.na(x)) & any(x>0) ))
  which_Arows = which_Arows[ which(predAtriplet$x[which_Arows] > 0) ]
  if( (nrow(object$internal$spacetime_term_ram$output$ram)==0) & (nrow(object$internal$delta_spacetime_term_ram$output$ram)==0) ){
    which_Arows = numeric(0)
  }
  AepsilonG_zz = AepsilonG_zz[which_Arows,,drop=FALSE]
  AepsilonG_z = predAtriplet$x[which_Arows]

  #
  AomegaG_zz = cbind(predAtriplet$i, predAtriplet$j, c_g[predAtriplet$i])
  which_Arows = which(apply( AomegaG_zz, MARGIN=1, FUN=\(x) all(!is.na(x)) ))
  which_Arows = which_Arows[ which(predAtriplet$x[which_Arows] > 0) ]
  if( (nrow(object$internal$space_term_ram$output$ram)==0) & (nrow(object$internal$delta_space_term_ram$output$ram)==0) ){
    which_Arows = numeric(0)
  }
  AomegaG_zz = AomegaG_zz[which_Arows,,drop=FALSE]
  AomegaG_z = predAtriplet$x[which_Arows]

  # Telescope
  if( !(object$internal$distribution_column %in% colnames(newdata)) ){
    if( length(object$internal$family)>1 ) stop("Must supply `dist` if using multiple `family_link` options")
    newdata = data.frame( newdata, matrix(names(object$internal$family)[1], nrow=nrow(newdata), ncol=1, dimnames=list(NULL,object$internal$distribution_column)) )
  }

  # Build e_i ... always has length nrow(data)
  e_g = match( newdata[,object$internal$distribution_column], names(object$internal$family) )

  #
  if( !(object$internal$distribution_column %in% colnames(newdata)) ){
    newdata = cbind( newdata, matrix(1, nrow=nrow(newdata), ncol=1, dimnames=list(NULL,object$internal$distribution_column)) )
  }

  #
  if( !(object$internal$distribution_column %in% colnames(newdata)) ){
    newdata = cbind( newdata, matrix(1, nrow=nrow(newdata), ncol=1, dimnames=list(NULL,object$internal$distribution_column)) )
  }
  # Error checks
  tmb_data2 = object$tmb_inputs$tmb_data
  if( ncol(tmb_data2$X_ij) != ncol(covariates$X_gj) ) stop("Check X_gj")
  if( ncol(tmb_data2$Z_ik) != ncol(covariates$Z_gk) ) stop("Check Z_gk")
  if( ncol(tmb_data2$X2_ij) != ncol(covariates2$X_gj) ) stop("Check X2_gj")
  if( ncol(tmb_data2$Z2_ik) != ncol(covariates2$Z_gk) ) stop("Check Z2_gk")

  # Swap in new predictive stuff
  tmb_data2$X_gj = covariates$X_gj
  tmb_data2$Z_gk = covariates$Z_gk
  tmb_data2$X2_gj = covariates2$X_gj
  tmb_data2$Z2_gk = covariates2$Z_gk
  tmb_data2$AepsilonG_zz = AepsilonG_zz - 1    # Triplet form, i, s, t
  tmb_data2$AepsilonG_z = AepsilonG_z
  tmb_data2$AomegaG_zz = AomegaG_zz - 1    # Triplet form, i, s, t
  tmb_data2$AomegaG_z = AomegaG_z
  tmb_data2$t_g = ivector_minus_one(t_g) # Convert to CPP indexing, and keep as integer-vector
  tmb_data2$c_g = ivector_minus_one(c_g) # Convert to CPP indexing, and keep as integer-vector
  tmb_data2$offset_g = covariates$offset_g
  tmb_data2$e_g = ivector_minus_one(e_g) # -1 to convert to CPP index, and keep as integer-vector
  tmb_data2$A_gs = A_gs
  tmb_data2$W_gl = W_gl
  tmb_data2$W2_gl = W2_gl

  # Simplify by eliminating observations ... experimental
  if( isTRUE(remove_origdata) ){
    warning("`remove_origdata` is experimental")
    tmb_data2$y_i = tmb_data2$y_i[numeric(0)]
    tmb_data2$X_ij = tmb_data2$X_ij[numeric(0),,drop=FALSE]
    tmb_data2$Z_ik = tmb_data2$Z_ik[numeric(0),,drop=FALSE]
    tmb_data2$t_i = tmb_data2$t_i[numeric(0)]
    tmb_data2$c_i = tmb_data2$c_i[numeric(0)]
    tmb_data2$e_i = tmb_data2$e_i[numeric(0)]
    tmb_data2$Aepsilon_zz = tmb_data2$Aepsilon_zz[numeric(0),,drop=FALSE]
    tmb_data2$Aepsilon_z = tmb_data2$Aepsilon_z[numeric(0)]
    tmb_data2$Aomega_zz = tmb_data2$Aomega_zz[numeric(0),,drop=FALSE]
    tmb_data2$Aomega_z = tmb_data2$Aomega_z[numeric(0)]
  }

  # Check for obvious issues ... no NAs except in RAMstart
  index_drop = match(c("ram_space_term_start","ram_time_term_start","ram_spacetime_term_start","ram2_space_term_start","ram2_time_term_start","ram2_spacetime_term_start"),names(tmb_data2))
  if( any(is.na(tmb_data2[-index_drop])) ){
    stop("Check output of `add_predictions` for NAs")
  }
  # Check for obvious issues ... length of inputs
  if( !all( sapply(tmb_data2[c('t_g','c_g','e_g','offset_g')],FUN=length) %in% c(0,nrow(newdata)) ) ){
    stop("Check output of `add_predictions` for variables with unexpected length")
  }
  if( any( sapply(tmb_data2[c('X_gj','Z_gk','X2_gj','Z2_gk')],FUN=nrow) != nrow(newdata) ) ){
    stop("Check output of `add_predictions` for mismatch in dimensions")
  }

  return( tmb_data2 )
}
