#' @title Make a RAM (Reticular Action Model)
#'
#' @inheritParams tinyVAST
#' @inheritParams sem::specifyModel
#'
#' @description \code{make_dsem_ram} converts SEM arrow notation to \code{ram} describing SEM parameters
#'
#' @param dsem dynamic structural equation model structure,
#'        passed to either \code{\link[sem]{specifyModel}}
#'        or \code{\link[sem]{specifyEquations}} and then parsed to control
#'        the set of path coefficients and variance-covariance parameters
#' @param times A character vector listing the set of times in order
#' @param variables A character vector listing the set of variables
#' @param quiet Boolean indicating whether to print messages to terminal
#' @param remove_na Boolean indicating whether to remove NA values from RAM (default) or not.
#'            \code{remove_NA=FALSE} might be useful for exploration and diagnostics for
#'            advanced users
#'
#' @details
#' \strong{RAM specification using arrow-and-lag notation}
#'
#' Each line of the RAM specification for \code{\link{make_dsem_ram}} consists of four (unquoted) entries,
#' separated by commas:
#'
#' \describe{
#'   \item{1. Arrow specification:}{This is a simple formula, of the form
#'     \code{A -> B} or, equivalently, \code{B <- A} for a regression
#'     coefficient (i.e., a single-headed or directional arrow);
#'     \code{A <-> A} for a variance or \code{A <-> B} for a covariance
#'     (i.e., a double-headed or bidirectional arrow). Here, \code{A} and
#'     \code{B} are variable names in the model. If a name does not correspond
#'     to an observed variable, then it is assumed to be a latent variable.
#'     Spaces can appear freely in an arrow specification, and
#'     there can be any number of hyphens in the arrows, including zero: Thus,
#'     e.g., \code{A->B}, \code{A --> B}, and \code{A>B} are all legitimate
#'     and equivalent.}
#'   \item{2. Lag (using positive values):}{An integer specifying whether the linkage
#'     is simultaneous (\code{lag=0}) or lagged (e.g., \code{X -> Y, 1, XtoY}
#'     indicates that X in time T affects Y in time T+1), where
#'     only one-headed arrows can be lagged. Using positive values to indicate lags
#'      then matches the notational convention used in package \pkg{dynlm}.}
#'   \item{3. Parameter name:}{The name of the regression coefficient, variance,
#'     or covariance specified by the arrow. Assigning the same name to two or
#'     more arrows results in an equality constraint. Specifying the parameter name
#'     as \code{NA} produces a fixed parameter.}
#'   \item{4. Value:}{start value for a free parameter or value of a fixed parameter.
#'     If given as \code{NA} (or simply omitted), the model is provide a default
#'     starting value.}
#' }
#'
#' Lines may end in a comment following #. The function extends code copied from package
#' `sem` under licence GPL (>= 2) with permission from John Fox.
#'
#' \strong{Simultaneous autoregressive process for simultaneous and lagged effects}
#'
#' This text then specifies linkages in a multivariate time-series model for variables \eqn{\mathbf X}
#' with dimensions \eqn{T \times C} for \eqn{T} times and \eqn{C} variables.
#' \code{make_dsem_ram} then parses this text to build a path matrix \eqn{\mathbf P} with
#' dimensions \eqn{TC \times TC}, where \eqn{\rho_{k_2,k_1}}
#' represents the impact of \eqn{x_{t_1,c_1}} on \eqn{x_{t_2,c_2}}, where \eqn{k_1=T c_1+t_1}
#' and \eqn{k_2=T c_2+t_2}.  This path matrix defines a simultaneous equation
#'
#' \deqn{ \mathrm{vec}(\mathbf X) = \mathbf P \mathrm{vec}(\mathbf X) + \mathrm{vec}(\mathbf \Delta)}
#'
#' where \eqn{\mathbf \Delta} is a matrix of exogenous errors with covariance \eqn{\mathbf{V = \Gamma \Gamma}^t},
#' where \eqn{\mathbf \Gamma} is the Cholesky of exogenous covariance.  This
#' simultaneous autoregressive (SAR) process then results in \eqn{\mathbf X} having covariance:
#'
#' \deqn{ \mathrm{Cov}(\mathbf X) = \mathbf{(I - P)}^{-1} \mathbf{\Gamma \Gamma}^t \mathbf{((I - P)}^{-1})^t }
#'
#' Usefully, it is also easy to compute the inverse-covariance (precision) matrix \eqn{\mathbf{Q = V}^{-1}}:
#'
#' \deqn{ \mathbf{Q} = (\mathbf{\Gamma}^{-1} \mathbf{(I - P)})^t \mathbf{\Gamma}^{-1} \mathbf{(I - P)} }
#'
#' \strong{Example: univariate and first-order autoregressive model}
#'
#' This simultaneous autoregressive (SAR) process across variables and times
#' allows the user to specify both simultaneous effects (effects among variables within
#' year \eqn{T}) and lagged effects (effects among variables among years \eqn{T}).
#' As one example, consider a univariate and first-order autoregressive process where \eqn{T=4}.
#' with independent errors.  This is specified by passing \code{ dsem = X -> X, 1, rho; X <-> X, 0, sigma } to \code{make_dsem_ram}.
#' This is then parsed to a RAM:
#'
#' \tabular{rrrrr}{
#'   \strong{heads} \tab \strong{to} \tab \strong{from} \tab \strong{paarameter} \tab \strong{start} \cr
#'   1 \tab 2 \tab 1 \tab 1 \tab NA \cr
#'   1 \tab 3 \tab 2 \tab 1 \tab NA \cr
#'   1 \tab 4 \tab 3 \tab  1 \tab NA \cr
#'   2 \tab 1 \tab 1 \tab 2 \tab NA \cr
#'   2 \tab 2 \tab 2 \tab  2 \tab NA \cr
#'   2 \tab 3 \tab 3 \tab 2 \tab NA \cr
#'   2 \tab 4 \tab 4 \tab 2 \tab NA
#' }
#'
#' Rows of this RAM where \code{heads=1} are then interpreted to construct the path matrix \eqn{\mathbf P}:
#'
#'     \deqn{ \mathbf P = \begin{bmatrix}
#'     0 & 0 & 0 & 0 \\
#'     \rho & 0 & 0 & 0 \\
#'     0 & \rho & 0 & 0 \\
#'     0 & 0 & \rho & 0\\
#'     \end{bmatrix} }
#'
#' While rows where \code{heads=2} are interpreted to construct the Cholesky of exogenous covariance \eqn{\mathbf \Gamma}:
#'
#'     \deqn{ \mathbf \Gamma = \begin{bmatrix}
#'     \sigma & 0 & 0 & 0 \\
#'     0 & \sigma & 0 & 0 \\
#'     0 & 0 & \sigma & 0 \\
#'     0 & 0 & 0 & \sigma\\
#'     \end{bmatrix} }
#'
#' with two estimated parameters \eqn{\mathbf \beta = (\rho, \sigma) }. This then results in covariance:
#'
#'     \deqn{ \mathrm{Cov}(\mathbf X) = \sigma^2 \begin{bmatrix}
#'     1 & \rho^1 & \rho^2 & \rho^3 \\
#'     \rho^1 & 1 & \rho^1 & \rho^2 \\
#'     \rho^2 & \rho^1 & 1 & \rho^1 \\
#'     \rho^3 & \rho^2 & \rho^1 & 1\\
#'     \end{bmatrix} }
#'
#' Similarly, the arrow-and-lag notation can be used to specify a SAR representing
#' a conventional structural equation model (SEM), cross-lagged (a.k.a. vector autoregressive)
#' models (VAR), dynamic factor analysis (DFA), or many other time-series models.
#'
#' @return A reticular action module (RAM) describing dependencies
#'
#' @examples
#' # Univariate AR1
#' dsem = "
#'   X -> X, 1, rho
#'   X <-> X, 0, sigma
#' "
#' make_dsem_ram( dsem=dsem, variables="X", times=1:4 )
#'
#' # Univariate AR2
#' dsem = "
#'   X -> X, 1, rho1
#'   X -> X, 2, rho2
#'   X <-> X, 0, sigma
#' "
#' make_dsem_ram( dsem=dsem, variables="X", times=1:4 )
#'
#' # Bivariate VAR
#' dsem = "
#'   X -> X, 1, XtoX
#'   X -> Y, 1, XtoY
#'   Y -> X, 1, YtoX
#'   Y -> Y, 1, YtoY
#'   X <-> X, 0, sdX
#'   Y <-> Y, 0, sdY
#' "
#' make_dsem_ram( dsem=dsem, variables=c("X","Y"), times=1:4 )
#'
#' # Dynamic factor analysis with one factor and two manifest variables
#' # (specifies a random-walk for the factor, and miniscule residual SD)
#' dsem = "
#'   factor -> X, 0, loadings1
#'   factor -> Y, 0, loadings2
#'   factor -> factor, 1, NA, 1
#'   X <-> X, 0, NA, 0           # No additional variance
#'   Y <-> Y, 0, NA, 0           # No additional variance
#' "
#' make_dsem_ram( dsem=dsem, variables=c("X","Y","factor"), times=1:4 )
#'
#' # ARIMA(1,1,0)
#' dsem = "
#'   factor -> factor, 1, rho1 # AR1 component
#'   X -> X, 1, NA, 1          # Integrated component
#'   factor -> X, 0, NA, 1
#'   X <-> X, 0, NA, 0         # No additional variance
#' "
#' make_dsem_ram( dsem=dsem, variables=c("X","factor"), times=1:4 )
#'
#' # ARIMA(0,0,1)
#' dsem = "
#'   factor -> X, 0, NA, 1
#'   factor -> X, 1, rho1     # MA1 component
#'   X <-> X, 0, NA, 0        # No additional variance
#' "
#' make_dsem_ram( dsem=dsem, variables=c("X","factor"), times=1:4 )
#'
#' @export
make_dsem_ram <-
function( dsem,
          times,
          variables,
          covs = NULL,
          quiet = FALSE,
          remove_na = TRUE ){
  # Docs : https://roxygen2.r-lib.org/articles/formatting.html

  ####### Error checks
  if( !is.numeric(times) ) stop("`times` must be numeric in `make_dsem_ram`")

  ####### Define local functions
  # helper function
  match_row = function( df, x ) which( df[1]==x[1] & df[2]==x[2] )
  #
  add.variances <- function() {
      variables <- need.variance()
      nvars <- length(variables)
      if (nvars == 0)
          return(model)
      message("NOTE: adding ", nvars, " variances to the model")
      paths <- character(nvars)
      par.names <- character(nvars)
      for (i in 1:nvars) {
          paths[i] <- paste(variables[i], "<->", variables[i])
          par.names[i] <- paste("V[", variables[i], "]", sep = "")
      }
      model.2 <- cbind(c(model[, 1], paths), c(model[,2], rep(0,nvars)), c(model[, 3],
          par.names), c(model[, 4], rep(NA, length(paths))))
      model.2
  }
  need.variance <- function() {
      all.vars <- classify_variables(model)
      exo.vars <- all.vars$exogenous
      end.vars <- all.vars$endogenous
      variables <- logical(0)
      for (i in seq_len(nrow(model))) {
          paths = model[i,1]
          lag = model[i,2]
          vars <- gsub(pattern=" ", replacement="", x=paths)
          vars <- sub("-*>", "->", sub("<-*", "<-", vars))
          vars <- sub("<->|<-", "->", vars)
          vars <- strsplit(vars, "->")[[1]]
          if ((vars[1] != vars[2]) | (lag != 0)) {
              for (a.variable in vars) {
                if (is.na(variables[a.variable]))
                  variables[a.variable] <- TRUE
              }
          }
          else {
              variables[vars[1]] <- FALSE
          }
      }
      if (!exog.variances && length(exo.vars) > 0)
          variables[exo.vars] <- FALSE
      if (!endog.variances && length(end.vars) > 0)
          variables[end.vars] <- FALSE
      names(variables)[variables]
  }

  ####### Step 2 -- Make RAM
  # convert to data frame
  model = scan( text = dsem,
                what = list(path = "", lag = 1, par = "", start = 1, dump = ""),
                sep = ",",
                strip.white = TRUE,
                comment.char = "#",
                fill = TRUE,
                quiet = quiet)
  model$path <- gsub("\\t", " ", model$path)
  model$par[model$par == ""] <- NA
  model <- cbind( "path"=model$path, "lag"=model$lag, "name"=model$par, "start"=model$start)

  # Standardize notation, i.e., eliminate spaces and then use " <-> " for 2-headed arrows
  # necessary to pattern match missing covariances in next step
  model[,1] = gsub(pattern=" ", replacement="", x=model[,1] )
  model[,1] = gsub(pattern="<->", replacement=" <-> ", x=model[,1] )

  # Add missing covariances
  if( !is.null(covs) ){
    for (cov in covs) {
      vars <- strsplit(cov, "[ ,]+")[[1]]
      nvar <- length(vars)
      for (i in 1:nvar) {
      for (j in i:nvar) {
        p1 = paste(vars[i], "<->", vars[j])
        p2 = if (i==j) paste("V[", vars[i], "]", sep = "") else paste("C[",vars[i], ",", vars[j], "]", sep = "")
        p3 = NA
        row <- c(p1, 0, p2, p3)
        #path_without_spaces = gsub(pattern=" ", replacement="", x=model[,1])
        #path_with_spaces = gsub(pattern="<->", replacement=" <-> ", x=path_without_spaces )
        if( any((row[1]==model[,1]) & (row[2]==model[,2])) ){
          next
        }else{
          model <- rbind(model, row, deparse.level = 0)
        }
      }}
    }
  }

  exog.variances = endog.variances = TRUE
  model = add.variances()

  ####### Step 2 -- Make RAM

  # Global stuff
  Q_names = expand.grid( times, variables )
  ram = NULL  # heads, to, from, parameter

  # Deal with fixed values
  par.names = model[, 3]
  pars = na.omit(unique(par.names))
  par.nos = apply(outer(pars, par.names, "=="), 2, which)
  #par.nos = ifelse( sapply(par.nos,length)==0, 0, unlist(par.nos) )
  par.nos = unlist(sapply( par.nos, FUN=\(x) ifelse(length(x)==0, 0, x) ))
  model = cbind( model, "parameter"=par.nos )
  startvalues = model[,4]

  # Add incidence to model
  model = cbind( model, first=NA, second=NA, direction=NA )
  for( i in seq_len(nrow(model)) ){
    path = parse_path(model[i,1])
    model[i,c('first','second','direction')] = unlist( path[c('first','second','direction')] )
  }

  # Loop through paths
  for( i in seq_len(nrow(model)) ){
  for( t in seq_along(times) ){
    lag = as.numeric(model[i,2])
    par.no = par.nos[i]
    # Get index for "from"
    from = c( times[t], model[i,'first'] )
    from_index = match_row( Q_names, from )
    from_index = ifelse( length(from_index)==0, NA, from_index )
    # Get index for "to"
    to = c( times[t+lag], model[i,'second'] )
    to_index = match_row( Q_names, to )
    to_index = ifelse( length(to_index)==0, NA, to_index )
    ram_new = data.frame( "heads"=abs(as.numeric(model[i,'direction'])), "to"=to_index, "from"=from_index, "parameter"=par.no, "start"=startvalues[i] )
    ram = rbind( ram, ram_new )
  }}
  rownames(ram) = NULL

  #
  if( isTRUE(remove_na) ){
    which_keep = which(apply( ram[,1:4], MARGIN=1, FUN=\(x)!any(is.na(x)) ))
    ram = ram[ which_keep, ]
  }

  #
  out = list( "model"=model, "ram"=ram)
  class(out) = "dsem_ram"
  return(out)
}
