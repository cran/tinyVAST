% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{deviance_explained}
\alias{deviance_explained}
\title{Calculate deviance explained}
\usage{
deviance_explained(x, null_formula, null_delta_formula = ~1)
}
\arguments{
\item{x}{output from \verb{\code{tinyVAST()}}}

\item{null_formula}{formula for the null model.  If missing, it uses
\code{null_formula = response ~ 1}. For multivariate models, it
might make sense to use \code{null_formula = response ~ category}}

\item{null_delta_formula}{formula for the null model for the delta component.
If missing, it uses
\code{null_formula = response ~ 1}. For multivariate models, it
might make sense to use \code{null_delta_formula = response ~ category}}
}
\value{
the proportion of conditional deviance explained.
}
\description{
\code{deviance_explained} fits a null model, calculates the deviance relative to
a saturated model for both the original and the null model, and uses these
to calculate the proportion of deviance explained.

This implementation conditions upon the maximum likelihood estimate of fixed effects
and the empirical Bayes ("plug-in") prediction of random effects.  It can
be described as "conditional deviance explained". A state-space model that
estimates measurement error variance approaching zero (i.e., collapses to
a process-error-only model) will have a conditional deviance explained
that approaches 1.0
}
