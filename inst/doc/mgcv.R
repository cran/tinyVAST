## ----include = FALSE----------------------------------------------------------
has_pdp = requireNamespace("pdp", quietly = TRUE)
has_lattice = requireNamespace("lattice", quietly = TRUE)
has_visreg = requireNamespace("visreg", quietly = TRUE)
EVAL <- has_pdp && has_lattice && has_visreg
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = EVAL,
  purl = EVAL
)
# Install locally
#  devtools::install_local( R'(C:\Users\James.Thorson\Desktop\Git\tinyVAST)', force=TRUE )
# Build
#  setwd(R'(C:\Users\James.Thorson\Desktop\Git\tinyVAST)'); devtools::build_rmd("vignettes/mgcv.Rmd"); rmarkdown::render( "vignettes/mgcv.Rmd", rmarkdown::pdf_document())

## ----setup, echo=TRUE, warning=FALSE, message=FALSE---------------------------
library(tinyVAST)
library(pdp)  # approx = TRUE gives effects for average of other covariates
library(lattice)
library(visreg)
library(mgcv)
set.seed(101)
options("tinyVAST.verbose" = FALSE)

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# Simulate
n_obs = 1000
x = rnorm(n_obs)
group = sample( x=1:5, size=n_obs, replace=TRUE )
w = runif(n_obs, min=0, max=2)
z = 1 + x^2 + cos((w+group/5)*2*pi) + rnorm(5)[group]
a = exp(0.1*rnorm(n_obs))
y = z + a + rnorm(n_obs, sd=0.2)
Data = data.frame( x=x, y=y, w=w, z=z, group=factor(group), a=a )

# fit model
Formula = y ~ 1 + s(group, bs="re") + poly(x, 2, raw=TRUE) + s(w, by=group, bs="ts") # + offset(a)
myfit = tinyVAST( data = Data,
           formula = Formula,
           control = tinyVASTcontrol( getsd=FALSE ) )

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# By default
myfit$deviance_explained

#
mygam_reduced = gam( Formula, data=Data ) #
summary(mygam_reduced)$dev.expl

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# simulate new data conditional on fixed and random effects
y_ir = replicate( n = 100, 
           expr = myfit$obj$simulate()$y_i )

#
res = DHARMa::createDHARMa( simulatedResponse = y_ir, 
                            observedResponse = Data$y, 
                            fittedPredictedResponse = fitted(myfit) )
plot(res)

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
predict(myfit, newdata=data.frame(x=0, y=1, w=0.4, group=2, a=1) )

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# compute partial dependence plot
Partial = partial( object = myfit,
                   pred.var = c("w","group"),
                   pred.fun = \(object,newdata) predict(object,newdata),
                   train = Data,
                   approx = TRUE )

# Lattice plots as default option
plotPartial( Partial )

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
visreg(myfit, xvar="group", what="p_g")
visreg(myfit, xvar="x", what="p_g")
visreg(myfit, xvar="w", by="group", what="p_g")

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# Predicted sample-weighted total
integrate_output(myfit)

# True (latent) sample-weighted total
sum( Data$z )

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# Simulate
R = exp(-0.4 * abs(outer(1:10, 1:10, FUN="-")) )
z = mvtnorm::rmvnorm(3, sigma=kronecker(R,R) )
Data = data.frame( expand.grid(x=1:10, y=1:10, group=1:3), z=as.vector(t(z)))
Data$n = Data$z + rnorm(nrow(Data), sd=0.1)
Data$group = factor(Data$group)

# fit model
Formula = n ~ s(x, y, by=group)
myfit = tinyVAST( data = Data,
           formula = Formula )

# compute partial dependence plot
mypartial = partial( object = myfit,
                   pred.var = c("x","y","group"),
                   pred.fun = \(object,newdata) predict(object,newdata),
                   train = Data,
                   approx = TRUE )

# Lattice plots as default option
plotPartial( mypartial )

# Lattice plot of true values
mypartial$yhat = Data$z
plotPartial( mypartial )

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
out = visreg2d( myfit, "x", "y", cond=list("group"=1), plot=FALSE )
plot( out, main="f(x,y) for group=1")

