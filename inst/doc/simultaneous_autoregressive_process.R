## ----include = FALSE----------------------------------------------------------
has_ggplot = requireNamespace("ggplot2", quietly = TRUE)
EVAL <- has_ggplot
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = EVAL,
  purl = EVAL
)
# Install locally
#  devtools::install_local( R'(C:\Users\James.Thorson\Desktop\Git\tinyVAST)', force=TRUE )
# Build
#  setwd(R'(C:\Users\James.Thorson\Desktop\Git\tinyVAST)'); devtools::build_rmd("vignettes/simultaneous_autoregressive_process.Rmd"); rmarkdown::render( "vignettes/simultaneous_autoregressive_process.Rmd", rmarkdown::pdf_document())

## ----setup, echo=TRUE, warning=FALSE, message=FALSE---------------------------
library(tinyVAST)
library(igraph)
library(rnaturalearth)
library(sf)
options("tinyVAST.verbose" = FALSE)

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
data( salmon_returns )

# Transform data
salmon_returns$Biomass_nozeros = ifelse( salmon_returns$Biomass==0,
                                         NA, salmon_returns$Biomass )
Data = na.omit(salmon_returns)

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# Define graph for SAR process
unconnected_graph = make_empty_graph( nlevels(Data$Region) )
V(unconnected_graph)$name = levels(Data$Region)
plot(unconnected_graph)

# Define SEM for AR2 process
dsem = "
  sockeye -> sockeye, -1, lag1_sockeye
  sockeye -> sockeye, -2, lag2_sockeye

  pink -> pink, -1, lag1_pink
  pink -> pink, -2, lag2_pink

  chum -> chum, -1, lag1_chum
  chum -> chum, -2, lag2_chum
"

# Fit tinyVAST model
mytiny0 = tinyVAST(
     formula = Biomass_nozeros ~ 0 + Species + Region,
     data = Data,
     spacetime_term = dsem,
     variable_column = "Species",
     time_column = "Year",
     space_column = "Region",
     distribution_column = "Species",
     family = list( "chum" = lognormal(),
                          "pink" = lognormal(),
                          "sockeye" = lognormal() ),
     spatial_domain = unconnected_graph,
     control = tinyVASTcontrol( profile="alpha_j" ) )

# Summarize output
Summary = summary(mytiny0, what="spacetime_term")
knitr::kable( Summary, digits=3)

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# Define graph for SAR process
adjacency_graph = make_graph( ~ Korea - Japan - M.I - WKam - EKam -
                                WAK - SPen - Kod - CI - PWS -
                                SEAK - NBC - SBC - WA )

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=5-----------
#maps = ne_countries( country = c("united states of america","russia","canada","south korea","north korea","japan") )
maps = ne_countries( continent = c("north america","asia","europe") )
maps = st_combine( maps )
maps = st_transform( maps, crs=st_crs(3832) )
#maps = st_crop( maps, xmin = -5*1e5, xmax = 12*1e5,
#                      ymin = 0 * 1e6, ymax=10 * 1e6 )

# Format inputs
loc_xy = cbind(
  x = c(129,143,140,156,163,-163,-161,-154,-154,-147,-138,-129,-126,-125),
  y = c(36,40,57,53,57,60,55,56,59,61,57,54,50,45)
)
loc_xy = sf_project( loc_xy, from=st_crs(4326), to=st_crs(3832) )

# Plot
xlim = c(-4,10) * 1e6
ylim = c(3,10) * 1e6
plot( maps, 
      xlim = xlim,
      ylim = ylim,
      col = "grey",
      asp = FALSE,
      add = FALSE )
plot( adjacency_graph, 
      layout = loc_xy,
      add = TRUE, 
      rescale = FALSE,
      vertex.label.color = "red",
      xlim = xlim, 
      ylim = ylim, 
      edge.width = 2,
      edge.color = "red" )

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6, warning=FALSE----
# Fit tinyVAST model
mytiny = tinyVAST(
     formula = Biomass_nozeros ~ 0 + Species + Region,
     data = Data,
     spacetime_term = dsem,
     variable_column = "Species",
     time_column = "Year",
     space_column = "Region",
     distribution_column = "Species",
     family = list( "chum" = lognormal(),
                          "pink" = lognormal(),
                          "sockeye" = lognormal() ),
     spatial_domain = adjacency_graph,
     control = tinyVASTcontrol( profile="alpha_j" ) )

# Summarize output
Summary = summary(mytiny, what="spacetime_term")
knitr::kable( Summary, digits=3)

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# AIC for unconnected time-series
AIC(mytiny0)
# AIC for SAR spatial variation
AIC(mytiny)

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# Compile long-form dataframe of observations and predictions
Resid = rbind( cbind(Data[,c('Species','Year','Region','Biomass_nozeros')], "Which"="Obs"),
               cbind(Data[,c('Species','Year','Region')], "Biomass_nozeros"=predict(mytiny0,Data), "Which"="Pred") )

# plot using ggplot
library(ggplot2)
ggplot( data=Resid, aes(x=Year, y=Biomass_nozeros, col=Which) ) + # , group=yhat.id
  geom_line() +
  facet_grid( rows=vars(Region), cols=vars(Species), scales="free" ) +
  scale_y_continuous(trans='log')  #

