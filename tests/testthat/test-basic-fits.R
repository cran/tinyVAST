sim_dat1 <- function(seed = 192838) {
  set.seed(seed)
  n_x <- n_y <- 25
  n_w <- 10
  R_xx <- exp(-0.4 * abs(outer(1:n_x, 1:n_x, FUN = "-")))
  R_yy <- exp(-0.4 * abs(outer(1:n_y, 1:n_y, FUN = "-")))
  z <- mvtnorm::rmvnorm(1, sigma = kronecker(R_xx, R_yy))
  # Simulate nuisance parameter z from oscillatory (day-night) process
  w <- sample(1:n_w, replace = TRUE, size = length(z))
  Data <- data.frame(expand.grid(x = 1:n_x, y = 1:n_y), w = w, z = as.vector(z) + cos(w / n_w * 2 * pi))
  Data$n <- Data$z + rnorm(nrow(Data), sd = 1)
  # Add columns for multivariate and temporal dimensions
  Data$var <- "n"
  Data$time <- 2020
  Data
}
dat <- sim_dat1()

require(fmesher)
mesh <- fm_mesh_2d(dat[, c("x", "y")], n = 100)
out <- tinyVAST(
  data = dat,
  formula = n ~ s(w),
  spatial_domain = mesh,
  space_term = "",
  control = tinyVASTcontrol(
    verbose = TRUE,
    newton_loops = 1,
    silent = FALSE
  )
)

test_that("Basic tinyVAST works", {
  expect_s3_class(out, "tinyVAST")
  s <- summary(out$sdrep)
  expect_true(sum(is.na(s[,2])) == 0L)
})

test_that("cv::cv works", {
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()

  # Run cv::cv
  CV = cv::cv(out, seed = 123)
  expect_equal( as.numeric(CV[['CV crit']]), 1.607874, tolerance = 0.0001 )
})

# test_that("data_colnames are robust", {
#
#   expect_error({out <- tinyVAST(
#     data = dat,
#     formula = n ~ s(w),
#     spatial_domain = mesh,
#     control = tinyVASTcontrol(quiet = TRUE, trace = 0),
#     data_colnames = list(
#       space = c("x", "y"),
#       variable = "banana",
#       time = "time",
#       distribution = "dist"
#     ),
#     space_term = ""
#   )
#   }, regexp = "variable")
  #
#  mesh <- fmesher::fm_mesh_2d(dat[, c("x", "y")], n = 100)
#  expect_error({out <- tinyVAST(
#    data = dat,
#    formula = n ~ s(w),
#    spatial_domain = mesh,
#    space_columns = c("x", "y"),
#    variable_column = "var",
#    time_column = "time",
#    distribution_column = "dist",
#    space_term = ""
#  )
#  }, regexp = "data_colnames")
# })

test_that("formula functions I(.) and poly(.,raw=TRUE) are parsed correctly", {
  fit1 <- tinyVAST(
    data = dat,
    formula = n ~ poly(w, 3, raw=TRUE),
    spatial_domain = mesh,
    space_term = "",
    control = tinyVASTcontrol(
      verbose = TRUE,
      newton_loops = 1,
      silent = FALSE
    )
  )
  fit2 <- tinyVAST(
    data = dat,
    formula = n ~ w + I(w^2) + I(w^3),
    spatial_domain = mesh,
    space_term = "",
    control = tinyVASTcontrol(
      verbose = TRUE,
      newton_loops = 1,
      silent = FALSE
    )
  )
  expect_equal( as.numeric(fit1$opt$obj), as.numeric(fit2$opt$obj), tolerance = 1e-3  )
})

test_that("formula function poly(.,raw=FALSE) is parsed correctly when fitting and predicting to new data", {
  fit1 <- tinyVAST(
    data = dat,
    formula = n ~ poly(w, 3),
    spatial_domain = mesh,
    space_term = "",
    control = tinyVASTcontrol(
      verbose = TRUE,
      newton_loops = 1,
      silent = FALSE
    )
  )
  newcols = poly(dat$w, 3)
  colnames(newcols) = paste0("w", 1:3)
  dat = cbind( dat, newcols )
  fit2 <- tinyVAST(
    data = dat,
    formula = n ~ 1 + w1 + w2 + w3,
    spatial_domain = mesh,
    space_term = "",
    control = tinyVASTcontrol(
      verbose = TRUE,
      newton_loops = 1,
      silent = FALSE
    )
  )
  expect_equal( as.numeric(fit1$opt$obj), as.numeric(fit2$opt$obj), tolerance = 1e-3  )

  #
  pred1 = predict( fit1, newdata = dat[1:10,] )
  pred2 = predict( fit2, newdata = dat[1:10,] )
  expect_equal( pred1, pred2, tolerance = 1e-3  )
})


test_that("tinyVAST works as dsem", {
  data(isle_royale, package="dsem")
  
  # Convert to long-form
  data = expand.grid( "time"=isle_royale[,1], "var"=colnames(isle_royale[,2:3]) )
  data$logn = unlist(log(isle_royale[2:3]))
  
  # Define cross-lagged DSEM
  dsem = "
    # Link, lag, param_name
    wolves -> wolves, 1, arW
    moose -> wolves, 1, MtoW
    wolves -> moose, 1, WtoM
    moose -> moose, 1, arM
    #wolves -> moose, 0, corr
    wolves <-> moose, 0, corr
  "
  
  # fit model ... spacetime_term
  fit1 = tinyVAST( spacetime_term = dsem,
                   data = data,
                   times = isle_royale[,1],
                   variables = colnames(isle_royale[,2:3]),
                   formula = logn ~ 0 + var )
  # fit model ... time_term
  fit2 = tinyVAST( time_term = dsem,
                   data = data,
                   times = isle_royale[,1],
                   variables = colnames(isle_royale[,2:3]),
                   formula = logn ~ 0 + var )

  expect_equal( as.numeric(fit1$opt$obj), as.numeric(fit2$opt$obj), tolerance = 1e-3  )
  expect_equal( as.numeric(fit1$opt$obj), 5.781919, tolerance = 1e-3 )

} )
