## kedd -- comparison of bandwidth selectors
## Run with: demo("bandwidths", package = "kedd")

library(kedd)
set.seed(1)
n <- 300
x <- c(rnorm(0.6 * n, -1, 0.5), rnorm(0.4 * n, 1.5, 0.7))

## true density and derivatives of the mixture 0.6 N(-1, 0.5^2) + 0.4 N(1.5, 0.7^2)
f_true <- function(t, r = 0L) {
  comp <- function(m, s) {
    z <- (t - m) / s
    He <- switch(r + 1L, 1, z, z^2 - 1)            # Hermite polynomials
    (-1)^r * He * dnorm(z) / s^(r + 1)
  }
  0.6 * comp(-1, 0.5) + 0.4 * comp(1.5, 0.7)
}

selectors <- list(
  AMISE = function(r) h.amise(x, deriv.order = r),
  UCV   = function(r) h.ucv(x, deriv.order = r),
  BCV1  = function(r) h.bcv(x, deriv.order = r, whichbcv = 1),
  BCV2  = function(r) h.bcv(x, deriv.order = r, whichbcv = 2),
  CCV   = function(r) h.ccv(x, deriv.order = r),
  MCV   = function(r) h.mcv(x, deriv.order = r),
  TCV   = function(r) h.tcv(x, deriv.order = r),
  SCV   = function(r) h.scv(x, deriv.order = r)
)

## integrated squared error of the estimate obtained with each bandwidth
ise <- function(h, r) {
  d <- dkde(x = seq(-4, 5, length.out = 400), data = x, h = h, deriv.order = r)
  sum((d@y - f_true(d@x, r))^2) * diff(d@x[1:2])
}

## 1. Selected bandwidths and ISE for r = 0, 1, 2 ----------------------------
##    On this bimodal sample the BCV/CCV/MCV criteria have no interior minimum
##    for r >= 1: they return the upper bound 2 * h_os with a warning. UCV is
##    unbiased but very variable and undersmooths derivatives.
for (r in 0:2) {
  h <- sapply(selectors, function(f) f(r)@h)
  cat("\nDerivative order r =", r, "\n")
  print(round(rbind(h = h, ISE = sapply(h, ise, r = r)), 4))
}

## 2. CV curves of several criteria on one figure (r = 0) ---------------------
p <- plot(h.ucv(x))
p$plot +
  lines(h.bcv(x), color = "darkred") +
  lines(h.ccv(x), color = "darkorange") +
  lines(h.mcv(x), color = "purple")

## 3. Estimate of f' with the UCV bandwidth, against the true f' --------------
h1 <- h.ucv(x, deriv.order = 1L)@h
p1 <- plot(dkde(data = x, h = h1, deriv.order = 1L))
p1 + ggplot2::stat_function(fun = f_true, args = list(r = 1L),
                            linetype = "dashed", colour = "black")

## 4. Same selector, different kernels ----------------------------------------
sapply(c("gaussian", "epanechnikov", "biweight", "triweight", "tricube", "cosine",
         "silverman", "logistic", "pow(2,4)", "pow(1.5,2.5)"),
       function(k) h.ucv(x, kernel = k)@h)

## 5. All selectors on the five Marron & Wand samples (?"kedd-data") ---------
samples <- list(kurtotic = kurtotic, outlier = outlier, bimodal = bimodal,
                trimodal = trimodal, claw = claw)
sel_data <- list(AMISE = h.amise, UCV = h.ucv, BCV1 = h.bcv,
                 BCV2 = function(s) h.bcv(s, whichbcv = 2),
                 CCV = h.ccv, MCV = h.mcv, TCV = h.tcv, SCV = h.scv, MLCV = h.mlcv)
round(sapply(samples, function(s)
  sapply(sel_data, function(f) suppressWarnings(f(s))@h)), 4)
