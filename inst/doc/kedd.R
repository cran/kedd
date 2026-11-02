## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6,
                      fig.height = 3.8, fig.align = "center", dpi = 96)
library(kedd)
## plot() methods print their figure; quiet() keeps the returned ggplot without
## drawing it, so that layers can be added before display.
quiet <- function(expr) { grDevices::pdf(NULL); on.exit(grDevices::dev.off()); expr }

## ----kfun---------------------------------------------------------------------
kf <- kernel.fun(kernel = "biweight", deriv.order = 1L)
kf
plot(kf)

## ----kconv--------------------------------------------------------------------
kc <- kernel.conv(kernel = "logistic", deriv.order = 1L)
plot(kc)
kernel.conv(x = 0, kernel = "logistic", deriv.order = 1L)@y   # -R(K') = -1/30

## ----constants----------------------------------------------------------------
kers <- c("gaussian", "epanechnikov", "uniform", "triangular", "biweight",
          "triweight", "tricube", "cosine", "silverman", "logistic",
          "pow(2,4)", "pow(1.5,2.5)")
round(t(sapply(kers, function(k) Cpp_kernel_constants(k, 0L)[c("mu2", "mu4", "R")])), 5)

## ----pow----------------------------------------------------------------------
kernel.pow(2, 3)
all.equal(kernel.fun(kernel = "pow(2,2)")@y, kernel.fun(kernel = "biweight")@y)
Cpp_kernel_constants(kernel.pow(1.5, 2.5), 0L)
plot(kernel.fun(kernel = kernel.pow(1.5, 2.5)))

## ----pow-shapes, fig.height = 3.4---------------------------------------------
grid <- seq(-1, 1, length.out = 201)
shapes <- do.call(rbind, lapply(c("pow(2,1)", "pow(2,4)", "pow(4,1)", "pow(1,2)"),
  function(k) data.frame(x = grid, K = Cpp_kernel_deriv(k, grid, 0L), kernel = k)))
ggplot2::ggplot(shapes, ggplot2::aes(x, K, colour = kernel)) +
  ggplot2::geom_line(linewidth = 0.9) + ggplot2::theme_minimal()

## ----data---------------------------------------------------------------------
set.seed(2027)
n <- 300
x <- c(rnorm(0.6 * n, -1, 0.5), rnorm(0.4 * n, 1.5, 0.7))
f_true <- function(t, r = 0L) {
  comp <- function(m, s) {
    z  <- (t - m) / s
    He <- switch(r + 1L, 1, z, z^2 - 1)        # Hermite polynomials
    (-1)^r * He * dnorm(z) / s^(r + 1)
  }
  0.6 * comp(-1, 0.5) + 0.4 * comp(1.5, 0.7)
}

## ----dkde---------------------------------------------------------------------
f0 <- dkde(data = x)
f0
p <- quiet(plot(f0))
p + ggplot2::stat_function(fun = f_true, linetype = "dashed")

## ----dkde-r1------------------------------------------------------------------
h1 <- h.scv(x, deriv.order = 1L)
p1 <- quiet(plot(dkde(data = x, h = h1@h, deriv.order = 1L)))
p1 + ggplot2::stat_function(fun = f_true, args = list(r = 1L), linetype = "dashed")

## ----dkde-r1-small------------------------------------------------------------
c(h_density = f0@h, h_derivative = h1@h)
plot(dkde(data = x, h = f0@h, deriv.order = 1L), band = FALSE)

## ----dkde-kernels-------------------------------------------------------------
grid <- seq(-3, 4, length.out = 300)
est <- do.call(rbind, lapply(c("gaussian", "logistic", "pow(2,4)"), function(k) {
  d <- dkde(x = grid, data = x, h = h.ucv(x, kernel = k)@h, kernel = k)
  data.frame(x = d@x, y = d@y, kernel = k)
}))
ggplot2::ggplot(est, ggplot2::aes(x, y, colour = kernel)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::stat_function(fun = f_true, colour = "black", linetype = "dashed") +
  ggplot2::theme_minimal()

## ----ucv----------------------------------------------------------------------
bw <- h.ucv(x)
bw
plot(bw)

## ----overlay------------------------------------------------------------------
p <- quiet(plot(h.ucv(x)))
p$plot +
  lines(h.bcv(x), color = "darkred") +
  lines(h.ccv(x), color = "darkorange") +
  lines(h.mcv(x), color = "purple")

## ----compare------------------------------------------------------------------
ise <- function(h, r) {
  d <- dkde(x = seq(-4, 5, length.out = 400), data = x, h = h, deriv.order = r)
  sum((d@y - f_true(d@x, r))^2) * diff(d@x[1:2])
}
sel <- list(AMISE = h.amise, UCV = h.ucv, BCV1 = h.bcv,
            BCV2 = function(x, ...) h.bcv(x, whichbcv = 2, ...),
            CCV = h.ccv, MCV = h.mcv, TCV = h.tcv, SCV = h.scv)
tab <- sapply(0:1, function(r) {
  h <- sapply(sel, function(f) f(x, deriv.order = r)@h)
  c(rbind(h = h, ISE = sapply(h, ise, r = r)))
})
dimnames(tab) <- list(paste(rep(names(sel), each = 2), c("h", "ISE")),
                      c("r = 0", "r = 1"))
round(tab, 4)
c(MLCV = h.mlcv(x)@h)   # density only

## ----kernels------------------------------------------------------------------
ks <- c("gaussian", "epanechnikov", "uniform", "triangular", "biweight", "triweight",
        "tricube", "cosine", "silverman", "logistic", "pow(2,4)", "pow(1.5,2.5)")
round(sapply(ks, function(k) h.ucv(x, kernel = k)@h), 4)

## ----claw---------------------------------------------------------------------
f_claw <- function(x) 0.5 * dnorm(x) +
  rowSums(sapply(0:4, function(l) 0.1 * dnorm(x, l / 2 - 1, 0.1)))
bw_claw <- sapply(list(UCV = h.ucv, BCV1 = h.bcv, MCV = h.mcv, SCV = h.scv,
                       AMISE = h.amise), function(f) suppressWarnings(f(claw))@h)
round(bw_claw, 4)
p <- quiet(plot(dkde(data = claw, h = bw_claw[["UCV"]])))
p + ggplot2::stat_function(fun = f_claw, linetype = "dashed")

## ----datasets-----------------------------------------------------------------
samples <- list(kurtotic = kurtotic, outlier = outlier, bimodal = bimodal,
                trimodal = trimodal, claw = claw)
round(sapply(samples, function(s)
  sapply(list(AMISE = h.amise, UCV = h.ucv, BCV1 = h.bcv, MCV = h.mcv, SCV = h.scv),
         function(f) suppressWarnings(f(s))@h)), 4)

## ----threads------------------------------------------------------------------
c(all = Cpp_omp_threads(0L), two = Cpp_omp_threads(2L))
y <- rnorm(1500)
system.time(h_seq <- h.ucv(y, nthreads = 1L)@h)
system.time(h_par <- h.ucv(y, nthreads = 0L)@h)
all.equal(h_seq, h_par)

