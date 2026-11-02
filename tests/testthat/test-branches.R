## Couverture des branches : validation des arguments, avertissements,
## entrees matricielles des fonctions C++, options graphiques.

set.seed(11)
xb <- rnorm(120)

with_png <- function(code) {
  tmp <- tempfile(fileext = ".png")
  png(tmp)
  on.exit({ dev.off(); unlink(tmp) })
  force(code)
}

# ── validation des arguments communs ────────────────────────────────────────
test_that(".check_bw_args : erreurs et avertissements", {
  expect_error(h.ucv("a"), "numeric vector")
  expect_error(h.ucv(1), "at least 2")
  expect_error(h.ucv(matrix(rnorm(10), 5)), "numeric vector")
  expect_warning(bw <- h.amise(xb, deriv.order = -1), "deriv.order")
  expect_equal(bw@deriv.order, 0L)
  expect_warning(h.amise(xb, deriv.order = 1.5), "deriv.order")
  expect_warning(bw <- h.ucv(xb, nthreads = -2), "nthreads")
  expect_true(is.finite(bw@h))
  expect_equal(kedd:::.normalize_kernel(NULL), "gaussian")
  expect_equal(h.ucv(c(xb, NA))@h, h.ucv(xb)@h)
})

test_that(".check_interval : bornes invalides remplacees par defaut", {
  expect_warning(h.ucv(xb, upper = "a"), "upper")
  expect_warning(h.ucv(xb, lower = "a"), "lower")
  expect_warning(h.ucv(xb, lower = 2, upper = 1), "invalid interval")
  expect_warning(h.ucv(xb, lower = -1, upper = 1), "invalid interval")
})

test_that("minimum au bord de l'intervalle : avertissement pour chaque selecteur", {
  lo <- 1e-3; up <- 2e-3
  expect_warning(h.ucv(xb, lower = lo, upper = up), "end of the range")
  expect_warning(h.bcv(xb, lower = lo, upper = up), "end of the range")
  expect_warning(h.ccv(xb, lower = lo, upper = up), "end of the range")
  expect_warning(h.mcv(xb, lower = lo, upper = up), "end of the range")
  expect_warning(h.tcv(xb, lower = lo, upper = up), "end of the range")
  expect_warning(h.mlcv(xb, lower = 10, upper = 20), "end of the range")
  expect_warning(h.scv(xb, lower = lo, upper = up), "end of the range")
})

test_that("h.bcv : whichbcv invalide -> BCV1", {
  expect_warning(bw <- h.bcv(xb, whichbcv = 3), "whichbcv")
  expect_equal(bw@method, "BCV1")
})

test_that("ordres non admissibles : NA + avertissement pour chaque selecteur", {
  for (f in list(h.ucv, h.bcv, h.ccv, h.mcv, h.tcv, h.amise)) {
    expect_warning(bw <- f(xb, deriv.order = 1L, kernel = "uniform"))
    expect_true(is.na(bw@h))
  }
  expect_warning(bw <- h.scv(xb, deriv.order = 1L, kernel = "uniform"), "maximum order")
  expect_true(is.na(bw@h))
  expect_warning(bw <- h.ccv(xb, kernel = "epanechnikov"), "CCV")
  expect_warning(bw <- h.mcv(xb, deriv.order = 1L, kernel = "epanechnikov"), "MCV")
})

# ── methodes graphiques et show ─────────────────────────────────────────────
test_that("plot/lines(bandwidth) : NA, h_os, echelle log, grille h fournie", {
  bwNA <- suppressWarnings(h.ucv(xb, deriv.order = 1L, kernel = "uniform"))
  expect_warning(expect_null(plot(bwNA)), "NA")
  expect_null(lines(bwNA))
  bw <- h.ucv(xb)
  with_png({
    res <- plot(bw, show.hos = TRUE, log_scale_x = TRUE, show.min = FALSE, type = "l")
    expect_s3_class(res$plot, "ggplot")
    res2 <- plot(bw, h = seq(0.2, 0.8, length.out = 30), type = "p",
                 main.title = "T", x.title = "h", y.title = "cv")
    expect_length(res2$cv, 30L)
    expect_no_error(plot(h.bcv(xb, deriv.order = 1L)))
  })
})

test_that("plot/lines(bandwidth) : MLCV trace la vraisemblance, AMISE refuse", {
  bm <- h.mlcv(xb)
  with_png({
    res <- plot(bm)
    expect_equal(res$h[which.min(res$cv)], bm@h, tolerance = 0.05)
    expect_s3_class(lines(bm), "LayerInstance")
  })
  expect_equal(min(res$cv), bm@min.obj, tolerance = 1e-3)
  expect_error(plot(h.amise(xb)), "closed-form")
})

test_that("plot(dkde) : derivee, points, sans bande ni rug", {
  d <- dkde(data = xb, h = 0.5, deriv.order = 1L)
  with_png({
    p <- plot(d, type = "o", band = FALSE, rug = FALSE)
    expect_s3_class(p, "ggplot")
    expect_s3_class(plot(dkde(data = xb, h = 0.4), type = "p",
                         main.title = "m", x.title = "x", y.title = "y"), "ggplot")
  })
  expect_output(show(d), "derivative of order 1")
  expect_error(dkde(data = 1), "at least 2")
  expect_error(dkde(data = xb, h = -1), "positive")
  d2 <- dkde(x = c(-1, 0, 1), data = xb, h = 0.5)
  expect_equal(d2@x, c(-1, 0, 1))
})

test_that("plot(kfun) / plot(kconv) : types et titres, show", {
  kf <- kernel.fun(kernel = "biweight", deriv.order = 2L)
  kc <- kernel.conv(kernel = "biweight", deriv.order = 1L)
  with_png({
    expect_s3_class(plot(kf, type = "o"), "ggplot")
    expect_s3_class(plot(kf, type = "p", zero.line = FALSE,
                         main.title = "m", x.title = "x", y.title = "y"), "ggplot")
    expect_s3_class(plot(kc, type = "o"), "ggplot")
    expect_s3_class(plot(kc, type = "p", main.title = "m",
                         x.title = "x", y.title = "y"), "ggplot")
  })
  expect_output(show(kf), "order 2")
  expect_output(show(kc), "K\\^\\{\\(1\\)\\}")
  expect_error(kernel.fun(deriv.order = -1L), "non-negative")
  expect_error(kernel.conv(deriv.order = -1L), "non-negative")
})

test_that("validite des classes S4", {
  expect_error(new("bandwidth", kernel = c("a", "b"), method = "UCV", deriv.order = 0L), "kernel")
  expect_error(new("bandwidth", kernel = "a", method = c("a", "b"), deriv.order = 0L), "method")
  expect_error(new("bandwidth", kernel = "a", method = "UCV", deriv.order = -1L), "deriv.order")
  expect_error(new("kfun", x = 1:2 + 0, y = 1, kernel = "g", deriv.order = 0L, call = quote(f())), "same length")
  expect_error(new("kfun", x = 1, y = 1, kernel = c("g", "h"), deriv.order = 0L, call = quote(f())), "kernel")
  expect_error(new("kfun", x = 1, y = 1, kernel = "g", deriv.order = -1L, call = quote(f())), "deriv.order")
  expect_error(new("kconv", x = 1:2 + 0, y = 1, kernel = "g", deriv.order = 0L, call = quote(f())), "same length")
  expect_error(new("kconv", x = 1, y = 1, kernel = c("g", "h"), deriv.order = 0L, call = quote(f())), "kernel")
  expect_error(new("kconv", x = 1, y = 1, kernel = "g", deriv.order = -1L, call = quote(f())), "deriv.order")
  expect_error(new("dkde", x = 1, y = 1, h = -1, kernel = "g", deriv.order = 0L,
                   n = 1L, data = 1, call = quote(f())), "'h'")
  expect_error(new("dkde", x = 1, y = 1, h = 1, kernel = c("g", "h"), deriv.order = 0L,
                   n = 1L, data = 1, call = quote(f())), "kernel")
  expect_error(new("dkde", x = 1, y = 1, h = 1, kernel = "g", deriv.order = -1L,
                   n = 1L, data = 1, call = quote(f())), "deriv.order")
  expect_error(new("dkde", x = 1:2 + 0, y = 1, h = 1, kernel = "g", deriv.order = 0L,
                   n = 1L, data = 1, call = quote(f())), "same length")
})

# ── fonctions C++ de bas niveau : branches matricielles ─────────────────────
test_that("Cpp_kernel_deriv / Cpp_kernel_conv acceptent une matrice", {
  m <- matrix(seq(-2, 2, length.out = 12), 3, 4)
  kd <- Cpp_kernel_deriv("gaussian", m, 0L)
  expect_true(is.matrix(kd)); expect_equal(dim(kd), c(3L, 4L))
  expect_equal(as.vector(kd), dnorm(as.vector(m)))
  kc <- Cpp_kernel_conv("gaussian", m, 0L)
  expect_equal(as.vector(kc), dnorm(as.vector(m), sd = sqrt(2)))
  expect_error(Cpp_kernel_conv("inconnu", 0, 0L), "Unsupported")
})

test_that("Cpp_plot_cv : vecteur, matrice, SCV, methode inconnue", {
  bw <- h.ucv(xb)
  hv <- c(0.3, 0.5)
  hm <- matrix(c(0.3, 0.4, 0.5, 0.6), 2)
  v  <- Cpp_plot_cv("UCV", hv, "gaussian", 0L, xb)
  mm <- Cpp_plot_cv("UCV", hm, "gaussian", 0L, xb)
  expect_equal(dim(mm), c(2L, 2L))
  expect_equal(mm[1, ], Cpp_plot_cv("UCV", c(0.3, 0.5), "gaussian", 0L, xb))
  for (m in c("BCV1", "BCV2", "CCV", "MCV", "TCV"))
    expect_true(all(is.finite(Cpp_plot_cv(m, hv, "gaussian", 0L, xb))))
  sv <- Cpp_plot_cv("SCV", hv, "gaussian", 0L, xb)
  sm <- Cpp_plot_cv("SCV", hm, "gaussian", 0L, xb)
  expect_equal(sm[1, ], sv)
  expect_error(Cpp_plot_cv("XYZ", hv, "gaussian", 0L, xb), "not supported")
  expect_equal(v[1], Cpp_h_ucv(0.3, "gaussian", 0L, xb, 1L))
})

test_that("Cpp_conv2conv : double convolution gaussienne = N(0, 4)", {
  t <- c(-1, 0, 1.5)
  z <- Cpp_conv2conv(t, 0L, 0L, "gaussian", "gaussian", 1L)
  expect_equal(z, dnorm(t, sd = 2), tolerance = 1e-6)
  zm <- Cpp_conv2conv(matrix(t[1:2], 1), 0L, 0L, "gaussian", "gaussian", 1L)
  expect_equal(as.vector(zm), z[1:2], tolerance = 1e-10)
  expect_null(Cpp_conv2conv("a", 0L, 0L, "gaussian", "gaussian", 1L))
})

test_that("Cpp_h_scv : noyau non gaussien rejete", {
  expect_error(Cpp_h_scv(0.5, 0.3, "biweight", 0L, xb, 1L), "gaussian")
  expect_error(h.scv(xb, g = -1), "positive")
})
