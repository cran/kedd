## Tests des noyaux Pow(s,t) et logistique

set.seed(2027)
xs <- rnorm(150)

## Pow(s,t) reproduit les noyaux nommes qu'il generalise
pow_equiv <- c(epanechnikov = "pow(2,1)", biweight = "pow(2,2)",
               triweight = "pow(2,3)", triangular = "pow(1,1)",
               tricube = "pow(3,3)", uniform = "pow(2,0)")
## points interieurs (hors 0 et +-1, ou les conventions de bord different)
xin <- c(-0.93, -0.61, -0.27, -0.05, 0.11, 0.38, 0.74, 0.97)

# ── kernel.pow() et normalisation du nom ────────────────────────────────────
test_that("kernel.pow() construit le nom canonique", {
  expect_equal(kernel.pow(), "pow(2,2)")
  expect_equal(kernel.pow(1.5, 2.5), "pow(1.5,2.5)")
  expect_equal(kernel.pow(3, 0), "pow(3,0)")
})

test_that("kernel.pow() rejette des parametres invalides", {
  expect_error(kernel.pow(0, 2), "'s'")
  expect_error(kernel.pow(-1, 2), "'s'")
  expect_error(kernel.pow(2, -1), "'t'")
  expect_error(kernel.pow(c(1, 2), 2), "'s'")
  expect_error(kernel.pow("a", 2), "'s'")
  expect_error(kernel.pow(2, Inf), "'t'")
})

test_that("le nom du noyau pow est normalise", {
  expect_equal(kernel.fun(kernel = "pow")@kernel, "pow(2,2)")
  expect_equal(kernel.fun(kernel = " POW( 2 , 3 ) ")@kernel, "pow(2,3)")
  expect_equal(kernel.fun(kernel = "pow(2.0,3.00)")@kernel, "pow(2,3)")
  expect_equal(kernel.fun(kernel = "epa")@kernel, "epanechnikov")
  expect_error(kernel.fun(kernel = "pow(2)"), "pow\\(s,t\\)")
  expect_error(kernel.fun(kernel = "pow(a,b)"), "'s'")
  expect_error(kernel.fun(kernel = "pow(0,2)"), "'s'")
  expect_error(kernel.fun(kernel = c("gaussian", "biweight")), "single")
  expect_error(kernel.fun(kernel = 1), "single")
  expect_error(kernel.fun(kernel = "inconnu"))
})

test_that("Cpp rejette un nom pow mal forme ou des parametres invalides", {
  expect_error(Cpp_kernel_deriv("pow(2;3)", 0, 0L), "pow\\(s,t\\)")
  expect_error(Cpp_kernel_deriv("pow(-1,2)", 0, 0L), "s > 0")
  expect_error(Cpp_kernel_conv("pow(2,-1)", 0, 0L), "s > 0")
  expect_error(Cpp_kernel_constants("inconnu", 0L), "Unsupported")
  expect_error(Cpp_hos("inconnu", 0L, 10L), "Unsupported")
  expect_true(is.na(Cpp_kernel_constants("gaussian", 0L)["c"]))
})

# ── Equivalences avec les noyaux nommes ─────────────────────────────────────
test_that("pow(s,t) = noyau nomme : K^(r) aux points interieurs", {
  for (k in names(pow_equiv)) for (r in 0:4) {
    a <- Cpp_kernel_deriv(k, xin, r)
    b <- Cpp_kernel_deriv(pow_equiv[[k]], xin, r)
    expect_equal(b, a, tolerance = 1e-10, label = paste(pow_equiv[[k]], "r =", r))
  }
})

test_that("pow(s,t) = noyau nomme : convolutions K^(r) * K^(r)", {
  u <- seq(-2.2, 2.2, length.out = 89)
  for (k in names(pow_equiv)) for (r in 0:2) {
    expect_equal(Cpp_kernel_conv(pow_equiv[[k]], u, r),
                 Cpp_kernel_conv(k, u, r), tolerance = 1e-6,
                 label = paste(pow_equiv[[k]], "conv r =", r))
  }
})

test_that("pow(s,t) = noyau nomme : bandwidth d'over-smoothing", {
  for (k in names(pow_equiv)) for (r in 0:2)
    expect_equal(Cpp_hos(pow_equiv[[k]], r, 100L), Cpp_hos(k, r, 100L),
                 tolerance = 1e-10)
})

test_that("Cpp_kernel_constants() donne les constantes exactes", {
  cb <- Cpp_kernel_constants("pow(2,2)", 0L)
  expect_equal(unname(cb), c(15/16, 1/7, 1/21, 5/7), tolerance = 1e-12)
  expect_equal(unname(Cpp_kernel_constants("pow(2,1)", 0L)["R"]), 3/5)
  expect_equal(unname(Cpp_kernel_constants("pow(2,2)", 2L)["R"]), 22.5)
  ct <- Cpp_kernel_constants("pow(3,3)", 0L)
  expect_equal(unname(ct[c("c", "mu2", "R")]), c(70/81, 35/243, 175/247),
               tolerance = 1e-12)
  expect_equal(unname(Cpp_kernel_constants("pow(2,0)", 0L)["c"]), 1/2)
})

# ── Pow non entier : verifications numeriques ───────────────────────────────
test_that("pow(1.5,2.5) : densite, moments et R(K^(r)) coherents", {
  k  <- kernel.pow(1.5, 2.5)
  f  <- function(x, r = 0L) Cpp_kernel_deriv(k, x, r)
  cs <- Cpp_kernel_constants(k, 0L)
  expect_equal(integrate(f, -1, 1)$value, 1, tolerance = 1e-6)
  expect_equal(integrate(function(x) x^2 * f(x), -1, 1)$value,
               unname(cs["mu2"]), tolerance = 1e-6)
  expect_equal(integrate(function(x) x^4 * f(x), -1, 1)$value,
               unname(cs["mu4"]), tolerance = 1e-6)
  for (r in 0:1) {
    num <- integrate(function(x) f(x, r)^2, -1, 1, subdivisions = 2000L)$value
    expect_equal(unname(Cpp_kernel_constants(k, r)["R"]), num, tolerance = 1e-6)
    expect_equal(Cpp_kernel_conv(k, 0, r) * (-1)^r, num, tolerance = 1e-4)
  }
})

test_that("pow non entier : derivees = differences finies", {
  for (k in c("pow(1.5,2.5)", "pow(2.5,3.7)", "pow(4,2.2)")) {
    x0 <- c(-0.7, -0.2, 0.3, 0.8); h <- 1e-5
    for (r in 1:3) {
      num <- (Cpp_kernel_deriv(k, x0 + h, r - 1L) -
              Cpp_kernel_deriv(k, x0 - h, r - 1L)) / (2 * h)
      expect_equal(Cpp_kernel_deriv(k, x0, r), num, tolerance = 1e-5,
                   label = paste(k, "r =", r))
    }
  }
})

test_that("pow : symetrie, support compact, convolution paire", {
  k <- "pow(2.5,3.5)"
  x <- seq(0.05, 0.95, by = 0.1)
  for (r in 0:3)
    expect_equal(Cpp_kernel_deriv(k, -x, r), (-1)^r * Cpp_kernel_deriv(k, x, r))
  expect_equal(Cpp_kernel_deriv(k, c(-1.5, 1.01, 3), 0L), c(0, 0, 0))
  expect_equal(Cpp_kernel_deriv(k, 0, 1L), 0)
  expect_equal(Cpp_kernel_conv(k, c(-2.5, 2, 7), 0L), c(0, 0, 0))
  u <- c(0.3, 0.9, 1.6)
  expect_equal(Cpp_kernel_conv(k, -u, 2L), Cpp_kernel_conv(k, u, 2L))
  ## ordres hors limites : zero
  expect_equal(Cpp_kernel_deriv(k, 0.2, 17L), 0)
  expect_equal(Cpp_kernel_conv(k, 0.2, 17L), 0)
})

test_that("pow avec t entier > 10 utilise la recurrence (pas le binome)", {
  k <- "pow(2,12)"
  f <- function(x) Cpp_kernel_deriv(k, x, 0L)
  expect_equal(integrate(f, -1, 1)$value, 1, tolerance = 1e-8)
  expect_true(all(is.finite(Cpp_kernel_deriv(k, seq(-1, 1, 0.1), 2L))))
})

# ── kernel.fun / kernel.conv / dkde avec pow ────────────────────────────────
test_that("kernel.fun() et kernel.conv() avec pow", {
  kf <- kernel.fun(kernel = "pow(2,3)", n.grid = 2048L)
  expect_s4_class(kf, "kfun")
  expect_equal(range(kf@x), c(-1.1, 1.1))
  expect_equal(sum(kf@y) * diff(kf@x[1:2]), 1, tolerance = 0.01)
  kc <- kernel.conv(kernel = kernel.pow(2, 3))
  expect_s4_class(kc, "kconv")
  expect_equal(range(kc@x), c(-2.1, 2.1))
  expect_true(all(kc@y >= -1e-12))
  expect_warning(kernel.fun(kernel = "pow(2,2)", deriv.order = 17L), "16")
  expect_warning(kernel.conv(kernel = "pow(2,2)", deriv.order = 17L), "16")
})

test_that("dkde() accepte pow et logistic", {
  for (k in c("pow(2,2)", "pow(1.5,2.5)", "logistic")) {
    d <- dkde(data = xs, kernel = k)
    expect_s4_class(d, "dkde")
    expect_equal(d@kernel, kedd:::.normalize_kernel(k))
    expect_equal(sum(d@y) * diff(d@x[1:2]), 1, tolerance = 0.02)
  }
  d1 <- dkde(data = xs, h = 0.6, deriv.order = 1L, kernel = "pow(2,3)")
  expect_true(all(is.finite(d1@y)))
})

# ── Selecteurs de bandwidth avec pow ────────────────────────────────────────
test_that("tous les selecteurs : pow(2,2) == biweight", {
  sel <- list(
    UCV  = function(k) h.ucv(xs, kernel = k),
    BCV1 = function(k) h.bcv(xs, whichbcv = 1L, kernel = k),
    BCV2 = function(k) h.bcv(xs, whichbcv = 2L, kernel = k),
    CCV  = function(k) h.ccv(xs, kernel = k),
    MCV  = function(k) h.mcv(xs, kernel = k),
    TCV  = function(k) h.tcv(xs, kernel = k),
    AMISE = function(k) h.amise(xs, kernel = k),
    MLCV = function(k) h.mlcv(xs, kernel = k)
  )
  for (m in names(sel)) {
    a <- sel[[m]]("biweight"); b <- sel[[m]]("pow(2,2)")
    expect_equal(b@method, m)
    expect_equal(b@kernel, "pow(2,2)")
    ## the criteria agree to ~1e-8 (tabulated pow convolution); a flat minimum
    ## can move the minimiser slightly more
    expect_equal(b@h, a@h, tolerance = 5e-3, label = paste(m, "pow(2,2)"))
  }
  crit <- list(UCV = Cpp_h_ucv, BCV1 = Cpp_h_bcv_1, BCV2 = Cpp_h_bcv_2,
               CCV = Cpp_h_ccv, MCV = Cpp_h_mcv, TCV = Cpp_h_tcv)
  for (m in names(crit)) for (h in c(0.6, 1.0, 1.4))
    expect_equal(as.numeric(crit[[m]](h, "pow(2,2)", 0L, xs, 0L)),
                 as.numeric(crit[[m]](h, "biweight", 0L, xs, 0L)),
                 tolerance = 1e-6, label = paste(m, "criterion at h =", h))
})

test_that("selecteurs avec pow non entier et derivees", {
  k <- kernel.pow(4, 4.5)
  for (bw in list(h.ucv(xs, kernel = k), h.bcv(xs, kernel = k),
                  h.bcv(xs, whichbcv = 2L, kernel = k), h.ccv(xs, kernel = k),
                  h.mcv(xs, kernel = k), h.tcv(xs, kernel = k),
                  h.amise(xs, kernel = k), h.mlcv(xs, kernel = k),
                  h.ucv(xs, deriv.order = 1L, kernel = k),
                  h.amise(xs, deriv.order = 2L, kernel = k))) {
    expect_s4_class(bw, "bandwidth")
    expect_true(is.finite(bw@h) && bw@h > 0)
  }
})

test_that("pow(s,t) et noyau nomme : memes ordres maximaux pour les selecteurs", {
  sel <- list(ucv = h.ucv, mcv = h.mcv, ccv = h.ccv)
  for (k in names(pow_equiv)) for (nm in names(sel)) for (r in 0:5) {
    a <- is.na(suppressWarnings(sel[[nm]](xs, deriv.order = r, kernel = k))@h)
    b <- is.na(suppressWarnings(sel[[nm]](xs, deriv.order = r, kernel = pow_equiv[[k]]))@h)
    expect_identical(b, a, label = paste(nm, pow_equiv[[k]], "r =", r))
  }
  ## tricube = pow(3,3) : UCV 3, MCV 2, CCV 1
  expect_false(is.na(suppressWarnings(h.ucv(xs, deriv.order = 3L, kernel = "tricube"))@h))
  expect_warning(h.ucv(xs, deriv.order = 4L, kernel = "tricube"), "maximum order")
  expect_warning(h.mcv(xs, deriv.order = 3L, kernel = "tricube"), "maximum order")
  expect_warning(h.ccv(xs, deriv.order = 2L, kernel = "tricube"), "maximum order")
})

test_that("pow(s,t) avec s impair : r < s + 1/2", {
  ## pow(1,4) : |x| non derivable en 0, donc r <= 1 (r <= 2 par t et st seuls)
  expect_false(is.na(suppressWarnings(h.ucv(xs, deriv.order = 1L, kernel = "pow(1,4)"))@h))
  expect_warning(h.ucv(xs, deriv.order = 2L, kernel = "pow(1,4)"), "maximum order")
  ## s pair : pas de restriction en 0
  expect_false(is.na(suppressWarnings(h.ucv(xs, deriv.order = 2L, kernel = "pow(2,4)"))@h))
})

test_that("cosinus : UCV r <= 1, MCV r = 0, CCV indisponible", {
  expect_false(is.na(suppressWarnings(h.ucv(xs, deriv.order = 1L, kernel = "cosine"))@h))
  expect_warning(h.ucv(xs, deriv.order = 2L, kernel = "cosine"), "maximum order")
  expect_warning(h.bcv(xs, deriv.order = 2L, kernel = "cosine"), "maximum order")
  expect_false(is.na(suppressWarnings(h.mcv(xs, kernel = "cosine"))@h))
  expect_warning(h.mcv(xs, deriv.order = 1L, kernel = "cosine"), "maximum order")
  expect_warning(h.ccv(xs, kernel = "cosine"), "maximum order")
})

test_that("logistique : R(K^(r)) = |B_{2r+2}| (nombres de Bernoulli)", {
  absB <- c(1/6, 1/30, 1/42, 1/30, 5/66, 691/2730, 7/6, 3617/510, 43867/798,
            174611/330, 854513/138)
  R <- sapply(0:10, function(r) Cpp_kernel_constants("logistic", r)[["R"]])
  expect_equal(R, absB, tolerance = 1e-14)
  ## et integration numerique pour r <= 4
  for (r in 0:4) {
    num <- integrate(function(u) Cpp_kernel_deriv("logistic", u, r)^2, -40, 40,
                     subdivisions = 2000L, rel.tol = 1e-12)$value
    expect_equal(R[r + 1L], num, tolerance = 1e-9)
  }
})

test_that("pow : ordres de derivee non admissibles -> NA + warning", {
  expect_warning(bw <- h.ucv(xs, deriv.order = 2L, kernel = "pow(2,1)"), "maximum order")
  expect_true(is.na(bw@h))
  expect_warning(bw <- h.bcv(xs, deriv.order = 7L, kernel = "pow(2,20)"), "maximum order")
  expect_true(is.na(bw@h))
  expect_warning(bw <- h.amise(xs, deriv.order = 2L, kernel = "pow(1.2,4)"), "maximum order")
  expect_true(is.na(bw@h))
  expect_warning(bw <- h.ccv(xs, kernel = "pow(1.5,2.5)"), "CCV")
  expect_true(is.na(bw@h))
  expect_warning(bw <- h.ccv(xs, kernel = "pow(2,1)"), "CCV")
  expect_true(is.na(bw@h))
  expect_warning(bw <- h.mcv(xs, deriv.order = 1L, kernel = "pow(2,1)"), "MCV")
  expect_true(is.na(bw@h))
})

test_that("h.scv rejette pow et logistic", {
  expect_error(h.scv(xs, kernel = "pow(2,2)"), "gaussian")
  expect_error(h.scv(xs, kernel = "logistic"), "gaussian")
})

test_that("show/plot/lines avec un bandwidth pow", {
  bw <- h.ucv(xs, kernel = "pow(2,3)")
  expect_output(show(bw), "pow\\(2,3\\)")
  tmp <- tempfile(fileext = ".png"); on.exit(unlink(tmp))
  png(tmp)
  p <- plot(bw)
  expect_no_error(print(p$plot + lines(h.ucv(xs, kernel = "biweight"))))
  expect_no_error(plot(kernel.fun(kernel = "pow(1.5,2.5)", deriv.order = 1L)))
  expect_no_error(plot(kernel.conv(kernel = "pow(2,2)")))
  dev.off()
  expect_true(is.finite(Cpp_plot_cv("BCV1", bw@h, bw@kernel, 0L, xs)))
})

# ── Noyau logistique : toutes derivees et convolutions ───────────────────────
test_that("logistic : R(K^(r)) exact = convolution en 0", {
  exact <- c(1/6, 1/30, 1/42, 1/30, 5/66)
  for (r in 0:4)
    expect_equal(Cpp_kernel_conv("logistic", 0, r) * (-1)^r, exact[r + 1L],
                 tolerance = 1e-7, label = paste("logistic r =", r))
  for (r in 5:6) {
    num <- integrate(function(x) Cpp_kernel_deriv("logistic", x, r)^2, -40, 40)$value
    expect_equal(Cpp_kernel_conv("logistic", 0, r) * (-1)^r, num, tolerance = 1e-6)
  }
})

test_that("logistic : derivees jusqu'a l'ordre 8 = differences finies", {
  x0 <- c(-3.1, -0.4, 0.7, 2.6); h <- 1e-5
  for (r in 1:8) {
    num <- (Cpp_kernel_deriv("logistic", x0 + h, r - 1L) -
            Cpp_kernel_deriv("logistic", x0 - h, r - 1L)) / (2 * h)
    expect_equal(Cpp_kernel_deriv("logistic", x0, r), num, tolerance = 1e-5,
                 label = paste("logistic r =", r))
  }
})

test_that("logistic : pas de NaN dans les queues extremes", {
  for (r in 0:3) {
    v <- Cpp_kernel_deriv("logistic", c(-1e4, -800, 800, 1e4), r)
    expect_true(all(is.finite(v)))
    expect_equal(v, c(0, 0, 0, 0))
  }
  expect_equal(Cpp_kernel_conv("logistic", c(-500, 500), 0L), c(0, 0))
})

test_that("logistic : convolution d'ordre r > 0 n'est plus nulle", {
  kc <- kernel.conv(kernel = "logistic", deriv.order = 2L)
  expect_true(max(abs(kc@y)) > 0.01)
  expect_warning(kernel.fun(kernel = "logistic", deriv.order = 21L), "20")
  expect_warning(kernel.conv(kernel = "logistic", deriv.order = 21L), "20")
})

test_that("logistic : tous les selecteurs, r = 0 et r = 1", {
  for (r in 0:1) {
    for (bw in list(h.ucv(xs, deriv.order = r, kernel = "logistic"),
                    h.bcv(xs, deriv.order = r, kernel = "logistic"),
                    h.bcv(xs, deriv.order = r, whichbcv = 2L, kernel = "logistic"),
                    h.ccv(xs, deriv.order = r, kernel = "logistic"),
                    h.mcv(xs, deriv.order = r, kernel = "logistic"),
                    h.tcv(xs, deriv.order = r, kernel = "logistic"),
                    h.amise(xs, deriv.order = r, kernel = "logistic"))) {
      expect_true(is.finite(bw@h) && bw@h > 0,
                  label = paste(bw@method, "logistic r =", r))
    }
  }
  expect_warning(bw <- h.ucv(xs, deriv.order = 9L, kernel = "logistic"), "maximum order")
  expect_true(is.na(bw@h))
})
