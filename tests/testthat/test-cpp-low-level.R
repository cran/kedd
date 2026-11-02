## Tests bas niveau — fonctions Rcpp exportées
## Cpp_hos(kernel, deriv_order, n)  — prend n (entier), pas les données

set.seed(99)
x <- rnorm(100)

# ── Cpp_kernel_deriv ─────────────────────────────────────────────────────────
test_that("Cpp_kernel_deriv gaussien r=0 integrale ≈ 1", {
  u  <- seq(-5, 5, length.out = 2001)
  y  <- Cpp_kernel_deriv("gaussian", u, 0L)
  du <- diff(u[1:2])
  expect_equal(sum(y) * du, 1, tolerance = 0.01)
})

test_that("Cpp_kernel_deriv est symetrique pour r pair", {
  u <- seq(-3, 3, length.out = 601)
  y <- Cpp_kernel_deriv("gaussian", u, 2L)
  expect_equal(y, rev(y), tolerance = 1e-10)
})

test_that("Cpp_kernel_deriv est antisymetrique pour r impair", {
  u <- seq(-3, 3, length.out = 601)
  y <- Cpp_kernel_deriv("gaussian", u, 1L)
  expect_equal(y, -rev(y), tolerance = 1e-10)
})

# ── Cpp_kernel_conv ──────────────────────────────────────────────────────────
test_that("Cpp_kernel_conv gaussien r=0 est symetrique et positif", {
  u <- seq(-5, 5, length.out = 1001)
  y <- Cpp_kernel_conv("gaussian", u, 0L)
  expect_true(all(y >= 0))
  expect_equal(y, rev(y), tolerance = 1e-10)
})

# ── Cpp_h_ucv / Cpp_h_bcv_1 ─────────────────────────────────────────────────
test_that("Cpp_h_ucv retourne un scalaire fini", {
  val <- Cpp_h_ucv(0.3, "gaussian", 0L, x, 1L)
  expect_length(val, 1)
  expect_true(is.finite(val))
})

test_that("Cpp_h_bcv_1 retourne un scalaire fini", {
  val <- Cpp_h_bcv_1(0.3, "gaussian", 0L, x, 1L)
  expect_length(val, 1)
  expect_true(is.finite(val))
})

# ── Cpp_h_scv ────────────────────────────────────────────────────────────────
test_that("Cpp_h_scv retourne un scalaire fini", {
  g   <- sd(x) * 0.9266 * length(x)^(-2/13)
  val <- Cpp_h_scv(0.3, g, "gaussian", 0L, x, 1L)
  expect_length(val, 1)
  expect_true(is.finite(val))
})

test_that("Cpp_h_scv est une fonction de h avec minimum unique", {
  g    <- sd(x) * 0.9266 * length(x)^(-2/13)
  hvec <- seq(0.05, 1.5, length.out = 50)
  vals <- sapply(hvec, function(h) Cpp_h_scv(h, g, "gaussian", 0L, x, 1L))
  idx_min <- which.min(vals)
  expect_gt(idx_min, 1L)
  expect_lt(idx_min, 50L)
})

test_that("Cpp_h_scv rejette noyau non-gaussien", {
  g <- sd(x) * 0.9266 * length(x)^(-2/13)
  expect_error(Cpp_h_scv(0.3, g, "epanechnikov", 0L, x, 1L), "gaussian")
})

# ── Cpp_hos ──────────────────────────────────────────────────────────────────
test_that("Cpp_hos retourne une valeur positive", {
  # Cpp_hos(kernel, deriv_order, n) — n est la taille d'échantillon (entier)
  h <- Cpp_hos("gaussian", 0L, 100L)
  expect_gt(h, 0)
  expect_true(is.finite(h))
})
