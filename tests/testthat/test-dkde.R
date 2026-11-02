## Tests pour dkde() — classe S4 "dkde"
## Signature : dkde(x=NULL, data, h, deriv.order=0L, kernel="gaussian", nthreads=0L)

set.seed(7)
obs <- rnorm(300)
kernels_dk <- c("gaussian", "epanechnikov", "biweight", "triangular",
                "uniform", "triweight", "tricube", "cosine", "silverman")

test_that("dkde() retourne un objet S4 de classe dkde", {
  d <- dkde(data = obs, kernel = "gaussian")
  expect_s4_class(d, "dkde")
  expect_true(is.numeric(d@x))
  expect_true(is.numeric(d@y))
  expect_equal(d@kernel, "gaussian")
  expect_equal(d@deriv.order, 0L)
  expect_equal(length(d@x), 512L)
})

test_that("dkde() fonctionne pour les 9 noyaux (r=0)", {
  for (k in kernels_dk) {
    d <- dkde(data = obs, kernel = k)
    expect_s4_class(d, "dkde")
    expect_true(all(is.finite(d@y)))
  }
})

test_that("dkde() densite non-negative pour noyaux classiques (r=0)", {
  # Le noyau Silverman est un noyau d'ordre superieur : peut etre negatif
  kernels_pos <- setdiff(kernels_dk, "silverman")
  for (k in kernels_pos) {
    d <- dkde(data = obs, kernel = k)
    expect_true(all(d@y >= 0))
  }
})

test_that("dkde() integrale ≈ 1 (r=0, gaussien)", {
  d  <- dkde(data = obs, kernel = "gaussian")
  dx <- diff(d@x[1:2])
  expect_equal(sum(d@y) * dx, 1, tolerance = 0.05)
})

test_that("dkde() avec bandwidth fournie manuellement", {
  d <- dkde(data = obs, h = 0.3, kernel = "gaussian")
  expect_equal(d@h, 0.3)
})

test_that("dkde() avec grille x fournie manuellement", {
  pts <- seq(-3, 3, length.out = 100)
  d   <- dkde(x = pts, data = obs, kernel = "gaussian")
  expect_equal(length(d@y), 100L)
})

test_that("dkde() derivee d'ordre 1 change de signe (gaussien)", {
  d1 <- dkde(data = obs, kernel = "gaussian", deriv.order = 1L)
  expect_s4_class(d1, "dkde")
  expect_equal(d1@deriv.order, 1L)
  expect_true(any(d1@y > 0) && any(d1@y < 0))
})

test_that("dkde() rejette une bandwidth negative", {
  expect_error(dkde(data = obs, h = -0.1))
})

test_that("dkde() rejette des donnees insuffisantes", {
  expect_error(dkde(data = 1.0))
})
