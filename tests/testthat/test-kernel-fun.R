## Tests pour kernel.fun() et kernel.conv() — classes S4 kfun / kconv

set.seed(1)
kernels <- c("gaussian", "epanechnikov", "uniform", "triangular",
             "biweight", "triweight", "tricube", "cosine", "silverman", "logistic",
             "pow(2,2)", "pow(1.5,2.5)")

test_that("kernel.fun() retourne un objet S4 de classe kfun", {
  kf <- kernel.fun(kernel = "gaussian", deriv.order = 0L)
  expect_s4_class(kf, "kfun")
  expect_true(is.numeric(kf@x))
  expect_true(is.numeric(kf@y))
  expect_equal(kf@kernel, "gaussian")
  expect_equal(kf@deriv.order, 0L)
  expect_equal(length(kf@x), 512L)
})

test_that("kernel.fun() fonctionne pour les 9 noyaux (r=0)", {
  for (k in kernels) {
    kf <- kernel.fun(kernel = k, deriv.order = 0L)
    expect_s4_class(kf, "kfun")
    expect_equal(length(kf@y), 512L)
    expect_true(all(is.finite(kf@y)))
  }
})

test_that("kernel.fun() integrale ≈ 1 pour r=0 (noyaux a support compact)", {
  # Gaussian et Silverman ont un support infini : la grille [-4,4] ne capture
  # pas tout. On teste uniquement les noyaux a support [-1,1].
  kernels_compact <- c("epanechnikov", "uniform", "triangular",
                       "biweight", "triweight", "tricube", "cosine",
                       "pow(2,2)", "pow(1.5,2.5)", "pow(3,0.5)")
  for (k in kernels_compact) {
    kf <- kernel.fun(kernel = k, deriv.order = 0L, n.grid = 2048L)
    dx <- diff(kf@x[1:2])
    integral <- sum(kf@y) * dx
    expect_equal(integral, 1, tolerance = 0.01)
  }
})

test_that("kernel.fun() gaussien integrale ≈ 1 sur support large", {
  kf <- kernel.fun(x = seq(-6, 6, length.out = 4096L), kernel = "gaussian")
  dx <- diff(kf@x[1:2])
  expect_equal(sum(kf@y) * dx, 1, tolerance = 0.01)
})

test_that("kernel.fun() avec points x fournis par l'utilisateur", {
  pts <- seq(-2, 2, length.out = 50)
  kf  <- kernel.fun(x = pts, kernel = "gaussian", deriv.order = 0L)
  expect_equal(length(kf@x), 50L)
  expect_equal(kf@x, pts)
})

test_that("kernel.fun() derivee d'ordre r=1 integrale ≈ 0", {
  kf <- kernel.fun(kernel = "gaussian", deriv.order = 1L, n.grid = 2048L)
  dx <- diff(kf@x[1:2])
  expect_equal(sum(kf@y) * dx, 0, tolerance = 0.01)
})

test_that("kernel.conv() retourne un objet S4 de classe kconv", {
  kc <- kernel.conv(kernel = "gaussian", deriv.order = 0L)
  expect_s4_class(kc, "kconv")
  expect_equal(kc@kernel, "gaussian")
  expect_equal(kc@deriv.order, 0L)
})

test_that("kernel.conv() valeurs positives pour r=0", {
  for (k in c("gaussian", "epanechnikov", "biweight")) {
    kc <- kernel.conv(kernel = k, deriv.order = 0L)
    expect_true(all(kc@y >= 0))
  }
})

test_that("kernel.fun() logistic integrale ≈ 1 sur grid large", {
  kf <- kernel.fun(kernel = "logistic", deriv.order = 0L, n.grid = 2048L)
  dx <- diff(kf@x[1:2])
  expect_equal(sum(kf@y) * dx, 1, tolerance = 0.01)
})

test_that("kernel.fun() logistic derivees r=1..4 integrale ≈ 0", {
  for (r in 1:4) {
    kf <- kernel.fun(kernel = "logistic", deriv.order = r, n.grid = 2048L)
    dx <- diff(kf@x[1:2])
    expect_equal(sum(kf@y) * dx, 0, tolerance = 0.02,
                 label = paste("logistic r=", r, "integral"))
  }
})

test_that("kernel.conv() logistic r=0 valeur en 0 ≈ 1/6", {
  kc <- kernel.conv(kernel = "logistic", deriv.order = 0L, n.grid = 1024L)
  # find index closest to u=0
  idx0 <- which.min(abs(kc@x))
  expect_equal(kc@y[idx0], 1/6, tolerance = 0.005)
})

test_that("kernel.conv() logistic r=1 en 0 = -R(K') = -1/30", {
  kc <- kernel.conv(x = 0, kernel = "logistic", deriv.order = 1L)
  expect_equal(kc@y, -1/30, tolerance = 1e-7)
})
