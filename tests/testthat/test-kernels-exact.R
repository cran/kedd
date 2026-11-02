## Validation numerique de tous les noyaux nommes, a tous les ordres implementes :
## constantes (mu2, mu4, R(K^(r))), convolutions K^(r) * K^(r) et derivees.

maxr <- c(gaussian = 6, silverman = 7, epanechnikov = 2, uniform = 0,
          triangular = 1, biweight = 4, triweight = 6, tricube = 9,
          cosine = 6, logistic = 6)
infinite <- c("gaussian", "silverman", "logistic")

Kr <- function(k, r) function(x) Cpp_kernel_deriv(k, x, as.integer(r))
pint <- function(f, brk)
  sum(mapply(function(a, b) integrate(f, a, b, subdivisions = 2000L,
                                      rel.tol = 1e-10)$value,
             head(brk, -1), tail(brk, -1)))
supp <- function(k) if (k %in% infinite) 40 else 1

test_that("mu2 et mu4 = moments numeriques, pour chaque noyau", {
  for (k in names(maxr)) {
    L  <- supp(k); k0 <- Kr(k, 0)
    cs <- Cpp_kernel_constants(k, 0L)
    expect_equal(unname(cs["mu2"]), pint(function(x) x^2 * k0(x), c(-L, 0, L)),
                 tolerance = 1e-7, label = paste(k, "mu2"))
    expect_equal(unname(cs["mu4"]), pint(function(x) x^4 * k0(x), c(-L, 0, L)),
                 tolerance = 1e-7, label = paste(k, "mu4"))
  }
})

test_that("R(K^(r)) tabule = integrale numerique = (-1)^r (K^(r)*K^(r))(0)", {
  for (k in names(maxr)) for (r in 0:maxr[[k]]) {
    L  <- supp(k); f <- Kr(k, r)
    Rn <- pint(function(x) f(x)^2, c(-L, 0, L))
    expect_equal(unname(Cpp_kernel_constants(k, r)["R"]), Rn, tolerance = 1e-7,
                 label = paste(k, "R(K^(r)), r =", r))
    expect_equal(Cpp_kernel_conv(k, 0, r) * (-1)^r, Rn, tolerance = 1e-7,
                 label = paste(k, "conv(0), r =", r))
  }
})

test_that("convolutions K^(r) * K^(r) = convolution numerique", {
  for (k in names(maxr)) for (r in 0:maxr[[k]]) {
    L  <- supp(k); f <- Kr(k, r)
    us <- if (k %in% infinite) c(0.3, 2.7) else c(0.3, 1.5)
    num <- sapply(us, function(u) {
      brk <- if (k %in% infinite) c(-L, 0, u, L) else
        sort(unique(c(max(-1, u - 1), 0, u, min(1, u + 1))))
      if (!(k %in% infinite)) brk <- brk[brk >= max(-1, u - 1) & brk <= min(1, u + 1)]
      pint(function(x) f(x) * f(u - x), brk)
    })
    expect_equal(Cpp_kernel_conv(k, us, r), num, tolerance = 1e-6,
                 label = paste(k, "conv, r =", r))
    expect_equal(Cpp_kernel_conv(k, -us, r), num, tolerance = 1e-6,
                 label = paste(k, "conv symetrique, r =", r))
  }
})

test_that("K^(r) = derivee numerique de K^(r-1)", {
  for (k in names(maxr)) if (maxr[[k]] > 0) for (r in 1:maxr[[k]]) {
    x0 <- if (k %in% infinite) c(-3.1, -0.83, 0.41, 2.3) else c(-0.83, -0.41, 0.27, 0.66)
    h  <- 1e-6
    num <- (Kr(k, r - 1)(x0 + h) - Kr(k, r - 1)(x0 - h)) / (2 * h)
    expect_equal(Kr(k, r)(x0), num, tolerance = 1e-6,
                 label = paste(k, "derivee r =", r))
  }
})

test_that("noyaux a support compact nuls hors de [-1, 1], conv hors de [-2, 2]", {
  for (k in setdiff(names(maxr), infinite)) {
    expect_equal(Cpp_kernel_deriv(k, c(-1.5, 1.2), 0L), c(0, 0))
    expect_equal(Cpp_kernel_conv(k, c(-2.5, 3), 0L), c(0, 0))
  }
})

test_that("au-dela du degre polynomial : K^(r), convolution et R(K^(r)) nuls", {
  for (k in c("epanechnikov", "uniform", "triangular", "biweight", "triweight", "tricube")) {
    r <- maxr[[k]] + 1L
    expect_equal(Cpp_kernel_deriv(k, c(-0.4, 0.3), r), c(0, 0), label = paste(k, r))
    expect_equal(Cpp_kernel_conv(k, c(-0.4, 0.3), r), c(0, 0), label = paste(k, r))
    expect_equal(unname(Cpp_kernel_constants(k, r)["R"]), 0, label = paste(k, r))
  }
  expect_equal(Cpp_kernel_deriv("triangular", 0, 1L), 0)
  expect_error(Cpp_kernel_deriv("inconnu", 0, 0L), "Unsupported")
})

test_that("Silverman : periodicite des derivees (K^(r+8) = K^(r))", {
  x <- c(-1.7, 0.4, 2.2)
  expect_equal(Cpp_kernel_deriv("silverman", x, 9L), Cpp_kernel_deriv("silverman", x, 1L))
  expect_equal(Cpp_kernel_deriv("silverman", x, 4L), -Cpp_kernel_deriv("silverman", x, 0L))
})
