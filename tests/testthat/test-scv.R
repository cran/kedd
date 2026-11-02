## SCV pour la densite et ses derivees (noyau et pilote gaussiens)

set.seed(13)
xs <- rnorm(40)
n  <- length(xs)

## phi^(k)(u) = (-1)^k He_k(u) phi(u)
phi_der <- function(u, k) {
  He0 <- 1; He1 <- u
  He <- if (k == 0) He0 else if (k == 1) He1 else {
    for (j in 2:k) { He2 <- u * He1 - (j - 1) * He0; He0 <- He1; He1 <- He2 }; He1 }
  (-1)^k * He * dnorm(u)
}
phi_s <- function(d, s, k) phi_der(d / s, k) / s^(k + 1)
Rr <- function(r) unname(Cpp_kernel_constants("gaussian", r)["R"])

test_that("Cpp_h_scv = calcul R independant, r = 0..3", {
  h <- 0.45; g <- 0.3
  s1 <- sqrt(2 * (h^2 + g^2)); s2 <- sqrt(h^2 + 2 * g^2); s3 <- sqrt(2) * g
  d  <- outer(xs, xs, "-"); d <- d[upper.tri(d)]
  for (r in 0:3) {
    k   <- 2L * r
    ref <- Rr(r) / (n * h^(2 * r + 1)) +
      (-1)^r * 2 / (n * (n - 1)) * sum(phi_s(d, s1, k) - 2 * phi_s(d, s2, k) + phi_s(d, s3, k))
    expect_equal(as.numeric(Cpp_h_scv(h, g, "gaussian", r, xs, 1L)), ref,
                 tolerance = 1e-10, label = paste("SCV r =", r))
  }
})

test_that("terme de biais SCV = integrale de [((K_h - delta) * f_g)^(r)]^2", {
  h <- 0.5; g <- 0.35
  sh <- sqrt(h^2 + g^2)
  s1 <- sqrt(2 * (h^2 + g^2)); s2 <- sqrt(h^2 + 2 * g^2); s3 <- sqrt(2) * g
  for (r in 0:2) {
    u_r <- function(x) sapply(x, function(t) mean(phi_s(t - xs, sh, r) - phi_s(t - xs, g, r)))
    lhs <- integrate(function(x) u_r(x)^2, -10, 10, subdivisions = 2000L, rel.tol = 1e-10)$value
    ## (-1)^r / n^2 sum over all (i, j), diagonal included
    B_off  <- as.numeric(Cpp_h_scv(h, g, "gaussian", r, xs, 1L)) - Rr(r) / (n * h^(2 * r + 1))
    diag0  <- phi_s(0, s1, 2 * r) - 2 * phi_s(0, s2, 2 * r) + phi_s(0, s3, 2 * r)
    rhs <- (n - 1) / n * B_off + (-1)^r * diag0 / n
    expect_equal(rhs, lhs, tolerance = 1e-7, label = paste("biais SCV r =", r))
  }
})

test_that("h.scv pour les derivees : minimum interieur, h croissant avec r", {
  set.seed(7)
  y <- rnorm(300)
  h <- sapply(0:2, function(r) {
    bw <- expect_silent(h.scv(y, deriv.order = r))
    expect_equal(bw@method, "SCV")
    bw@h
  })
  expect_true(all(is.finite(h)))
  expect_true(all(diff(h) > 0))
  ## h.scv et Cpp_plot_cv evaluent le meme critere
  g <- sd(y) * 0.9266 * length(y)^(-2/13)
  expect_equal(as.numeric(Cpp_plot_cv("SCV", c(0.4, 0.6), "gaussian", 1L, y)),
               sapply(c(0.4, 0.6), function(hh)
                 as.numeric(Cpp_h_scv(hh, g, "gaussian", 1L, y, 1L))),
               tolerance = 1e-10)
})
