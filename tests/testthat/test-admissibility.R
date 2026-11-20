## Ordres admissibles : regularite q du noyau et ordre de derivee de chaque critere

## table de kedd-package.Rd, section "Maximum derivative order"
tab <- rbind(
  gaussian     = c(Inf, Inf, Inf, Inf, Inf, Inf, Inf),
  logistic     = c(20,  8,   8,   8,   8,   8,   8),
  silverman    = c(3,   3,   1,   NA,  NA,  NA,  NA),
  triweight    = c(3,   3,   1,   0,   1,   NA,  NA),
  tricube      = c(3,   3,   1,   0,   1,   NA,  NA),
  biweight     = c(2,   2,   1,   0,   0,   NA,  NA),
  epanechnikov = c(1,   1,   0,   NA,  NA,  NA,  NA),
  triangular   = c(1,   1,   0,   NA,  NA,  NA,  NA),
  cosine       = c(1,   1,   0,   NA,  NA,  NA,  NA),
  uniform      = c(0,   0,   0,   NA,  NA,  NA,  NA),
  "pow(2,4)"   = c(4,   4,   2,   1,   2,   0,   0),
  "pow(2,8)"   = c(8,   6,   4,   3,   6,   2,   2),
  "pow(1.5,2.5)" = c(1, 1,   1,   0,   NA,  NA,  NA))
colnames(tab) <- c("DKDE", "AMISE", "UCV", "MCV", "BCV1", "BCV2", "CCV")

test_that("ordres maximaux par critere et par noyau", {
  for (k in rownames(tab)) for (m in colnames(tab))
    expect_identical(as.numeric(kedd:::.max_order(k, m)), tab[k, m],
                     label = paste(k, m))
  ## TCV comme UCV, SCV sans limite (noyau gaussien), MLCV a r = 0
  for (k in rownames(tab))
    expect_identical(kedd:::.max_order(k, "TCV"), kedd:::.max_order(k, "UCV"))
  expect_identical(kedd:::.max_order("gaussian", "SCV"), Inf)
})

test_that("regularite q des noyaux", {
  q <- c(gaussian = Inf, logistic = Inf, silverman = 3, triweight = 3, tricube = 3,
         biweight = 2, epanechnikov = 1, triangular = 1, cosine = 1, uniform = 0)
  for (k in names(q)) expect_identical(kedd:::.kernel_q(k), q[[k]], label = k)
  ## Pow(s,t) : q = t (t entier), ceiling(t + 1) - 1 sinon (K^(q) non borne
  ## mais integrable) ; s pair sans effet
  expect_identical(kedd:::.kernel_q("pow(2,4)"), 4)
  expect_identical(kedd:::.kernel_q("pow(2,2.5)"), 3)
  expect_identical(kedd:::.kernel_q("pow(1,4)"), 1)
  expect_identical(kedd:::.kernel_q("pow(3,3)"), 3)
  expect_identical(kedd:::.kernel_q("pow(1.5,6)"), 2)
})

test_that("les selecteurs suivent la table", {
  set.seed(7)
  x <- rnorm(120)
  sel <- list(UCV = h.ucv, TCV = h.tcv, MCV = h.mcv, CCV = h.ccv,
              BCV1 = function(x, ...) h.bcv(x, whichbcv = 1L, ...),
              BCV2 = function(x, ...) h.bcv(x, whichbcv = 2L, ...))
  for (k in c("biweight", "triweight", "silverman", "epanechnikov"))
    for (m in names(sel)) {
      rmax <- kedd:::.max_order(k, m)
      if (!is.na(rmax)) {
        bw <- suppressWarnings(sel[[m]](x, deriv.order = rmax, kernel = k))
        expect_true(is.finite(bw@h), label = paste(k, m, rmax))
      }
      r_bad <- if (is.na(rmax)) 0L else rmax + 1L
      expect_warning(bw <- sel[[m]](x, deriv.order = r_bad, kernel = k), m)
      expect_true(is.na(bw@h))
    }
})

test_that("dkde avertit au-dela de la regularite du noyau", {
  set.seed(7)
  x <- rnorm(50)
  expect_silent(dkde(data = x, h = 0.5, deriv.order = 2L, kernel = "biweight"))
  expect_warning(dkde(data = x, h = 0.5, deriv.order = 3L, kernel = "biweight"),
                 "maximum order")
  expect_warning(dkde(data = x, h = 0.5, deriv.order = 4L, kernel = "silverman"),
                 "maximum order")
  expect_error(suppressWarnings(dkde(data = x, deriv.order = 2L, kernel = "biweight")),
               "UCV")
})

test_that("Silverman (mu2 = 0) : BCV1, BCV2, MCV et CCV non definis", {
  set.seed(7)
  x <- rnorm(80)
  for (f in list(h.mcv, h.ccv, function(x, ...) h.bcv(x, whichbcv = 1L, ...),
                 function(x, ...) h.bcv(x, whichbcv = 2L, ...))) {
    expect_warning(bw <- f(x, kernel = "silverman"), "order 4")
    expect_true(is.na(bw@h))
  }
  expect_false(is.na(h.ucv(x, deriv.order = 1L, kernel = "silverman")@h))
  expect_false(is.na(h.tcv(x, deriv.order = 1L, kernel = "silverman")@h))
})

test_that("Silverman : h.amise avertit (noyau d'ordre 4)", {
  set.seed(7)
  x <- rnorm(50)
  expect_warning(bw <- h.amise(x, kernel = "silverman"), "order 4")
  expect_equal(bw@h, h.amise(x)@h)
})

## Fondement de la regle : pour X, Y iid N(0, 1), le terme des paires
## E[h^-(m+1) K^(m)((X - Y)/h)] vaut sa cible int (K_h * f^(m)) f si et seulement si
## m <= q (biweight, q = 2 : vrai pour m = 2, faux pour m = 4)
test_that("le terme K^(m) des paires est sans biais si et seulement si m <= q", {
  fd <- function(x, m) switch(m / 2, (x^2 - 1), (x^4 - 6 * x^2 + 3)) * dnorm(x)
  h <- 0.9
  term <- function(m) {
    conv <- function(x) vapply(x, function(xx) integrate(function(u)
      kernel.fun(u, kernel = "biweight")@y * fd(xx - h * u, m), -1, 1)$value, 0)
    target <- integrate(function(x) conv(x) * dnorm(x), -8, 8)$value
    est <- integrate(function(d) kernel.fun(d / h, deriv.order = m, kernel = "biweight")@y /
                       h^(m + 1) * dnorm(d, 0, sqrt(2)), -h, h, rel.tol = 1e-10)$value
    c(target, est)
  }
  t2 <- term(2)
  expect_equal(t2[2], t2[1], tolerance = 1e-4)
  t4 <- term(4)
  expect_gt(abs(t4[2] - t4[1]), 0.1)
})
