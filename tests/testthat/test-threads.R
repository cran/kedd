## nthreads : 0 = tous les coeurs (plafonne par OMP_THREAD_LIMIT), k > 0 = k threads,
## fixe a chaque appel (aucun effet sur les appels suivants), meme resultat quel que
## soit le nombre de threads.

set.seed(321)
xt  <- c(rnorm(120, -1, 0.6), rnorm(80, 1.4, 0.8))
nts <- c(0L, 1L, 2L, 3L)

has_openmp <- function() Cpp_omp_threads(4L) > 1L
all_cores  <- function() {
  n   <- parallel::detectCores(logical = TRUE)
  lim <- suppressWarnings(as.integer(Sys.getenv("OMP_THREAD_LIMIT", "")))
  if (!is.na(lim) && lim > 0) n <- min(n, lim)
  n
}

# ── nombre de threads effectivement utilises ─────────────────────────────────
test_that("Cpp_omp_threads : 1 thread demande = 1 thread obtenu", {
  expect_identical(Cpp_omp_threads(1L), 1L)
})

test_that("Cpp_omp_threads : k > 0 donne k threads, 0 donne tous les coeurs", {
  skip_if_not(has_openmp(), "OpenMP non disponible")
  skip_on_cran()
  expect_identical(Cpp_omp_threads(2L), min(2L, all_cores()))
  expect_identical(Cpp_omp_threads(0L), as.integer(all_cores()))
  expect_identical(Cpp_omp_threads(), Cpp_omp_threads(0L))
})

test_that("nthreads ne persiste pas d'un appel a l'autre", {
  skip_if_not(has_openmp(), "OpenMP non disponible")
  all0 <- Cpp_omp_threads(0L)
  ## chaque fonction appelee avec 1 thread ...
  h.ucv(xt, nthreads = 1L); h.bcv(xt, nthreads = 1L); h.bcv(xt, whichbcv = 2, nthreads = 1L)
  h.ccv(xt, nthreads = 1L); h.mcv(xt, nthreads = 1L); h.tcv(xt, nthreads = 1L)
  h.scv(xt, nthreads = 1L); h.mlcv(xt, nthreads = 1L)
  dkde(data = xt, h = 0.4, nthreads = 1L)
  Cpp_kernel_deriv("gaussian", xt, 0L, 1L); Cpp_kernel_conv("gaussian", xt, 0L, 1L)
  Cpp_plot_cv("UCV", c(0.3, 0.5), "gaussian", 0L, xt, 1L)
  Cpp_conv2conv(0, 0L, 0L, "gaussian", "gaussian", 1L)
  ## ... ne change pas ce que 0 signifie ensuite
  expect_identical(Cpp_omp_threads(0L), all0)
  ## et h.ucv avec 0 n'est plus force a 1 thread
  h.ucv(xt, nthreads = 0L)
  expect_identical(Cpp_omp_threads(0L), all0)
})

# ── meme resultat quel que soit nthreads ─────────────────────────────────────
test_that("criteres C++ identiques pour nthreads = 0, 1, 2, 3", {
  crit <- list(UCV = Cpp_h_ucv, BCV1 = Cpp_h_bcv_1, BCV2 = Cpp_h_bcv_2,
               CCV = Cpp_h_ccv, MCV = Cpp_h_mcv, TCV = Cpp_h_tcv)
  for (k in c("gaussian", "biweight", "logistic", "pow(2,4)")) for (r in 0:1)
    for (m in names(crit)) {
      v <- sapply(nts, function(nt) as.numeric(crit[[m]](0.45, k, r, xt, nt)))
      expect_equal(v, rep(v[2], length(v)), tolerance = 1e-12,
                   label = paste(m, k, "r =", r))
    }
  g <- 0.3
  v <- sapply(nts, function(nt) as.numeric(Cpp_h_scv(0.45, g, "gaussian", 1L, xt, nt)))
  expect_equal(v, rep(v[2], length(v)), tolerance = 1e-12)
})

test_that("selecteurs : meme h pour nthreads = 0, 1, 2, 3", {
  sel <- list(
    UCV  = function(nt) h.ucv(xt, deriv.order = 1L, nthreads = nt),
    BCV1 = function(nt) h.bcv(xt, nthreads = nt),
    BCV2 = function(nt) h.bcv(xt, whichbcv = 2, kernel = "triweight", nthreads = nt),
    CCV  = function(nt) h.ccv(xt, kernel = "logistic", nthreads = nt),
    MCV  = function(nt) h.mcv(xt, kernel = "pow(2,3)", nthreads = nt),
    TCV  = function(nt) h.tcv(xt, nthreads = nt),
    SCV  = function(nt) h.scv(xt, deriv.order = 1L, nthreads = nt),
    MLCV = function(nt) h.mlcv(xt, kernel = "epanechnikov", nthreads = nt)
  )
  for (m in names(sel)) {
    h <- suppressWarnings(sapply(nts, function(nt) sel[[m]](nt)@h))
    expect_equal(h, rep(h[2], length(h)), tolerance = 1e-8, label = m)
  }
})

test_that("dkde, Cpp_kernel_deriv/conv, Cpp_plot_cv, Cpp_conv2conv : identiques", {
  ref <- dkde(data = xt, h = 0.4, deriv.order = 1L, kernel = "biweight", nthreads = 1L)@y
  for (nt in nts)
    expect_identical(dkde(data = xt, h = 0.4, deriv.order = 1L, kernel = "biweight",
                          nthreads = nt)@y, ref)
  u <- matrix(seq(-3, 3, length.out = 60), 6)
  for (k in c("gaussian", "tricube", "logistic", "pow(1.5,2.5)")) for (nt in nts) {
    expect_identical(Cpp_kernel_deriv(k, u, 1L, nt), Cpp_kernel_deriv(k, u, 1L, 1L))
    expect_identical(Cpp_kernel_conv(k, u, 0L, nt), Cpp_kernel_conv(k, u, 0L, 1L))
  }
  hg <- c(0.3, 0.45, 0.6)
  for (m in c("UCV", "BCV1", "BCV2", "CCV", "MCV", "TCV", "SCV"))
    expect_equal(Cpp_plot_cv(m, hg, "gaussian", 0L, xt, 3L),
                 Cpp_plot_cv(m, hg, "gaussian", 0L, xt, 1L), tolerance = 1e-12, label = m)
  expect_equal(Cpp_conv2conv(c(-1, 0.5), 0L, 0L, "gaussian", "gaussian", 3L),
               Cpp_conv2conv(c(-1, 0.5), 0L, 0L, "gaussian", "gaussian", 1L), tolerance = 1e-12)
})

test_that("table de convolution pow construite en parallele = construction sequentielle", {
  u <- seq(-2.2, 2.2, length.out = 2000)
  Cpp_kernel_conv("pow(2,2)", 0, 0L)                       # change de parametres
  par <- Cpp_kernel_conv("pow(2.7,3.3)", u, 1L, 4L)        # table construite par 4 threads
  Cpp_kernel_conv("pow(2,2)", 0, 0L)                       # invalide le cache
  seq1 <- Cpp_kernel_conv("pow(2.7,3.3)", u, 1L, 1L)       # reconstruite par 1 thread
  expect_identical(par, seq1)
})

# ── validation de nthreads ───────────────────────────────────────────────────
test_that("nthreads invalide : avertissement et 0 (tous les coeurs) utilise", {
  for (bad in list(-1, 1.5, NA, "a", c(1, 2))) {
    expect_warning(bw <- h.ucv(xt, nthreads = bad), "nthreads")
    expect_true(is.finite(bw@h))
    expect_warning(d <- dkde(data = xt, h = 0.4, nthreads = bad), "nthreads")
    expect_true(all(is.finite(d@y)))
  }
  expect_identical(kedd:::.check_nthreads(3), 3L)
  expect_identical(kedd:::.check_nthreads(0L), 0L)
})
