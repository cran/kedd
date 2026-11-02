## Tests pour les sélecteurs de bandwidths — classe S4 "bandwidth"

set.seed(42)
x200 <- rnorm(200)
x500 <- rnorm(500)
kernels_bw <- c("gaussian", "epanechnikov", "biweight", "logistic")

# ── helper ──────────────────────────────────────────────────────────────────
check_bandwidth <- function(bw_obj, method_name) {
  expect_s4_class(bw_obj, "bandwidth")
  expect_true(is.finite(bw_obj@h))
  expect_true(bw_obj@h > 0)
  expect_equal(bw_obj@method, method_name)
}

# ── h.amise ─────────────────────────────────────────────────────────────────
test_that("h.amise retourne un objet bandwidth valide", {
  for (k in kernels_bw) {
    bw <- h.amise(x200, kernel = k)
    check_bandwidth(bw, "AMISE")
  }
})

test_that("h.amise est coherent (h croît avec l'écart-type)", {
  x_large <- rnorm(200, sd = 3)
  expect_gt(h.amise(x_large)@h, h.amise(x200)@h)
})

# ── h.ucv ───────────────────────────────────────────────────────────────────
test_that("h.ucv retourne un objet bandwidth valide", {
  for (k in kernels_bw) {
    bw <- h.ucv(x200, kernel = k)
    check_bandwidth(bw, "UCV")
  }
})

test_that("h.ucv nthreads=1 et nthreads=0 donnent le meme resultat", {
  h1 <- h.ucv(x200, nthreads = 1L)@h
  hp <- h.ucv(x200, nthreads = 0L)@h
  expect_equal(h1, hp, tolerance = 1e-8)
})

# ── h.bcv ───────────────────────────────────────────────────────────────────
test_that("h.bcv (BCV1) retourne un objet bandwidth valide", {
  bw <- h.bcv(x200, whichbcv = 1L)
  check_bandwidth(bw, "BCV1")
})

test_that("h.bcv (BCV2) retourne un objet bandwidth valide", {
  bw <- h.bcv(x200, whichbcv = 2L)
  check_bandwidth(bw, "BCV2")
})

# ── h.ccv ───────────────────────────────────────────────────────────────────
test_that("h.ccv retourne un objet bandwidth valide", {
  bw <- h.ccv(x200)
  check_bandwidth(bw, "CCV")
})

# ── h.mcv ───────────────────────────────────────────────────────────────────
test_that("h.mcv retourne un objet bandwidth valide", {
  bw <- h.mcv(x200)
  check_bandwidth(bw, "MCV")
})

# ── h.tcv ───────────────────────────────────────────────────────────────────
test_that("h.tcv retourne un objet bandwidth valide", {
  bw <- h.tcv(x200)
  check_bandwidth(bw, "TCV")
})

# ── h.mlcv ──────────────────────────────────────────────────────────────────
test_that("h.mlcv retourne un objet bandwidth valide", {
  bw <- h.mlcv(x200)
  check_bandwidth(bw, "MLCV")
})

# ── h.scv ───────────────────────────────────────────────────────────────────
test_that("h.scv retourne un objet bandwidth valide (gaussian)", {
  bw <- h.scv(x200)
  check_bandwidth(bw, "SCV")
})

test_that("h.scv avec pilot g fourni manuellement", {
  g  <- sd(x200) * 0.9266 * length(x200)^(-2/13)
  bw <- h.scv(x200, g = g)
  check_bandwidth(bw, "SCV")
})

test_that("h.scv rejette un noyau non-gaussien", {
  expect_error(h.scv(x200, kernel = "epanechnikov"), "gaussian")
})

test_that("h.scv rejette un pilot g negatif", {
  expect_error(h.scv(x200, g = -0.1))
})

# ── Coherence inter-méthodes ─────────────────────────────────────────────────
test_that("toutes les bandwidths sont dans un intervalle raisonnable", {
  methods <- list(
    h.amise(x500), h.ucv(x500), h.bcv(x500),
    h.ccv(x500),   h.mcv(x500), h.scv(x500)
  )
  hos <- h.amise(x500, kernel = "gaussian")@hos
  for (m in methods) {
    expect_true(m@h > hos * 0.05)
    expect_true(m@h < hos * 5.0)
  }
})

# ── deriv.order > 0 ──────────────────────────────────────────────────────────
test_that("h.ucv avec deriv.order=1 retourne h valide", {
  bw <- h.ucv(x200, deriv.order = 1L, kernel = "gaussian")
  check_bandwidth(bw, "UCV")
})

# ── noyau logistic ───────────────────────────────────────────────────────────
test_that("noyau logistic: h.amise, h.ucv, h.bcv2 retournent h valide", {
  for (fn in list(
    function() h.amise(x200, kernel = "logistic"),
    function() h.ucv(x200, kernel = "logistic"),
    function() h.bcv(x200, kernel = "logistic", whichbcv = 2L)
  )) {
    bw <- fn()
    expect_s4_class(bw, "bandwidth")
    expect_true(is.finite(bw@h))
    expect_true(bw@h > 0)
  }
})

test_that("noyau logistic: h.ccv, h.mcv, h.tcv retournent h valide", {
  for (fn in list(
    function() h.ccv(x200, kernel = "logistic"),
    function() h.mcv(x200, kernel = "logistic"),
    function() h.tcv(x200, kernel = "logistic")
  )) {
    bw <- fn()
    expect_s4_class(bw, "bandwidth")
    expect_true(is.finite(bw@h))
    expect_true(bw@h > 0)
  }
})
