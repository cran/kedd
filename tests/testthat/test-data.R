## Jeux de donnees : format, densite documentee (Marron & Wand 1992), selecteurs

mw <- list(
  kurtotic = list(c(2/3, 1/3), c(0, 0), c(1, 1/10)),
  outlier  = list(c(1/10, 9/10), c(0, 0), c(1, 1/10)),
  bimodal  = list(c(1/2, 1/2), c(-3/2, 3/2), c(1/2, 1/2)),
  trimodal = list(c(9/20, 9/20, 1/10), c(-6/5, 6/5, 0), c(3/5, 3/5, 1/4)),
  claw     = list(c(1/2, rep(1/10, 5)), c(0, (0:4)/2 - 1), c(1, rep(1/10, 5)))
)
pmix <- function(q, m)
  rowSums(sapply(seq_along(m[[1]]), function(i) m[[1]][i] * pnorm(q, m[[2]][i], m[[3]][i])))
get_data <- function(nm) get(nm, envir = asNamespace("kedd"))

test_that("les cinq jeux de donnees : vecteurs numeriques de longueur 200", {
  for (nm in names(mw)) {
    x <- get_data(nm)
    expect_type(x, "double")
    expect_length(x, 200L)
    expect_true(all(is.finite(x)), label = nm)
  }
  e <- new.env()
  expect_identical(data("claw", package = "kedd", envir = e), "claw")
  expect_identical(e$claw, get_data("claw"))
})

test_that("chaque echantillon est compatible avec sa densite documentee", {
  for (nm in names(mw)) {
    p <- suppressWarnings(ks.test(get_data(nm), function(q) pmix(q, mw[[nm]]))$p.value)
    expect_gt(p, 0.01, label = paste("test KS", nm))
  }
})

test_that("tous les selecteurs fonctionnent sur les cinq echantillons", {
  sel <- list(AMISE = h.amise, UCV = h.ucv, BCV1 = h.bcv,
              BCV2 = function(x) h.bcv(x, whichbcv = 2), CCV = h.ccv, MCV = h.mcv,
              TCV = h.tcv, SCV = h.scv, MLCV = h.mlcv)
  for (nm in names(mw)) for (m in names(sel)) {
    bw <- suppressWarnings(sel[[m]](get_data(nm)))
    expect_true(is.finite(bw@h) && bw@h > 0, label = paste(m, nm))
  }
})

test_that("claw : l'estimation UCV suit la vraie densite", {
  f_claw <- function(x) 0.5 * dnorm(x) +
    rowSums(sapply(0:4, function(l) 0.1 * dnorm(x, l / 2 - 1, 0.1)))
  claw <- get_data("claw")
  d <- dkde(x = seq(-3, 3, length.out = 301), data = claw, h = h.ucv(claw)@h)
  ise <- sum((d@y - f_claw(d@x))^2) * diff(d@x[1:2])
  expect_lt(ise, 0.05)
})
