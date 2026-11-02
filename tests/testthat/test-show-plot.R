## Tests des methodes show() et plot() — classes S4
## Les plots sont evalues via un fichier temporaire pour eviter les fenetres graphiques.

set.seed(5)
obs <- rnorm(150)

# ── bandwidth ────────────────────────────────────────────────────────────────
test_that("show(bandwidth) s'execute sans erreur", {
  bw <- h.ucv(obs)
  expect_output(show(bw), regexp = "UCV")
})

test_that("plot(bandwidth) s'execute sans erreur", {
  bw <- h.ucv(obs)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  png(tmp)
  expect_no_error(plot(bw))
  dev.off()
})

test_that("plot(bandwidth) fonctionne pour BCV1, CCV, MCV, TCV, SCV", {
  methods_list <- list(
    h.bcv(obs, whichbcv = 1L),
    h.ccv(obs),
    h.mcv(obs),
    h.tcv(obs),
    h.scv(obs)
  )
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  for (bw in methods_list) {
    png(tmp); expect_no_error(plot(bw)); dev.off()
  }
})

test_that("lines(bandwidth) retourne un layer ggplot2", {
  bw1 <- h.ucv(obs)
  bw2 <- h.bcv(obs)
  lyr <- lines(bw2)
  expect_s3_class(lyr, "LayerInstance")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  png(tmp)
  p <- plot(bw1)
  expect_no_error(print(p$plot + lyr))
  dev.off()
})

# ── dkde ─────────────────────────────────────────────────────────────────────
test_that("show(dkde) s'execute sans erreur", {
  d <- dkde(data = obs)
  expect_output(show(d), regexp = "gaussian")
})

test_that("plot(dkde) s'execute sans erreur", {
  d   <- dkde(data = obs)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  png(tmp)
  expect_no_error(plot(d))
  dev.off()
})

# ── kfun ─────────────────────────────────────────────────────────────────────
test_that("show(kfun) s'execute sans erreur", {
  kf <- kernel.fun(kernel = "gaussian")
  expect_output(show(kf), regexp = "gaussian")
})

test_that("plot(kfun) s'execute sans erreur pour plusieurs noyaux", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  for (k in c("gaussian", "epanechnikov", "biweight")) {
    kf <- kernel.fun(kernel = k)
    png(tmp); expect_no_error(plot(kf)); dev.off()
  }
})

test_that("plot(kfun) avec derivee (zero.line automatique)", {
  kf  <- kernel.fun(kernel = "gaussian", deriv.order = 1L)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  png(tmp)
  expect_no_error(plot(kf))
  dev.off()
})

# ── kconv ────────────────────────────────────────────────────────────────────
test_that("show(kconv) s'execute sans erreur", {
  kc <- kernel.conv(kernel = "gaussian")
  expect_output(show(kc), regexp = "gaussian")
})

test_that("plot(kconv) s'execute sans erreur", {
  kc  <- kernel.conv(kernel = "gaussian")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  png(tmp)
  expect_no_error(plot(kc))
  dev.off()
})
