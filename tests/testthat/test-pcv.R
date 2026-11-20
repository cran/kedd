## Partitioned cross-validation (PCV)

set.seed(11)
xp <- rnorm(300)

test_that("h.pcv renvoie un objet bandwidth de methode PCV", {
  set.seed(1)
  bw <- h.pcv(xp, m = 3)
  expect_s4_class(bw, "bandwidth")
  expect_identical(bw@method, "PCV")
  expect_identical(bw@criterion, "UCV")
  expect_identical(length(bw@groups), length(xp))
  expect_identical(sort(unique(bw@groups)), 1:3)
  expect_true(all(abs(table(bw@groups) - 100) <= 1))
  expect_true(is.finite(bw@h) && bw@h > 0)
  expect_output(show(bw), "Criterion   : UCV")
  expect_output(show(bw), "Subsamples  : 3")
})

test_that("m = 1 donne le critere lui-meme", {
  for (cr in c("UCV", "TCV", "MCV", "CCV", "BCV1", "BCV2")) {
    a <- suppressWarnings(h.pcv(xp, m = 1, criterion = cr, deriv.order = 1))@h
    f <- switch(cr, UCV = h.ucv, TCV = h.tcv, MCV = h.mcv, CCV = h.ccv,
                BCV1 = function(x, ...) h.bcv(x, whichbcv = 1L, ...),
                BCV2 = function(x, ...) h.bcv(x, whichbcv = 2L, ...))
    b <- suppressWarnings(f(xp, deriv.order = 1))@h
    expect_equal(a, b, tolerance = 1e-3, label = cr)
  }
})

test_that("le critere PCV est la moyenne des criteres des groupes", {
  set.seed(2)
  bw <- h.pcv(xp, m = 4, deriv.order = 1)
  h <- c(0.3, 0.5, 0.8)
  direct <- sapply(h, function(hh) mean(sapply(1:4, function(k)
    as.numeric(Cpp_h_ucv(hh * 4^(1 / (2 + 5)), "gaussian", 1L, xp[bw@groups == k], 1L)))))
  expect_equal(kedd:::.cv_curve(bw, h), direct, tolerance = 1e-10)
  ## minimum of the criterion at the selected bandwidth
  hg <- seq(0.5 * bw@h, 2 * bw@h, length.out = 401)
  expect_equal(hg[which.min(kedd:::.cv_curve(bw, hg))], bw@h, tolerance = 0.01)
  expect_equal(kedd:::.cv_curve(bw, bw@h), bw@min.obj, tolerance = 1e-8)
})

test_that("mise a l'echelle : exposant 1/(2r+5), et 1/(2r+9) pour Silverman", {
  expect_equal(kedd:::.pcv_exponent("gaussian", 0L), 1 / 5)
  expect_equal(kedd:::.pcv_exponent("pow(2,4)", 2L), 1 / 9)
  expect_equal(kedd:::.pcv_exponent("silverman", 0L), 1 / 9)
  expect_equal(kedd:::.pcv_exponent("silverman", 1L), 1 / 11)
})

test_that("partition reproductible avec set.seed", {
  set.seed(5); b1 <- h.pcv(xp, m = 3, nthreads = 1)
  set.seed(5); b2 <- h.pcv(xp, m = 3, nthreads = 1)
  expect_identical(b1@groups, b2@groups)
  expect_identical(b1@h, b2@h)
})

test_that("limites des ordres : celles du critere de chaque groupe", {
  expect_warning(bw <- h.pcv(xp, m = 2, deriv.order = 2, kernel = "biweight"),
                 "maximum order of UCV")
  expect_true(is.na(bw@h))
  expect_warning(bw <- h.pcv(xp, m = 2, criterion = "CCV", kernel = "triweight"), "CCV")
  expect_true(is.na(bw@h))
  expect_warning(bw <- h.pcv(xp, m = 2, criterion = "MCV", kernel = "silverman"), "order 4")
  expect_true(is.na(bw@h))
  expect_true(is.finite(h.pcv(xp, m = 2, deriv.order = 1, kernel = "silverman")@h))
  expect_true(is.finite(h.pcv(xp, m = 2, deriv.order = 2, kernel = "pow(2,4)")@h))
})

test_that("nrep : moyenne du critere sur plusieurs partitions", {
  set.seed(4)
  bw <- h.pcv(xp, m = 3, nrep = 4, deriv.order = 1)
  expect_identical(length(bw@groups), 4L * length(xp))
  expect_output(show(bw), "4 random partitions")
  ## criterion = mean of the criteria of the four partitions taken separately
  h <- c(0.4, 0.7)
  sep <- sapply(1:4, function(i) {
    one <- bw
    one@groups <- bw@groups[(i - 1) * length(xp) + seq_along(xp)]
    kedd:::.cv_curve(one, h)
  })
  expect_equal(kedd:::.cv_curve(bw, h), rowMeans(sep), tolerance = 1e-10)
  ## nrep = 1 is the partition of a single call
  set.seed(9); a <- h.pcv(xp, m = 3, nthreads = 1)
  set.seed(9); b <- h.pcv(xp, m = 3, nrep = 1, nthreads = 1)
  expect_identical(a@groups, b@groups)
  expect_identical(a@h, b@h)
  expect_error(h.pcv(xp, nrep = 0), "'nrep'")
  expect_error(h.pcv(xp, nrep = 1.5), "'nrep'")
})

test_that("arguments invalides", {
  expect_error(h.pcv(xp, m = 0), "'m'")
  expect_error(h.pcv(xp, m = 2.5), "'m'")
  expect_error(h.pcv(xp, m = 200), "too large")
  expect_error(h.pcv(xp, criterion = "SCV"))
  expect_identical(h.pcv(xp, m = 2, criterion = "tcv")@criterion, "TCV")
})

test_that("plot() et lines() tracent le critere PCV", {
  set.seed(3)
  bw <- h.pcv(xp, m = 2)
  pdf(NULL)
  on.exit(grDevices::dev.off())
  p <- plot(bw)
  expect_type(p, "list")
  expect_s3_class(p$plot, "ggplot")
  expect_equal(p$cv, kedd:::.cv_curve(bw, p$h))
  expect_s3_class(lines(bw), "LayerInstance")
})
