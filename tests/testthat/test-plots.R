## Tests de toutes les methodes graphiques : plot() et lines() pour chaque
## classe, chaque methode de selection et chaque noyau, contenu verifie.

set.seed(99)
xp <- c(rnorm(90, -1, 0.5), rnorm(60, 1.3, 0.7))

## plot() imprime la figure : on l'envoie sur un peripherique nul
quiet_plot <- function(...) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  plot(...)
}
geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], "")
title_of <- function(p) {
  if (exists("get_labs", asNamespace("ggplot2"))) ggplot2::get_labs(p)$title else p$labels$title
}
renders <- function(p) {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  print(p)
  TRUE
}

selectors <- list(
  UCV  = function() h.ucv(xp),
  BCV1 = function() h.bcv(xp),
  BCV2 = function() h.bcv(xp, whichbcv = 2),
  CCV  = function() h.ccv(xp),
  MCV  = function() h.mcv(xp),
  TCV  = function() h.tcv(xp),
  SCV  = function() h.scv(xp),
  MLCV = function() h.mlcv(xp)
)
bws <- lapply(selectors, function(f) suppressWarnings(f()))
## each method is drawn once and reused by the tests below
bw_res <- lapply(bws, quiet_plot)

# ── plot(<bandwidth>) ─────────────────────────────────────────────────────────
test_that("plot(bandwidth) pour chaque methode : courbe = critere, h marque", {
  for (m in names(bws)) {
    bw  <- bws[[m]]
    res <- bw_res[[m]]
    expect_named(res, c("h", "cv", "plot"))
    expect_length(res$h, 100L)
    expect_equal(range(res$h), c(0.4, 2) * bw@h)
    ## valeurs tracees = critere recalcule independamment
    ref <- if (m == "MLCV")
      vapply(res$h, kedd:::.mlcv_obj, 0, kernel = bw@kernel, x = bw@data)
    else as.numeric(Cpp_plot_cv(m, res$h, bw@kernel, bw@deriv.order, bw@data))
    expect_equal(res$cv, ref, tolerance = 1e-10, label = m)
    p <- res$plot
    expect_s3_class(p, "ggplot")
    g <- geoms(p)
    expect_true(all(c("GeomRibbon", "GeomLine", "GeomPoint", "GeomVline", "GeomText") %in% g),
                label = m)
    vl <- ggplot2::layer_data(p, which(g == "GeomVline")[1])
    expect_equal(vl$xintercept, bw@h, label = m)
    expect_equal(ggplot2::layer_data(p, which(g == "GeomLine")[1])$y, res$cv, label = m)
    expect_match(title_of(p), "kernel", fixed = TRUE)
    if (m == "UCV") expect_true(renders(p))
  }
})

test_that("plot(bandwidth) : titres des methodes", {
  lab <- c(UCV = "Unbiased", BCV1 = "BCV1", BCV2 = "BCV2", CCV = "Complete",
           MCV = "Modified", TCV = "Trimmed", SCV = "Smoothed", MLCV = "Maximum-Likelihood")
  for (m in names(lab))
    expect_match(title_of(bw_res[[m]]$plot), lab[[m]], fixed = TRUE)
})

test_that("plot(bandwidth) : options type, show.min, show.hos, log, grille, titres, theme", {
  bw <- bws$UCV
  p_l <- quiet_plot(bw, type = "l", show.min = FALSE)$plot
  expect_false("GeomPoint" %in% geoms(p_l))
  expect_false("GeomText" %in% geoms(p_l))
  p_p <- quiet_plot(bw, type = "p")$plot
  expect_false("GeomLine" %in% geoms(p_p))
  expect_true("GeomPoint" %in% geoms(p_p))
  p_h <- quiet_plot(bw, show.hos = TRUE)$plot
  vls <- unlist(lapply(which(geoms(p_h) == "GeomVline"),
                       function(i) ggplot2::layer_data(p_h, i)$xintercept))
  expect_setequal(vls, c(bw@h, bw@hos))
  res <- quiet_plot(bw, log_scale_x = TRUE, type = "l")
  ln  <- ggplot2::layer_data(res$plot, which(geoms(res$plot) == "GeomLine")[1])
  expect_equal(ln$x, log10(res$h))
  hg  <- seq(0.1, 0.9, length.out = 37)
  expect_equal(quiet_plot(bw, h = hg)$h, hg)
  expect_length(quiet_plot(bw, n.points = 50L)$cv, 50L)
  pt <- quiet_plot(bw, main.title = "Titre", x.title = "hh", y.title = "yy",
                   theme = ggplot2::theme_bw())$plot
  expect_identical(title_of(pt), "Titre")
  expect_true(renders(pt))
  ## derivee : titre mentionne l'ordre
  expect_match(title_of(quiet_plot(h.ucv(xp, deriv.order = 1L))$plot), "r = 1", fixed = TRUE)
})

test_that("plot(bandwidth) : NA -> avertissement, AMISE -> erreur explicite", {
  bwNA <- suppressWarnings(h.ucv(xp, deriv.order = 1L, kernel = "uniform"))
  expect_warning(expect_null(quiet_plot(bwNA)), "NA")
  expect_error(quiet_plot(h.amise(xp)), "closed-form")
})

# ── lines(<bandwidth>) ────────────────────────────────────────────────────────
test_that("lines(bandwidth) pour chaque methode : couche ggplot2 = critere", {
  for (m in names(bws)) {
    bw  <- bws[[m]]
    lyr <- lines(bw, n.points = 40L, color = "red", linetype = "dashed", linewidth = 0.5)
    expect_s3_class(lyr, "LayerInstance")
    expect_s3_class(lyr$geom, "GeomLine")
    expect_equal(nrow(lyr$data), 40L)
    ref <- kedd:::.cv_curve(bw, lyr$data$h)
    expect_equal(lyr$data$cv, ref, tolerance = 1e-10, label = m)
    expect_identical(lyr$aes_params$colour, "red")
    expect_identical(lyr$aes_params$linetype, "dashed")
  }
  p <- quiet_plot(bws$UCV)$plot + lines(bws$BCV1) + lines(bws$MCV)
  expect_equal(sum(geoms(p) == "GeomLine"), 3L)
  expect_true(renders(p))
  bwNA <- suppressWarnings(h.ucv(xp, deriv.order = 1L, kernel = "uniform"))
  expect_null(lines(bwNA))
  expect_error(lines(h.amise(xp)), "closed-form")
})

# ── plot(<dkde>) ──────────────────────────────────────────────────────────────
test_that("plot(dkde) : couches selon band, rug, zero.line, type ; donnees = estimation", {
  d0 <- dkde(data = xp, h = 0.4)
  p  <- quiet_plot(d0)
  expect_s3_class(p, "ggplot")
  g <- geoms(p)
  expect_true(all(c("GeomRibbon", "GeomLine", "GeomRug") %in% g))
  expect_false("GeomHline" %in% g)                        # r = 0 : pas de ligne zero
  expect_equal(ggplot2::layer_data(p, which(g == "GeomLine"))$y, d0@y)
  expect_match(title_of(p), "Kernel Density Estimate", fixed = TRUE)
  g2 <- geoms(quiet_plot(d0, band = FALSE, rug = FALSE, type = "p"))
  expect_false(any(c("GeomRibbon", "GeomRug", "GeomLine") %in% g2))
  expect_true("GeomPoint" %in% g2)
  expect_true(all(c("GeomLine", "GeomPoint") %in% geoms(quiet_plot(d0, type = "o"))))
  d1 <- dkde(data = xp, h = 0.6, deriv.order = 1L)
  p1 <- quiet_plot(d1)
  expect_true("GeomHline" %in% geoms(p1))
  expect_match(title_of(p1), "r = 1", fixed = TRUE)
  expect_false("GeomHline" %in% geoms(quiet_plot(d1, zero.line = FALSE)))
  pt <- quiet_plot(d1, main.title = "T", x.title = "x", y.title = "y",
                   colors = list(line = "black", fill = "grey80", rug = "red", zero = "blue"),
                   theme = ggplot2::theme_classic())
  expect_identical(title_of(pt), "T")
  expect_true(renders(pt))
  for (k in c("epanechnikov", "logistic", "pow(2,3)"))
    expect_s3_class(quiet_plot(dkde(data = xp, h = 0.5, kernel = k)), "ggplot")
})

# ── plot(<kfun>) et plot(<kconv>) pour tous les noyaux ────────────────────────
all_kernels <- c("gaussian", "epanechnikov", "uniform", "triangular", "biweight",
                 "triweight", "tricube", "cosine", "silverman", "logistic",
                 "pow(2,2)", "pow(1.5,2.5)")
## every kernel at r = 0; a representative subset at r = 1 (each plot() renders)
kernel_cases <- rbind(data.frame(k = all_kernels, r = 0L),
                      data.frame(k = c("gaussian", "biweight", "logistic", "pow(1.5,2.5)"), r = 1L))

test_that("plot(kfun) pour chaque noyau (r = 0) et pour r = 1", {
  for (i in seq_len(nrow(kernel_cases))) {
    k <- kernel_cases$k[i]; r <- kernel_cases$r[i]
    kf <- kernel.fun(kernel = k, deriv.order = r, n.grid = 101L)
    p  <- quiet_plot(kf)
    g  <- geoms(p)
    expect_s3_class(p, "ggplot")
    expect_equal(ggplot2::layer_data(p, which(g == "GeomLine"))$y, kf@y,
                 label = paste(k, r))
    expect_identical("GeomHline" %in% g, r > 0L, label = paste(k, r))
    expect_match(title_of(p), if (r == 0L) "Kernel Function" else "r = 1", fixed = TRUE)
    if (k == "gaussian") expect_true(renders(p), label = paste(k, r))
  }
})

test_that("plot(kconv) pour chaque noyau (r = 0) et pour r = 1", {
  for (i in seq_len(nrow(kernel_cases))) {
    k <- kernel_cases$k[i]; r <- kernel_cases$r[i]
    kc <- kernel.conv(kernel = k, deriv.order = r, n.grid = 101L)
    p  <- quiet_plot(kc)
    g  <- geoms(p)
    expect_s3_class(p, "ggplot")
    expect_equal(ggplot2::layer_data(p, which(g == "GeomLine"))$y, kc@y,
                 label = paste(k, r))
    expect_identical("GeomHline" %in% g, r > 0L, label = paste(k, r))
    expect_match(title_of(p), if (r == 0L) "Kernel Convolution" else "r = 1", fixed = TRUE)
    if (k == "gaussian") expect_true(renders(p), label = paste(k, r))
  }
})

test_that("plot(kfun/kconv) : type p et o, titres et couleurs personnalises", {
  kf <- kernel.fun(kernel = "biweight", n.grid = 51L)
  expect_false("GeomLine" %in% geoms(quiet_plot(kf, type = "p")))
  expect_true(all(c("GeomLine", "GeomPoint") %in% geoms(quiet_plot(kf, type = "o"))))
  kc <- kernel.conv(kernel = "biweight", n.grid = 51L)
  expect_false("GeomLine" %in% geoms(quiet_plot(kc, type = "p")))
  pt <- quiet_plot(kc, main.title = "C", x.title = "u", y.title = "v",
                   colors = list(line = "black", fill = "grey90", zero = "red"),
                   zero.line = TRUE, theme = ggplot2::theme_bw())
  expect_identical(title_of(pt), "C")
  expect_true("GeomHline" %in% geoms(pt))
})

# ── show() pour toutes les classes ────────────────────────────────────────────
test_that("show() pour chaque classe et chaque methode", {
  for (m in names(bws)) expect_output(show(bws[[m]]), m, fixed = TRUE)
  expect_output(show(h.amise(xp)), "AMISE")
  expect_output(show(suppressWarnings(h.ucv(xp, deriv.order = 1L, kernel = "uniform"))), "NA")
  expect_output(show(dkde(data = xp, h = 0.4)), "density")
  expect_output(show(kernel.fun(kernel = "pow(2,3)")), "pow(2,3)", fixed = TRUE)
  expect_output(show(kernel.conv(kernel = "logistic", deriv.order = 2L)), "K^{(2)}", fixed = TRUE)
})
