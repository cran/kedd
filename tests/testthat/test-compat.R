## Compatibilite avec le code ecrit pour kedd 1.x (resultats sous forme de listes),
## utilise par les packages dependants (ROCModels : h.ucv(x)$h ;
## baggingbwsel : kedd::kernel.fun(u, 2)$kx).

set.seed(2027)
xc <- rnorm(120)

test_that("bandwidth : $h, $kernel, $deriv.order, $x, $n, $min.ucv", {
  bw <- h.ucv(xc)
  expect_identical(bw$h, bw@h)
  expect_identical(bw$kernel, "gaussian")
  expect_identical(bw$deriv.order, 0L)
  expect_identical(bw$x, bw@data)
  expect_identical(bw$n, length(xc))
  expect_identical(bw$min.ucv, bw@min.obj)
  expect_identical(bw$hos, bw@hos)
  expect_null(bw$inconnu)
  expect_identical(h.bcv(xc, deriv.order = 1L)$h, h.bcv(xc, deriv.order = 1L)@h)
})

test_that("kfun / kconv : $kx (appel positionnel de kedd 1.x)", {
  u <- seq(-2, 2, by = 0.5)
  kf <- kedd::kernel.fun(u, 2)
  expect_identical(kf$kx, kf@y)
  expect_equal(kf$kx, (u^2 - 1) * dnorm(u))
  expect_identical(kf$x, u)
  kc <- kernel.conv(u, 1)
  expect_identical(kc$kx, kc@y)
  expect_null(kc$inconnu)
})

test_that("dkde : $est.fx, $eval.points, $h", {
  d <- dkde(data = xc, h = 0.5)
  expect_identical(d$est.fx, d@y)
  expect_identical(d$eval.points, d@x)
  expect_identical(d$h, 0.5)
  expect_identical(d$n, 120L)
})
