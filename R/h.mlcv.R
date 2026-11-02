## Negative leave-one-out log-likelihood at bandwidth h (minimised by h.mlcv)
.mlcv_obj <- function(h, kernel, x, nthreads = 0L) {
  n    <- length(x)
  mat  <- matrix(Cpp_kernel_deriv(kernel, (x - rep(x, each = n)) / h, 0L, nthreads), n, n)
  diag(mat) <- 0
  fhat <- rowSums(mat) / ((n - 1) * h)
  fhat[fhat <= 0] <- .Machine$double.eps
  -mean(log(fhat))
}

##' Maximum-likelihood cross-validation bandwidth selector (r = 0 only)
##'
##' Uses the log-likelihood leave-one-out cross-validation criterion:
##'   MLCV(h) = (1/n) * sum_i log( hat{f}_{-i}(X_i) )
##' where hat{f}_{-i} is the leave-one-out KDE.  Only for density estimation
##' (deriv.order = 0).
##'
##' @param x       numeric vector
##' @param kernel  kernel name (default "gaussian")
##' @param lower,upper  search interval
##' @param tol     tolerance for optimize()
##' @param nthreads  OpenMP threads (0 = all available)
##' @return  S4 "bandwidth" object with method = "MLCV"
##' @export
h.mlcv <- function(x, kernel = "gaussian",
                   lower, upper, tol,
                   nthreads = 0L) {
  v <- .check_bw_args(x, deriv.order = 0L, kernel, nthreads)
  hos <- .hos(v$kernel, 0L, v$x)
  if (missing(lower)) lower <- 0.01 * hos
  if (missing(upper)) upper <- 2.0  * hos
  iv <- .check_interval(lower, upper, hos)
  if (missing(tol))   tol   <- 0.01 * iv[1]
  obj <- optimize(.mlcv_obj, iv, tol = tol, kernel = v$kernel, x = v$x,
                  nthreads = v$nthreads)
  if (obj$minimum < iv[1] + tol || obj$minimum > iv[2] - tol)
    warning("minimum occurred at one end of the range.")
  new("bandwidth", data = v$x, kernel = v$kernel, deriv.order = 0L,
      h = obj$minimum, min.obj = obj$objective, hos = hos, method = "MLCV")
}
