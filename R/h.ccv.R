##' Complete cross-validation bandwidth selector
##'
##' @param x       numeric vector
##' @param deriv.order  derivative order r
##' @param kernel  kernel name (default "gaussian")
##' @param lower,upper  search interval
##' @param tol     tolerance
##' @param nthreads  OpenMP threads
##' @return  S4 "bandwidth" object
##' @export
h.ccv <- function(x, deriv.order = 0L, kernel = "gaussian",
                  lower, upper, tol,
                  nthreads = 0L) {
  v <- .check_bw_args(x, deriv.order, kernel, nthreads)
  ## CCV requires at least 4 more derivative orders: checks 2*(r+2) >= limit
  bad_ccv <- v$bad ||
    (v$kernel == "uniform"      && 2L * (v$r + 2L) >= 1L) ||
    (v$kernel == "triangular"   && 2L * (v$r + 2L) >= 2L) ||
    (v$kernel == "epanechnikov" && 2L * (v$r + 2L) >= 3L) ||
    (v$kernel == "biweight"     && 2L * (v$r + 2L) >= 5L) ||
    (v$kernel == "triweight"    && 2L * (v$r + 2L) >= 7L) ||
    (v$kernel == "tricube"      && 2L * (v$r + 2L) >= 7L) ||
    (v$kernel == "cosine"       && v$r + 2L >= 2L) ||
    (startsWith(v$kernel, "pow(") && .pow_bad(v$kernel, v$r + 2L))
  if (bad_ccv) {
    warning("deriv.order is above the maximum order of CCV for this kernel; h = NA.")
    return(new("bandwidth", data = v$x, kernel = v$kernel,
               deriv.order = v$r, h = NA_real_, min.obj = NA_real_,
               hos = NA_real_, method = "CCV"))
  }
  hos <- .hos(v$kernel, v$r, v$x)
  if (missing(lower)) lower <- 0.01 * hos
  if (missing(upper)) upper <- 2.0  * hos
  iv  <- .check_interval(lower, upper, hos)
  if (missing(tol))   tol   <- 0.01 * iv[1]
  obj <- optimize(function(h) Cpp_h_ccv(h, v$kernel, v$r, v$x, v$nthreads),
                  iv, tol = tol)
  if (obj$minimum < iv[1] + tol || obj$minimum > iv[2] - tol)
    warning("minimum occurred at one end of the range.")
  new("bandwidth", data = v$x, kernel = v$kernel, deriv.order = v$r,
      h = obj$minimum, min.obj = obj$objective, hos = hos, method = "CCV")
}
