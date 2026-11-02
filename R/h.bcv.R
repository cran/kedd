##' Biased cross-validation bandwidth selector (BCV1 and BCV2)
##'
##' @param x       numeric vector of observations
##' @param whichbcv  1 (BCV1, via K^{r+2} convolution) or 2 (BCV2, via K^{2r+4} derivative)
##' @param deriv.order  non-negative integer r
##' @param kernel  kernel name (default "gaussian")
##' @param lower,upper  search interval
##' @param tol     tolerance for optimize()
##' @param nthreads  number of OpenMP threads (0 = all available)
##' @return  an S4 object of class "bandwidth"
##' @export
h.bcv <- function(x, whichbcv = 1L, deriv.order = 0L, kernel = "gaussian",
                  lower, upper, tol,
                  nthreads = 0L) {
  whichbcv <- as.integer(whichbcv)
  if (!whichbcv %in% 1:2) {
    warning("'whichbcv' must be 1 or 2; using 1.")
    whichbcv <- 1L
  }
  v <- .check_bw_args(x, deriv.order, kernel, nthreads)
  method <- if (whichbcv == 1L) "BCV1" else "BCV2"
  if (v$bad) {
    warning("deriv.order is above the maximum order for this kernel; h = NA.")
    return(new("bandwidth", data = v$x, kernel = v$kernel,
               deriv.order = v$r, h = NA_real_, min.obj = NA_real_,
               hos = NA_real_, method = method))
  }
  hos <- .hos(v$kernel, v$r, v$x)
  if (missing(lower)) lower <- 0.01 * hos
  if (missing(upper)) upper <- 2.0  * hos
  iv <- .check_interval(lower, upper, hos)
  if (missing(tol))   tol   <- 0.01 * iv[1]
  cpp_fun <- if (whichbcv == 1L) Cpp_h_bcv_1 else Cpp_h_bcv_2
  obj <- optimize(function(h) cpp_fun(h, v$kernel, v$r, v$x, v$nthreads),
                  iv, tol = tol)
  if (obj$minimum < iv[1] + tol || obj$minimum > iv[2] - tol)
    warning("minimum occurred at one end of the range.")
  new("bandwidth", data = v$x, kernel = v$kernel, deriv.order = v$r,
      h = obj$minimum, min.obj = obj$objective, hos = hos, method = method)
}
