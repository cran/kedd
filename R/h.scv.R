##' Smoothed cross-validation bandwidth selector
##'
##' @description
##' Selects an optimal bandwidth for kernel density (or derivative) estimation
##' by minimising the Smoothed Cross-Validation (SCV) criterion of
##' Hall, Marron & Park (1992), extended to the r-th derivative through the
##' (2r)-th derivatives of the normal convolutions. Gaussian kernel only.
##'
##' @param x        numeric vector of observations
##' @param g        pilot bandwidth (positive numeric). If \code{NULL} (default),
##'                 the data-driven pilot of Hall et al. (1992, eq. 4.2) is used:
##'                 \eqn{g = \hat{\sigma} \cdot 0.9266 \cdot n^{-2/13}}.
##' @param deriv.order  non-negative integer \eqn{r} (derivative order, default 0)
##' @param kernel   character; must be \code{"gaussian"} (the only kernel
##'                 for which SCV has a closed-form analytical expression)
##' @param lower,upper  search interval for \code{optimize()} (default
##'                 \code{0.01 * h_os} and \code{2 * h_os})
##' @param tol      tolerance passed to \code{optimize()} (default \code{0.01 * lower})
##' @param nthreads number of OpenMP threads (\code{0} = all available)
##' @return  an S4 object of class \code{\linkS4class{bandwidth}}
##' @references
##'   Hall, P., Marron, J. S. and Park, B. U. (1992).
##'   Smoothed cross-validation. \emph{Probability Theory and Related Fields},
##'   \bold{92}, 1--20.
##' @seealso \code{\link{h.ucv}}, \code{\link{h.bcv}}, \code{\linkS4class{bandwidth}}
##' @export
h.scv <- function(x, g = NULL, deriv.order = 0L, kernel = "gaussian",
                  lower, upper, tol, nthreads = 0L) {
  v <- .check_bw_args(x, deriv.order, kernel, nthreads)
  if (v$bad) {
    warning("deriv.order is above the maximum order for this kernel; h = NA.")
    return(new("bandwidth", data = v$x, kernel = v$kernel,
               deriv.order = v$r, h = NA_real_, min.obj = NA_real_,
               hos = NA_real_, method = "SCV"))
  }
  if (v$kernel != "gaussian")
    stop("h.scv currently supports only kernel = \"gaussian\".")
  n   <- length(v$x)
  hos <- .hos(v$kernel, v$r, v$x)
  if (missing(lower)) lower <- 0.01 * hos
  if (missing(upper)) upper <- 2.0  * hos
  iv  <- .check_interval(lower, upper, hos)
  if (missing(tol))   tol   <- 0.01 * iv[1]
  if (is.null(g))
    g <- sd(v$x) * 0.9266 * n^(-2/13)
  g <- as.double(g)
  if (!is.finite(g) || g <= 0)
    stop("pilot bandwidth g must be a positive finite number.")
  obj <- optimize(
    function(h) Cpp_h_scv(h, g, v$kernel, v$r, v$x, v$nthreads),
    iv, tol = tol
  )
  if (obj$minimum < iv[1] + tol || obj$minimum > iv[2] - tol)
    warning("minimum occurred at one end of the range.")
  new("bandwidth", data = v$x, kernel = v$kernel, deriv.order = v$r,
      h = obj$minimum, min.obj = obj$objective, hos = hos, method = "SCV")
}
