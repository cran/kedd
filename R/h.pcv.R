##' Partitioned cross-validation bandwidth selector
##'
##' Marron (1987): the indices 1, ..., n are partitioned at random into m subsamples;
##' a cross-validation score is computed in each subsample, the bandwidth minimises
##' their average and is rescaled from the subsample size n/m to the sample size n.
##' With nrep > 1 the score is also averaged over nrep random partitions (extension).
##'
##' @param x          numeric vector of observations
##' @param m          number of subsamples (integer >= 1)
##' @param criterion  score computed in each subsample: "UCV", "TCV", "MCV", "CCV",
##'                   "BCV1" or "BCV2"
##' @param nrep       number of random partitions (integer >= 1; 1 = Marron 1987)
##' @param deriv.order  non-negative integer r
##' @param kernel     kernel name (default "gaussian")
##' @param lower,upper  search interval for h (defaults 0.01*h_os, 2*h_os)
##' @param tol        tolerance for optimize()
##' @param nthreads   number of OpenMP threads (0 = all available)
##' @return  an S4 object of class "bandwidth" with method "PCV"
##' @export
h.pcv <- function(x, m = 2L, criterion = "UCV", nrep = 1L, deriv.order = 0L,
                  kernel = "gaussian", lower, upper, tol, nthreads = 0L) {
  crits <- c("UCV", "TCV", "MCV", "CCV", "BCV1", "BCV2")
  if (!is.character(criterion) || length(criterion) != 1L)
    stop("'criterion' must be one of ", paste(dQuote(crits, FALSE), collapse = ", "), ".")
  criterion <- match.arg(toupper(criterion), crits)
  v <- .check_bw_args(x, deriv.order, kernel, nthreads, criterion)
  if (!is.numeric(m) || length(m) != 1L || is.na(m) || m < 1 || m != round(m))
    stop("'m' must be a positive integer.")
  if (!is.numeric(nrep) || length(nrep) != 1L || is.na(nrep) || nrep < 1 ||
      nrep != round(nrep))
    stop("'nrep' must be a positive integer.")
  m    <- as.integer(m)
  nrep <- as.integer(nrep)
  if (v$n %/% m < 2L)
    stop("'m' is too large: each subsample must contain at least 2 observations.")
  if (v$bad) {
    warning(v$msg)
    return(new("bandwidth", data = v$x, kernel = v$kernel,
               deriv.order = v$r, h = NA_real_, min.obj = NA_real_,
               hos = NA_real_, method = "PCV", criterion = criterion,
               groups = integer(0)))
  }
  ## nrep random partitions of the indices into m subsamples whose sizes differ by
  ## at most one, independent of the data, stored one after the other
  groups <- as.integer(unlist(lapply(seq_len(nrep), function(i)
    sample(rep_len(seq_len(m), v$n)))))
  hos <- .hos(v$kernel, v$r, v$x)
  if (missing(lower)) lower <- 0.01 * hos
  if (missing(upper)) upper <- 2.0  * hos
  iv  <- .check_interval(lower, upper, hos)
  if (missing(tol))   tol   <- 0.01 * iv[1]
  ## average score minimised with optimize(), as for the other selectors
  obj <- optimize(function(h) .pcv_obj(h, criterion, v$kernel, v$r, v$x, groups,
                                       v$nthreads),
                  iv, tol = tol)
  if (obj$minimum < iv[1] + tol || obj$minimum > iv[2] - tol)
    warning("minimum occurred at one end of the range.")
  new("bandwidth", data = v$x, kernel = v$kernel, deriv.order = v$r,
      h = obj$minimum, min.obj = obj$objective, hos = hos, method = "PCV",
      criterion = criterion, groups = groups)
}

## Rescaling exponent of PCV: the optimal bandwidth is of order n^(-1/(2r+5)) for a
## second-order kernel and n^(-1/(2r+9)) for the fourth-order Silverman kernel; for
## r = 0 and a second-order kernel this is the rescaling m^(-1/5) of Marron (1987)
.pcv_exponent <- function(kernel, r) {
  if (kernel == "silverman") 1 / (2 * r + 9) else 1 / (2 * r + 5)
}

## PCV score at the bandwidths h (a vector) of the whole sample: average, over the
## partitions and their subsamples, of the score computed in each subsample with the
## bandwidths h * m^a suited to its size n/m; minimising it in h is the same as
## minimising the average score in the subsample bandwidth and rescaling it by
## m^(-a). `groups` holds the partitions one after the other, and each subsample is
## evaluated at all the bandwidths in one C++ call
.pcv_obj <- function(h, criterion, kernel, r, x, groups, nthreads = 0L) {
  n    <- length(x)
  nrep <- length(groups) %/% n
  m    <- max(groups)
  hm   <- h * m^.pcv_exponent(kernel, r)
  tot  <- numeric(length(h))
  for (i in seq_len(nrep)) {
    gi <- groups[(i - 1L) * n + seq_len(n)]
    for (k in seq_len(m))
      tot <- tot + as.numeric(Cpp_plot_cv(criterion, hm, kernel, r, x[gi == k],
                                          nthreads))
  }
  tot / (nrep * m)
}
