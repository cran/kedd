## columns referenced inside ggplot2::aes()
utils::globalVariables(c("h", "cv", "xv", "y", "ylo", "yhi"))

.KER_NAMES <- c("gaussian", "epanechnikov", "triangular", "biweight", "uniform",
                "triweight", "tricube", "cosine", "silverman", "logistic")

##' Pow(s,t) kernel specification
##'
##' Builds the kernel name of the power family
##' K(x) = c_{s,t} (1 - |x|^s)^t 1{|x| <= 1}, usable wherever a kernel is expected.
##'
##' @param s  positive shape exponent
##' @param t  non-negative power
##' @return  character string "pow(s,t)"
##' @export
kernel.pow <- function(s = 2, t = 2) {
  if (!is.numeric(s) || length(s) != 1L || !is.finite(s) || s <= 0)
    stop("'s' must be a single positive number.")
  if (!is.numeric(t) || length(t) != 1L || !is.finite(t) || t < 0)
    stop("'t' must be a single non-negative number.")
  paste0("pow(", format(s, digits = 15), ",", format(t, digits = 15), ")")
}

## Canonical kernel name: partial matching for named kernels, "pow(s,t)" for the
## power family ("pow" alone means pow(2,2)).
.normalize_kernel <- function(kernel) {
  if (is.null(kernel)) return("gaussian")
  if (!is.character(kernel) || length(kernel) != 1L || is.na(kernel))
    stop("'kernel' must be a single character string.")
  k <- gsub("[[:space:]]", "", tolower(kernel))
  if (startsWith(k, "pow")) {
    if (k == "pow") return(kernel.pow())
    m <- regmatches(k, regexec("^pow\\(([^,()]+),([^,()]+)\\)$", k))[[1L]]
    if (length(m) != 3L)
      stop("Pow kernel must be written \"pow(s,t)\", e.g. \"pow(2,3)\".")
    st <- suppressWarnings(as.numeric(m[2:3]))
    return(kernel.pow(st[1L], st[2L]))
  }
  match.arg(k, .KER_NAMES)
}

## c(s, t) of a canonical "pow(s,t)" name
.pow_params <- function(kernel) {
  as.numeric(strsplit(sub("^pow\\((.*)\\)$", "\\1", kernel), ",")[[1L]])
}

## Order of regularity q of the kernel: for m <= q the pointwise derivative K^(m) is
## the derivative of K in the sense of distributions (K^(m-1) is absolutely
## continuous); for m > q it misses Dirac masses at the jumps of K^(q).
## Pow(s,t): (1 - |x|)^t at |x| = 1 gives q = ceiling(t + 1) - 1, and |x|^s at 0
## gives q = ceiling(s + 1) - 1 unless s is an even integer.
.kernel_q <- function(kernel) {
  if (startsWith(kernel, "pow(")) {
    st <- .pow_params(kernel)
    q1 <- ceiling(st[2L] + 1) - 1
    q0 <- if (st[1L] == round(st[1L]) && st[1L] %% 2 == 0) Inf else ceiling(st[1L] + 1) - 1
    return(min(q0, q1))
  }
  switch(kernel,
         uniform = 0,
         triangular = , epanechnikov = , cosine = 1,
         biweight = 2,
         triweight = , tricube = , silverman = 3,
         gaussian = , logistic = Inf)
}

## TRUE when K^(m) is square integrable: always for m <= q, except for Pow(s,t),
## whose derivatives behave like (1 - |x|)^(t - m) and |x|^(s - m)
.kernel_l2 <- function(kernel, m) {
  if (m > .kernel_q(kernel)) return(FALSE)
  if (!startsWith(kernel, "pow(")) return(TRUE)
  st   <- .pow_params(kernel)
  even <- st[1L] == round(st[1L]) && st[1L] %% 2 == 0
  (2 * m < 2 * st[2L] + 1) && (even || 2 * m < 2 * st[1L] + 1)
}

## Warning message of a selector when deriv.order r is not admissible
.bad_msg <- function(kernel, method) {
  if (kernel == "silverman" && method %in% c("BCV1", "BCV2", "MCV", "CCV"))
    paste0("the Silverman kernel is of order 4 (mu2 = 0): ", method,
           " is not defined for it; h = NA.")
  else
    paste0("deriv.order is above the maximum order of ", method,
           " for this kernel; h = NA.")
}

## TRUE when deriv.order r is admissible for the criterion `method` and the kernel:
## the criterion is then built on derivatives K^(m) taken pointwise with m <= q,
## and on a square-integrable K^(r) (K^(r+2) for BCV1). The caps come from the
## derivative orders implemented in C++, 20 (logistic) and 16 (Pow): r for dkde,
## 2r + 4 for the selectors.
.admissible <- function(kernel, r, method) {
  m <- switch(method,
              AMISE = , DKDE = r,
              UCV = , TCV = , SCV = 2L * r,
              MCV = 2L * r + 2L,
              CCV = , BCV2 = 2L * r + 4L,
              BCV1 = r + 2L,
              MLCV = 0L)
  ## BCV1, BCV2, MCV and CCV use the bias expansion of a second-order kernel
  ## (mu2(K), mu4(K)); the Silverman kernel is of order 4 (mu2 = 0)
  if (kernel == "silverman" && method %in% c("BCV1", "BCV2", "MCV", "CCV"))
    return(FALSE)
  l2  <- if (method == "BCV1") r + 2L else r
  top <- if (method == "DKDE") r else 2L * r + 4L
  cap <- (kernel == "logistic" && top > 20L) ||
         (startsWith(kernel, "pow(") && top > 16L)
  !cap && m <= .kernel_q(kernel) && .kernel_l2(kernel, l2)
}

## Largest admissible deriv.order of a criterion for a kernel
## (Inf when unbounded, NA when no order is admissible)
.max_order <- function(kernel, method) {
  kernel <- .normalize_kernel(kernel)
  ok <- vapply(0:25, function(r) .admissible(kernel, r, method), logical(1L))
  if (all(ok)) return(Inf)
  if (!ok[1L]) return(NA_integer_)
  which(!ok)[1L] - 2L
}

## OpenMP threads: 0 = all processors (capped by OMP_THREAD_LIMIT), k > 0 = k threads.
.check_nthreads <- function(nthreads) {
  if (!is.numeric(nthreads) || length(nthreads) != 1L || is.na(nthreads) ||
      nthreads < 0 || nthreads != round(nthreads)) {
    warning("'nthreads' must be a non-negative integer; using 0 (all cores).")
    return(0L)
  }
  as.integer(nthreads)
}

## Internal: validate common arguments shared by all bandwidth selectors.
.check_bw_args <- function(x, deriv.order, kernel, nthreads,
                            method_name = "UCV") {
  if (missing(kernel) || is.null(kernel)) kernel <- "gaussian"
  kernel <- .normalize_kernel(kernel)
  if (!is.numeric(x) || !is.null(dim(x)) || length(x) < 2L)
    stop("'x' must be a numeric vector with at least 2 observations.")
  if (length(deriv.order) != 1L || deriv.order < 0 ||
      deriv.order != round(deriv.order)) {
    warning("'deriv.order' must be a non-negative integer; using 0.")
    deriv.order <- 0L
  }
  nthreads <- .check_nthreads(nthreads)
  x <- x[!is.na(x)]
  r <- as.integer(deriv.order)
  n <- length(x)
  ## admissible derivative order of the criterion for this kernel
  bad <- !.admissible(kernel, r, method_name)
  msg <- if (bad) .bad_msg(kernel, method_name) else NA_character_
  list(x = x, r = r, n = n, kernel = kernel,
       nthreads = nthreads, bad = bad, msg = msg)
}

## Oversmoothing bandwidth (Silverman rule). The Silverman kernel is of order 4
## (mu2 = 0): the Gaussian value is used for the search interval.
.hos <- function(kernel, r, x) {
  ker <- if (kernel == "silverman") "gaussian" else kernel
  Cpp_hos(ker, r, length(x)) * sqrt(var(x))
}

## Validate and return [lower, upper] search interval
.check_interval <- function(lower, upper, hos) {
  if (!is.numeric(upper) || length(upper) != 1L) {
    warning("'upper' must be numeric; using 2 * hos.")
    upper <- 2 * hos
  }
  if (!is.numeric(lower) || length(lower) != 1L) {
    warning("'lower' must be numeric; using 0.01 * hos.")
    lower <- 0.01 * hos
  }
  if (lower <= 0 || lower >= upper) {
    warning("invalid interval [lower, upper]; using defaults.")
    lower <- 0.01 * hos
    upper <- 2   * hos
  }
  c(lower, upper)
}

## `$` access kept for code written for kedd 1.x, where results were lists:
## a name is first translated (1.x element -> 2.0 slot), then read as a slot;
## unknown names give NULL, as for a list.
.dollar <- function(obj, name, alias = character()) {
  if (name %in% names(alias)) name <- alias[[name]]
  if (name %in% methods::slotNames(obj)) methods::slot(obj, name) else NULL
}
