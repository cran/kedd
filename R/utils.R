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

## TRUE when K^(r) is not square integrable or K^(2r) vanishes for the power family:
## r < t + 1/2 (behaviour at |x| = 1), r < s + 1/2 unless s is an even integer
## (behaviour of |x|^s at 0), and 2r <= st for integer s, t (K is then a
## polynomial of degree st in |x|, as the named kernels)
.pow_bad <- function(kernel, r) {
  st   <- .pow_params(kernel)
  int  <- all(st == round(st))
  even <- st[1L] == round(st[1L]) && st[1L] %% 2 == 0
  (2 * r >= 2 * st[2L] + 1) || (!even && 2 * r >= 2 * st[1L] + 1) ||
    (int && 2 * r > st[1L] * st[2L])
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
                            method_name = "h.bw") {
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
  ## kernel-order compatibility for finite-support kernels
  bad <- (kernel == "uniform"      && (2L * r) >= 1L) ||
         (kernel == "triangular"   && (2L * r) >= 2L) ||
         (kernel == "epanechnikov" && (2L * r) >= 3L) ||
         (kernel == "biweight"     && (2L * r) >= 5L) ||
         (kernel == "triweight"    && (2L * r) >= 7L) ||
         (kernel == "tricube"      && (2L * r) >= 7L) ||
         (kernel == "cosine"       && r >= 2L) ||
         (kernel == "logistic"     && 2L * r + 4L > 20L) ||
         (startsWith(kernel, "pow(") && (2L * r + 4L > 16L || .pow_bad(kernel, r)))
  list(x = x, r = r, n = n, kernel = kernel,
       nthreads = nthreads, bad = bad)
}

## Oversmoothing bandwidth (Silverman rule)
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
