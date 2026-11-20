##' Asymptotic MISE (AMISE) bandwidth selector
##'
##' Reference bandwidth h_os: AMISE-optimal bandwidth with R(f^(r+2)) replaced by
##' the maximal smoothing value of Terrell (1990).
##'
##' @param x       numeric vector
##' @param deriv.order  derivative order r
##' @param kernel  kernel name (default "gaussian")
##' @return  S4 "bandwidth" object with method = "AMISE"
##' @export
h.amise <- function(x, deriv.order = 0L, kernel = "gaussian") {
  v <- .check_bw_args(x, deriv.order, kernel, nthreads = 0L, "AMISE")
  if (v$bad) {
    warning(v$msg)
    return(new("bandwidth", data = v$x, kernel = v$kernel,
               deriv.order = v$r, h = NA_real_, min.obj = NA_real_,
               hos = NA_real_, method = "AMISE"))
  }
  ## maximal smoothing bandwidth h_os (Terrell 1990), extended to order r
  sigma <- sd(v$x)
  n     <- v$n
  r     <- v$r
  ## the Silverman kernel is of order 4 (mu2 = 0): the second-order rule does not
  ## apply and the Gaussian value is returned
  if (v$kernel == "silverman")
    warning("the Silverman kernel is of order 4 (mu2 = 0); ",
            "the value of the Gaussian kernel is returned.")
  ker   <- if (v$kernel == "silverman") "gaussian" else v$kernel
  ## maximal smoothing bandwidth, normalised by n^(-1/(2r+5)) in Cpp_hos
  h_amise <- Cpp_hos(ker, r, n) * sigma
  new("bandwidth", data = v$x, kernel = v$kernel, deriv.order = v$r,
      h = h_amise, min.obj = NA_real_, hos = h_amise, method = "AMISE")
}
