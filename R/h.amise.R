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
  v <- .check_bw_args(x, deriv.order, kernel, nthreads = 0L)
  if (v$bad) {
    warning("deriv.order is above the maximum order for this kernel; h = NA.")
    return(new("bandwidth", data = v$x, kernel = v$kernel,
               deriv.order = v$r, h = NA_real_, min.obj = NA_real_,
               hos = NA_real_, method = "AMISE"))
  }
  ## maximal smoothing bandwidth h_os (Terrell 1990), extended to order r
  sigma <- sd(v$x)
  n     <- v$n
  r     <- v$r
  ker   <- if (v$kernel == "silverman") "gaussian" else v$kernel
  A3r   <- Cpp_hos(ker, r, 1L)  # gives (sigma_K^{(r)})^2 / R(K^{(r)}) in standardised units
  # AMISE-optimal bandwidth:  h* = hos * n^{-1/(2r+5)}  (normalised by n^{1/(2r+5)} in Cpp_hos)
  h_amise <- Cpp_hos(ker, r, n) * sigma
  new("bandwidth", data = v$x, kernel = v$kernel, deriv.order = v$r,
      h = h_amise, min.obj = NA_real_, hos = h_amise, method = "AMISE")
}
