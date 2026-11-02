## S4 classes: kfun, kconv
## Returned by kernel.fun() and kernel.conv()

## ---------- kfun --------------------------------------------------------

setClass("kfun",
  representation(
    x           = "numeric",   # evaluation grid
    y           = "numeric",   # K^{(r)}(x) values
    kernel      = "character",
    deriv.order = "integer",
    call        = "call"
  )
)

setValidity("kfun", function(object) {
  if (length(object@kernel) != 1L)
    return("'kernel' must be a single character string.")
  if (length(object@deriv.order) != 1L || object@deriv.order < 0L)
    return("'deriv.order' must be a non-negative integer.")
  if (length(object@x) != length(object@y))
    return("'x' and 'y' must have the same length.")
  TRUE
})

setMethod("show", "kfun", function(object) {
  r <- object@deriv.order
  label <- if (r == 0L) "Kernel function" else
           paste0("Kernel derivative of order ", r)
  cat("\n", label, "\n", sep = "")
  cat("Kernel :", object@kernel, "\n")
  cat("Range  : [", round(min(object@x), 4), ",",
      round(max(object@x), 4), "]\n", sep = " ")
  cat("Points :", length(object@x), "\n")
  invisible(object)
})

## Derivative orders above these limits are returned as zero by the C++ code
.warn_max_order <- function(kernel, r) {
  if (kernel == "logistic" && r > 20L)
    warning("logistic kernel derivatives are implemented up to order 20; returning zero.")
  if (startsWith(kernel, "pow(") && r > 16L)
    warning("pow(s,t) kernel derivatives are implemented up to order 16; returning zero.")
}

##' Evaluate kernel function and its derivatives
##'
##' @param x          numeric vector of evaluation points; if NULL uses a grid
##' @param deriv.order  derivative order r (0 = kernel itself)
##' @param kernel     kernel name (default "gaussian")
##' @param n.grid     number of grid points when x is NULL
##' @return  an object of class \code{"kfun"}
##' @export
kernel.fun <- function(x = NULL, deriv.order = 0L, kernel = "gaussian",
                       n.grid = 512L) {
  kernel <- .normalize_kernel(kernel)
  r <- as.integer(deriv.order)
  if (r < 0L) stop("'deriv.order' must be a non-negative integer.")
  .warn_max_order(kernel, r)
  if (is.null(x)) {
    xrange <- if (kernel %in% c("logistic", "gaussian", "silverman")) 8 else 1.1
    x <- seq(-xrange, xrange, length.out = as.integer(n.grid))
  }
  y <- as.numeric(Cpp_kernel_deriv(kernel, as.numeric(x), r))
  new("kfun", x = as.numeric(x), y = y,
      kernel = kernel, deriv.order = r,
      call = match.call())
}

setMethod("plot", "kfun",
  function(x,
           type        = "l",
           zero.line   = (x@deriv.order > 0L),
           main.title, x.title, y.title,
           colors      = list(line = "#2166AC", fill = "#DEEBF7",
                              zero = "#999999"),
           title.size  = 11, title.face = "bold",
           axis.size   = 10,
           theme       = ggplot2::theme_minimal(),
           ...) {

    kernel_label <- tools::toTitleCase(x@kernel)
    r <- x@deriv.order

    if (missing(y.title))
      y.title <- if (r == 0L) expression(K(x))
                 else         bquote(K^{(.(r))}(x))

    if (missing(x.title)) x.title <- "x"

    if (missing(main.title))
      main.title <- paste0(
        if (r == 0L) "Kernel Function" else paste0("Kernel Derivative  (r = ", r, ")"),
        "\n", kernel_label, " kernel"
      )

    df <- data.frame(xv = x@x, y = x@y)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = xv, y = y))

    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pmin(y, 0), ymax = pmax(y, 0)),
      fill = colors$fill, alpha = 0.40
    )

    if (zero.line)
      p <- p + ggplot2::geom_hline(
        yintercept = 0, linetype = "dashed",
        color = colors$zero, linewidth = 0.5
      )

    if (type %in% c("l", "o"))
      p <- p + ggplot2::geom_line(color = colors$line, linewidth = 0.9)
    if (type %in% c("p", "o"))
      p <- p + ggplot2::geom_point(color = colors$line, size = 0.7)

    p <- p +
      ggplot2::ggtitle(main.title) +
      ggplot2::xlab(x.title) +
      ggplot2::ylab(y.title) +
      theme +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(size = title.size, face = title.face,
                                             hjust = 0.5),
        axis.title   = ggplot2::element_text(size = axis.size),
        panel.border = ggplot2::element_rect(colour = "grey40", fill = NA,
                                             linewidth = 0.5),
        plot.margin  = ggplot2::margin(8, 12, 6, 8)
      )

    print(p)
    invisible(p)
  }
)

## ---------- kconv -------------------------------------------------------

setClass("kconv",
  representation(
    x           = "numeric",   # evaluation grid
    y           = "numeric",   # (K^{(r)} * K^{(r)})(x) values
    kernel      = "character",
    deriv.order = "integer",
    call        = "call"
  )
)

setValidity("kconv", function(object) {
  if (length(object@kernel) != 1L)
    return("'kernel' must be a single character string.")
  if (length(object@deriv.order) != 1L || object@deriv.order < 0L)
    return("'deriv.order' must be a non-negative integer.")
  if (length(object@x) != length(object@y))
    return("'x' and 'y' must have the same length.")
  TRUE
})

setMethod("show", "kconv", function(object) {
  r <- object@deriv.order
  cat("\nKernel convolution K^{(", r, ")} * K^{(", r, ")}\n", sep = "")
  cat("Kernel :", object@kernel, "\n")
  cat("Range  : [", round(min(object@x), 4), ",",
      round(max(object@x), 4), "]\n", sep = " ")
  cat("Points :", length(object@x), "\n")
  invisible(object)
})

##' Evaluate kernel convolution K^{(r)} * K^{(r)}
##'
##' @param x          numeric vector; if NULL uses a grid
##' @param deriv.order  derivative order r
##' @param kernel     kernel name (default "gaussian")
##' @param n.grid     grid size when x is NULL
##' @return  an object of class \code{"kconv"}
##' @export
kernel.conv <- function(x = NULL, deriv.order = 0L, kernel = "gaussian",
                        n.grid = 512L) {
  kernel <- .normalize_kernel(kernel)
  r <- as.integer(deriv.order)
  if (r < 0L) stop("'deriv.order' must be a non-negative integer.")
  .warn_max_order(kernel, r)
  if (is.null(x)) {
    xrange <- if (kernel %in% c("logistic", "gaussian", "silverman")) 8 else 2.1
    x <- seq(-xrange, xrange, length.out = as.integer(n.grid))
  }
  y <- as.numeric(Cpp_kernel_conv(kernel, as.numeric(x), r))
  new("kconv", x = as.numeric(x), y = y,
      kernel = kernel, deriv.order = r,
      call = match.call())
}

setMethod("plot", "kconv",
  function(x,
           type        = "l",
           zero.line   = (x@deriv.order > 0L),
           main.title, x.title, y.title,
           colors      = list(line = "#D6604D", fill = "#FDDBC7",
                              zero = "#999999"),
           title.size  = 11, title.face = "bold",
           axis.size   = 10,
           theme       = ggplot2::theme_minimal(),
           ...) {

    kernel_label <- tools::toTitleCase(x@kernel)
    r <- x@deriv.order

    if (missing(y.title))
      y.title <- if (r == 0L) expression((K %*% K)(x))
                 else         bquote((K^{(.(r))} %*% K^{(.(r))})(x))

    if (missing(x.title)) x.title <- "x"

    if (missing(main.title))
      main.title <- paste0(
        if (r == 0L) "Kernel Convolution" else paste0("Kernel Convolution  (r = ", r, ")"),
        "\n", kernel_label, " kernel"
      )

    df <- data.frame(xv = x@x, y = x@y)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = xv, y = y))

    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pmin(y, 0), ymax = pmax(y, 0)),
      fill = colors$fill, alpha = 0.40
    )

    if (zero.line)
      p <- p + ggplot2::geom_hline(
        yintercept = 0, linetype = "dashed",
        color = colors$zero, linewidth = 0.5
      )

    if (type %in% c("l", "o"))
      p <- p + ggplot2::geom_line(color = colors$line, linewidth = 0.9)
    if (type %in% c("p", "o"))
      p <- p + ggplot2::geom_point(color = colors$line, size = 0.7)

    p <- p +
      ggplot2::ggtitle(main.title) +
      ggplot2::xlab(x.title) +
      ggplot2::ylab(y.title) +
      theme +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(size = title.size, face = title.face,
                                             hjust = 0.5),
        axis.title   = ggplot2::element_text(size = axis.size),
        panel.border = ggplot2::element_rect(colour = "grey40", fill = NA,
                                             linewidth = 0.5),
        plot.margin  = ggplot2::margin(8, 12, 6, 8)
      )

    print(p)
    invisible(p)
  }
)

## kedd 1.x compatibility: kernel.fun(...)$kx, kernel.conv(...)$kx, $x, ...
setMethod("$", "kfun",  function(x, name) .dollar(x, name, c(kx = "y")))
setMethod("$", "kconv", function(x, name) .dollar(x, name, c(kx = "y")))
