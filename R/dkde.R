## S4 class: dkde
## Returned by dkde() — kernel density (derivative) estimator.

setClass("dkde",
  representation(
    x           = "numeric",   # evaluation grid
    y           = "numeric",   # estimated values hat{f}^{(r)}(x)
    h           = "numeric",   # bandwidth used
    kernel      = "character",
    deriv.order = "integer",
    n           = "integer",   # sample size
    data        = "numeric",   # raw observations
    call        = "call"
  )
)

setValidity("dkde", function(object) {
  if (length(object@h) != 1L || object@h <= 0)
    return("'h' must be a single positive number.")
  if (length(object@kernel) != 1L)
    return("'kernel' must be a single character string.")
  if (length(object@deriv.order) != 1L || object@deriv.order < 0L)
    return("'deriv.order' must be a non-negative integer.")
  if (length(object@x) != length(object@y))
    return("'x' and 'y' must have the same length.")
  TRUE
})

## show method
setMethod("show", "dkde", function(object) {
  r_label <- if (object@deriv.order == 0L) "density" else
             paste0("derivative of order ", object@deriv.order)
  cat("\nKernel ", r_label, " estimate\n", sep = "")
  cat("Kernel     :", object@kernel, "\n")
  cat("Bandwidth h:", format(object@h, digits = 6), "\n")
  cat("n          :", object@n, "\n")
  cat("Grid points:", length(object@x), "\n")
  invisible(object)
})

##' Kernel density derivative estimator
##'
##' Computes  hat{f}^{(r)}(x) = (1 / (n * h^{r+1})) * sum_i K^{(r)}((x - X_i)/h)
##'
##' @param x        numeric vector of evaluation points (or NULL to use a grid)
##' @param data     numeric vector of observations
##' @param h        bandwidth (scalar); if missing, uses UCV
##' @param deriv.order  derivative order r (0 = density)
##' @param kernel   kernel name (default "gaussian")
##' @param nthreads OpenMP threads
##' @return  an object of class \code{"dkde"}
##' @export
dkde <- function(x = NULL, data, h, deriv.order = 0L,
                 kernel = "gaussian", nthreads = 0L) {
  kernel <- .normalize_kernel(kernel)
  nthreads <- .check_nthreads(nthreads)
  r <- as.integer(deriv.order)
  data <- data[!is.na(data)]
  n    <- as.integer(length(data))
  if (n < 2L) stop("'data' must have at least 2 observations.")
  if (missing(h) || is.null(h)) {
    bw <- h.ucv(data, deriv.order = r, kernel = kernel, nthreads = nthreads)
    h  <- bw@h
  }
  if (!is.numeric(h) || length(h) != 1L || h <= 0)
    stop("'h' must be a positive scalar.")
  if (is.null(x))
    x <- seq(min(data) - 3 * h, max(data) + 3 * h, length.out = 512L)
  diffs_mat <- outer(x, data, "-") / h
  K_vals    <- matrix(Cpp_kernel_deriv(kernel, as.numeric(diffs_mat), r, nthreads),
                      nrow = length(x))
  y <- rowSums(K_vals) / (n * h^(r + 1L))
  new("dkde",
      x = as.numeric(x), y = as.numeric(y),
      h = h, kernel = kernel, deriv.order = r,
      n = n, data = as.numeric(data),
      call = match.call())
}

## plot method for dkde using ggplot2
setMethod("plot", "dkde",
  function(x,
           rug        = TRUE,
           band       = TRUE,
           zero.line  = (x@deriv.order > 0L),
           type       = "l",
           main.title, x.title, y.title,
           colors     = list(line = "#2166AC", fill = "#DEEBF7",
                             rug  = "#555555", zero = "#999999"),
           title.size = 11, title.face = "bold",
           axis.size  = 10,
           theme      = ggplot2::theme_minimal(),
           ...) {

    kernel_label <- tools::toTitleCase(x@kernel)

    if (missing(y.title))
      y.title <- if (x@deriv.order == 0L)
        expression(hat(f)(x))
      else
        bquote(hat(f)^{(.(x@deriv.order))}(x))

    if (missing(x.title)) x.title <- "x"

    if (missing(main.title))
      main.title <- paste0(
        if (x@deriv.order == 0L) "Kernel Density Estimate"
        else paste0("Kernel Density Derivative  (r = ", x@deriv.order, ")"),
        "\n",
        kernel_label, " kernel",
        "  |  h = ", round(x@h, 4),
        "  |  n = ", x@n
      )

    df <- data.frame(xv = x@x, y = x@y)

    if (band) {
      se <- stats::sd(x@y) / sqrt(x@n)
      df$ylo <- x@y - se
      df$yhi <- x@y + se
    }

    p <- ggplot2::ggplot(df, ggplot2::aes(x = xv, y = y))

    if (band)
      p <- p + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ylo, ymax = yhi),
        fill = colors$fill, alpha = 0.45
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

    if (rug) {
      df_rug <- data.frame(xv = x@data)
      p <- p + ggplot2::geom_rug(
        data  = df_rug,
        ggplot2::aes(x = xv, y = NULL),
        color = colors$rug, alpha = 0.4,
        sides = "b", linewidth = 0.3
      )
    }

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

## kedd 1.x compatibility: dkde(...)$est.fx, $eval.points, $h, $n, ...
setMethod("$", "dkde", function(x, name)
  .dollar(x, name, c(eval.points = "x", est.fx = "y")))
