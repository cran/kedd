## S4 class: bandwidth
## Returned by all bandwidth selectors in the kedd package.

setClass("bandwidth",
  representation(
    data        = "numeric",
    kernel      = "character",
    deriv.order = "integer",
    h           = "numeric",
    min.obj     = "numeric",
    hos         = "numeric",
    method      = "character"
  )
)

setValidity("bandwidth", function(object) {
  if (length(object@kernel) != 1L)
    return("'kernel' must be a single character string.")
  if (length(object@method) != 1L)
    return("'method' must be a single character string.")
  if (length(object@deriv.order) != 1L || object@deriv.order < 0L)
    return("'deriv.order' must be a non-negative integer.")
  TRUE
})

## show method
setMethod("show", "bandwidth", function(object) {
  cat("\nCall: ", object@method, " bandwidth selector\n", sep = "")
  cat("Kernel      :", object@kernel, "\n")
  cat("Deriv. order:", object@deriv.order, "\n")
  cat("Bandwidth  h:", format(object@h, digits = 6), "\n")
  cat("Objective   :", format(object@min.obj, digits = 6), "\n")
  cat("h_os        :", format(object@hos, digits = 6), "\n")
  invisible(object)
})

## Criterion of a bandwidth object evaluated on a grid of bandwidths
.cv_curve <- function(bw, h) {
  if (bw@method == "AMISE")
    stop("AMISE bandwidths come from a closed-form rule: there is no criterion curve.")
  if (bw@method == "MLCV")
    return(vapply(h, .mlcv_obj, numeric(1), kernel = bw@kernel, x = bw@data))
  as.numeric(Cpp_plot_cv(bw@method, h, bw@kernel, bw@deriv.order, bw@data))
}

## plot method: CV curve using ggplot2
setMethod("plot", "bandwidth",
  function(x,
           h          = NULL,
           type       = "o",
           n.points   = 100L,
           log_scale_x = FALSE,
           show.min   = TRUE,
           show.hos   = FALSE,
           main.title, x.title, y.title,
           colors     = list(line = "#2166AC", point = "#D6604D",
                             vline = "#D6604D", hos = "#4DAC26",
                             fill  = "#DEEBF7"),
           title.size = 11, title.face = "bold",
           axis.size  = 10,
           theme      = ggplot2::theme_minimal(),
           ...) {

    if (is.na(x@h)) {
      warning("bandwidth is NA - nothing to plot.")
      return(invisible(NULL))
    }

    ## Method full names (including AMISE / MLCV)
    method_labels <- c(
      UCV  = "Unbiased Cross-Validation",
      BCV1 = "Biased Cross-Validation (BCV1)",
      BCV2 = "Biased Cross-Validation (BCV2)",
      CCV  = "Complete Cross-Validation",
      MCV  = "Modified Cross-Validation",
      TCV  = "Trimmed Cross-Validation",
      SCV  = "Smoothed Cross-Validation",
      AMISE = "Asymptotic MISE",
      MLCV  = "Maximum-Likelihood CV"
    )
    method_label <- if (!is.null(method_labels[x@method]) &&
                        !is.na(method_labels[x@method]))
                      method_labels[[x@method]] else x@method
    kernel_label <- tools::toTitleCase(x@kernel)

    ## Build h grid
    if (is.null(h))
      h <- seq(x@h * 0.4, x@h * 2.0, length.out = as.integer(n.points))

    ## Evaluate CV objective
    cv_vals <- .cv_curve(x, h)
    df <- data.frame(h = h, cv = cv_vals)

    ## Axis / title defaults
    if (missing(x.title))    x.title    <- "Bandwidth  h"
    if (missing(y.title))    y.title    <- paste0(x@method, "(h)")
    if (missing(main.title)) main.title <- paste0(
      method_label, "\n",
      kernel_label, " kernel",
      if (x@deriv.order > 0L) paste0("  |  derivative order r = ", x@deriv.order) else ""
    )

    ## Base plot
    p <- ggplot2::ggplot(df, ggplot2::aes(x = h, y = cv))

    ## Optional ribbon (±1 SE visual band around the curve)
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = cv - 0.02 * diff(range(cv)),
        ymax = cv + 0.02 * diff(range(cv))
      ),
      fill = colors$fill, alpha = 0.35
    )

    ## Line / point / both
    if (type %in% c("l", "o"))
      p <- p + ggplot2::geom_line(color = colors$line, linewidth = 0.8)
    if (type %in% c("p", "o"))
      p <- p + ggplot2::geom_point(color = colors$point, size = 0.9)

    ## Vertical line at optimal h
    p <- p + ggplot2::geom_vline(
      xintercept = x@h, linetype = "dashed",
      color = colors$vline, linewidth = 0.7
    )

    ## Star at the minimum value
    if (show.min) {
      idx_min <- which.min(cv_vals)
      df_min  <- data.frame(h = h[idx_min], cv = cv_vals[idx_min])
      p <- p + ggplot2::geom_point(
        data = df_min,
        ggplot2::aes(x = h, y = cv),
        shape = 18, size = 4, color = colors$vline
      ) +
      ggplot2::annotate(
        "text",
        x     = x@h,
        y     = max(cv_vals, na.rm = TRUE),
        label = paste0("h* = ", round(x@h, 4)),
        hjust = -0.1, vjust = 1, size = 3.2,
        color = colors$vline
      )
    }

    ## Optional vertical line at h_os
    if (show.hos && !is.na(x@hos)) {
      p <- p + ggplot2::geom_vline(
        xintercept = x@hos, linetype = "dotted",
        color = colors$hos, linewidth = 0.6
      ) +
      ggplot2::annotate(
        "text",
        x = x@hos, y = max(cv_vals, na.rm = TRUE),
        label = paste0("h_os = ", round(x@hos, 4)),
        hjust = 1.1, vjust = 1, size = 3.0,
        color = colors$hos
      )
    }

    ## Labels
    p <- p +
      ggplot2::ggtitle(main.title) +
      ggplot2::xlab(x.title) +
      ggplot2::ylab(y.title)

    ## Log scale
    if (log_scale_x)
      p <- p + ggplot2::scale_x_log10()

    ## Theme
    p <- p + theme +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(size = title.size, face = title.face,
                                             hjust = 0.5),
        axis.title   = ggplot2::element_text(size = axis.size),
        panel.border = ggplot2::element_rect(colour = "grey40", fill = NA,
                                             linewidth = 0.5),
        plot.margin  = ggplot2::margin(8, 12, 6, 8)
      )

    print(p)
    invisible(list(h = h, cv = cv_vals, plot = p))
  }
)

## lines method: returns a ggplot2 geom_line layer to add with +
setMethod("lines", "bandwidth", function(x, n.points = 100L,
                                          color = "#1B7837", linewidth = 0.8,
                                          linetype = "solid", ...) {
  if (is.na(x@h)) return(invisible(NULL))
  hgrid <- seq(x@h * 0.4, x@h * 2.0, length.out = as.integer(n.points))
  yvals <- .cv_curve(x, hgrid)
  df <- data.frame(h = hgrid, cv = yvals)
  ggplot2::geom_line(
    data      = df,
    mapping   = ggplot2::aes(x = h, y = cv),
    color     = color,
    linewidth = linewidth,
    linetype  = linetype,
    ...
  )
})

## kedd 1.x compatibility: bw$h, bw$kernel, bw$deriv.order, bw$x, bw$n, bw$min.ucv, ...
setMethod("$", "bandwidth", function(x, name) {
  if (name == "n") return(length(x@data))
  if (startsWith(name, "min.")) return(x@min.obj)
  .dollar(x, name, c(x = "data"))
})
