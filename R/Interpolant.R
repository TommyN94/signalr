#' @import R6
#' @import splines
#' @export
Interpolant = R6::R6Class("Interpolant",

  private = list(
    x = numeric(),
    y = numeric(),
    spline = NULL,
    splineFun = NULL
  ),

  public = list(
    initialize = function(x, y) {
      private$x = x
      private$y = y
      private$spline = splines::interpSpline(x, y)
      private$splineFun = function(x) self$eval(x)$y
    },

    eval = function(x, n = 100L, deriv = 0L) {
      values = splines:::predict.polySpline(private$spline, x, nseg = n,
                                             deriv = deriv)
      class(values) = "data.frame"
      attr(values, "row.names") = .set_row_names(n + 1L)
      values
    },

    integrate = function() {
      lower = private$x[1L]
      upper = private$x[length(private$x)]
      integrate(private$splineFun, lower, upper)$value
    },

    max = function() {
      range = private$x[c(1L, length(private$x))]
      max = optimize(private$splineFun, range, maximum = TRUE)
      names(max) = c("t", "max")
      max
    },

    plot = function(n = 100L, deriv = 0L, color = "steelblue", ...) {
      plot(self$eval(n, deriv), type = "l", bty = "n", col = color, ...)
    }
  )
)
