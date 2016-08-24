#' @import R6
#' @export
Signal = R6::R6Class(
  "Signal",

  public = list(
    initialize = function(signal, samplingFrequency) {
      private$size = length(signal)
      private$signal = signal
      private$samplingFrequency = samplingFrequency
    },

    get = function() {
      private$signal
    },

    getSamplingFrequency = function() {
      private$samplingFrequency
    },

    plot = function(lines = TRUE, color = "steelblue") {
      time = private$makeTime()
      plot(time, private$signal, col = color, pch = 20L, xlab = "t (s)",
           ylab = "Signal", bty = "n")
      if (lines) {
        segments(x0 = time, y0 = 0, y1 = private$signal, col = color)
      }
    },

    interpolate = function() {
      Interpolant$new(private$makeTime(), private$signal)
    }

  ),

  private = list(
    size = integer(),
    signal = numeric(),
    samplingFrequency = integer(),

    makeTime = function(samplingFrequency) {
      seq(from = 0, by = 1 / private$samplingFrequency, length.out = private$size)
    }

  )
)
