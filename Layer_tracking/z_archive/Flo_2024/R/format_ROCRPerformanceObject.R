#' Format ROCR performance object
#'
#' This function removes all values that correspond to a threshold criterion of 0 %, which is
#' not meaningful in our application
#'
#' I also tested whether switching x and y values would result in a nicer plot next to the
#' precision--recall curve with recall always being on the x-axis. (it did not)
#'
#' @export
format_ROCRPerformanceObject <- function(perf, switchXY = FALSE) {

  toRemove <- which(perf@alpha.values[[1]] == 0)
  if (length(toRemove) > 0) {
    perf@alpha.values[[1]] <- perf@alpha.values[[1]][-toRemove]
    perf@x.values[[1]] <- perf@x.values[[1]][-toRemove]
    perf@y.values[[1]] <- perf@y.values[[1]][-toRemove]
  }

  if (switchXY) {
    xname <- perf@x.name
    xvals <- perf@x.values
    perf@x.name <- perf@y.name
    perf@x.values <- perf@x.values
    perf@y.name <- xname
    perf@y.values <- xvals
  }

  return(perf)
}
