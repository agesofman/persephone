#-------------------------------------------------------------------------------
# AICc
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Akaike Information Criterion (corrected)
#'
#' @param object an object of class `ProgressModel` or `ProgressModelList`.
#' @param ... extra arguments.
#'
#' @return numeric. The model AICc.
#'
#' @importFrom AICcmodavg AICc
#' @export
#'
#' @inherit fit examples
setGeneric("aicc", signature = c("object"),
           function(object, ...) { standardGeneric("aicc") })

#' @rdname aicc
setMethod("aicc",
          signature  = c(object = "ProgressBM"),
          definition = function(object, ...) {

  # Initialize
  stages <- get_stages(object)
  x <- numeric()

  # Group by stage
  for (stage in stages[-1]) {
    x[stage] <- AICcmodavg::AICc(object@model[[stage]], ...)
  }

  object@metrics$aicc <- data.frame(Stage = factor(names(x), levels = stages),
                                    AICc = x,
                                    row.names = NULL)

  # Return the object
  object

})

#' @rdname aicc
setMethod("aicc",
          signature  = c(object = "ProgressCLM"),
          definition = function(object, ...) {

  # Calculate AICc
  object@metrics$aicc <- AICcmodavg::AICc(object@model, ...)

  # Return the object
  object

})

#' @rdname aicc
setMethod("aicc",
          signature  = c(object = "ProgressBMM"),
          definition = function(object, ...) {

  # Initialize
  stages <- get_stages(object)
  x <- numeric()

  # Group by stage
  for (stage in stages[-1]) {
    x[stage] <- AICcmodavg::AICc(object@model[[stage]], ...)
  }

  object@metrics$aicc <- data.frame(Stage = factor(names(x), levels = stages),
                                    AICc = x,
                                    row.names = NULL)

  # Return the object
  object

})

#' @rdname aicc
setMethod("aicc",
          signature  = c(object = "ProgressCLMM"),
          definition = function(object, ...) {

    # Calculate AICc
    object@metrics$aicc <- AICcmodavg::AICc(object@model, ...)

    # Return the object
    object

})

#' @rdname aicc
setMethod("aicc",
          signature  = c(object = "ProgressModelList"),
          definition = function(object, ...) {

  object <- lapply(object, aicc, ...)
  class(object) <- "ProgressModelList"
  object

})
