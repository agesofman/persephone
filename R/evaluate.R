#-------------------------------------------------------------------------------
# Evaluate model performance
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Evaluate model performance
#'
#' @description
#' Calculate the model performance metrics using leave-group-out (or Monte
#' Carlo) cross-validation.
#'
#' @param object an object of class `ProgressModel` or `ProgressModelList`.
#' @param ... extra arguments.
#'
#' @return An object identical to `object`, with the `metrics` slot(s) altered.
#' @export
#'
#' @inherit fit examples
setGeneric("evaluate", signature = c("object"),
           function(object, ...) { standardGeneric("evaluate") })

#' @rdname evaluate
setMethod("evaluate",
          signature  = c(object = "ProgressModel"),
          definition = function(object, ...) {

  # Fit
  object <- fit(object)

  # Compute RMSPE
  object <- rmspe(object, ...)

  # Compute AICc
  object <- aicc(object)

  # Return the object
  object

})

#' @rdname evaluate
setMethod("evaluate",
          signature  = c(object = "ProgressModelList"),
          definition = function(object, ...) {

  object <- lapply(object, evaluate, ...)
  class(object) <- "ProgressModelList"
  object

})
