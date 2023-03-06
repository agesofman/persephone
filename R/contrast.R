#-------------------------------------------------------------------------------
# Model Comparison and Selection
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title contrast models
#'
#' @description Contrast models inside a `PersephoneModelList` object based on a
#' criterion of choice, such as AICc.
#'
#' @param object an object of class `PersephoneModelList`.
#' @param ... extra arguments.
#'
#' @return A `data.frame()` with one row for each model.
#'
#' @export
#'
#' @inherit fit examples
setGeneric("contrast", signature = c("object"),
           function(object, ...) { standardGeneric("contrast") })

#' @rdname contrast
setMethod("contrast",
          signature = c(object = "ProgressModelList"),
          definition = function(object, ...) {

  contrast_table(object[[1]], validate_contrast(object, ...))

})

setGeneric("validate_contrast", signature = c("object"),
           function(object, ...) { standardGeneric("validate_contrast") })

setMethod("validate_contrast",
          signature = c(object = "ProgressModelList"),
          definition = function(object, ...) {

  # Gather the models
  classes <- get_class(object)
  labels <- get_label(object)
  list_contrast <- list()

  # Check that all classes are identical
  if (length(unique(classes)) > 1) {
    stop("All models must be of the same class. Instead got ", unique(classes))
  }

  # Return the result
  object

})

setGeneric("contrast_table", signature = c("x", "object"),
           function(x, object, ...) { standardGeneric("contrast_table") })

setMethod("contrast_table",
          signature = c(x = "ProgressBM", object = "ProgressModelList"),
          definition = function(x, object) {

  # Initialize
  x <- list()

  for (i in seq_along(object)) {
    x[[i]] <- list()
    x[[i]]$link <- get_link(object[[i]])
    x[[i]]$formula <- names(get_formula(object[[i]]))
    x[[i]] <- cbind(x[[i]], object[[i]]@metrics$aicc)
  }

  # Return the result
  do.call(rbind, x)

})

setMethod("contrast_table",
          signature = c(x = "ProgressCLM", object = "ProgressModelList"),
          definition = function(x, object, slotnames) {

  # Initialize
  x <- list()

  for (i in seq_along(object)) {
    x[[i]] <- list()
    x[[i]]$link <- get_link(object[[i]])
    x[[i]]$formula <- names(get_formula(object[[i]]))
    x[[i]] <- cbind(x[[i]], AICc = object[[i]]@metrics$aicc)
  }

  # Return the result
  do.call(rbind, x)

})
