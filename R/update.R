#-------------------------------------------------------------------------------
# Update
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Update Model
#'
#' @description
#' Update one or more slots of a `ProgressModel` object.
#'
#' @param object an object of class `ProgressModel` or `ProgressModelList`.
#' whose slots are to be updated.
#' @param ... extra arguments.
#'
#' @return an object of class `ProgressModel` or `ProgressModelList`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a Region object
#' library(cronus)
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#'
#' object <- new("ProgressCLM", region = region, crop = "Corn", model = "A")
#' object <- update(object, region = region, crop = "Soybeans", model = "B")
#' }
setGeneric("update")

#' @rdname update
setMethod("update",
          signature = c(object = "ProgressModel"),
          definition = function(object, ...) {

  # Get the object slots to update
  elements <- list(...)

  # Update the slots
  if (length(elements) > 0) {
    for (i in seq_along(elements)) {
      if (.hasSlot(object, names(elements)[i])) {
        slot(object, names(elements)[i]) <- elements[[i]]
      }
    }
  }

  # Return the object
  object

})

#' @rdname update
setMethod("update",
          signature = c(object = "ProgressModelList"),
          definition = function(object, ...) {

  # Get the object slots to update
  elements <- list(...)

  # Update the slots
  if (length(elements) > 0) {
    for (i in seq_along(object)) {
      elements_i <- lapply(elements, function(x){ x[[i]] })
      object[[i]] <- do.call(update, args = c(elements_i, object = object[[i]]))
    }
  }

  # Return the object
  object

})

