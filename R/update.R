#-------------------------------------------------------------------------------
# Update
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Update Model
#'
#' @description Update one or more slots of a PersephoneModel object.
#'
#' @param object PersephoneModel. The object whose slots are to be updated.
#' @param ... extra arguments.
#'
#' @return PersephoneModel
#' @export
#'
#' @examples
#' \dontrun{
#' object <- new("PersephoneBinomial", region = "A")
#' object <- update(object, region = "B")
#' }
setGeneric("update")

#' @rdname update
setMethod("update",
          signature = c(object = "PersephoneModel"),
          definition = function(object, ...) {

  # Get the object slots to update
  elements <- list(...)

  # Update the slots
  if (length(elements) > 0) {
    for (i in 1:length(elements)) {
      slot(object, names(elements)[i]) = elements[[i]]
    }
  }

  # Return the object
  object

})

#' @rdname update
setMethod("update",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, ...) {

  # Get the object slots to update
  elements <- list(...)

  # Update the slots
  if (length(elements) > 0) {
    for (i in 1:length(object)) {
      elements_crop <- lapply(elements, function(x){ x[[i]] })
      object[[i]] <- do.call(update, args = c(elements_crop, object = object[[i]]))
    }
  }

  # Return the object
  object

})

