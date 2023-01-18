#-------------------------------------------------------------------------------
# Update
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Update Model
#'
#' @description
#' Update one or more slots of a PersephoneModel object.
#'
#' @param object an object of class `PersephoneModel` or `PersephoneModelList`.
#' whose slots are to be updated.
#' @param ... extra arguments.
#'
#' @return an object of class `PersephoneModel` or `PersephoneModelList`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a Region object
#' library(cronus)
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#'
#' object <- new("PersephoneCumLink", region = region, crop = "Corn", model = "A")
#' object <- update(object, region = region, crop = "Soybeans", model = "B")
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
    for (i in seq_along(elements)) {
      slot(object, names(elements)[i]) <- elements[[i]]
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
    for (i in seq_along(object)) {
      elements_crop <- lapply(elements, function(x){ x[[i]] })
      object[[i]] <- do.call(update, args = c(elements_crop, object = object[[i]]))
    }
  }

  # Return the object
  object

})

