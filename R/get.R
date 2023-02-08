#-------------------------------------------------------------------------------
# Get object slots
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Get object slots
#'
#' @param object an object of class `PersephoneModel` or `PersephoneModelList`.
#' @param crop character. A crop of choice.
#' @param ... extra arguments
#'
#' @return character. The `class()` of the `object`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a Region object
#' library(cronus)
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#'
#' # Create a model
#' object <- new("PersephoneBin",
#'               label = "Base_logit",
#'               region = region,
#'               crop = "Corn",
#'               data = progress_ne$Corn,
#'               link = "logit",
#'               formula = "CumPercentage ~ Time + agdd + adayl") # PersephoneModel
#'
#' # Fit
#' object <- fit(object)
#'
#' # Get slots
#' get_class(object)
#' get_label(object)
#' get_formula(object)
#' get_model(object)
#' get_crops(object)
#' get_region(object)
#' get_index(object, crop = "Corn")
#' get_stages(object)
#' get_seasons(object)
#' }
setGeneric("get_class", signature = c("object"),
           function(object, ...) { standardGeneric("get_class") })

#' @rdname get_class
setMethod("get_class",
          signature = c(object = "PersephoneModel"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Get model
  class(object)

})

#' @rdname get_class
setMethod("get_class",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Get models
  sapply(object, get_class)

})

#' @describeIn get_class returns a character. The `region` slot of the `object`.
#' @export
setGeneric("get_region", signature = c("object"),
           function(object, ...) { standardGeneric("get_region") })

#' @rdname get_class
setMethod("get_region",
          signature = c(object = "PersephoneModel"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Get region
  object@region@name

})

#' @rdname get_class
setMethod("get_region",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Get models
  lapply(object, get_region)

})

#' @describeIn get_class returns a character. The `label` slot of the `object`.
#' @export
setGeneric("get_label", signature = c("object"),
           function(object, ...) { standardGeneric("get_label") })

#' @rdname get_class
setMethod("get_label",
          signature = c(object = "PersephoneModel"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Get model
  object@label

})

#' @rdname get_class
setMethod("get_label",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Get models
  sapply(object, get_label)

})

#' @describeIn get_class returns a character. The `formula` slot of the `object`.
#' @export
setGeneric("get_formula", signature = c("object"),
           function(object, ...) { standardGeneric("get_formula") })

#' @rdname get_class
setMethod("get_formula",
          signature = c(object = "PersephoneModel"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Get model
  object@formula

})

#' @rdname get_class
setMethod("get_formula",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Get models
  lapply(object, get_formula)

})

#' @describeIn get_class returns a character. The `model` slot of the `object`.
#' @export
setGeneric("get_model", signature = c("object"),
           function(object, ...) { standardGeneric("get_model") })

#' @rdname get_class
setMethod("get_model",
          signature = c(object = "PersephoneModel"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Fit the model
  object <- fit(object)

  # Get model
  object@model

})

#' @rdname get_class
setMethod("get_model",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Get models
  models <- lapply(object, get_model)
  names(models) <- get_label(object)
  models

})

#' @describeIn get_class returns a character. The `crop` slot of the `object`.
#' @export
setGeneric("get_crops", signature = c("object"),
           function(object, ...) { standardGeneric("get_crops") })

#' @rdname get_class
setMethod("get_crops",
          signature = c(object = "PersephoneModel"),
          definition = function(object) {

  object@crop

})

#' @rdname get_class
setMethod("get_crops",
          signature = c(object = "PersephoneModelList"),
          definition = function(object) {

  sapply(object, "get_crops")

})

#' @describeIn get_class returns a numeric The indices that correspond to the
#' `crop` slot of the `object`.
#' @export
setGeneric("get_index", signature = c("object"),
           function(object, ...) { standardGeneric("get_index") })

#' @rdname get_class
setMethod("get_index",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, crop = NULL) {

  which(crop == get_crops(object))

})

#' @describeIn get_class returns a factor. The `stages` included in `data`.
#' @export
setGeneric("get_stages", signature = c("object"),
           function(object, ...) { standardGeneric("get_stages") })

#' @rdname get_class
setMethod("get_stages",
          signature = c(object = "PersephoneModel"),
          definition = function(object) {

  unique(object@data$Stage)

})

#' @rdname get_class
setMethod("get_stages",
          signature = c(object = "PersephoneModelList"),
          definition = function(object) {

  x <- lapply(object, get_stages)
  names(x) <- get_crops(object)
  x

})

#' @describeIn get_class returns a numeric. The `seasons` included in `data`.
#' @export
setGeneric("get_seasons", signature = c("object"),
           function(object, ...) { standardGeneric("get_seasons") })

#' @rdname get_class
setMethod("get_seasons",
          signature = c(object = "PersephoneModel"),
          definition = function(object) {

  unique(object@data$Season)

})

#' @rdname get_class
setMethod("get_seasons",
          signature = c(object = "PersephoneModelList"),
          definition = function(object) {

  x <- lapply(object, get_seasons)
  names(x) <- get_crops(object)
  x

})
