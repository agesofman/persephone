#-------------------------------------------------------------------------------
# Tools
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Tools
#'
#' @description
#' Access PersephoneModel slots and information.
#'
#' @param object an object of class `PersephoneModel` or `PersephoneModelList`.
#' @param crop character. A crop of choice.
#' @param ... extra arguments
#'
#' @return A vector or list of vectors
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Create a Region object
#' library(cronus)
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#'
#' # Create a model
#' object1 <- new("PersephoneQuasiBin",
#'                region = region,
#'                crop = "Corn",
#'                data = progress_ne$Corn,
#'                formula = "CumPercentage ~ Time + agdd") # PersephoneModel
#'
#' # Create another model
#' object2 <- new("PersephoneCumLink",
#'                region = region,
#'                crop = "Soybeans",
#'                data = progress_ne$Soybeans,
#'                formula = "Stage ~ Time + agdd + adayl") # PersephoneModel
#'
#' # Concatenate the models
#' object <- c(object1, object2) # PersephoneModelList
#'
#' # Tools
#' get_crops(object)
#' get_region(object)
#' get_index(object, crop = "Corn")
#' get_stages(object)
#' get_seasons(object)
#' }
setGeneric("get_crops", signature = c("object"),
           function(object, ...) { standardGeneric("get_crops") })

#' @rdname get_crops
setMethod("get_crops",
          signature = c(object = "PersephoneModel"),
          definition = function(object) {

  object@crop

})

#' @rdname get_crops
setMethod("get_crops",
          signature = c(object = "PersephoneModelList"),
          definition = function(object) {

  sapply(object, "get_crops")

})

#' @export
#' @rdname get_crops
setGeneric("get_region", signature = c("object"),
           function(object, ...) { standardGeneric("get_region") })

#' @rdname get_crops
setMethod("get_region",
          signature = c(object = "PersephoneModel"),
          definition = function(object) {

  object@region@name

})

#' @rdname get_crops
setMethod("get_region",
          signature = c(object = "PersephoneModelList"),
          definition = function(object) {

  sapply(object, "get_region")

})

#' @export
#' @rdname get_crops
setGeneric("get_index", signature = c("object"),
           function(object, ...) { standardGeneric("get_index") })

#' @rdname get_crops
setMethod("get_index",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, crop = NULL) {

  which(crop == get_crops(object))

})

#' @export
#' @rdname get_crops
setGeneric("get_stages", signature = c("object"),
           function(object, ...) { standardGeneric("get_stages") })

#' @rdname get_crops
setMethod("get_stages",
          signature = c(object = "PersephoneModel"),
          definition = function(object) {

  unique(object@data$Stage)

})

#' @rdname get_crops
setMethod("get_stages",
          signature = c(object = "PersephoneModelList"),
          definition = function(object) {

  x <- lapply(object, get_stages)
  names(x) <- get_crops(object)
  x

})

#' @export
#' @rdname get_crops
setGeneric("get_seasons", signature = c("object"),
           function(object, ...) { standardGeneric("get_seasons") })

#' @rdname get_crops
setMethod("get_seasons",
          signature = c(object = "PersephoneModel"),
          definition = function(object) {

  unique(object@data$Season)

})

#' @rdname get_crops
setMethod("get_seasons",
          signature = c(object = "PersephoneModelList"),
          definition = function(object) {

  x <- lapply(object, get_seasons)
  names(x) <- get_crops(object)
  x

})
