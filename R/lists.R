#-------------------------------------------------------------------------------
# Concatenate Classes
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Concatenate
#'
#' @description Concatenate PersephoneModel objects into a PersephoneModelList.
#'
#' @param x PersephoneModel.
#' @param ... extra models to concatenate.
#'
#' @return PersephoneModelList
#' @export
#'
#' @examples
#' \dontrun{
#' object1 <- new("PersephoneCumLink")
#' object2 <- new("PersephoneBinomial")
#' object <- c(object1, object2)
#' }
setMethod("c",
          signature(x = "PersephoneModel"),
          definition = function(x, ...) {

  y <- list(x, ...)
  class(y) <- "PersephoneModelList"
  names(y) <- get_crops(y)
  y

})
