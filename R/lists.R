#-------------------------------------------------------------------------------
# Concatenate Classes
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Concatenate
#'
#' @description
#' Concatenate ProgressModel objects into a ProgressModelList.
#'
#' @param x an object of class `ProgressModel`.
#' @param ... extra models to concatenate.
#'
#' @return an object of class `ProgressModelList`.
#' @export
#'
#' @inherit fit examples
setMethod("c",
          signature(x = "ProgressModel"),
          definition = function(x, ...) {

  y <- list(x, ...)
  class(y) <- c("ProgressModelList", "list")
  names(y) <- get_crops(y)
  y

})

#' @export
"[.ProgressModelList" <- function(x, ...) {

  y <- x
  class(y) <- "list"
  y <- y[...]
  class(y) <- class(x)
  y

}
