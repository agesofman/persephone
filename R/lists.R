#-------------------------------------------------------------------------------
# Concatenate Classes
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Concatenate
#'
#' @description
#' Concatenate PersephoneModel objects into a PersephoneModelList.
#'
#' @param x an object of class `PersephoneModel`.
#' @param ... extra models to concatenate.
#'
#' @return an object of class `PersephoneModelList`.
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
#' object1 <- new("PersephoneBin",
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
#' # Fit
#' object <- fit(object)
#'
#' # Plot
#' plot(object, cumulative = TRUE, seasons = 2002)
#'
#' # Predict
#' predict(object, progress_ne)
#'
#' # Evaluate
#' object <- crossval(object, maxsam = 100, seed = 1)
#' plot_metrics(object)
#'
#' # Summarize
#' summary(object)
#'
#' # Report
#' report(object, name = "example_report", dir = getwd())
#' }
setMethod("c",
          signature(x = "PersephoneModel"),
          definition = function(x, ...) {

  y <- list(x, ...)
  class(y) <- "PersephoneModelList"
  names(y) <- get_crops(y)
  y

})
