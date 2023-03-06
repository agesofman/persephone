#-------------------------------------------------------------------------------
# Create new objects
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Create new objects
#'
#' @description The function creates a `ProgressModelList`.
#'
#' @param prm a list with all the necessary elements to build a model.
#'
#' @details The function uses the first entry of each list element to create
#' the first `ProgressModel`, then the second entry and so on.
#'
#' @export
#'
#' @return An S4 object of class `ProgressModelList`
#'
#' @examples
#' \dontrun{
#' # Create a Region object
#' library(cronus)
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#'
#' # Class
#' Class <- "ProgressBM"
#'
#' # Formula
#' formula <- list(c(global = "CumPercentage ~ Time + agdd * adayl"),
#'                 c(base = "CumPercentage ~ Time + agdd"))
#'
#' # Link
#' link <- list("logit", "probit")
#'
#' # Set the parameters
#' prm <- combine(Class = Class,
#'                formula = formula,
#'                link = link,
#'                region = list(region),
#'                data = data_progress)
#'
#' prm$crop <- names(prm$data)
#'
#' # Create the object
#' object <- create(prm)
#' }
create <- function(prm) {

  # Create the model
  object <- do.call(mapply, c(prm, FUN = "new"))

  if (length(object) == 1) {
    object <- object[[1]]
  } else {
    class(object) <- c("ProgressModelList", class(object))
    names(object) <- get_label(object)
  }

  # Return the object
  object

}
