#-------------------------------------------------------------------------------
# Model Prediction
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title `persephone` model prediction
#'
#' @description
#' Calculate the predicted values of a model. The `object`, provided needs to
#' have already passed from `fit`, in order to include a `model` slot.
#'
#' @param object an object of class `PersephoneModel` or `PersephoneModelList`.
#' @param pdata data.frame. The data used in model prediction.
#' @param ... extra arguments.
#'
#' @return data.frame.
#'
#' @import dplyr
#' @importFrom cronus calc_percentage
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
setGeneric("predict")

#' @rdname predict
setMethod("predict",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, pdata) {

  # Model Prediction
  mapply(predict, object, pdata, SIMPLIFY = FALSE)

})

#' @rdname predict
setMethod("predict",
          signature = c(object = "PersephoneQuasiBin"),
          definition = function(object, pdata) {

  # Initialize
  newdata <- data.frame()

  # Model Prediction
  pdata <- dplyr::select(pdata, -c("Percentage", "CumPercentage", "Stage"))
  pdata <- dplyr::distinct(pdata)
  stages <- get_stages(object)
  for (i in seq_along(stages)){
    stage <- stages[i]
    pdata$Stage <- rep(stage, times = nrow(pdata))
    pdata$CumPercentage <- predict(object@model[[stage]], pdata, type = "response")
    newdata <- rbind(newdata, pdata)
  }
  newdata <- cronus::calc_percentage(newdata, cum = FALSE)

  # Return the predicted data
  newdata

})

#' @rdname predict
setMethod("predict",
          signature = c(object = "PersephoneCumLink"),
          definition = function(object, pdata) {

  # Model Prediction
  pdata$Percentage <- predict(object@model, pdata, type = "prob")$fit
  pdata <- cronus::calc_percentage(pdata, cum = TRUE)

  # Return the predicted data
  pdata

})
