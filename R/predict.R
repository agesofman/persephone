#-------------------------------------------------------------------------------
# Model Prediction
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Persephone model prediction
#'
#' @param object S4 object. The model of interest.
#' @param pdata data.frame. The data used in model prediction.
#' @param ... extra arguments.
#'
#' @return data.frame.
#' @export
#' @import dplyr
#' @importFrom cronus calc_percentage
#' @examples
#' \dontrun{
#' predicted_data <- predict(object, pdata)
#' }
setGeneric("predict")

#' @rdname predict
setMethod("predict",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, pdata) {

  # Model Prediction
  mapply(predict, object, pdata)

})

#' @rdname predict
setMethod("predict",
          signature = c(object = "PersephoneBinomial"),
          definition = function(object, pdata) {

  # Initialize
  newdata <- data.frame()

  # Model Prediction
  pdata <- dplyr::select(pdata, -c("Percentage", "CumPercentage", "Stage"))
  pdata <- dplyr::distinct(pdata)
  stages <- get_stages(object)
  for (i in 1:length(stages)){
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
