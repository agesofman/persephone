#-------------------------------------------------------------------------------
# Model Prediction
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Progress model prediction
#'
#' @description
#' Calculate the predicted values of a model. The `object`, provided needs to
#' have already passed from `fit`, in order to include a `model` slot.
#'
#' @param object an object of class `ProgressModel` or `ProgressModelList`.
#' @param pdata data.frame. The data used in model prediction.
#' @param ... extra arguments.
#'
#' @return data.frame.
#'
#' @import dplyr
#' @importFrom cronus calc_perc calc_cumperc
#' @export
#'
#' @inherit fit examples
setGeneric("predict")

#' @rdname predict
setMethod("predict",
          signature = c(object = "ProgressModelList"),
          definition = function(object, pdata) {

  # Model Prediction
  ProgressList(mapply(predict, object, pdata, SIMPLIFY = FALSE))

})

#' @rdname predict
setMethod("predict",
          signature = c(object = "ProgressBM"),
          definition = function(object, pdata) {

  # Initialize
  newdata <- data.frame()

  # Model Prediction
  pdata <- dplyr::select(pdata, -c("Percentage", "CumPercentage", "Stage"))
  pdata <- dplyr::distinct(pdata)
  stages <- get_stages(object)
  n <- nrow(pdata)

  # for i = 1
  pdata$Stage <- rep(stages[1], times = n)
  pdata$CumPercentage <- rep(1, times = n)
  newdata <- rbind(newdata, pdata)

  for (stage in stages[-1]){
    pdata$Stage <- rep(stage, times = n)
    pdata$CumPercentage <- predict(object@model[[stage]], pdata, type = "response")
    newdata <- rbind(newdata, pdata)
  }
  newdata <- Progress(newdata)
  newdata <- cronus::calc_perc(newdata)

  # Return the predicted data
  newdata

})

#' @rdname predict
setMethod("predict",
          signature = c(object = "ProgressBMM"),
          definition = function(object, pdata) {

  # Initialize
  newdata <- data.frame()

  # Model Prediction
  pdata <- dplyr::select(pdata, -c("Percentage", "CumPercentage", "Stage"))
  pdata <- dplyr::distinct(pdata)
  stages <- get_stages(object)
  n <- nrow(pdata)

  # for i = 1
  pdata$Stage <- rep(stages[1], times = n)
  pdata$CumPercentage <- rep(1, times = n)
  newdata <- rbind(newdata, pdata)

  for (stage in stages[-1]){
    pdata$Stage <- rep(stage, times = n)
    pdata$CumPercentage <- predict(object@model[[stage]], pdata, type = "response")
    newdata <- rbind(newdata, pdata)
  }
  newdata <- Progress(newdata)
  newdata <- cronus::calc_perc(newdata)

  # Return the predicted data
  newdata

})

#' @rdname predict
setMethod("predict",
          signature = c(object = "ProgressCLM"),
          definition = function(object, pdata) {

  # Model Prediction
  pdata$Percentage <- predict(object@model, pdata, type = "prob")$fit
  pdata <- cronus::calc_cumperc(pdata)

  # Return the predicted data
  pdata

})

#' @rdname predict
setMethod("predict",
          signature = c(object = "ProgressCLMM"),
          definition = function(object, pdata) {

  # Model Prediction
  pdata$Percentage <- predict(object@model, pdata, type = "prob")
  pdata <- Progress(pdata)
  pdata <- cronus::calc_cumperc(pdata)

  # Return the predicted data
  pdata

})

#' @rdname predict
setMethod("predict",
          signature = c(object = "ProgressSRF"),
          definition = function(object, pdata) {

  # Initialize
  newdata <- data.frame()

  # Model Prediction
  pdata <- dplyr::select(pdata, -c("Percentage", "CumPercentage", "Stage"))
  pdata <- dplyr::distinct(pdata)
  stages <- get_stages(object)
  n <- nrow(pdata)

  # for i = 1
  pdata$Stage <- rep(stages[1], times = n)
  pdata$Percentage <- rep(1, times = n)
  newdata <- rbind(newdata, pdata)

  if (object@scaled){
    for (stage in stages[-1]){
      pdata$Stage <- rep(stage, times = n)
      pdata$Percentage <- randomForestSRC::predict.rfsrc(object@model[[stage]], pdata)$predicted
      newdata <- rbind(newdata, pdata)
    }
  }
  #newdata <- Progress(newdata)
  newdata <- cronus::calc_cumperc(newdata)
 # newdata <- Progress(newdata)

  # Return the predicted data
  newdata

})

#' @rdname predict
setMethod("predict",
          signature = c(object = "ProgressMRF"),
          definition = function(object, pdata) {

  # Initialize
  newdata <- data.frame()

  # Bind global variables
  Stage <- Percentage <- NULL

  pdata <- pdata %>%
    dplyr::select(-c("CumPercentage")) %>%
    tidyr::pivot_wider(names_from = Stage, values_from = Percentage)


  # Model Prediction
  mrf <- randomForestSRC::predict.rfsrc(object@model,
                                        pdata,
                                        membership = !(object@scaled),
                                        seed = object@seed)

  newdata <- randomForestSRC::get.mv.predicted(mrf)


  #
  # # Model Prediction
  # pdata <- dplyr::select(pdata, -c("Percentage", "CumPercentage", "Stage"))
  # pdata <- dplyr::distinct(pdata)
  # stages <- get_stages(object)
  # n <- nrow(pdata)
  #
  # # for i = 1
  # pdata$Stage <- rep(stages[1], times = n)
  # pdata$Percentage <- rep(1, times = n)
  # newdata <- rbind(newdata, pdata)
  #
  # if (object@scaled){
  #   for (stage in stages[-1]){
  #     pdata$Stage <- rep(stage, times = n)
  #     pdata$Percentage <- randomForestSRC::predict.rfsrc(object@model[[stage]], pdata)$predicted
  #     newdata <- rbind(newdata, pdata)
  #   }
  # }
  # #newdata <- Progress(newdata)
  # newdata <- cronus::calc_cumperc(newdata)
  # # newdata <- Progress(newdata)
  #
  # Return the predicted data
  newdata

})
