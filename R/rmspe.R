#-------------------------------------------------------------------------------
# Metrics
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Root Mean Square Prediction Error
#'
#' @param object An object of class `ProgressModel` or `ProgressModelList`.
#'
#' @return A data.frame with columns Stage, Time and rmspe or an object of class
#' `ProgressModel` with its `metrics` slot altered.
#' @param ... extra arguments.
#'
#' @return An object identical to `object`, with the `metrics` slot(s) altered.
#'
#' @importFrom dplyr full_join mutate group_by summarise ungroup
#' @export
#'
#' @inherit fit examples
setGeneric("rmspe", signature = c("object"),
           function(object, ...) { standardGeneric("rmspe") })

#' @rdname rmspe
setMethod("rmspe",
          signature  = c(object = "ProgressModel"),
          definition = function(object, ...) {

  # Calculate the rmspe
  df_rmspe <- crossval(object, fun = rmspe_data, ...)

  # Compute RMSPE
  object@metrics$rmspe <- df_rmspe %>%
    dplyr::group_by(.data$Time, .data$Stage) %>%
    dplyr::summarise(rmspe = mean(.data$rmspe)) %>%
    dplyr::ungroup()

  # Compute ARMSPE
  object@metrics$armspe <- object@metrics$rmspe %>%
    dplyr::group_by(.data$Time) %>%
    dplyr::summarise(armspe = sum(.data$rmspe)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(armspe = mean(.data$armspe))

  # Return the object
  object

})

#' @rdname rmspe
setMethod("rmspe",
          signature  = c(object = "ProgressModelList"),
          definition = function(object, ...) {

  object <- lapply(object, rmspe, ...)
  class(object) <- "ProgressModelList"
  object

})

setGeneric("rmspe_data", signature = c("x", "y"),
           function(x, y, ...) { standardGeneric("rmspe_data") })

setMethod("rmspe_data",
          signature  = c(x = "Progress", y = "Progress"),
          definition = function(x, y, ...) {

  # Join the two data.frames
  Percentage.x <- Percentage.y <- NULL
  data <- dplyr::full_join(x, y, by = c("Stage", "Season", "Time", "Date"))
  data$Stage <- factor(data$Stage, levels = levels(x$Stage))

  # Calculate the Root Mean Square Prediction Error (RMSPE)
  data %>%
    dplyr::mutate(spe = (Percentage.x - Percentage.y) ^ 2) %>%
    dplyr::group_by(.data$Time, .data$Stage) %>%
    dplyr::summarise(rmspe = sqrt(mean(.data$spe))) %>%
    dplyr::ungroup()

})

