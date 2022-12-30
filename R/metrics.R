#-------------------------------------------------------------------------------
# Metrics
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Root Mean Square Prediction Error
#'
#' @param x data.frame. A data.frame with columns named "Stage", "Season", "Time", "Date" and "Percentage".
#' @param y data.frame. A data.frame with columns named "Stage", "Season", "Time", "Date" and "Percentage".
#'
#' @return A data.frame with columns Stage, Time and rmspe.
rmspe <- function(x, y) {

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

}

#' Average Root Mean Square Prediction Error
#'
#' @param data data.frame. A data.frame with columns Stage, Time and spe.
#'
#' @return A data.frame with columns Stage, Time and rmspe.
armspe <- function(data) {

  data <- data %>%
    dplyr::group_by(.data$Time, .data$Stage) %>%
    dplyr::summarise(rmspe = mean(.data$rmspe)) %>%
    dplyr::ungroup()

  armspe <- data %>%
    dplyr::group_by(.data$Time) %>%
    dplyr::summarise(armspe = sum(.data$rmspe)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(armspe = mean(.data$armspe))

  list(rmspe = data, armspe = armspe$armspe)

}
