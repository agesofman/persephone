#' Corn and Soybeans progress data for Nebraska
#'
#' A subset of data from the USDA NASS Quick Stats crop progress product. The
#' dataset concerns 2 crops, corn and soybeans, for 20 years, from 2002 to 2021.
#'
#' @format ## `progress_ne`
#' A list with two named elements, `Corn` and `Soybeans`. Each one is a
#' data.frame with columns:
#' \describe{
#'   \item{Crop}{Crop name}
#'   \item{Stage}{Phenological stage}
#'   \item{Season}{Cultivation season / year}
#'   \item{Time}{Integer time-step within season}
#'   \item{Date}{Full date}
#'   \item{CumPercentage}{Accumulated crop progress percentages}
#'   \item{Percentage}{Crop progress percentages}
#'   \item{adayl}{Accumulated daylight}
#'   \item{agdd}{Accumulated growing degree days}
#' }
#' @source <https://quickstats.nass.usda.gov/>
"progress_ne"
