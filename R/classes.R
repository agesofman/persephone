#-------------------------------------------------------------------------------
# Class Definitions
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title `persephone` Model Classes
#'
#' @description
#' `persephone` defines a number of model classes. Objects belonging to a
#'  model class can be passed to the main package functions for fitting and
#'  prediction.
#'
#' @slot region an object of class Region. A region of interest. See package
#' `cronus` for more details
#' @slot data data.frame. The data on which the model will be fitted.
#' @slot model ANY. An object holding information about the fitting.
#' @slot fitted data.frame. The fitted data.
#' @slot metrics list. A list of metrics calculated using `persephone::crossval()`.
#' @slot formula character. The model formula. See details.
#' @slot nominal character. The nominal effects. See details.
#' @slot scale character. The scale effects. See details.
#' @slot link character. The link function used.
#' @slot threshold character. The type of thresholds used. See details.
#'
#' @return An S4 object of the appropriate class.
#'
#' @details
#' Details on the `PersephoneCumLink` class:
#' Model fitting is based on `ordinal::clm()`. Information on arguments
#' `formula`, `nominal`, `scale`, `link` and `threshold` can be found there.
#'
#' @importClassesFrom cronus Region
#' @export
#'
#' @seealso [persephone::fit()], [persephone::predict()],
#' [persephone::crossval()], [ordinal::clm()]
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
#'              region = region,
#'             crop = "Corn",
#'             data = progress_ne$Corn,
#'             formula = "Stage ~ Time + agdd") # PersephoneModel
#'
#' # Create another model
#' object2 <- new("PersephoneCumLink",
#'             region = region,
#'             crop = "Soybeans",
#'             data = progress_ne$Soybeans,
#'             formula = "Stage ~ Time + agdd + adayl") # PersephoneModel
#'
#' # Concatenate the models
#' object <- c(object1, object2) # PersephoneModelList
#' }
setClass("PersephoneModel",
         slots = list(region  = "Region",
                      crop    = "character",
                      data    = "data.frame",
                      model   = "ANY",
                      fitted  = "data.frame",
                      metrics = "list"),
         prototype = list(region  = NULL,
                          crop    = "",
                          data    = data.frame(),
                          model   = NULL,
                          fitted  = data.frame(),
                          metrics = list()))

#' @rdname PersephoneModel-class
setClass("PersephoneQuasiBin",
         contains  = "PersephoneModel",
         slots     = list(formula = "character",
                          link    = "character"),
         prototype = list(formula = "",
                          link = "logit"))

#' @rdname PersephoneModel-class
setClass("PersephoneCumLink",
         contains = "PersephoneModel",
         slots = list(formula   = "character",
                      scale     = "character",
                      nominal   = "character",
                      link      = "character",
                      threshold = "character"),
         prototype = list(formula = "",
                          scale = "~ 1",
                          nominal = "~ 1",
                          link = "logit",
                          threshold = "flexible"))

setOldClass("PersephoneModelList")
setOldClass(c("PersephoneQuasiBinList", "PersephoneModelList"))
setOldClass(c("PersephoneCumLinkList", "PersephoneModelList"))
