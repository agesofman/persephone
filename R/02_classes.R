#-------------------------------------------------------------------------------
# Class definitions
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title `persephone` Model classes
#'
#' @description
#' Package `persephone` defines a number of model classes. The main functions
#' of the package receive a model object and edit its slots.
#'
#' @slot label character. The object's label.
#' @slot region an object of class Region, defined in package `cronus`. A
#' region of interest.
#' @slot data an object of class `Progress`, defined in package `cronus`. The
#' data on which the model will be fitted.
#' @slot model ANY. An object holding information about the fitting. Filled by
#' function `fit()`.
#' @slot fitted an object of class `Progress`, defined in package `cronus`.
#' The fitted data. Filled by function `fit()`.
#' @slot metrics an object of class `ProgressMetrics`. The model performance
#' metrics. Filled by function `evaluate()`.
#' @slot formula character. The model formula. See details.
#' @slot nominal character. The nominal effects. See details.
#' @slot scale character. The scale effects. See details.
#' @slot link character. The link function used.
#' @slot threshold character. The type of thresholds used. See details.
#' @slot nAGQ numeric. The number of quadrature points to be used in the
#' adaptive Gauss-Hermite quadrature approximation to the marginal likelihood.
#' See details.
#'
#' @details
#'
#' - ProgressBM is based on `glm()`.
#' - ProgressBMM is based on `lme4::glmer()`.
#' - ProgressCLM is based on `ordinal::clm()`.
#' - ProgressCLMm is based on `ordinal::clm2()`.
#'
#' @return An S4 object of the appropriate class.
#'
#' @importClassesFrom cronus Region
#' @importFrom cronus Progress ProgressList
#' @export
#'
#' @seealso [persephone::fit()], [persephone::predict()],
#' [persephone::evaluate()], [ordinal::clm()], [ordinal::clm2()]
#'
#' @examples
#' \dontrun{
#' # Create a Region object
#' library(cronus)
#' region <- Region(name = "nebraska", type = "us state",
#'                  div = c(country = "United States", state = "Nebraska"))
#'
#' # Create a model
#' object1 <- new("ProgressBM",
#'                region = region,
#'                crop = "Corn",
#'                data = data_progress$Corn,
#'                formula = "CumPercentage ~ Time + agdd") # ProgressModel
#'
#' # Create another model
#' object2 <- new("ProgressCLM",
#'                region = region,
#'                crop = "Soybeans",
#'                data = data_progress$Soybeans,
#'                formula = "Stage ~ Time + agdd + adayl") # ProgressModel
#'
#' # Concatenate the models
#' object <- c(object1, object2) # ProgressModelList
#' }
setClass("ProgressModel",
         slots = list(label   = "character",
                      region  = "Region",
                      crop    = "character",
                      data    = "Progress",
                      model   = "ANY",
                      fitted  = "Progress",
                      metrics = "list"),
         prototype = list(label   = "",
                          region  = cronus::Region(),
                          crop    = "",
                          data    = cronus::Progress(),
                          model   = NULL,
                          fitted  = cronus::Progress(),
                          metrics = list()))

setOldClass(c("ProgressModelList", "list"))

#' @rdname ProgressModel-class
setClass("ProgressBM",
         contains  = "ProgressModel",
         slots     = list(formula = "character",
                          link    = "character"),
         prototype = list(formula = "",
                          link = "logit"))

#' @rdname ProgressModel-class
setClass("ProgressBMM",
         contains  = "ProgressModel",
         slots     = list(formula = "character",
                          link    = "character",
                          nAGQ    = "numeric"),
         prototype = list(formula = "",
                          link = "logit",
                          nAGQ = 1))

#' @rdname ProgressModel-class
setClass("ProgressCLM",
         contains = "ProgressModel",
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

#' @rdname ProgressModel-class
setClass("ProgressCLMM",
         contains = "ProgressModel",
         slots = list(formula   = "character",
                      scale     = "character",
                      nominal   = "character",
                      random    = "character",
                      link      = "character",
                      threshold = "character",
                      nAGQ      = "numeric"),
         prototype = list(formula = "",
                          scale = "~ 1",
                          nominal = "~ 1",
                          random = "Season",
                          link = "logistic",
                          threshold = "flexible",
                          nAGQ = 1))

#' @rdname ProgressModel-class
setClass("ProgressSRF",
         contains = "ProgressModel",
         slots = list(formula   = "character",
                      ntree     = "numeric",
                      mtry      = "numeric",
                      nodesize  = "numeric",
                      scaled    = "logical",
                      samptype  = "character",
                      seed      = "numeric"),
         prototype = list(formula = "",
                          ntree = 100,
                          mtry  = 1,
                          nodesize = 5,
                          scaled = TRUE,
                          samptype = "swr",
                          seed = 1))


#' @rdname ProgressModel-class
setClass("ProgressMRF",
         contains = "ProgressModel",
         slots = list(formula   = "character",
                      ntree     = "numeric",
                      mtry      = "numeric",
                      splitrule = "character",
                      nodesize  = "numeric",
                      scaled    = "logical",
                      samptype  = "character",
                      seed      = "numeric"),
         prototype = list(formula = "",
                          ntree = 100,
                          mtry  = 1,
                          splitrule = "mv.mse",
                          nodesize = 5,
                          scaled = TRUE,
                          samptype = "swr",
                          seed = 1))
