#-------------------------------------------------------------------------------
# Class Definitions
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Persephone Model Classes
#'
#' @slot region character. A region of interest.
#' @slot data list.
#' @slot model list.
#' @slot fitted list.
#' @slot formula character.
#' @slot correlation ANY.
#' @slot weights ANY.
#' @slot link character.
#' @slot nominal character.
#'
#' @return An S4 object of the appropriate class.
#' @importClassesFrom cronus Region
#' @export
#'
#' @examples
#' \dontrun{
#' object <- new("PersephoneCumLink")
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
setClass("PersephoneBinomial",
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
setOldClass(c("PersephoneBinomialList", "PersephoneModelList"))
setOldClass(c("PersephoneCumLinkList", "PersephoneModelList"))
