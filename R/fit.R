#-------------------------------------------------------------------------------
# Model Fitting
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Persephone model fitting
#'
#' @param object S4 object. The model of interest.
#' @param ... extra arguments passed to update().
#'
#' @return S4 object. The model of interest.
#' @export
#' @import dplyr
#' @importFrom nlme gls
#' @importFrom ordinal clm
#'
#' @examples
#' \dontrun{
#' region <- c(name = "nebraska", type = "us state")
#' formula <- "Percentage ~ Time"
#' object <- new("PersephoneBinomial", region = region, formula = formula)
#' object <- fit(object, data = data)
#' }
setGeneric("fit", signature = c("object"),
           function(object, ...) { standardGeneric("fit") })

#' @rdname fit
setMethod("fit",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Model Fitting
  object <- lapply(object, fit)
  class(object) <- "PersephoneModelList"
  object

})

#' @rdname fit
setMethod("fit",
          signature = c(object = "PersephoneBinomial"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Model Fitting
  for (stage in get_stages(object)){
    data_stage <- dplyr::filter(object@data, .data$Stage == stage)
    data_stage$weights <- rep(100, nrow(data_stage))
    suppressWarnings(
      object@model[[stage]] <- glm(formula  = object@formula,
                                   family   = stats::binomial(link = object@link),
                                   data     = data_stage,
                                   weights  = weights)
    )
  }

  # Return the object
  object@fitted <- predict(object, object@data)
  object

})

#' @rdname fit
setMethod("fit",
          signature = c(object = "PersephoneCumLink"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Model Fitting
  object@model <- eval(bquote(
    ordinal::clm(formula   = .(as.formula(object@formula)),
                 nominal   = .(object@nominal),
                 scale     = .(object@scale),
                 link      = .(object@link),
                 threshold = .(object@threshold),
                 data      = .(object@data),
                 weights   = Percentage)))

  # Return the object
  object@fitted <- predict(object, object@data)
  object

})
