#-------------------------------------------------------------------------------
# Model Fitting
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Persephone model fitting
#'
#' @description
#' Fit the model of interest. The model is fitted on the `model` and `data`
#' already inside the `object`, unless extra arguments are provided, in which
#' case the model is first updated and then fitted.
#'
#' @param object an object of class `PersephoneModel` or `PersephoneModelList`.
#' @param ... extra arguments passed to `update()`.
#'
#' @return an object of class `PersephoneModel` or `PersephoneModelList`.
#'
#' @import dplyr
#' @importFrom ordinal clm
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
          signature = c(object = "PersephoneQuasiBin"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Model Fitting
  for (stage in get_stages(object)){
    data_stage <- dplyr::filter(object@data, .data$Stage == stage)
    data_stage$weights <- rep(100, nrow(data_stage))
    suppressWarnings(
      object@model[[stage]] <- glm(formula  = object@formula,
                                   family   = stats::quasibinomial(link = object@link),
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
    ordinal::clm(formula   = .(formula(object@formula)),
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
