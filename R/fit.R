#-------------------------------------------------------------------------------
# Model Fitting
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Progress model fitting
#'
#' @description
#' Fit the model of interest. The model is fitted on the `model` and `data`
#' already inside the `object`, unless extra arguments are provided, in which
#' case the model is first updated and then fitted.
#'
#' @param object an object of class `ProgressModel` or `ProgressModelList`.
#' @param ... extra arguments passed to `update()`.
#'
#' @return An object identical to `object`, with the `model` and `fitted`
#' slot(s) altered.
#'
#' @import dplyr
#' @importFrom lme4 glmer glmerControl
#' @importFrom ordinal clm clm2 clmm2 clmm2.control
#' @importFrom rsample form_pred
#' @importFrom tidyr pivot_wider
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
#'
#' # Fit
#' object <- fit(object)
#'
#' # Plot
#' plot(object, cumulative = TRUE, seasons = 2002)
#'
#' # Predict
#' predict(object, data_progress)
#'
#' # Evaluate
#' object <- evaluate(object, maxsam = 100, seed = 1)
#' plot_metrics(object)
#'
#' # Summarize
#' summary(object)
#'
#' # Report
#' report(object, name = "example_report", path = getwd())
#' }
setGeneric("fit", signature = c("object"),
           function(object, ...) { standardGeneric("fit") })

#' @rdname fit
setMethod("fit",
          signature = c(object = "ProgressModelList"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Model Fitting
  object <- lapply(object, fit)
  class(object) <- "ProgressModelList"
  object

})

#' @rdname fit
setMethod("fit",
          signature = c(object = "ProgressBM"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Model Fitting
  for (stage in get_stages(object)[-1]){
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
          signature = c(object = "ProgressBMM"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Model Fitting
  for (stage in get_stages(object)[-1]){
    data_stage <- dplyr::filter(object@data, .data$Stage == stage)
    data_stage$weights <- rep(100, nrow(data_stage))
    suppressWarnings(
      object@model[[stage]] <- lme4::glmer(formula  = object@formula,
                                           family   = stats::binomial(link = object@link),
                                           data     = data_stage,
                                           weights  = weights,
                                           nAGQ     = object@nAGQ)
    )
  }

  # Return the object
  object@fitted <- predict(object, object@data)
  object

})

#' @rdname fit
setMethod("fit",
          signature = c(object = "ProgressCLM"),
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

#' @rdname fit
setMethod("fit",
          signature = c(object = "ProgressCLMM"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  object@data$weights <- round(100*object@data$Percentage)
  object@data$Season <- factor(object@data$Season)

  # Model Fitting
  # object@model <- eval(bquote(
  #   ordinal::clmm(formula  = .(formula(object@formula)),
  #                 link      = .(object@link),
  #                 threshold = .(object@threshold),
  #                 data      = object@data,
  #                 weights   = weights,
  #                 Hess      = TRUE,
  #                 nAGQ      = .(object@nAGQ),
  #                 control   = ordinal::clmm.control(method = "nlminb",
  #                                                   maxIter = 200,
  #                                                   gradTol = 1e-3,
  #                                                   maxLineIter = 200,
  #                                                   innerCtrl = "noWarn"))))

  object@model <- eval(bquote(
    ordinal::clmm2(location  = .(formula(object@formula)),
                   random    = Season,
                   link      = .(object@link),
                   data      = object@data,
                   weights   = weights,
                   Hess      = TRUE,
                   nAGQ      = .(object@nAGQ),
                   control   = ordinal::clmm2.control(method = "ucminf"))))

  # Return the object
  object@fitted <- predict(object, object@data)
  object

})

#' @rdname fit
setMethod("fit",
          signature = c(object = "ProgressSRF"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Model Fitting
  for (stage in get_stages(object)[-1]){
    data_stage <- dplyr::filter(object@data, .data$Stage == stage)
    object@model[[stage]] <- randomForestSRC::rfsrc(formula    = formula(object@formula),
                                                    data       = data_stage,
                                                    ntree      = object@ntree,
                                                    mtry       = object@mtry,
                                                    nodesize   = object@nodesize,
                                                    membership = !(object@scaled),
                                                    samptype   = object@samptype)

  }

  # Return the object
  object@fitted <- predict(object, object@data)
  object

})

#' @rdname fit
setMethod("fit",
          signature = c(object = "ProgressMRF"),
          definition = function(object, ...) {

  # Update object
  update(object, ...)

  # Model Fitting
  stagenames <- levels(get_stages(object))
  formula <- randomForestSRC::get.mv.formula(stagenames)

  # Extract covariates
  covars <- rsample::form_pred(formula(object@formula))

  # Bind global variables
  Stage <- Percentage <- NULL

  data <- object@data %>%
    dplyr::select(-c("CumPercentage")) %>%
    tidyr::pivot_wider(names_from = Stage, values_from = Percentage) %>%
    dplyr::select(stagenames, covars)

  object@model <- randomForestSRC::rfsrc(formula      = formula,
                                         data         = data,
                                         ntree        = object@ntree,
                                         mtry         = object@mtry,
                                         nodesize     = object@nodesize,
                                         splitrule    = object@splitrule,
                                         samptype     = object@samptype,
                                         membership   = !(object@scaled),
                                         seed         = object@seed
  )

  # Return the object
  object@fitted <- predict(object, object@data)
  object

})
