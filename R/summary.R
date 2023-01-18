#-------------------------------------------------------------------------------
# Model Summary
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Persephone model summary
#'
#' @description
#' Print a model summary.
#'
#' @param object an object of class `PersephoneModel` or `PersephoneModelList`.
#' @param ... extra arguments.
#'
#' @return an object of class `PersephoneModel` or `PersephoneModelList`.
#'
#' @importFrom ordinal nominal_test scale_test
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
#'                region = region,
#'                crop = "Corn",
#'                data = progress_ne$Corn,
#'                formula = "CumPercentage ~ Time + agdd") # PersephoneModel
#'
#' # Create another model
#' object2 <- new("PersephoneCumLink",
#'                region = region,
#'                crop = "Soybeans",
#'                data = progress_ne$Soybeans,
#'                formula = "Stage ~ Time + agdd + adayl") # PersephoneModel
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
setGeneric("summary")

#' @rdname summary
setMethod("summary",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, ...) {

            # Model summary
            x <- lapply(object, summary)

          })

#' @rdname summary
setMethod("summary",
          signature = c(object = "PersephoneQuasiBin"),
          definition = function(object, ...) {

  dash_simple <- paste0(rep("-", 50))
  dash_double <- paste0(rep("=", 70))

  cat("General Information \n\n")
  cat("Region:", object@region@name, "\n")
  cat("Crop:", object@crop, "\n")
  cat("Formula:", object@formula, "\n\n")

  if (!is.null(object@model)) {
    stages <- as.character(get_stages(object))
    for (i in seq_along(stages)) {
      cat(dash_simple, "\n\n", sep = "")
      cat("Stage:", stages[i], "\n")
      print(summary(object@model[[i]]))
    }
    cat(dash_double, "\n\n", sep = "")
  }

})

#' @rdname summary
setMethod("summary",
          signature = c(object = "PersephoneCumLink"),
          definition = function(object, ...) {

  dash_simple <- paste0(rep("-", 50))
  dash_double <- paste0(rep("=", 70))

  cat("General Information \n\n")
  cat("Region:", object@region@name, "\n")
  cat("Crop:", object@crop, "\n")
  cat("Formula:", object@formula, "\n")
  cat("Nominal:", object@nominal, "\n")
  cat("Scale:", object@scale, "\n\n")

  if (!is.null(object@model)) {
    x <- summary(object@model)
    print(x$info)
    cat("\n", x$message, "\n")
    cat("\n", dash_simple, "\n\n", sep = "")

    cat("Model Coefficients \n")
    print(x$coefficients)
    cat("\n", dash_simple, "\n\n", sep = "")

    print(ordinal::nominal_test(object@model))
    cat("\n", dash_simple, "\n\n", sep = "")

    print(ordinal::scale_test(object@model))
    cat("\n", dash_double, "\n\n", sep = "")
  }

})
