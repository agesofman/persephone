#-------------------------------------------------------------------------------
# Model Summary
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Progress model summary
#'
#' @description
#' Print a model summary.
#'
#' @param object an object of class `ProgressModel` or `ProgressModelList`.
#' @param ... extra arguments.
#'
#' @return an object of class `ProgressModel` or `ProgressModelList`.
#'
#' @importFrom ordinal nominal_test scale_test
#' @export
#'
#' @inherit fit examples
setGeneric("summary")

#' @rdname summary
setMethod("summary",
          signature = c(object = "ProgressModelList"),
          definition = function(object, ...) {

            # Model summary
            x <- lapply(object, summary)

          })

#' @rdname summary
setMethod("summary",
          signature = c(object = "ProgressBM"),
          definition = function(object, ...) {

  dash_simple <- paste0(rep("-", 50))
  dash_double <- paste0(rep("=", 70))

  cat("General Information \n\n")
  cat("Region:", object@region@name, "\n")
  cat("Crop:", object@crop, "\n")
  cat("Formula:", object@formula, "\n\n")

  if (!is.null(object@model)) {
    stages <- as.character(get_stages(object))[-1]
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
          signature = c(object = "ProgressBMM"),
          definition = function(object, ...) {

  dash_simple <- paste0(rep("-", 50))
  dash_double <- paste0(rep("=", 70))

  cat("General Information \n\n")
  cat("Region:", object@region@name, "\n")
  cat("Crop:", object@crop, "\n")
  cat("Formula:", object@formula, "\n\n")

  if (!is.null(object@model)) {
    stages <- as.character(get_stages(object))[-1]
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
          signature = c(object = "ProgressCLM"),
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
