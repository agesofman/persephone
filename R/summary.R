#-------------------------------------------------------------------------------
# Model Summary
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Persephone model summary
#'
#' @param object S4 object. The model of interest.
#' @param ... extra arguments.
#'
#' @return S4 object. The model of interest.
#' @importFrom ordinal nominal_test scale_test
#' @export
#'
#' @examples
#' \dontrun{
#' region <- c(name = "nebraska", type = "us state")
#' formula <- "Percentage ~ Time"
#' object <- new("PersephoneBinomial", region = region, formula = formula)
#' object <- fit(object, data = data)
#' summary(object)
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
          signature = c(object = "PersephoneBinomial"),
          definition = function(object, ...) {

          })

#' @rdname summary
setMethod("summary",
          signature = c(object = "PersephoneCumLink"),
          definition = function(object, ...) {

  x <- summary(object@model)
  dash_simple <- paste0(rep("-", 50))
  dash_double <- paste0(rep("=", 70))

  cat("General Information \n\n")
  cat("Formula:", object@formula, "\n")
  cat("Nominal:", object@nominal, "\n")
  cat("Scale:", object@scale, "\n\n")

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

})
