#-------------------------------------------------------------------------------
# Model Report
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Create reports
#'
#' @description
#' Create html reports that present the model performance, including, fitting,
#' predictions, evaluation metrics and corresponding plots.
#'
#' @param object an object of class `PersephoneModel` or `PersephoneModelList`.
#' @param name character. The html file name.
#' @param path character. The path to store the html report.
#' @param test Passed to `persephone::crossval()`.
#' @param maxsam Passed to `persephone::crossval()`.
#' @param seed Passed to `persephone::crossval()`.
#' @param ... extra arguments.
#'
#' @return nothing. The html report(s) is (are) created and stored.
#'
#' @importFrom rmarkdown render
#' @importFrom kableExtra kable_styling
#' @import rmdformats
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
#' object1 <- new("PersephoneBin",
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
#' report(object, name = "example_report", path = getwd())
#' }
setGeneric("report", signature = c("object"),
           function(object, ...) { standardGeneric("report") })

#' @rdname report
setMethod("report",
          signature  = c(object = "PersephoneModel"),
          definition = function(object, name = NULL, path = getwd(),
                                test = 0.25, maxsam = 500, seed = 1){

  # Get the directories
  path_input <- system.file("report.Rmd", package = 'persephone')
  dir_output <- file.path(path, "projects", "persephone", "out", "reports",
                          get_region(object), tolower(get_crops(object)), class(object))
  dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)

  # Get name
  if (is.null(name)) {
    name <- get_label(object)
  }

  # Create the report
  rmarkdown::render(input = path_input,
                    output_file = file.path(dir_output, paste0(name, ".html")),
                    params = list(object = object, test = test, maxsam = maxsam, seed = seed),
                    quiet = TRUE)

})

#' @rdname report
setMethod("report",
          signature  = c(object = "PersephoneModelList"),
          definition = function(object, name = NULL, path = getwd(),
                                test = 0.25, maxsam = 500, seed = 1){

  lapply(object, report, name = name, path = path,
         test = test, maxsam = maxsam, seed = seed)

})