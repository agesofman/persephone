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
#' @param object an object of class `ProgressModel` or `ProgressModelList`.
#' @param name character. The html file name.
#' @param path character. The path to store the html report.
#' @param test Passed to `persephone::evaluate()`.
#' @param maxsam Passed to `persephone::evaluate()`.
#' @param seed Passed to `persephone::evaluate()`.
#' @param ... extra arguments.
#'
#' @return nothing. The html report(s) is (are) created and stored.
#'
#' @importFrom rmarkdown render
#' @importFrom kableExtra kable_styling
#' @import rmdformats
#' @export
#'
#' @inherit fit examples
setGeneric("report", signature = c("object"),
           function(object, ...) { standardGeneric("report") })

#' @rdname report
setMethod("report",
          signature  = c(object = "ProgressModel"),
          definition = function(object,
                                name = NULL,
                                path = cronus::get_path_hermes(),
                                test = 0.25,
                                maxsam = 500,
                                seed = 1){

  # Get the directories
  path_input <- system.file("report.Rmd", package = 'persephone')
  dir_output <- file.path(path, "progress", "report",
                          get_region(object), tolower(get_crops(object)), class(object))
  dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)

  # Get name
  if (is.null(name)) {
    name <- get_label(object)
    if (is.null(name)) {
      name <- "progress_report"
    }
  }

  # Create the report
  rmarkdown::render(input = path_input,
                    output_file = file.path(dir_output, paste0(name, ".html")),
                    params = list(object = object,
                                  test = test,
                                  maxsam = maxsam,
                                  seed = seed),
                    quiet = TRUE)

})

#' @rdname report
setMethod("report",
          signature  = c(object = "ProgressModelList"),
          definition = function(object, ...){

  lapply(object, report, ...)

})
