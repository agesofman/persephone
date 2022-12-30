#-------------------------------------------------------------------------------
# Model Report
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' Create reports
#'
#' @param object S4 object. The model of interest.
#' @param name character. The html file name.
#' @param path character. The path to store the html report.
#' @param bell numeric. A sound notifying the user that the report(s) is (are) ready.
#' @param ... extra arguments.
#'
#' @return nothing. The html report(s) is (are) created and stored.
#' @importFrom rmarkdown render
#' @importFrom beepr beep
#' @importFrom kableExtra kable_styling
#' @import rmdformats
#' @export
#'
#' @examples
#' \dontrun{
#' report(object, name = "myreport", path = getwd())
#' }
setGeneric("report", signature = c("object"),
           function(object, ...) { standardGeneric("report") })

#' @rdname report
setMethod("report",
          signature  = c(object = "PersephoneModel"),
          definition = function(object, name, path = getwd(), bell = 2){

  # Get the directories
  path_input <- system.file("report.Rmd", package = 'persephone')
  dir_output <- file.path(path, "persephone", "output", "reports", object@region@name, tolower(object@crop), class(object))
  dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)

  # Create the report
  rmarkdown::render(input = path_input,
                    output_file = file.path(dir_output, paste0(name, ".html")),
                    params = list(object = object))

  # Ring the bell
  if (!is.null(bell)) {
    beepr::beep(bell)
  }

})

#' @rdname report
setMethod("report",
          signature  = c(object = "PersephoneModelList"),
          definition = function(object, name, path = getwd(), bell = 2){

  lapply(object, report, name = name, path = path, bell = NULL)

  # Ring the bell
  if (!is.null(bell)) {
    beepr::beep(bell)
  }

})
