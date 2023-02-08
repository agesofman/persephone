#-------------------------------------------------------------------------------
# Model Comparison and Selection
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title contrast models
#'
#' @description Contrast models inside a `PersephoneModelList` object based on a
#' criterion of choice, such as AICc.
#'
#' @param object an object of class `PersephoneModelList`.
#' @param by character. Currently, only "AICc" is accepted.
#' @param save logical. Should the result be saved?
#' @param name character. The file name.
#' @param path character. The path to store the file.
#' @param ... extra arguments.
#'
#' @return If `by = "aicc"`, a list of objects of class `aictab`.
#'
#' @importFrom AICcmodavg aictab
#' @export
#'
#' @seealso [AICcmodavg::aictab()]
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
#' object2 <- new("PersephoneBin",
#'                region = region,
#'                crop = "Corn",
#'                data = progress_ne$Corn,
#'                formula = "Stage ~ Time + agdd + adayl") # PersephoneModel
#'
#' # Concatenate the models
#' object <- c(object1, object2) # PersephoneModelList
#'
#' # Fit
#' object <- fit(object)
#'
#' # Contrast
#' contrast(object)
#' }
setGeneric("contrast", signature = c("object"),
           function(object, ...) { standardGeneric("contrast") })

#' @rdname contrast
setMethod("contrast",
          signature = c(object = "PersephoneModelList"),
          definition = function(object, by = "AICc", save = TRUE,
                                name = "model_contrast", path = getwd(), ...) {

  # Gather the models
  classes <- get_class(object)
  models <- get_model(object)
  crops <- get_crops(object)
  labels <- get_label(object)
  region_name <- get_region(object)[1]
  list_contrast <- list()

  # Choose criterion function
  contrast_by <- paste0("contrast_", tolower(by))

  # Group by object class and crop
  for (clas in unique(classes)) {

    # Create list for each class
    list_contrast[[clas]] <- list()
    i <- which(clas == classes)

    for (crop in unique(crops[i])) {

      # Create sublist for each crop
      j <- which(crop == crops[i])
      args <- list(signa = object[[min(j)]],
                   models = models[j],
                   modnames = labels[j])

      # Call the contrasting function
      tab <- do.call(contrast_by, args)
      list_contrast[[clas]][[crop]] <- tab

      # Save
      if (save) {
        dir_output <- file.path(path, "projects", "persephone", "out", "contrasts",
                          region_name, tolower(crop), clas)
        dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)
        file <- file.path(dir_output, paste0(name, ".RData"))
        save(tab, file = file)
      }

    }
  }

  # Return the contrast list
  list_contrast

})

setGeneric("contrast_aicc", signature = c("signa", "models"),
           function(signa, models, ...) { standardGeneric("contrast_aicc") })

setMethod("contrast_aicc",
          signature = c(signa = "PersephoneBin", models = "list"),
          definition = function(signa, models, modnames, ...) {

  # Initialize
  list_aicc <- list()
  models_stage <- list()
  stages <- names(models[[1]])

  # Group by stage
  for (stage in stages) {

    # Gather all models for each stage
    for (k in seq_along(models)) {
      models_stage[[modnames[k]]] <- models[[k]][[stage]]
    }

    # Call the contrasting function
    list_aicc[[stage]] <- AICcmodavg::aictab(models_stage, modnames = modnames)

  }

  # Return the contrast list
  list_aicc

})

setMethod("contrast_aicc",
          signature = c(signa = "PersephoneCumLink", models = "list"),
          definition = function(signa, models, modnames, ...) {

  # Call the contrasting function
  AICcmodavg::aictab(models, modnames = modnames, ...)

})

setMethod("contrast_aicc",
          signature = c(signa = "PersephoneMixedBin", models = "list"),
definition = function(signa, models, modnames, ...) {

  # Initialize
  list_aicc <- list()
  models_stage <- list()
  stages <- names(models[[1]])

  # Group by stage
  for (stage in stages) {

    # Gather all models for each stage
    for (k in seq_along(models)) {
      models_stage[[modnames[k]]] <- models[[k]][[stage]]
    }

    # Call the contrasting function
    list_aicc[[stage]] <- AICcmodavg::aictab(models_stage, modnames = modnames)

  }

  # Return the contrast list
  list_aicc

})

setMethod("contrast_aicc",
          signature = c(signa = "PersephoneMixedCumLink", models = "list"),
          definition = function(signa, models, modnames, ...) {

  # Call the contrasting function
  AICcmodavg::aictab(models, modnames = modnames, ...)

})
