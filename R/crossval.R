#-------------------------------------------------------------------------------
# Cross-Validation
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Cross-Validation
#'
#' @description
#' Apply leave-group-out (or Monte Carlo) cross-validation.
#'
#' @param object an object of class `ProgressModel` or `ProgressModelList`.
#' @param fun function. The function to apply on the train-test split.
#' @param test numeric. The proportion of seasons to be used for testing
#' @param maxsam numeric. The maximum number of Monte - Carlo samples to use.
#' @param seed numeric. The seed to be used in Monte - Carlo sampling.
#' @param simplify logical. Should the resulting list be simplified into a
#' data.frame?
#' @param .id The data.frame identifier (see `dplyr::bind_rows`). Only used if
#' `simplify = TRUE`.
#' @param ... extra arguments.
#'
#' @return A list containing the calculated metrics.
#'
#' @importFrom cronus combinations
#' @export
#'
#' @inherit fit examples
setGeneric("crossval", signature = c("object"),
           function(object, ...) { standardGeneric("crossval") })

#' @rdname crossval
setMethod("crossval",
          signature  = c(object = "ProgressModel"),
          definition = function(object, fun = rmspe, test = 0.25, maxsam = 500, seed = 1, simplify = TRUE, .id = NULL) {

  # Combinations of training - testing seasons
  seasons <- get_seasons(object)
  mat_vseasons <- cronus::combinations(seasons, test, maxsam, seed)
  ncomb <- ncol(mat_vseasons)

  # Initialize result
  x <- list()

  # Create a progress bar object
  frm <- paste0("Processing ", get_label(object), " [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
  pb  <- progress::progress_bar$new(format = frm, total = ncomb, clear = FALSE)

  # Repeat for all combinations
  for(k in 1:ncomb) {

    # Progress bar
    pb$tick()

    # Get the training and testing subsets
    tdata <- dplyr::filter(object@data, .data$Season %in% setdiff(seasons, mat_vseasons[ , k]))
    vdata <- dplyr::filter(object@data, .data$Season %in% mat_vseasons[ , k])

    # Fit and predict
    fobject <- fit(object, data = tdata)
    pdata <- predict(fobject, pdata = vdata)

    # Compute fun for comb k
    x[[k]] <- fun(vdata, pdata)

  }

  # Concatenate the list into a data.frame
  if (simplify) {
    x <- dplyr::bind_rows(x, .id = .id)
  }

  # Return the object
  x

})

#' @rdname crossval
setMethod("crossval",
          signature  = c(object = "ProgressModelList"),
          definition = function(object, ...) {

  object <- lapply(object, crossval, ...)
  class(object) <- "ProgressModelList"
  object

})
