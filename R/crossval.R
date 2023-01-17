#-------------------------------------------------------------------------------
# Cross - Validation
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Cross - Validation
#'
#' @description
#' Calculate the model performance metrics using leave-group-out (or Monte
#' Carlo) cross-validation.
#'
#' @param object an object of class `PersephoneModel` or `PersephoneModelList`.
#' @param test numeric. The proportion of seasons to be used for testing
#' @param maxsam numeric. The maximum number of Monte - Carlo samples to use.
#' @param seed numeric. The seed to be used in Monte - Carlo sampling.
#' @param ... extra arguments.
#'
#' @return A list containing the calculated metrics.
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
setGeneric("crossval", signature = c("object"),
           function(object, ...) { standardGeneric("crossval") })

#' @rdname crossval
setMethod("crossval",
          signature  = c(object = "PersephoneModel"),
          definition = function(object, test = 0.25, maxsam = 500, seed = 1) {

  # Combinations of training - testing seasons
  seasons <- get_seasons(object)
  mat_vseasons <- get_combinations(seasons, test, maxsam, seed)
  ncomb <- ncol(mat_vseasons)

  # Initialize RMSPE
  df_rmspe <- data.frame()

  # Create a progress bar object
  frm <- paste0("Processing ", object@crop, " [:bar] :percent | Remaining: :eta | Elapsed: :elapsedfull")
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

    # Compute RMSPE for comb k
    df_rmspe <- rbind(df_rmspe, rmspe(vdata, pdata))

  }

  # Compute MRMSPE
  object@metrics <- armspe(df_rmspe)

  # Return the object
  object

})

#' @rdname crossval
setMethod("crossval",
          signature  = c(object = "PersephoneModelList"),
          definition = function(object, ...) {

  object <- lapply(object, crossval, ...)
  class(object) <- "PersephoneModelList"
  object

})

#' Get Combinations
#'
#' @param x a vector with the combination elements.
#' @param prop numeric. the proportion of elements to be drawn.
#' @param maxsam numeric. maximum sample of combinations
#' @param seed numeric. `set.seed()` argument.
#'
#' @return matrix. Each column of the matrix is a different combination.
#' @export
#'
#' @examples
#' \dontrun{
#' get_combinations(1:10, 0.25, maxsam = 7, seed = 2938)
#' }
get_combinations <- function(x, prop, maxsam = Inf, seed = 1) {

  x <- unique(x)
  mat_comb <- combn(x, round(prop * length(x)))
  n <- ncol(mat_comb)
  if (maxsam < n) {
    set.seed(seed)
    mat_comb <- mat_comb[ , sample(1:n, size = maxsam)]
  }
  mat_comb
}
