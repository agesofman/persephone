#-------------------------------------------------------------------------------
# Plots
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Plot
#'
#' @param x an object of class `ProgressModel` or `ProgressModelList`.
#' @param y not used.
#' @param seasons numeric. The seasons of choice to plot.
#' @param cumulative logical. Should the cumulative percentages be plotted?
#' @param save logical. Should the plot be saved?
#' @param path character. The directory in which the plot will be saved.
#' @param file character. The file name.
#' @param width numeric. The width of the plot in inches.
#' @param height numeric. The height of the plot in inches.
#' @param ... extra arguments
#'
#' @return A plot, created with `ggplot2` The plot is returned wrapped inside
#' `invisible()`.
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_point geom_line aes labs theme_minimal theme element_text margin
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
#' object <- crossval(object, maxsam = 100, seed = 1)
#' plot_rmspe(object)
#'
#' # Summarize
#' summary(object)
#'
#' # Report
#' report(object, name = "example_report")
#' }
setGeneric("plot")

#' @rdname plot
setMethod("plot",
          signature  = c(x = "ProgressModel"),
          definition = function(x,
                                seasons = 2002,
                                cumulative = TRUE,
                                save = FALSE,
                                path = cronus::get_path_hermes(),
                                file = "plot.pdf",
                                width = 15,
                                height = 8) {

  # Save the plot
  if (save) {
    dir_output <- file.path(path, "progress", "plot", "fit",
                            get_region(x), tolower(get_crops(x)), class(x))
    dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)
    pdf(dir_output, width = width, height = height)
  }

  # Get slots
  data <- x@data
  fitted <- x@fitted

  # Get seasons
  if (is.null(seasons)) {
    seasons <- get_seasons(x)
  }
  data <- dplyr::filter(data, .data$Season %in% seasons)

  # Choose variable
  if (cumulative) {
    y <- "CumPercentage"
  } else {
    y <- "Percentage"
  }

  # Initialize plot
  p <- ggplot2::ggplot()

  # Plot fitted values
  if (nrow(fitted) > 0) {
    fitted <- dplyr::filter(fitted, .data$Season %in% seasons)
    p <- p +
      ggplot2::geom_point(data = data,
                          ggplot2::aes(x = .data$Date,
                                       y = .data[[y]],
                                       col = .data$Stage)) +
      ggplot2::geom_line(data = fitted,
                         ggplot2::aes(x = .data$Date,
                                      y = .data[[y]],
                                      col = .data$Stage))
  } else {
    p <- p +
      ggplot2::geom_point(data = data,
                          ggplot2::aes(x = .data$Date,
                                       y = .data[[y]],
                                       col = .data$Stage)) +
      ggplot2::geom_line(data = data,
                          ggplot2::aes(x = .data$Date,
                                       y = .data[[y]],
                                       col = .data$Stage))
  }

  # Customize plot
  p <- p +
    ggplot2::labs(title = create_title(x@crop, seasons), y = "Percentage", x = "Time") +
    ggplot2::theme_minimal(base_size = 18)
    ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(t = 10, b = 10, unit = "pt")))

  plot(p)

  # Close the device
  if (save) dev.off()

  # Return the plot
  invisible(p)

})

#' @rdname plot
setMethod("plot",
          signature  = c(x = "ProgressModelList"),
          definition = function(x, ...) {

    lapply(x, plot, ...)

})

#' @title Plot RMSPE
#'
#' @param object an object of class `ProgressModel`.
#' @param save logical. Should the plot be saved?
#' @param path character. The directory in which the plot will be saved.
#' @param file character. The file name.
#' @param width numeric. The width of the plot in inches.
#' @param height numeric. The height of the plot in inches.
#' @param ... extra arguments
#'
#' @return A plot, created with `ggplot2` The plot is returned wrapped inside
#' `invisible()`.
#'
#' @importFrom ggplot2 ggplot geom_bar geom_label aes theme_minimal
#' @export
#'
#' @inherit fit examples
setGeneric("plot_rmspe", signature = c("object"),
           function(object, ...) { standardGeneric("plot_rmspe") })

#' @rdname plot_rmspe
setMethod("plot_rmspe",
          signature  = c(object = "ProgressModel"),
          definition = function(object,
                                save = FALSE,
                                path = cronus::get_path_hermes(),
                                file = "plot.pdf",
                                width = 15,
                                height = 8) {

  # Save the plot
  if (save) {
    dir_output <- file.path(path, "progress", "plot", "rmspe",
                            class(object), get_region(object), tolower(get_crops(object)))
    dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)
    pdf(dir_output, width = width, height = height)
  }

  # Create the plot
  p <- ggplot2::ggplot(data = object@metrics$rmspe) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$Time, y = .data$rmspe, fill = .data$Stage), stat = "identity") +
    ggplot2::labs(title = "Root Mean Square Prediction Error", y = "Error", x = "Time") +
    ggplot2::geom_label(x = Inf, y = Inf, hjust = 1, vjust = 1, label = paste0("ARMSPE = ", round(object@metrics$armspe, 3))) +
    ggplot2::theme_minimal()

  print(p)

  # Close the device
  if (save) dev.off()

  # Return the plot
  invisible(p)

})

#' @rdname plot_rmspe
setMethod("plot_rmspe",
          signature  = c(object = "ProgressModelList"),
          definition = function(object, ...) {

  lapply(object, plot_rmspe, ...)

})

#' @title Plot model contrast
#'
#' @param x data.frame.
#' @param y an object of class `ProgressModel`.
#' @param save logical. Should the plot be saved?
#' @param path character. The directory in which the plot will be saved.
#' @param file character. The file name.
#' @param width numeric. The width of the plot in inches.
#' @param height numeric. The height of the plot in inches.
#' @param ... extra arguments
#'
#' @return A plot, created with `ggplot2` The plot is returned wrapped inside
#' `invisible()`.
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_jitter aes labs theme_minimal
#' @export
#'
#' @inherit fit examples
setGeneric("plot_contrast", signature = c("x", "y"),
           function(x, y, ...) { standardGeneric("plot_contrast") })

#' @rdname plot_contrast
setMethod("plot_contrast",
          signature  = c(x = "data.frame", y = "ProgressBM"),
          definition = function(x, y,
                                save = FALSE,
                                path = cronus::get_path_hermes(),
                                file = "plot.pdf",
                                width = 15,
                                height = 8) {

  # Save the plot
  if (save) {
    dir_output <- file.path(path, "progress", "plot", "rmspe",
                            class(y), get_region(y), tolower(get_crops(y)))
    dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)
    pdf(dir_output, width = width, height = height)
  }

  # Remove outliers
  min_aicc <- min(x$AICc)
  x <- dplyr::filter(x, AICc <= max(10 * min_aicc, 0.1 * min_aicc))

  # Create the plot
  p <- ggplot2::ggplot(x) +
    ggplot2::geom_jitter(ggplot2::aes(x = .data$Stage, y = .data$AICc, shape = .data$formula, col = .data$link),
                         size = 3, width = 0.3, height = 0) +
    ggplot2::labs(title = "Model Selection") +
    ggplot2::theme_minimal()

  print(p)

  # Close the device
  if (save) dev.off()

  # Return the plot
  invisible(p)

})

#' @title Plot carousel
#'
#' @description
#' Create an interactive carousel of plots that can be added to the model
#' reports.
#'
#' @param fun function. A plot function.
#' @param x an object of class `ProgressModel` or `ProgressModelList`.
#' @param ... extra arguments.
#'
#' @return Same as the function `slickR::slickR()`.
#'
#' @importFrom svglite xmlSVG
#' @importFrom slickR slickR
#' @export
carousel <- function(fun, x, ...) {
  plotsToSVG <- list()
  seasons <- get_seasons(x)
  for (i in seq_along(seasons)) {
    plotsToSVG[[i]] <- svglite::xmlSVG(code = {fun(x = x, seasons = seasons[i], ...)},
                                       standalone = TRUE, width = 14, height = 6.5)
  }
  slickR::slickR(plotsToSVG, width = "100%")
}

#' @title Create plot title
#'
#' @param crop character. A crop of interest.
#' @param seasons numeric. A vector of seasons.
#'
#' @return character. A title for the plot
#'
#' @examples
#' \dontrun{
#' create_title("Corn", 2002)
#' create_title("Corn", 2002:2009)
#' }
create_title <- function(crop, seasons) {

  if (length(seasons) == 1) {
    paste0(crop, " Progress for ", seasons)
  } else {
    paste0(crop, " Progress for ", min(seasons), " - ", max(seasons))
  }

}
