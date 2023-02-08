#-------------------------------------------------------------------------------
# Plots
# Created by: Ioannis Oikonomidis
#-------------------------------------------------------------------------------

#' @title Plot
#'
#' @param x an S4 object. The model of interest.
#' @param y not used.
#' @param seasons numeric. The seasons of choice to plot.
#' @param cumulative logical. Should the cumulative percentages be plotted?
#' @param ncol numeric. Number of rows in the plot layout.
#' @param nrow numeric. Number of columns in the plot layout.
#' @param save logical. Should the plot be saved?
#' @param dir character. The directory in which the plot will be saved.
#' @param file character. The file name.
#' @param width numeric. The width of the plot in inches.
#' @param height numeric. The height of the plot in inches.
#' @param ... extra arguments
#'
#' @return A plot, created with `ggplot2` and `ggpubr`. The plot is
#' returned wrapped inside `invisible()`.
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
#' report(object, name = "example_report", dir = getwd())
#' }
setGeneric("plot")

#' @rdname plot
setMethod("plot",
          signature  = c(x = "PersephoneModel"),
          definition = function(x,
                                seasons = 2002,
                                cumulative = TRUE,
                                save = FALSE,
                                dir = getwd(),
                                file = "plot.pdf",
                                width = 15,
                                height = 8) {

  # Save the plot
  if (save) pdf(file.path(dir, file), width = width, height = height)

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
          signature  = c(x = "PersephoneModelList"),
          definition = function(x,
                                ncol = NULL,
                                nrow = NULL,
                                save = FALSE,
                                dir = getwd(),
                                file = "plot.pdf",
                                width = 15,
                                height = 8,
                                ...) {

  list_plots <- lapply(x, plot, save = FALSE, ...)

  # Save the plot
  if (save) pdf(file.path(dir, file), width = width, height = height)

  print(do.call(ggpubr::ggarrange, c(list_plots, ncol = ncol, nrow = nrow)))

  # Close the device
  if (save) dev.off()

})

#' @title Plot Metrics
#'
#' @description
#' Create plots of the model metrics.
#'
#' @param object an S4 object. The model of interest.
#' @param title character. The plot title.
#' @param ncol numeric. Number of rows in the plot layout.
#' @param nrow numeric. Number of columns in the plot layout.
#' @param save logical. Should the plot be saved?
#' @param dir character. The directory in which the plot will be saved.
#' @param file character. The file name.
#' @param width numeric. The width of the plot in inches.
#' @param height numeric. The height of the plot in inches.
#' @param print_plots logical. Should the plots be printed?
#' @param ... extra arguments.
#'
#' @return A plot, created with `ggplot2` and `ggpubr`. The plot is
#' returned wrapped inside `invisible()`.
#'
#' @importFrom ggplot2 ggplot geom_point geom_line aes labs theme element_text theme_minimal
#' @importFrom ggpubr ggarrange text_grob annotate_figure
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
setGeneric("plot_metrics", signature = c("object"),
           function(object, ...) { standardGeneric("plot_metrics") })

#' @rdname plot_metrics
setMethod("plot_metrics",
          signature  = c(object = "PersephoneModel"),
          definition = function(object,
                                title = NULL,
                                save = FALSE,
                                dir = getwd(),
                                file = "plot.pdf",
                                width = 15,
                                height = 8,
                                print_plots = TRUE) {

  # Save the plot
  if (save) pdf(file.path(dir, file), width = width, height = height)

  if (is.null(title)) {
    title <- paste0(object@crop, " Root Mean Square Prediction Error")
  }

  plot_rmspe <- ggplot2::ggplot(data = object@metrics$rmspe) +
      ggplot2::geom_bar(ggplot2::aes(x = .data$Time, y = .data$rmspe, fill = .data$Stage), stat = "identity") +
      ggplot2::labs(title = title, y = "Error", x = "Time") +
      ggplot2::geom_label(x = Inf, y = Inf, hjust = 1, vjust = 1, label = paste0("ARMSPE = ", round(object@metrics$armspe, 3))) +
      ggplot2::theme_minimal()

  if (print_plots) {
    plot(plot_rmspe)
  }

  # Close the device
  if (save) dev.off()

  # Return the plot
  invisible(plot_rmspe)

})

#' @rdname plot_metrics
setMethod("plot_metrics",
          signature  = c(object = "PersephoneModelList"),
          definition = function(object,
                                ncol = NULL,
                                nrow = NULL,
                                save = FALSE,
                                dir = getwd(),
                                file = "plot.pdf",
                                width = 15,
                                height = 8,
                                ...) {

  # Create the plots
  title <- ggpubr::text_grob("Root Mean Square Prediction Error", just = c("right", "top"), size = 14)
  list_plots <- lapply(object, plot_metrics, save = FALSE, print_plots = FALSE, ...)
  plots <- do.call(ggpubr::ggarrange, c(list_plots, ncol = ncol, nrow = nrow))

  # Save the plot
  if (save) pdf(file.path(dir, file), width = width, height = height)

  print(plots)

  # Close the device
  if (save) dev.off()

})

#' @title Plot carousel
#'
#' @description
#' Create an interactive carousel of plots that can be added to the model
#' reports.
#'
#' @param fun function. A plot function.
#' @param x an object of class `PersephoneModel` or `PersephoneModelList`.
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
