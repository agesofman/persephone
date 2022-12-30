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
#' @return A plot, created with \code{ggplot2} and \code{ggpubr}. The plot is
#' returned wrapped inside `invisible()`.
#' @export
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

  # Choose variable
  if (cumulative) {
    y = "CumPercentage"
  } else {
    y = "Percentage"
  }

  # Create the plots
  data <- dplyr::filter(data, .data$Season %in% seasons)
  fitted <- dplyr::filter(fitted, .data$Season %in% seasons)
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = data,
                        ggplot2::aes(x = .data$Date,
                                     y = .data[[y]],
                                     col = .data$Stage)) +
    ggplot2::geom_line(data = fitted,
                       ggplot2::aes(x = .data$Date,
                                    y = .data[[y]],
                                    col = .data$Stage)) +
    ggplot2::labs(title = create_title(x@crop, seasons), y = "Percentage", x = "Time") +
    ggplot2::theme_minimal(base_size = 18) +
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
#' @description Create plots of the model metrics.
#'
#' @param object an S4 object. The model of interest.
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
#' @return A plot, created with \code{ggplot2} and \code{ggpubr}. The plot is
#' returned wrapped inside `invisible()`.
#'
#' @export
#' @importFrom ggplot2 ggplot geom_point geom_line aes labs theme element_text theme_minimal
#' @importFrom ggpubr ggarrange text_grob annotate_figure
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

#' Plot carousel
#'
#' @param fun function. A plot function.
#' @param x S4 object. A PersephoneModel.
#' @param ... extra arguments.
#'
#' @return Same as the function \code{slickR::slickR()}.
#' @importFrom svglite xmlSVG
#' @importFrom slickR slickR
#' @export
carousel <- function(fun, x, ...) {
  plotsToSVG <- list()
  seasons <- get_seasons(x)
  for (i in 1:length(seasons)) {
    plotsToSVG[[i]] <- svglite::xmlSVG(code = {fun(x = x, seasons = seasons[i], ...)},
                                       standalone = TRUE, width = 14, height = 6.5)
  }
  slickR::slickR(plotsToSVG, width = "100%")
}

#' Create plot title
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
