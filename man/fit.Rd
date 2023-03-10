% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.R
\name{fit}
\alias{fit}
\alias{fit,ProgressModelList-method}
\alias{fit,ProgressBM-method}
\alias{fit,ProgressBMM-method}
\alias{fit,ProgressCLM-method}
\alias{fit,ProgressCLMM-method}
\alias{fit,ProgressSRF-method}
\alias{fit,ProgressMRF-method}
\title{Progress model fitting}
\usage{
fit(object, ...)

\S4method{fit}{ProgressModelList}(object, ...)

\S4method{fit}{ProgressBM}(object, ...)

\S4method{fit}{ProgressBMM}(object, ...)

\S4method{fit}{ProgressCLM}(object, ...)

\S4method{fit}{ProgressCLMM}(object, ...)

\S4method{fit}{ProgressSRF}(object, ...)

\S4method{fit}{ProgressMRF}(object, ...)
}
\arguments{
\item{object}{an object of class \code{ProgressModel} or \code{ProgressModelList}.}

\item{...}{extra arguments passed to \code{update()}.}
}
\value{
An object identical to \code{object}, with the \code{model} and \code{fitted}
slot(s) altered.
}
\description{
Fit the model of interest. The model is fitted on the \code{model} and \code{data}
already inside the \code{object}, unless extra arguments are provided, in which
case the model is first updated and then fitted.
}
\examples{
\dontrun{
# Create a Region object
library(cronus)
region <- Region(name = "nebraska", type = "us state",
                 div = c(country = "United States", state = "Nebraska"))

# Create a model
object1 <- new("ProgressBM",
               region = region,
               crop = "Corn",
               data = data_progress$Corn,
               formula = "CumPercentage ~ Time + agdd") # ProgressModel

# Create another model
object2 <- new("ProgressCLM",
               region = region,
               crop = "Soybeans",
               data = data_progress$Soybeans,
               formula = "Stage ~ Time + agdd + adayl") # ProgressModel

# Concatenate the models
object <- c(object1, object2) # ProgressModelList

# Fit
object <- fit(object)

# Plot
plot(object, cumulative = TRUE, seasons = 2002)

# Predict
predict(object, data_progress)

# Evaluate
object <- evaluate(object, maxsam = 100, seed = 1)
plot_metrics(object)

# Summarize
summary(object)

# Report
report(object, name = "example_report", path = getwd())
}
}
