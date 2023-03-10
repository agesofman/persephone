% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update.R
\name{update}
\alias{update}
\alias{update,ProgressModel-method}
\alias{update,ProgressModelList-method}
\title{Update Model}
\usage{
update(object, ...)

\S4method{update}{ProgressModel}(object, ...)

\S4method{update}{ProgressModelList}(object, ...)
}
\arguments{
\item{object}{an object of class \code{ProgressModel} or \code{ProgressModelList}.
whose slots are to be updated.}

\item{...}{extra arguments.}
}
\value{
an object of class \code{ProgressModel} or \code{ProgressModelList}.
}
\description{
Update one or more slots of a \code{ProgressModel} object.
}
\examples{
\dontrun{
# Create a Region object
library(cronus)
region <- Region(name = "nebraska", type = "us state",
                 div = c(country = "United States", state = "Nebraska"))

object <- new("ProgressCLM", region = region, crop = "Corn", model = "A")
object <- update(object, region = region, crop = "Soybeans", model = "B")
}
}
