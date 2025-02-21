% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aftable.R
\name{summary.aftable}
\alias{summary.aftable}
\title{Summarise An 'aftable' Object}
\usage{
\method{summary}{aftable}(object, ...)
}
\arguments{
\item{object}{An aftable-class object for which to get a summary.}

\item{...}{Other arguments to pass.}
}
\value{
\code{object} unaltered.
}
\description{
A concise result summary of an aftable-class object to see information
about the sheet content. Shows a numbered list of sheets with each tab title,
sheet type and table dimensions.
}
\examples{
# Print a concise summary of the aftable-class object
summary(demo_aftable)

# Alternatively, look at the structure
str(demo_aftable, max.level = 2)

}
