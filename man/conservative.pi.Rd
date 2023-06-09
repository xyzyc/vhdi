% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conservative.pi.R
\name{conservative.pi}
\alias{conservative.pi}
\title{Conservative prediction interval}
\usage{
conservative.pi(x, alpha)
}
\arguments{
\item{x}{data}

\item{alpha}{nominal error rate}
}
\description{
Conservative prediction interval
}
\examples{
\dontrun{
example_data <- rnorm(500, 0, 5)
conservative.pi(example_data, alpha = 0.01)
}

}
