% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/response.R
\name{setupResponse}
\alias{setupResponse}
\title{Set up response}
\usage{
setupResponse(req, routes)
}
\arguments{
\item{req}{The request triggering the response setup.}

\item{routes}{The configured routes.}
}
\value{
An empty request with middlewares attached.
}
\description{
Set up an empty response to be handled by the attached middlewares.
}
