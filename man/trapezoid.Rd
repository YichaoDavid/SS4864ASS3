% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trapezoid.R
\name{trapezoid}
\alias{trapezoid}
\title{Trapezoid Rule}
\usage{
trapezoid(f, range, n)
}
\arguments{
\item{f}{a function with a single argument x}

\item{range}{a vector specifying the lower and upper end points of the range of integration (a and b)}

\item{n}{the number of intervals for the Trapezoid approximation}
}
\value{
a single value which is the Trapezoid approximation to the integral
}
\description{
Trapezoid Rule
}
\examples{
trapezoid(function(x){x^2},c(1,20),200)
2666.362
}
