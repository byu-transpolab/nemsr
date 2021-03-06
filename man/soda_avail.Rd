% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nems_s.R
\name{soda_avail}
\alias{soda_avail}
\title{Compute the availability points associated with diet soda}
\usage{
soda_avail(diet_soda_varieties)
}
\arguments{
\item{diet_soda_varieties}{The number of diet sodas available.}
}
\value{
The NEMS-S points associated with availability of diet soda.
}
\description{
This function takes in the number of varieties of diet sodas available and returns the NEMS-S points associated with availability.
}
\details{
This function implements the scoring method described in Measure 8 of the NEMS-S Protocol. "Diet soda" has 0 kcal per serving.
}
\examples{
diet_soda_varieties <- sample(0:3, 10)
soda_avail(diet_soda_varieties)
}
