% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prices_scores.R
\name{hot_dog_avail}
\alias{hot_dog_avail}
\title{Compute the availability points associated with hot dogs}
\usage{
hot_dog_avail(fat_free_hot_dogs, light_hot_dogs)
}
\arguments{
\item{fat_free_hot_dogs}{The number of varieties of fat free hot dogs (0g fat per serving).}

\item{light_hot_dogs}{The number of varieties of light hot dogs (<=7g fat per serving).}
}
\value{
the NEMS-S points associated with availability of fat free and light fat hot dogs.
}
\description{
This function takes in the number of varieties of fat free and light hot dogs and returns the NEMS-S points associated with availability.
}
\details{
This function implements the scoring method described in Measure 5 of the NEMS-S Protocol. "Fat free hot dogs" have 0 grams of fat per serving, and "light hot dogs" have less than or equal to 7 grams of fat per serving.
}
\examples{
fat_free_hot_dogs <- sample()
light_hot_dogs <- sample()
hot_dog_avail(fat_free_hot_dogs, light_hot_dogs)
}
