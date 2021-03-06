% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nems_s.R
\name{baked_goods_cost}
\alias{baked_goods_cost}
\title{Compute baked goods cost points}
\usage{
baked_goods_cost(healthier_baked_goods_price, regular_baked_goods_price)
}
\arguments{
\item{healthier_baked_goods_price}{The price of lower fat baked goods (fat content).}

\item{regular_baked_goods_price}{The price of normal fat baked goods.}
}
\value{
The NEMS-S points associated with baked goods price.
}
\description{
This function takes in the cost of healthier and regular baked goods and compares them to return a NEMS-S score for the cost.
}
\details{
This function implements the scoring method described in Table 4.1 of Lundsford (2016). "Low-fat baked goods" are baked goods with less than or equal to 3 grams of fat per serving.
}
\examples{
healthier_baked_goods_price <- rnorm(10,3.2,.5)
regular_baked_goods_price <- rnorm(10,3.0,.3)
}
