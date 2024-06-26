% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wv_pivot_longer.R
\name{wv_pivot_flags_longer}
\alias{wv_pivot_flags_longer}
\title{Pivot flag columns}
\usage{
wv_pivot_flags_longer(dat, qc_test = "grossrange")
}
\arguments{
\item{dat}{Data frame of flagged wave data with the variables in a long
format and flags in wide format.}

\item{qc_test}{Flag columns to pivot.}
}
\value{
Returns \code{dat} with the qc_test flag columns pivoted to a long
  format.
}
\description{
Pivot flag columns
}
