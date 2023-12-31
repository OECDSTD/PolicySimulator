% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation.R
\name{const_in_group}
\alias{const_in_group}
\title{Check whether target column is constant within each group}
\usage{
const_in_group(data, vector, groups_cols, target_col, omit = 0)
}
\arguments{
\item{data}{Data in which check should be done.}

\item{vector}{Column name that should be checked for constant values.}

\item{target_col}{Name of the target column in which results should be stored.}

\item{omit}{What values should be ignored in constants check.}

\item{group_cols}{Column defining groups.}
}
\description{
Check whether target column is constant within each group
}
