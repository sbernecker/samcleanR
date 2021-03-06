% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/calcSubscale.R
\name{calcSubscale}
\alias{calcSubscale}
\title{This scores a given subscale and outputs a data frame with one column containing all subjects' scores.}
\usage{
calcSubscale(dataFrame, forwItems, revItems = NULL, revInt = NULL,
  idxOfSubj = 1)
}
\arguments{
\item{dataFrame}{A data frame containing the appropriate measure items.}

\item{forwItems}{A vector of the items that should be forward-scored.}

\item{revItems}{A vector of the items that should be reverse scored. Default is NULL in case there are no reverse-scored items.}

\item{revInt}{A number (usually an integer, but does not have to be integer type) that will be used to reverse score the reverse-scored items by subtracting the subject-entered item value from the revInt. For example, if the items are on a 1-7 Likert scale, the revInt would be 8 (because 8-1 is 7, 8-7 is 1, etc.); if the items are on a 0-4 Likert scale, the revInt would be 4. In general the revint is the maximum of the Likert scale if it starts at zero, or is 1 greater than the max if it starts at 1. Default is NULL in case there are no reverse scored items.}

\item{idxOfSubj}{A numeric index giving the position in the original data frame that contains the subject IDs. Default is 1. If the subject IDs are the row names of the original data frame, change this to 0.}
}
\value{
A data frame with two columns: one containing subject IDs, the other containing the scores for each subject.
}
\description{
This scores a given subscale and outputs a data frame with one column containing all subjects' scores.
}

