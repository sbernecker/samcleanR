% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/meanSubstitute.R
\name{meanSubstitute}
\alias{meanSubstitute}
\title{Substitutes a subject's item mean subscale score for missing values.}
\usage{
meanSubstitute(dataFrame, forwItems, revItems = NULL, revInt = NULL,
  criterion = 50)
}
\arguments{
\item{dataFrame}{A data frame containing the subscale to be scored.}

\item{forwItems}{A vector of the items that should be forward-scored.}

\item{revItems}{A vector of the items that should be reverse scored. Default is NULL in case there are no reverse-scored items.}

\item{revInt}{A number (usually an integer, but does not have to be integer type) that will be used to reverse score the reverse-scored items by subtracting the subject-entered item value from the revInt. Default is NULL in case there are no reverse scored items.}

\item{criterion}{A number indicating the maximum percent of items a subject can have missing in order to use mean substitution. Default is 50, but a warning is produced if the value is over 20 because there are more robust ways to deal with missing values than mean substitution! Maximum is 100*((number of items - 1)/number of items). Round down just to be safe.}
}
\value{
A data frame in which the NAs have been replaced with the item means for the given subscale (only for the subjects that meet the criterion).
}
\description{
Substitutes a subject's item mean subscale score for missing values.
}

