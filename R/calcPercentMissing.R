#' Computes the percentage of missing items in a subscale for each subject.
#'
#' @param dataFrame A data frame containing the appropriate measure items.
#' @param items A vector of the items for which missingness is to be computed.
#' @param idxOfSubj A numeric index giving the position in the original data frame that contains the subject IDs. Default is 1. If the subject IDs are the row names of the original data frame, change this to 0.
#' @return A data frame with two columns: one containing subject IDs, the other containing the percent missing for each subject.
#' @export

calcPercentMissing <- function(dataFrame, items, idxOfSubj = 1){
  #creates a vector containing the percent of items that are missing for each subject by calling my function "percentMissing"
  missingPercent <- apply(dataFrame[,items], 1, percentMissing)
  #creates a data frame by binding together the subject name column with the missingness vector
  missingDf <- cbind(dataFrame[idxOfSubj], missingPercent)
  return(missingDf)
}
