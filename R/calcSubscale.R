#' This scores a given subscale and outputs a data frame with one column containing all subjects' scores.
#'
#' @param dataframe A data frame containing the appropriate measure items.
#' @param fornames A character vector of the names of the items that should be forward-scored.
#' @param revnames A character vector of the names of the items that should be reverse scored. Default is NULL in case there are no reverse-scored items.
#' @param revint A numeric scalar that will be used to reverse score the reverse-scored items by subtracting the entered item value from the revint. For example, if the items are on a 1-7 Likert scale, the revint would be 8 (because 8-1 is 7, 8-7 is 1, etc.); if the items are on a 0-4 Likert scale, the revint would be 4. In general the revint is the maximum of the Likert scale if it starts at zero, or is 1 greater than the max if it starts at 1. Default is NULL in case there are no reverse scored items.
#' @idxofsubj A numeric index giving the position in the original data frame that contains the subject IDs. Default is 1. If the subject IDs are the row names of the original data frame, change this to 0.
#' @return A data frame with two columns: one containing subject IDs, the other containing the scores for each subject.
#' @export

calcSubscale <- function(dataFrame, forwNames, revNames = NULL, revInt = NULL, idxOfSubj = 1){
  #creates a data frame just of the items to be forward scored; note that this does not include subject names
  forwScorDf <- dataFrame[,forwNames]
  #creates a data frame with one column of zeroes with a length corresponding to the number of subjects/observations
  revScorDf <- data.frame(zeroes = rep(0,times=nrow(dataFrame)))
  #checks if anything has been supplied to the revNames argument, and if so, checks whether the first element is something other than NA
  if(!is.null(revNames) && !is.na(revNames[1])){
    #creates a data frame just of the items to be reverse scored
      revScorDf <- dataFrame[,revNames]
      #reverse scores the reverse scored items by subtracting each value from the given number
      revScorDf <- revInt - revScorDf
  }
  #creates a data frame with two columns: for first column, just has subject IDs; for second column, sums across the forward and reverse scored items to create a total data frame (if there are no reverse-scored items, zero is added to each row)
  totalDf <- cbind(dataFrame[idxOfSubj], as.data.frame(rowSums(forwScorDf) + rowSums(revScorDf)))
  #returns the data frame
  return(totalDf)
}







