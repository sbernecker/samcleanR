#' Substitutes a subject's item mean subscale score for missing values.
#'
#' @param dataFrame A data frame containing the subscale to be scored.
#' @param dataFrame A data frame containing the appropriate measure items.
#' @param forwItems A vector of the items that should be forward-scored.
#' @param revItems A vector of the items that should be reverse scored. Default is NULL in case there are no reverse-scored items.
#' @param revInt A number (usually an integer, but does not have to be integer type) that will be used to reverse score the reverse-scored items by subtracting the subject-entered item value from the revInt. Default is NULL in case there are no reverse scored items.
#' @param criterion A number indicating the maximum percent of items a subject can have missing in order to use mean substitution. Default is 50, but a warning is produced if the value is over 20 because there are more robust ways to deal with missing values than mean substitution!
#' @return A data frame in which the NAs have been replaced with the item means for the given subscale (only for the subjects that meet the criterion).
#' @export

meanSubstitute <- function(dataFrame, forwItems, revItems = NULL, revInt = NULL, criterion = 50){
  #warns about the ills of mean substitution if the criterion is greater than 20%
  if(criterion > 20) warning("Mean substitution may jeopardize the validity of inferences! Consider using it only in cases where a few items are missing, and use a more robust method for dealing with other missing data.")

  #creates a vector of all of the items in the subscale
  allitems <- na.omit(c(forwItems, revItems))

  #computes the maximum percent that can be missing without breaking the function: at least one item must be present for all subjects, or the function will break!
  maxCriterion <- round(100*(length(allitems) - 1)/length(allitems), digits = 2)
  #throws an error if the criterion is over the maximum
  if(criterion >= maxCriterion) stop(paste("Cannot substitute a mean if all items are missing! For the current subscale, the criterion value must be below ", maxCriterion, ".", sep = ""))
  #gets a vector of rows indices in which less than or equal to the criterion percentage of the items are missing
  rowsToReplace <- which(calcPercentMissing(dataFrame, items = allitems)[,2] <= criterion)

  #if there are items to be reverse scored ....
  if(!is.null(revItems) && length(revItems > 0L) && !is.na(revItems[1])){
    #for each row that meets the missingness cutoff ...
    for (row in rowsToReplace){
      #calculates the person mean: creates a vector of all of the forward items, concatenates it with the revInt minus all of the reverse items, and computes the mean while ignoring NA values
      personMean <- mean(as.numeric(c(dataFrame[row, forwItems], revInt - dataFrame[row, revItems])), na.rm = T)

      #replaces all of the NAs for the forward items with the person mean
      dataFrame[row, forwItems] <- replace(dataFrame[row, forwItems], is.na(dataFrame[row, forwItems]), personMean)
      #replaces all of the NAs for the reverse-scored items with the reverse-scored person mean (i.e., the revInt minus the person mean)
      dataFrame[row, revItems] <- replace(dataFrame[row, revItems], is.na(dataFrame[row, revItems]), revInt-personMean)
    }

  #if there aren't items to be reverse scored ....
  }else{
    #using only the forward scored items, create a vector of the person means for all subjects  (only those with the correct row indices will be used in the loop)
    personMeans <- apply(dataFrame[, forwItems], 1, mean, na.rm = T)
    #for each row, replace only the items that are NA with the person mean for that row
    for (row in rowsToReplace){
     dataFrame[row, forwItems] <- replace(dataFrame[row, forwItems], is.na(dataFrame[row, forwItems]), personMeans[row])
      }
  }
  return(dataFrame)
}
