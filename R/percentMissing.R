#' Computes the percentage of items that are missing for a given subject on a given subscale.
#'
#' @param itemvec A vector containing the items for which missingness is to be computed.
#' @return The percent of items on the scale that are missing for that subject, rounded to two decimal places. 100 indicates that all items are missing, 0 indicates that no items are missing.
#' @export

percentMissing <- function(itemvec){
  #calculates the proportion of the items in the vector that are
  prcnt <- sum(is.na(itemvec))/length(itemvec)
  #multiplies by 100 to transform proportion to percent, round to two decimal places
  prcnt <- round(100*prcnt, digits = 2)
  return(prcnt)
}
