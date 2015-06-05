#'
#' Okay, now I want to make some kind of function that takes a vector of measure names, scores the measures for all participants, outputs a data frame containing all of their total scores.
#'
#' the row names should be the subject ids
#' the column names should be the session name (which will probably be the variable name of the list of data frames) plus the subscale name
#'
#'
#' so it should take in a series of lists-of-dataframes, find the appropriate measures in those that contain it, score them, and merge them into a final data frame with the appropriate column names
#'
#' you should probably put on the column names prior to merging 'cause otherwise you might end up with duplicate column names ... although there is an argument in merge that allows you to add a suffix
#'
#' @param ... One or more lists that contain named data frames as their elements. Each list in my data set corresponds to a measurement occasion.
#' @param subscales A character vector of the names of subscales that the user wants to compute, written so the names match those in the lookup list.
#' @param lookupList A list object that contains scoring "instructions" for each subscales.
#' @return A data frame containing the requested subscale scores for each observation/subject.
#' @idxOfSubj The column index in the data frames that contains subject IDs. Default is 1.
#' @export

occasions <- list(...)
occasionNames <- names(occasions) <- alist(...)

initialDf <- occasions[[1]][1][idxOfSubj]

#s3 is equivalent to occassions[occ] ... or maybe occasions[[occ]], i will think about that later

for (occ in 1:length(occasions)){
  #creates a vector of indices of the data frames in the current occasion that match the measure needed for the current subscale
  measIdx <- grep(measName, names(occasions[occ]), ignore.case = T)
  if(length(measIdx) == 0){
    #if no data frames match, tells the user that the measure was not collected at the time point
    cat("The", measName, "was not collected at", occasionNames[occ], "\n")
  }else if (length(measIdx) > 1){
    #if there is more than one data frame that matches the measure name, warns the user that there might be an error and does not score the measure
    warning(paste("There are", length(measIdx), "sheets that match", measName, "at", occasionNames[occ], "\n", "This measure at this occassion will not be scored.\n"))
  }else if (length(measIdx) == 1){
    calcSubscale(occassions[occ], forwNames = forwNames, revNames = revNames, revInt = revInt)
  }

}

#merge(df1, df2, by = "row.names", all = T, sort = F, suffixes = "a character vector of length 2 specifying the suffixes to be used for making unique the names of columns in the result which not used for merging (appearing in by etc).")


