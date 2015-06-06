#' Scores requested subscales from a given set of lists of data frames.
#'
#' @param ... One or more lists that contain named data frames as their elements. Each list in my data set corresponds to a measurement occasion.
#' @param subscales A character vector of the names of subscales that the user wants to compute, written so the names match those in the lookup list.
#' @param lookupList A list object that contains scoring "instructions" for each subscale.
#' @param writeToExcel A logical vector of length one that specifies whether the resulting data frame should be written to a new Excel file. Default is F. The file will be created in the current working directory.
#' @param idxOfSubj The column index in the data frames that contains subject IDs. Default is 1.
#' @return A data frame containing the requested subscale scores for each subject and occasion. That is, each subject gets a row, and each subscale-occasion combo gets its own column.
#' @export

megaScorer <- function(..., subscales, lookupList, writeToExcel = F, idxOfSubj = 1){
  #makes all of the lists of data frames into a list of lists of data frames (ouch!)
  occasions <- list(...)
  #gives each list the name of the list item as passed in, and also assigns this character vector to occasionNames
  occasionNames <- names(occasions) <- as.character(substitute(list(...)))[-1L]

  #creates a one-column data frame with just subject names ... it's not "big" yet but it will be!
  bigDf <- occasions[[1]][[1]][idxOfSubj]
  #gets the character name of the subject column just in case it's something other than "Subjects"
  subjChar <- names(bigDf)

  #loops through each of the subscales requested ...
  for (subsc in 1:length(subscales)){

    #... and gets the appropriate measure names and scoring information from the lookup table
    measName <- lookupList[[subscales[subsc]]]$measName
    forwNames <- lookupList[[subscales[subsc]]]$forwNames
    revNames <- lookupList[[subscales[subsc]]]$revNames
    revInt <- lookupList[[subscales[subsc]]]$revInt
    #tells the user which subscale is being scored
    cat("Now scoring", subscales[subsc], "\n")

    #then, for each occasion, scores that subscale as follows.
    for (occ in 1:length(occasions)){
      #creates a vector of indices of the data frames in the current   occasion that match the measure needed for the current subscale
      measIdx <- grep(measName, names(occasions[[occ]]), ignore.case = T)

      if(length(measIdx) == 0){

        #if no data frames match, tells the user that the measure was not collected at the time point
        cat("The", measName, "was not collected at", occasionNames[occ], "\n")
      }else if (length(measIdx) > 1){

        #if there is more than one data frame that matches the measure name, warns the user that there might be an error and does not score the measure
        warning(paste("There are", length(measIdx), "sheets that match", measName, "at", occasionNames[occ], ", so this measure at this occasion will not be scored.\n"))
      }else if (length(measIdx) == 1){
        #tell the user what occasion is being scored
        cat("Now scoring", occasionNames[occ], subscales[subsc], "\n")

        #creates a two-column data frame with the subject IDs and the total scores
        littleDf <- calcSubscale(occasions[[occ]][[measIdx]], forwNames = forwNames, revNames = revNames, revInt = revInt)
        #gives the total score the name of the subscale and the occasion
        colnames(littleDf)[2] <- paste(occasionNames[occ], subscales[subsc], sep = "_")
        #merges the new data frame with the previous data frame, by whatever name is given to subjects in these data sets (in my data sets it's just "Subject")
        bigDf <- merge(bigDf, littleDf, by = subjChar, all = T, sort = F, suffixes = c("", occasionNames[occ]))
      }
    }
  }
  return(bigDf)

  if(writeToExcel = T){
    #pastes together the names of the occassions and then the subscales in order, separated by underscores
    filename <- paste(paste(occasionNames, collapse = "_"), "_", paste(subscales, collapse = "_"), "_scored.xlsx", sep = "")
    xlsx::write.xlsx(bigDf, file = filename, sheetName = "Scored", row.names = F, showNA = F)
  }

}





