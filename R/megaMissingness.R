#' Creates a data frame containing the percentage of items that are missing for a given set of subscales and measurement occasions for each subject.
#'
#' @param filenames A list of Excel files that contain named spreadsheets.
#' @param subscales A character vector of the names of subscales for which the user wants to know missingness.
#' @param lookupList A list object that contains scoring "instructions" for each subscale.
#' @param idxOfSubj The column index in the data frames that contains subject IDs. Default is 1.
#' @param myStartRow The row index in the spreadsheets containing the variable names (i.e., headings). Defaults to row 3 because that's what I need for this project!
#' @param writeToExcel A logical vector of length one that specifies whether the resulting data frame should be written to a new Excel file. Default is FALSE. The file will be created in the current working directory and named with occassion and subscale names and the word "scored."
#' @param path The desired location at which to save the file. Default is the working directory.
#' @return A data frame containing the percentage of values that are missing for each subject and occasion. That is, each subject gets a row, and each subscale-occasion combo gets its own column.
#' @export

megaMissingness <- function(filenames, subscales, lookupList, idxOfSubj = 1, myStartRow = 3, writeToExcel = FALSE, path = getwd()){
  #makes all of the lists of data frames into a list of lists of data frames (ouch!)
  occasions <- filenames
  #gives each list the name of the list item as passed in, and also assigns this character vector to occasionNames
  occasionNames <- sub(".xlsx", "", filenames)

  #creates a one-column data frame with just subject names ...
  missingDf <- XLConnect::readWorksheetFromFile(filenames[1], sheet = 1, startRow = myStartRow)[idxOfSubj]
  #gets the character name of the subject column just in case it's something other than "Subjects"!
  subjChar <- names(missingDf)

  #loops through each of the subscales requested ...
  for (occ in 1:length(occasions)){

    #... and gets the appropriate measure name and scoring information from the lookup table
    measName <- lookupList[[subscales[subsc]]]$measName
    forwItems <- lookupList[[subscales[subsc]]]$forwItems
    revItems <- lookupList[[subscales[subsc]]]$revItems

    #creates a character vector containing all items that are used to score the subscale (regardless of forward or reverse); removes NAs if they somehow ended up on there
    allitems <- na.omit(c(forwItems, revItems))

        #tells the user which subscale is being checked
    cat("==== Now checking", subscales[subsc], " ====\n", sep = "")

    #then, for each occasion, calculates missingness for that subscale as follows.
    for (occ in 1:length(occasions)){
      #creates a vector of indices of the data frames in the current occasion that match the measure needed for the current subscale
      measIdx <- grep(measName, names(occasions[[occ]]), ignore.case = T)
      if(length(measIdx) == 0){
        #if no data frames match, tells the user that the measure was not collected at the time point
        cat("The", measName, "was not collected at", occasionNames[occ], "\n")
      }else if (length(measIdx) > 1){
        #if there is more than one data frame that matches the measure name, warns the user that there might be an error and does not score the measure
        warning(paste("There are", length(measIdx), "sheets that match", measName, "at", occasionNames[occ], ", so missingness at this occasion will not be computed.\n"))
      }else if (length(measIdx) == 1){

        #tells the user what occasion is being computed
        cat("Computing missingness at", occasionNames[occ], "for", subscales[subsc], "\n")

        #creates a two-column data frame with the subject IDs and the missing percentages
        littleDf <- calcPercentMissing(occasions[[occ]][[measIdx]], items = allitems)
        #names the column containing the percent with the name of the subscale and the occasion
        colnames(littleDf)[2] <- paste(occasionNames[occ], subscales[subsc], sep = "_")
        #merges the new data frame with the previous data frame, by whatever name is given to subjects in these data sets (in my data sets it's just "Subject")
        missingDf <- merge(missingDf, littleDf, by = subjChar, all = T, sort = F, suffixes = c("", occasionNames[occ]))
      }
    }
  }

  if(writeToExcel == T){
    #pastes together the names of the occassions and then the subscales in order, separated by underscores
    filename <- paste(paste(occasionNames, collapse = "_"), "_", paste(subscales, collapse = "_"), "_MISSINGNESS.xlsx", sep = "")
    #pastes together the filename and the specified path at which to store the file; default is working directory
    totalpath <- paste(path, "/", filename, sep = "")
    #writes the data frame to an Excel file at the specified location
    xlsx::write.xlsx(missingDf, file = totalpath, sheetName = "Missing", row.names = F, showNA = F)
  }

  return(missingDf)
}





