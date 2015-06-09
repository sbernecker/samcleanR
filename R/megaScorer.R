#' Scores requested subscales from a given set of lists of data frames.
#'
#' @param filenames A list of Excel files that contain named spreadsheets.
#' @param subscales A character vector of the names of subscales that the user wants to compute, written so the names match those in the lookup list.
#' @param lookupList A list object that contains scoring "instructions" for each subscale.
#' @param idxOfSubj The column index in the spreadsheets that contains subject IDs. Default is 1.
#' @param myStartRow The row index in the spreadsheets containing the variable names (i.e., headings). Defaults to row 3 because that's what I need for this project!
#' @param writeToExcel A logical vector of length one that specifies whether the resulting data frame should be written to a new Excel file. Default is FALSE. The file will be created in the current working directory and named with occassion and subscale names and the word "scored."
#' @param path The desired location at which to save the file. Default is the working directory.
#' @param meanSub Should item means be substituted before calculating the total? Default is FALSE.
#' @param criterion A number indicating the maximum percent of items a subject can have missing in order to use mean substitution. Default is 50, but a warning is produced if the value is over 20 because there are more robust ways to deal with missing values than mean substitution!
#' @return A data frame containing the requested subscale scores for each subject and occasion. That is, each subject gets a row, and each subscale-occasion combo gets its own column.
#' @export

megaScorer <- function(filenames, subscales, lookupList, idxOfSubj = 1, myStartRow = 3, writeToExcel = FALSE, path = getwd(), meanSub = FALSE, criterion = 50){
  occasions <- filenames

  #strips off the .xlsx extension to create a vector of measurement occasion names
  occasionNames <- sub(".xlsx", "", filenames)

  #creates a one-column data frame with just subject names ... it's not "big" yet but it will be!
  bigDf <- XLConnect::readWorksheetFromFile(filenames[1], sheet = 1, startRow = myStartRow)[idxOfSubj]
  #gets the character name of the subject column just in case it's something other than "Subjects"
  subjChar <- names(bigDf)

  #loops through each of the occassions requested ...
  for (occ in 1:length(occasions)){
    #reads in the workbook as a "workbook" object
    wb <- XLConnect::loadWorkbook(filenames[occ])
    #creates a character vector of all sheet names within the workbook
    sheetNames <- XLConnect::getSheets(wb)
    #creates a character vector of the sheet names that do NOT contain the word "check"
    sheetNames <- grep("check", sheetNames, ignore.case = T, value = T, invert = T)

    #tell the user what occasion is being scored
    cat("==== Now scoring ", occasionNames[occ], " ====\n", sep = "")

    #then, for each subscale, scores it as follows.
    for (subsc in 1:length(subscales)){

      #gets the appropriate measure names and scoring information from the lookup table
      measName <- lookupList[[subscales[subsc]]]$measName
      forwItems <- lookupList[[subscales[subsc]]]$forwItems
      revItems <- lookupList[[subscales[subsc]]]$revItems
      revInt <- lookupList[[subscales[subsc]]]$revInt

      #creates a vector of indices of the workbook sheets in the current occasion that match the measure needed for the current subscale
      measIdx <- grep(measName, sheetNames, ignore.case = T)

      #if no sheets match, tells the user that the measure was not collected at the time point
      if(length(measIdx) == 0){
        cat("The", subscales[subsc], "was not assessed for", occasionNames[occ], "\n")

        #if there is more than one sheet that matches the measure name, warns the user that there might be an error and does not score the measure
      }else if (length(measIdx) > 1){
        warning(paste("There are", length(measIdx), "sheets that match", measName, "at", occasionNames[occ], ", so this measure at this occasion will not be scored.\n"))

        #if there is one and only one sheet that matches the measure name, scores that subscale
      }else if (length(measIdx) == 1){

        #tells the user which subscale is being scored
        cat("Scoring", occasionNames[occ], subscales[subsc], "\n")

        #read in the necessary sheet to a data frame
        measDf <- XLConnect::readWorksheet(wb, sheet = measName, startRow = myStartRow)
        #if mean substitution requested, replaces the data frame with one in which item means are substituted for missing values (for those subjects that meet the missingness criterion)
        if (meanSub){
          measDf <- meanSubstitute(measDf, forwItems = forwItems, revItems = revItems, revInt = revInt, criterion = criterion)
        }

        #creates a two-column data frame with the subject IDs and the total scores
        littleDf <- calcSubscale(measDf, forwItems = forwItems, revItems = revItems, revInt = revInt)
        #names the column containing the total score with the name of the subscale and the occasion
        colnames(littleDf)[2] <- paste(occasionNames[occ], subscales[subsc], sep = "_")
        #merges the new data frame with the previous data frame, by whatever name is given to subjects in these data sets (in my data sets it's just "Subject")
        bigDf <- merge(bigDf, littleDf, by = subjChar, all = T, sort = F, suffixes = c("", occasionNames[occ]))
      }
    }

    #once all of the subscales have been looped-over, removes the workbook to free up memory (I hope!)
    rm(wb)
  }

  if(writeToExcel == T){
    #pastes together the names of the occassions and then the subscales in order, separated by underscores; if mean sub requested, also includes the word MEANSUB and the criterion value
    if (meanSub){
      filename <- paste(paste(occasionNames, collapse = "_"), "_", paste(subscales, collapse = "_"), "_SCORED_MEANSUB", criterion, ".xlsx", sep = "")
    }else{
          filename <- paste(paste(occasionNames, collapse = "_"), "_", paste(subscales, collapse = "_"), "_SCORED.xlsx", sep = "")
    }

    #pastes together the filename and the specified path at which to store the file; default is working directory
    totalpath <- paste(path, "/", filename, sep = "")
    #writes the data frame to an Excel file at the specified location
    XLConnect::writeWorksheetToFile(file = totalpath, data = bigDf, sheet = "Scored")
  }

  return(bigDf)
}
