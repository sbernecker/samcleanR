#' Read in an Excel workbook and produce a list of data frames, one for each sheet.
#'
#' @param filename The name/path of an Excel file (xls or xlsx)
#' @param mystartrow The row containing the variable names (i.e., headings). Defaults to row 3 because that's what I need for this project!
#' @return A list of data frames, one for each sheet, with the name of the list element being the name of the sheet.
#' @export

workbookToDfList <- function(filename, mystartrow = 3){
  #reads in the workbook as a "workbook" object
  wb <- xlsx::loadWorkbook(filename)
  #creates a character vector of all sheet names within the workbook; I need to use "names" because the result of "getSheets" is a weird object (either a list or another weird Java-y thing, I forget)
  sheetnames <- names(xlsx::getSheets(wb))
  #creates a character vector of the sheet names that do NOT contain the word "check"; the argument ignore case does what it says on the tin, the argument value is set to true so that it returns a vector of the actual contents that match the pattern (if false, would return indices), invert returns the items that do NOT match the pattern
  measures <- grep("check", sheetnames, ignore.case = T, value = T, invert = T)
  #initializes an empty list into which to put the data frames
  dfList = list()
  #gives the list items the names of the measures
  dfList[measures] <- list(NULL)
  #loops through the measures that are in the file ...
  for (i in 1:(length(measures))){
    #... and assigns a data frame containing the given sheet to the list element with the corresponding measure name
    dfList[[measures[i]]] <- xlsx::read.xlsx(filename, sheetName = measures[i], header = T, startRow = mystartrow, stringsAsFactors = F)
  }
  #returns the list of data frames
  return(dfList)
}
