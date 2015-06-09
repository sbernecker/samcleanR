#' Read in an Excel workbook and produce a list of data frames, one for each sheet.
#'
#' @param filename The name/path of an Excel file (xls or xlsx)
#' @return A list of data frames, one for each sheet, with the name of the list element being the name of the sheet.
#' @export

workbookToDfList <- function(filename, mystartrow = 3){
  #reads in the workbook as a "workbook" object
  wb <- XLConnect::loadWorkbook(filename)
  #creates a character vector of all sheet names within the workbook
  sheetNames <- XLConnect::getSheets(wb)
    #initializes an empty list into which to put the data frames
  dfList = list()
  #gives the list items the names of the sheets
  dfList[sheetNames] <- list(NULL)
  #loops through the sheets that are in the file ...
  for (i in 1:(length(sheetNames))){
    #... and assigns a data frame containing the given sheet to the list element with the corresponding measure name
    dfList[[sheetNames[i]]] <- XLConnect::readWorksheet(wb, sheet = sheetNames[i])
  }
  #returns the list of data frames
  return(dfList)
}
