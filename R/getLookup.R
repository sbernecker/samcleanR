#' Reads in and formats the scoring lookup list.
#'
#'@param filename The name/location of the file containing the scoring lookup list. This function assumes that it is an Excel file formatted in a very specific way.
#'@param myStartRow The row in the spreadsheet that contains the headings. Default is 1.
#'@return A list of lists, one list for each subscale. Each upperl-level list is named with the subscale name and contains four sublists: measName, forwItems, revItems, and revInt.
#'@export


getLookup <- function(filename, myStartRow = 1){
  #reads in the workbook containing the scoring info as a "workbook" object
  wb <- XLConnect::loadWorkbook(filename)
  #creates a character vector of all sheet names within the workbook, which correspond to the subscales that are listed in the lookup table
  subscales <- XLConnect::getSheets(wb)
  #initializes an empty list into which to put the scoring info
  listList = list()
  #gives the list items the names of the subscales
  listList[subscales] <- list(NULL)
  #loops through the subscales that are in the file ...
  for (i in 1:length(subscales)){
    #... reads in a data frame containing the given sheet, and assigns the data frame to the list element with the corresponding subscale name
    listList[[subscales[i]]] <- XLConnect::readWorksheet(wb, sheet = subscales[i], header = T, startRow = myStartRow)
  }
  #transforms all data frames within the list into lists
  listList <- lapply(listList, as.list)
  #goes through all subscales and strips NAs off of each column
  for(i in 1:length(listList)){
    #makes measName the first element of measName
    listList[[i]]$measName <- listList[[i]]$measName[1]
    #strips NAs off of forwItems
    listList[[i]]$forwItems <- na.omit(listList[[i]]$forwItems)
    #if the first element of revItems is NA, makes it an empty character vector; otherwise, strips off NAs
    if(is.na(listList[[i]]$revItems[1])){
      listList[[i]]$revItems <- character(length = 0L)
    }else{
      listList[[i]]$revItems <- na.omit(listList[[i]]$revItems)
    }
    #if the first element of revInt is NA, makes it an empty numeric vector; otherwise, strips off NAs
    if(is.na(listList[[i]]$revInt[1])){
      listList[[i]]$revInt <- numeric(length = 0L)
    }else{
      listList[[i]]$revInt <- na.omit(listList[[i]]$revInt)
    }
  }
  #returns the list of lists
  return(listList)
}

# My lookup file is currently located at "C:/Users/Sam/Documents/research/samcleanR/scoring_lookup.xlsx"
