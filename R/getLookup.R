#' Reads in and cleans up the scoring lookup list.
#'
#'@param filename The location of the file containing the scoring lookup list. This function assumes that it is an Excel file formatted in a very specific way.
#'@return A list of lists, one list for each subscale, named with the subscale name and containing four sublists: measName, forwNames, revNames, and revInt.
#'@export


getLookup <- function(filename, mystartrow = 1){
  #reads in the workbook containing the scoring info as a "workbook" object
  wb <- xlsx::loadWorkbook(filename)
  #creates a character vector of all sheet names within the workbook, which correspond to the subscales that are listed in the lookup table
  subscales <- names(xlsx::getSheets(wb))
  #gets the number of subscales
  numSubsc <- length(subscales)
  #initializes an empty list into which to put the scoring info
  listList = list()
  #gives the list items the names of the subscales
  listList[subscales] <- list(NULL)
  #loops through the subscales that are in the file ...
  for (i in 1:numSubsc){
    #... reads in a data frame containing the given sheet, and assigns the data frame to the list element with the corresponding subscale name
    listList[[subscales[i]]] <- xlsx::read.xlsx(filename, sheetName = subscales[i], header = T, startRow = mystartrow, stringsAsFactors = F)
  }
  #transforms all data frames within the list into lists
  listList <- lapply(listList, as.list)
  #goes through all subscales and strips NAs off of each column
  for(i in 1:length(listList)){
    #makes measName the first element of measName
    listList[[i]]$measName <- listList[[i]]$measName[1]
    #strips NAs off of forwNames
    listList[[i]]$forwNames <- na.omit(listList[[i]]$forwNames)
    #if the first element of revNames is NA, makes it just one NA; otherwise, strips off NA values
    if(is.na(listList[[i]]$revNames[1])){
      listList[[i]]$revNames <- NA
    }else{
      listList[[i]]$revNames <- na.omit(listList[[i]]$revNames)
    }
    #does the same with revInt
    if(is.na(listList[[i]]$revInt[1])){
      listList[[i]]$revInt <- NA
    }else{
      listList[[i]]$revInt <- na.omit(listList[[i]]$revInt)
    }
  }
  #returns the list of lists
  return(listList)
}

# My lookup file is currently located at C:/Users/Sam/Documents/research/samcleanR/scoring_lookup.xlsx



