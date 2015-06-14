#' Centers longitudinal measurements within and between subjects, as described by Bolger and Laurenceau (2013).
#'
#' @param dataframe A data frame containing the uncentered variables. May be wide or long.
#' @param measures A vector of the measures/subscales to be centered. FOR WIDE FORMAT, must be a character vector where the names uniquely match the appropriate column names of the data frame. For example, if the data were in wide format and mood was measured on multiple occasions labeled "Mood1", "Mood2," "Mood3," etc., the corresponding string could be "Mood." But if there were two different measures with mood in the title, like "MoodHusband1," "MoodHusband2," "MoodWife1", "MoodWife2", etc., the argument "Mood" would be insufficient because it would match both of these measures; you would need to include "MoodHusband" and "MoodWife" as separate strings in the vector. FOR LONG FORMAT, can be a vector of indices of the columns containing the measures/subscales or a character vector in which names EXACTLY match the column names.
#' @param format The format of the input data frame, either "long" or "wide".
#' @param idxOfSubj The column index in the data frame that contains subject IDs. Default is 1.
#' @return A data frame containing the original data as well as the requested measures grand-mean centered, between-person centered, and within-person centered. Measures labeled "gmc" are grand-mean centered, measures labeled "cb" are between-persons centered (i.e., are the average value of the grand-mean centered score for each subject), and measures labeled "cw" are within-person centered (i.e., a subject's deviation from that person's own average).
#' @export

centerer <- function(dataframe, measures, format, idxOfSubj = 1){
  if (format == "wide"){
    for (i in 1:length(measures)){
      #get the column indices of the measures/subscale
      subsCols <- grep(measures[i], colnames(dataframe), ignore.case = TRUE)
      #find the grand mean of all measurements for all rows/subjects (a single value)
      grandMean <- mean(as.matrix(dataframe[,subsCols]), na.rm = TRUE)
      #grand mean center every data point--this creates a new data frame
      gmCentered <- dataframe[,subsCols] - grandMean
      #name the columns of the new data frame with the old data frame plus "gmc" for "grand mean centered"
      names(gmCentered) <- paste(names(dataframe[,subsCols]), "_gmc", sep = "")
      #get the person mean of the grand-mean centered scores ... this creates a single vector of person means called "cb" for "centered between"
      cb <- apply(gmCentered, 1, mean, na.rm = TRUE)
      #center occasions around within-person means--makes a new data frame
      cw <- gmCentered - cb
      #name the columns of the new data frame with the old data frame plus "cw" for "centered within"
      names(cw) <- paste(names(dataframe[,subsCols]), "_cw", sep = "")

      #combine all centered versions
      dataframe <- cbind(dataframe, gmCentered, cb, cw)
      #rename the cb column with the measure name (otherwise, between-centered columns for all measures would have the same name)
      names(dataframe)[which(names(dataframe)=="cb")] <- paste(measures[i], "cb", sep = "_")
    }
  }else if (format == "long"){
    #sorts data frame by subject
    dataframe <- dataframe[order(dataframe[idxOfSubj]),]
    #grand mean centers all columns including the given measures--creates a data frame
    gmCentered <- scale(dataframe[measures], center = T, scale = F)
    #adds the "subjects" column to the grand-mean centered data frame
    gmCentered <- cbind(dataframe[idxOfSubj], gmCentered)
    #renames columns with "gmc"
    names(gmCentered)[-1] <- paste(colnames(dataframe[measures]), "gmc", sep = "_")
    #gets person means of those grand means (creates a data frame)
    cb <- aggregate(gmCentered[-1], by = list(gmCentered[[1]]), FUN = mean, na.rm = T)
    #gets the character name of the subject vector for future renaming/matching
    subjChar <- names(dataframe[idxOfSubj])
    #renames the "group" variable in the aggregated cb data frame with the subject name
    names(cb)[1] <- subjChar
    #renames the other columns with "cb"
    names(cb)[-1] <- paste(colnames(dataframe[measures]), "cb", sep = "_")
    #merges the two data frames by subject name
    centeredDf <- merge(gmCentered, cb, by = subjChar, all = T)
    cbcols <- grep("_cb", colnames(centeredDf))
    #center within person by subtracting individual means from grand-centered scores
    cw <- gmCentered[-1] - centeredDf[,cbcols]
    names(cw) <- paste(colnames(dataframe[measures]), "cw", sep = "_")
    #add all centered columns to new data frame
    dataframe <- cbind(dataframe, centeredDf[-1], cw)
  }else{
    stop("Please specify a valid format for the input data frame.")
  }
  return(dataframe)
}
