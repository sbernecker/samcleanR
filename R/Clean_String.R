#' A Function to clean a single input string by removing punctuation and numbers and tokenizing it.
#'
#' @param string A single input string such as "This is a cool function!"
#' @return A vector contain all valid tokens in the original input string
#' @export

Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  # Remove everything that is not a number letter ? or !
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  }
  return(temp)
}
