#' @title Recreate rangeModelMetaData Object From .csv File
#'
#' @description Takes user-input .csv file and converts it back to a rangeModelMetaData object.
#'
#' @details
#' See Examples.
#'
#' @param csv A character file path to the csv file.
#' @param useCase character string; 'apAll', 'apObligate', 'apMinimal'
#'
# @examples
#'
#' @author Hannah Owens <hannah.owens@@gmail.com>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export
#'

## NOT FUNCTIONAL


csvToRMM <- function(csv, useCase='apAll') {

  if(!(useCase %in% c('apAll','apObligate','apMinimal'))) stop('Specify a correct useCase.')

  # read in csv from path
  dd <- read.csv(csv, stringsAsFactors=FALSE)

  #Creating named values from the .csv file

  values <- mapply(assign, dd$entity, dd$value)

  # create a blank rmm to fill from the values in csv
  rmm <- rmmTemplate(useCase = useCase)

  # loop over all rows, determine how many fields are not NA,
  # then fill in the value from csv to the slot in the rmm list
  # NOTE: all values are split to vectors by ";" by default
  for(i in 1:nrow(dd)) {
    curRow <- dd[i,!is.na(dd[i,])]
    if (length(curRow) == 5){
      if(curRow[[5]] != "NULL") {
        rmm[[curRow[[1]]]][[curRow[[2]]]][[curRow[[3]]]][[curRow[[4]]]] <- strsplit(curRow[[5]], "; ")[[1]]
      }
    }
    else if (length(curRow) == 4){
      if(curRow[[4]] != "NULL") {
        rmm[[curRow[[1]]]][[curRow[[2]]]][[curRow[[3]]]] <- strsplit(curRow[[4]], "; ")[[1]]
      }
    }
    else if (length(curRow) == 3){
      if(curRow[[3]] != "NULL") {
        rmm[[curRow[[1]]]][[curRow[[2]]]] <- strsplit(curRow[[3]], "; ")[[1]]
      }
    }
    else if (length(curRow) == 2){
      if(curRow[[2]] != "NULL") {
        rmm[[curRow[[1]]]] <- strsplit(curRow[[2]], "; ")[[1]]
      }
    }
  }

  return(rmm)
}
