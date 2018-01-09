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
#' @examples
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

  # for testing
  # useCase='apObligate'

  if(!(useCase %in% c('apAll','apObligate','apMinimal'))) stop('Specify a correct useCase.')

  # read in csv from path
  dd <- read.csv(csv, stringsAsFactors=FALSE)

  #Creating named values from the .csv file
  values <- mapply(assign, dd$entity, dd$value)

  #Creating a blank rmm rmm to fill from the .csv
  rmm <- rangeModelMetadataTemplate(useCase = useCase)

  count <- 1
  while(count <= nrow(dd)){
    coords <- dd[count,!is.na(dd[count,])]
    if (length(coords) == 5){
      rmm[[coords[[1]]]][[coords[[2]]]][[coords[[3]]]][[coords[[4]]]] <- coords[[5]];
    }
    else if (length(coords) == 4){
      rmm[[coords[[1]]]][[coords[[2]]]][[coords[[3]]]] <- coords[[4]];
    }
    else if (length(coords) == 3){
      rmm[[coords[[1]]]][[coords[[2]]]] <- coords[[3]];
    }
    else if (length(coords) == 2){
      rmm[[coords[[1]]]] <- coords[[2]];
    }
    count <- count + 1;
  }

  return(rmm)
}
