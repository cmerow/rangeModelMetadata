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
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>, Jamie Kass <jamie.m.kass@@gmail.com>
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

  # # convert to old format that worked with other code
  # dd <- .rmmLeftJustify(dd)

  # Level 1 fields
  field1 <- as.character(unique(dd$field1))
  rmm <- sapply(field1,function(x) NULL)

  # Level 2-5 fields
  for(i in 1:length(rmm)){
    if(!useCase == 'apAll'){
      dd.f2 <- dd[dd$field1 == names(rmm)[i] & dd[,useCase] == 1,]
    } else {dd.f2 <- subset(dd,field1 == names(rmm)[i])}
    #paste0(dd.f2[,c('field2','field3','entity')],collapse='$')

    field2 <- as.character(unique(dd.f2$field2))
    field2 <- field2[complete.cases(field2)]
    rmm[[i]] <- sapply(field2,function(x) NULL)
    for(j in 1:length(field2)){
      if(!useCase == 'apAll'){
        dd.f3 <- dd.f2[dd.f2$field2 == names(rmm[[i]])[j] & dd.f2[,useCase] == 1,]
        #subset(dd.f2,field2 == names(rmm[[i]])[j]  & Obligate == 1)
      } else { dd.f3 <- subset(dd.f2,field2 == names(rmm[[i]])[j]) }
      field3 <- as.character(unique(dd.f3$field3))
      field3 <- field3[complete.cases(field3)]
      if(!all(is.na(field3) | is.null(field3) | field3 == '')){
        rmm[[i]][[j]] <- sapply(field3,function(x) NULL)
        for(k in 1:length(field3)){
          if(!useCase == 'apAll'){
            dd.f4 <- dd.f3[dd.f3$field3 == names(rmm[[i]][[j]])[k] & dd.f3[,useCase] == 1,]
            #subset(dd.f3,field3 == names(rmm[[i]][[j]])[k] & Obligate == 1)
          } else {  dd.f4 <- subset(dd.f3,field3 == names(rmm[[i]][[j]])[k]) }
          field4 <- as.character(unique(dd.f4$entity))
          field4 <- field4[complete.cases(field4)]
          if(!all(is.na(field4) | is.null(field4) | field4 == '')) rmm[[i]][[j]][[k]] <- sapply(field4,function(x) NULL)
        }
      }
    }
  }
  return(rmm)
}
