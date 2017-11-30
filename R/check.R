#' @title Clean unused fields from range model metadata list
#'
#' @description Only optional fields that are empty are removed.
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @export
#'
# @examples
#'
#'
#' @return
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
#' @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

rmmCleanEmpties=function(rmm){

}


#' @title Check field names of a range model metadata list against conventions
#'
#' @description Identify nonstandard fields
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @export
#'
# @examples
#'
#'
#' @return A vector of names that are not found in the range model metadata dictionary.
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
#' @note Names returned by this check may be either incorrectly named or correctly named but missing from the data dictionary.
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
rmmNameCheck=function(rmm){

  #nametree function by Vincent Zoonekynd
  nametree <- function(X, prefix = "")
    if( is.list(X) )
      for( i in seq_along(X) ) {
        cat( prefix, names(X)[i], "\n", sep="" )
        nametree(X[[i]], paste0(prefix, ""))
      }

  list_names<-capture.output(nametree(rmm))
  dd=read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetaData'), stringsAsFactors = T)
  accepted_names<-c(as.character(dd$Field1), as.character(dd$Field2), as.character(dd$Field3), as.character(dd$Field4))

  questionable_names <- list_names[which(!list_names%in%accepted_names)]

  return(questionable_names)

}
