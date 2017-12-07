
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

  list_names<-utils::capture.output(nametree(rmm))

  dd=utils::read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetadata'),stringsAsFactors=F)

  accepted_names<-c(as.character(dd$Field1), as.character(dd$Field2), as.character(dd$Field3), as.character(dd$Field4))

  questionable_names <- list_names[which(!list_names%in%accepted_names)]

  return(questionable_names)

}

