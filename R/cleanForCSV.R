#' @title Helper function for non-string metadata in rmmToCSV
#'
#' @description Cleans up metadata instances that get messy if one tries to write them directly to csv tables (i.e. extent objects, bibtex objects.)
#'
#' @details
#' See Examples.
#'
#' @param x An \code{rmm} entry that will be entered into an \code{rmmToCSV} function.
#'
#' @examples
#' rmm=rmmTemplate(useCase='apAll')
#' rmm=rmmAutofillPackageCitation(rmm,c('raster','sp'))
#' cleanForCSV(rmm$data$environment$extent)
#'
# @return
#' @author Hannah Owens <hannah.owens@@gmail.com>, Cory Merow <cory.merow@@gmail.com>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

cleanForCSV <- function(x = NULL){
  y <- unlist(x[1]);
  z <- unlist(x)
  if(class(x)=="Extent"){
    temp <- paste("xmin: ", x[1], "; xmax: ", x[2], "; ymin: ", x[3], "; ymax: ", x[4], sep = "")
  }
  #For vectors with more than one element
  else if(length(z) > 1){
    temp <- paste(z, collapse = "; ")
  }
  #For vectors with a single element
  else{
    temp <- z;
  }
  print(temp);
  #return(temp);
}
