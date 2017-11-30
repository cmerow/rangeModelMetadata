#' @title
#'
#' @description
#'
#' @details
#' See Examples.
#'
#' @param
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
# packages=c('raster','sp')
# rmmPackageCitation=function(packages){
#   lapply(packages,function(x) toBibtex(citation(x)))
# }
rmmPackageCitation=function(packages){
  out<-lapply(packages,function(x) toBibtex(citation(x)))
  out<-unlist(out)
  out<-paste0(out,collapse = "")
  #The following bit could be done in a more clever way with regex, but this works for now
  out<-strsplit(x = out,split = "}@")
  out<-out[[1]]
  if(length(out)>1){

    out[2:length(out)]<-paste0("@",out[2:length(out)])
    out[1:(length(out))-1]<-paste0(out[1:(length(out))-1], "}" )
  }

  return(out)

}
