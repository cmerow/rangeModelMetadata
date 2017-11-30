#' @title Add all package citations to an rmm object
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
rmmAutofillPackageCitation=function(packages){
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

##############################################################################################
##############################################################################################
##############################################################################################

#' @title  Add relevant environmental data information to an rmm object
#'
#' @description This can be used with environmental layers used for fitting or transferring
#'
#' @details
#' See Examples.
#'
#' @param rrm an rmm list
#' @param env a raster stack
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

rmmAutofillEnvironment=function(rmm,env,transfer=FALSE){

}

##############################################################################################
##############################################################################################
##############################################################################################

#' @title Add relevant model info to an rmm object
#'
#' @description
#'
#' @details
#' See Examples.
#'
#' @param rrm an rmm list
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

rmmAutofillModelObj=function(rmm,modelObj){

}

##############################################################################################
##############################################################################################
##############################################################################################

#' @title Add relevant model prediction info to an rmm object
#'
#' @description
#'
#' @details
#' See Examples.
#'
#' @param rrm an rmm list
#' @param prediction a raster layer or stack
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

rmmAutofillPrediction=function(rmm,prediction){

}
