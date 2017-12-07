#' @title Add all package citations to an rmm object
#'
#' @description
#'
#' @details
#' See Examples.
#'
#' @param rrm an rmm list
#' @param package a vector of quoted package names
#'
#' @examples
#' rmm=rangeModelMetadataTemplate(useCase='apAll')
#' rmm=rmmAutofillPackageCitation(rmm,c('raster','sp'))
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
#' @export

# packages=c('raster','sp')
# rmmPackageCitation=function(packages){
#   lapply(packages,function(x) toBibtex(citation(x)))
# }
rmmAutofillPackageCitation=function(rmm,packages){
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
  rmm$code$software$packages=out
  return(rmm)

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
#'
# @examples


#rmm=rangeModelMetadataTemplate(useCase='apAll')
#' raster.files=list.files(system.file("extdata/Env_Demo",package='rangeModelMetadata'),full.names = T)
#' env=raster::stack(raster.files)
#' # for fitting environment
#rmmAutofillEnvironment(rmm,env,transfer=0)
#' # for the first environment that you're transfering to
#rmmAutofillEnvironment(rmm,env,transfer=1)
#' # for the second environment that you're transfering to, etc.
#rmmAutofillEnvironment(rmm,env,transfer=2)


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
#' @export

rmmAutofillEnvironment=function(rmm,env,transfer){
  if(is.null(transfer)) error('specify whether this environment is used for transfer (>1) or not (0)')
  if(transfer==0){
    rmm$data$environment$resolution=raster::res(env)
    rmm$data$environment$extent=raster::extent(env)
    #rmm$data$environment$variableNames=names(env)
  } else {

  }
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
#' @param rmm an rmm list
#' @param
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
#' @export

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
#' @export

rmmAutofillPrediction=function(rmm,prediction){

}
