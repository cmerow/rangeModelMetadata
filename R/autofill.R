#' @title Add all package citations to an rmm object
#'
#' @description Using bibtex citations
#'
#' @details
#' See Examples.
#'
#' @param rmm an rmm list
#' @param packages a vector of quoted package names
#'
#' @examples
#' rmm=rmmTemplate()
#' rmm=rmmAutofillPackageCitation(rmm,c('raster','sp'))
#'
#' @return a range model metadata list
#' @author Brian Maitner <bmaitner@@gmail.com>, Cory Merow <cory.merow@@gmail.com>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

rmmAutofillPackageCitation=function(rmm,packages){
  out<-lapply(packages,function(x) utils::toBibtex(utils::citation(x)))
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

####################################################################################
####################################################################################
####################################################################################

#' @title  Add relevant environmental data information to an rmm object
#'
#' @description This can be used with environmental layers used for fitting or transferring
#'
#' @details
#' See Examples.
#'
#' @param rmm an rmm list
#' @param env a raster stack
#' @param transfer 0 if not transfer, 1:n for n environments that you're transferring to
#'
#' @examples
#' rmm=rmmTemplate()
#' raster.dir=system.file("extdata/Env_Demo",package='rangeModelMetadata')
#' raster.files=list.files(raster.dir,full.names = TRUE)
#' env=raster::stack(raster.files)
#' # for fitting environment
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=0)
#' # for the first environment that you're transfering to
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=1)
#' # for the second environment that you're transfering to, etc.
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=2)
#'
#' @return a range model metadata list
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

rmmAutofillEnvironment=function(rmm,env,transfer){

  .worldclimFill=function(env,rmm){
    if(length(grep('wc2.0',names(env)))>0){
      rmm$data$environment$sources='@article{Fick:2017bq, author = {Fick, Stephen E and Hijmans, Robert J}, title = {{WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas}}, journal = {International Journal of Climatology}, year = {2017},  volume = {37}, number = {12},  pages = {4302--4315}}'
      rmm$data$environment$yearMin=1970
      rmm$data$environment$yearMax=2000
    }
    if(length(grep('wc1.3',names(env)))>0){
      rmm$data$environment$sources='@misc{hijmans2004worldclim, title={The WorldClim interpolated global terrestrial climate surfaces. Version 1.3}, author={Hijmans, RJ and Cameron, SE and Parra, JL and Jones, PG and Jarvis, A}, year={2004}}'
      rmm$data$environment$yearMin=1970
      rmm$data$environment$yearMax=2000
    }
    return(rmm)
  }

  if(is.null(transfer)) stop('specify whether this environment is used for transfer (>1) or not (0)')
  if(transfer==0){
    rmm$data$environment$resolution=raster::res(env)
    rmm$data$environment$extent=raster::extent(env)
    rmm$data$environment$variableNames=names(env)
    rmm=.worldclimFill(env,rmm)
  } else {
    rmm$output$transfer[paste0('environment',transfer)][[1]]$resolution=raster::res(env)
    rmm$output$transfer[paste0('environment',transfer)][[1]]$extent=raster::extent(env)
    rmm$output$transfer[paste0('environment',transfer)][[1]]$variableNames=names(env)
  }
  return(rmm)
}


####################################################################################
####################################################################################
####################################################################################

#' @title Add relevant model prediction info to an rmm object
#'
#' @description Add relevant model prediction info to an rmm object
#'
#' @details
#' See Examples.
#'
#' @param rmm an rmm list
#' @param prediction a raster layer or stack
#'
# @examples
#'
#'
#' @return a range model metadata list
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

rmmAutofillPrediction=function(rmm,prediction){
  print('not done')
}

####################################################################################
####################################################################################
####################################################################################

#' @title Add relevant model info to an rmm object
#'
#' @description Does stuff
#'
#' @details
#' See Examples.
#'
#' @param rmm an rmm list
#' @param modelObj a model object
#'
# @examples
#'
#'
# @return
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

rmmAutofillModelObj=function(rmm,modelObj){
  print('not done')
}


####################################################################################
####################################################################################
####################################################################################

#' @title Fill in relevant rmm fields from an ENMevaluation object.
#'
#' @description Fill in relevant rmm fields from an ENMevaluation object.
#'
#' @details
#' See Examples.
#'
#' @param rmm an rmm list
#' @param e an ENMevaluation object
#' @param i a numeric index value referring to the chosen model
#' (e.g., if you chose the model corresponding to row 5 in the results table, this number would be 5)
#'
# @examples
#'
#'
#' @return a range model metadata list
#' @author Jamie M. Kass <jamie.m.kass@@gmail.com>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

rmmAutofillENMeval <- function(rmm, e, i) {
  rmm$model$algorithm <- e@algorithm
  rmm$model$maxent$backgroundSizeSet <- nrow(e@bg.pts)
  rmm$model$partition$partitionSet <- unname(e@partition.method[1])

  if ("block" %in% e@partition.method | "checkerboard2" %in% e@partition.method) k <- 4
  else if ("checkerboard1" %in% e@partition.method) k <- 2
  else if ("randomkfold" %in% e@partition.method | "user" %in% e@partition.method) k <- as.numeric(e@partition.method[2])
  rmm$model$partition$numberFolds <- k

  if (grepl("maxnet", e@algorithm) == TRUE | grepl("maxent", e@algorithm) == TRUE) {
    rmm$model$maxent$featureSet <- as.character(e@results[i, "features"])
    rmm$model$maxent$regularizationMultiplierSet <- e@results[i, "rm"]
  }

  return(rmm)


}



####################################################################################
####################################################################################
####################################################################################


#' @title  Add occurrence metadata from a BIEN query to an rmm object
#'
#' @description This function populates occurrence field in an rmm object with output from a BIEN_occurrence_... query
#'
#' @details
#' See Examples.
#'
#' @param rmm an rmm list
#' @param occurrences an occurrence data.frame obtained from a BIEN occurrence query
#' @examples
#' rmm=rmmTemplate()
#' library(BIEN)
#' xs <- BIEN_occurrence_species(species="Xanthium strumarium)
#' rmmAutofillBIEN(rmm = rmm, occurrences = xs)
#'
#' @return a range model metadata list
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @note
#' @seealso  \code{\link[BIEN]{BIEN_occurrence_species}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export
rmmAutofillBIEN <- function(rmm, occurrences){

  rmm$data$occurrence$taxa <- unique(occurrences$scrubbed_species_binomial)

  rmm$data$occurrence$dataType   <- "presence only"

  rmm$data$occurrence$yearMin <- NA
  rmm$data$occurrence$yearMax <- NA

  rmm$data$occurrence$sources <- BIEN::BIEN_metadata_citation(dataframe = occurrences)$references

  rmm$data$occurrence$presenceSampleSize <- unlist(lapply(X = rmm$data$occurrence$taxa, FUN = function(x){
    nrow(occurrences[which(occurrences$scrubbed_species_binomial==x),]) }))

  return(rmm)

}
