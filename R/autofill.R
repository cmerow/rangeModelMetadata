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
#' rmm=rmmTemplate(useCase='apAll')
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
#' @param rmm an rmm list
#' @param env a raster stack
#' @param transfer 0 if not transfer, 1:n for n environments that you're transferring to
#'
#' @examples
#' rmm=rmmTemplate(useCase='apAll')
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


##############################################################################################
##############################################################################################
##############################################################################################
#'
#' #' @title Add relevant model info to an rmm object
#' #'
#' #' @description
#' #'
#' #' @details
#' #' See Examples.
#' #'
#' #' @param rmm an rmm list
#' #' @param
#' #'
#' # @examples
#' #'
#' #'
#' #' @return a range model metadata list
#' #' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
#' # @note
#' # @seealso
#' # @references
#' # @aliases - a list of additional topic names that will be mapped to
#' # this documentation when the user looks them up from the command
#' # line.
#' # @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' #' @export
#'
#' rmmAutofillModelObj=function(rmm,modelObj){
#'
#' }

##############################################################################################
##############################################################################################
##############################################################################################

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

##############################################################################################
##############################################################################################
##############################################################################################

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

##############################################################################################
##############################################################################################
##############################################################################################


##############################################################################################
##############################################################################################
##############################################################################################

#' @title Fill in relevant rmm fields from an ENMevaluation object.
#'
#' @description Fill in relevant rmm fields from an ENMevaluation object.
#'
#' @details
#' See Examples.
#'
#' @param rmm an rmm list
#' @param e an ENMevaluation object
#' @param sr a character string indicating the model selection rules used to determine which model to choose
#' @param i a numeric value indicating the row number of the model chosen by the user
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

rmmAutofillENMeval <- function(rmm, e, sr, i) {
  # NOTES: still need to include clamping, aggregation factors for checkerboard partitions
  p <- e@partition.method
  k <- length(unique(e@occ.grp))

  rmm$data$occurrence$backgroundSampleSizeSet <- nrow(e@bg.pts)
  rmm$model$partition$occurrenceSubsampling <- "k-fold cross validation"
  rmm$model$partition$partitionSet <- p
  rmm$model$partition$notes <- "background points also partitioned"

  rmm$model$partition$numberFolds <- k
  e@results$settings <- as.character(e@results$settings)

  rmm$model$maxent$featureSet <- as.character(e@results[, "features"])
  rmm$model$maxent$regularizationMultiplierSet <- e@results[, "rm"]

  if(p == "block") rmm$model$partition$partitionRule <- "spatial blocks defined by longitude and latitude lines"
  if(p == "checkerboard1") rmm$model$partition$partitionRule <- "binary checkerboard with user-defined aggregation factor"
  if(p == "checkerboard2") rmm$model$partition$partitionRule <- "nested binary checkerboard with user-defined aggregation factor"

  rmm$model$selectionRules <- sr
  rmm$model$finalModelSettings <- e@results[i, "settings"]
  rmm$performance$trainingDataStats$AUC <- e@results[i, "full.AUC"]
  rmm$performance$trainingDataStats$AIC <- e@results[i, "AICc"]

  rmm$performance$testingDataStats$AUC <- e@results[i, "mean.AUC"]
  rmm$performance$testingDataStats$omissionRate <- c(e@results[i, "Mean.ORmin"], e@results[i, "Mean.OR10"])
  rmm$performance$testingDataStats$notes <- "omission rate thresholds are 1) minimum training presence, 2) 10% training presence"

  return(rmm)
}


