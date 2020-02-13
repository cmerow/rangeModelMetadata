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
#' @family autofill
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

  rmm=rmm
  rmm$code$software$packages=out
  return(rmm)
}

#############################################################################
#############################################################################
#############################################################################

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
#' \dontrun{
#' rmm=rmmTemplate()
#' rasterFiles=list.files(path=paste(system.file(package='dismo'), '/ex', sep=''),
#'                        pattern='grd', full.names=TRUE)
#' #make a stack of the rasters
#' env=raster::stack(rasterFiles)
#' # for fitting environment
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=0)
#' # for the first environment that you're transfering to
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=1)
#' # for the second environment that you're transfering to, etc.
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=2)
#' }
#' @return a range model metadata list
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family autofill
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
    rmm$data$environment$extentSet=raster::extent(env)
    rmm$data$environment$variableNames=names(env)
    rmm=.worldclimFill(env,rmm)
  } else {
    rmm$data$transfer[paste0('environment',transfer)][[1]]$resolution=raster::res(env)
    rmm$data$transfer[paste0('environment',transfer)][[1]]$extentSet=raster::extent(env)
    # I don't think we need this
    #rmm$data$transfer[paste0('environment',transfer)][[1]]$variableNames=names(env)
  }
  return(rmm)
}


#############################################################################
#############################################################################
#############################################################################
# CM: 7/11/18: I'm not sure there's really much to autofill from a prediction, so bailing until someone has a better idea
# @title Add relevant model prediction info to an rmm object
# @description Add relevant model prediction info to an rmm object
# @details
# See Examples.
# @param rmm an rmm list
# @param prediction a raster layer or stack
# @examples
#
# @return a range model metadata list
# @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family autofill
# @export

# rmmAutofillPrediction=function(rmm,prediction){
#   print('not done')
#   rmm
# }

#############################################################################
#############################################################################
#############################################################################

# @title Add relevant model info to an rmm object
# @description Does stuff
# @details
# See Examples.
# @param rmm an rmm list
# @param modelObj a model object
# @examples
# @return
# @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family autofill
# @export

# rmmAutofillModelObj=function(rmm,modelObj){
#   print('not done')
#   rmm
# }


#############################################################################
#############################################################################
#############################################################################

#' @title Fill in relevant rmm fields from an ENMevaluation object.
#'
#' @description Fill in relevant rmm fields from an ENMevaluation object.
#'
#' @details
#' See Examples.
#'
#' @param rmm an rmm list
#' @param e an ENMevaluation object
#' @param selectionCriteria a character string indicating the model selection
#' rules used (e.g., "first chose models with lowest MTP omission rate, then
#' chose the model with highest average test AUC")
#' @param optimalModelIndex a numeric value indicating the row number of the
#' model chosen by the user
#' (e.g., if you chose the model corresponding to row 5 in the results table,
#' this number would be 5); multiple models
#' may be selected in theory (for the purposes of model averaging, etc.), but
#' selecting one is preferable to reduce confusion
#'
#' @examples
#' #see vignette('rmm_workflow')
#'
#' @return a range model metadata list
#' @author Jamie M. Kass <jamie.m.kass@@gmail.com>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family autofill
#' @export

rmmAutofillENMeval <- function(rmm, e, selectionCriteria, optimalModelIndex) {
  rmm$model$algorithms <- e@algorithm
  rmm$data$occurrence$presenceSampleSize <- nrow(e@occ.pts)
  rmm$data$occurrence$backgroundSampleSize <- nrow(e@bg.pts)
  rmm$model$partition$occurrenceSubsampling <- "k-fold cross validation"
  rmm$model$partition$notes <- "background points also partitioned"
  k <- length(unique(e@occ.grp))
  rmm$model$partition$numberFolds <- k
  # for now, we ignore sampling bias -- future versions will include this option
  rmm$model$algorithm$maxent$samplingBiasRule <- 'ignored'

  if (e@partition.method == "randomkfold") {
    rmm$model$partition$partitionSet <- "random"
    rmm$model$partition$partitionRule <- "user-specified random partitions"
  }
  if (e@partition.method == "jackknife") {
    rmm$model$partition$partitionSet <- "jackknife"
    rmm$model$partition$partitionRule <- "leave-one-out partitions (each occurrence locality receives its own bin)"
  }
  if (e@partition.method == "block") {
    rmm$model$partition$partitionSet <- "spatial blocks"
    rmm$model$partition$partitionRule <- "4 spatial partitions defined by latitude/longitude lines that ensure a balanced number of occurrence localities in each bin"
  }
  if (e@partition.method == "checkerboard1") {
    rmm$model$partition$partitionSet <- "checkerboard blocks"
    rmm$model$partition$partitionRule <- "2 spatial partitions in a checkerboard formation that subdivide geographic space equally but do not ensure a balanced number of occurrence localities in each bin"
  }
  if (e@partition.method == "checkerboard2") {
    rmm$model$partition$partitionSet <- "spatial blocks"
    rmm$model$partition$partitionRule <- "4 spatial partitions with two levels of spatial aggregation in a checkerboard formation that subdivide geographic space equally but do not ensure a balanced number of occurrence localities in each bin"
  }
  if (grepl("maxnet", e@algorithm) == TRUE | grepl("maxent.jar", e@algorithm) == TRUE) {
    rmm$model$algorithm$maxent$featureSet <- as.character(e@results[optimalModelIndex, "fc"])
    rmm$model$algorithm$maxent$regularizationMultiplierSet <- e@results[optimalModelIndex, "rm"]
    rmm$model$algorithm$maxent$numberParameters <- e@results[optimalModelIndex, "nparam"]
  }

  rmm$model$selectionRules <- selectionCriteria
  rmm$model$finalModelSettings <- e@results[optimalModelIndex, "settings"]
  rmm$assessment$trainingDataStats$AUC <- e@results[optimalModelIndex, "auc.train"]
  rmm$assessment$trainingDataStats$AIC <- e@results[optimalModelIndex, "AICc"]

  rmm$assessment$testingDataStats$AUC <- e@results[optimalModelIndex, "avg.test.AUC"]
  rmm$assessment$testingDataStats$omissionRate <- c(e@results[optimalModelIndex, "avg.test.orMTP"], e@results[optimalModelIndex, "avg.test.or10pct"])
  rmm$assessment$testingDataStats$notes <- "omission rate thresholds are 1) minimum training presence, 2) 10% training presence"

  return(rmm)
}



#############################################################################
#############################################################################
#############################################################################


#' @title  Add occurrence metadata from a BIEN query to an rmm object
#'
#' @description This function populates occurrence field in an rmm object with output from a BIEN_occurrence_... query
#'
#' @details
#' See Examples.
#'
#' @param rmm an rmm list
#'
#' @param occurrences an occurrence data.frame obtained from a BIEN occurrence query
#'
#' @examples
#' \dontrun{
#' rmm <- rmmTemplate()
#' xs <- BIEN::BIEN_occurrence_species(species="Xanthium strumarium")
#' rmmAutofillBIEN(rmm = rmm, occurrences = xs)
#' }
#' @return a range model metadata list
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @note
#' @seealso  \code{\link[BIEN]{BIEN_occurrence_species}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family autofill
#' @export
rmmAutofillBIEN <- function(rmm, occurrences){

  rmm$data$occurrence$taxon <- unique(occurrences$scrubbed_species_binomial)

  rmm$data$occurrence$dataType   <- "presence only"

  rmm$data$occurrence$yearMin <- NA
  rmm$data$occurrence$yearMax <- NA

  rmm$data$occurrence$sources <- BIEN::BIEN_metadata_citation(dataframe = occurrences)$references

  rmm$data$occurrence$presenceSampleSize <- unlist(lapply(X = rmm$data$occurrence$taxa, FUN = function(x){
    nrow(occurrences[which(occurrences$scrubbed_species_binomial==x),]) }))

  return(rmm)

}


#############################################################################
#############################################################################
#############################################################################


#' @title  Add occurrence metadata from a spocc query to an rmm object
#'
#' @description This function populates occurrence field in an rmm object with output from a spocc query
#'
#' @details
#' See Examples.
#'
#' @param rmm an rmm list
#' @param occ Output from \code{\link[spocc]{occ}}
#'
#' @examples
#' \dontrun{
#' rmm=rmmTemplate()
#' xs <- spocc::occ("Xanthium strumarium")
#' rmmAutofillspocc(rmm = rmm, occ = xs)
#' }
#' @return a range model metadata list
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @note
#' @seealso  \code{\link[spocc]{occ}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family autofill
#' @export
rmmAutofillspocc <- function(rmm, occ){

  #If the data are formatted as an "occdat", convert to table
  #CM: 7/11/18: looks like the class is occdatind, but i can't figure out if there are also objects
  if("occdatind" %in% class(occ)){occ <- spocc::occ2df(occ) }

  #If the data are formatted as a list, take the data
  if("list" %in% class(occ)){occ <- occ$data }

  #Convert to dataframe
  occ <- as.data.frame(occ)

  rmm$data$occurrence$taxon <- unique(occ$name)

  rmm$data$occurrence$dataType   <- "presence only"

  rmm$data$occurrence$yearMin <- NA
  rmm$data$occurrence$yearMax <- NA

  rmm$data$occurrence$sources <- utils::toBibtex(utils::citation(package = "spocc"))

  rmm$data$occurrence$presenceSampleSize <- unlist(lapply(X = rmm$data$occurrence$taxon, FUN = function(x){
    nrow(occ[which(occ$name==x),]) }))

  return(rmm)
}


