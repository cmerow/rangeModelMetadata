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
#' rmm <- rmmTemplate()
#' rmm <- rmmAutofillPackageCitation(rmm,c('terra','sf'))
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

  out <- lapply(packages,function(x) utils::toBibtex(utils::citation(x)))
  out <- unlist(out)
  out <- paste0(out,collapse = "")
  #The following bit could be done in a more clever way with regex, but this works for now
  out <- strsplit(x = out,split = "}@")
  out <- out[[1]]

  if(length(out) > 1){

    out[2:length(out)] <- paste0("@",out[2:length(out)])
    out[1:(length(out))-1] <- paste0(out[1:(length(out))-1], "}" )
  }

  rmm <- rmm
  rmm$code$software$packages <- out
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
#' @param env a SPatRaster object
#' @param transfer 0 if not transfer, 1:n for n environments that you're transferring to
#'
#' @examples
#' \dontrun{
#' rmm <- rmmTemplate()
#' rasterFiles <- list.files(path=paste(system.file(package='dismo'), '/ex', sep=''),
#'                        pattern='grd', full.names=TRUE)
#' #make a stack of the rasters
#' env <- terra::rast(rasterFiles)
#' # for fitting environment
#' rmm <- rmmAutofillEnvironment(rmm,env,transfer=0)
#' # for the first environment that you're transfering to
#' rmm <- rmmAutofillEnvironment(rmm,env,transfer=1)
#' # for the second environment that you're transfering to, etc.
#' rmm <- rmmAutofillEnvironment(rmm,env,transfer=2)
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
#' @importFrom terra res ext

rmmAutofillEnvironment=function(rmm,env,transfer){

  .worldclimFill=function(env,rmm){
    if(length(grep('wc2.0',names(env)))>0){
      rmm$data$environment$sources <- '@article{Fick:2017bq, author = {Fick, Stephen E and Hijmans, Robert J}, title = {{WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas}}, journal = {International Journal of Climatology}, year = {2017},  volume = {37}, number = {12},  pages = {4302--4315}}'
      rmm$data$environment$yearMin <- 1970
      rmm$data$environment$yearMax <- 2000
    }
    if(length(grep('wc1.3',names(env)))>0){
      rmm$data$environment$sources <- '@misc{hijmans2004worldclim, title={The WorldClim interpolated global terrestrial climate surfaces. Version 1.3}, author={Hijmans, RJ and Cameron, SE and Parra, JL and Jones, PG and Jarvis, A}, year={2004}}'
      rmm$data$environment$yearMin <- 1970
      rmm$data$environment$yearMax <- 2000
    }
    return(rmm)
  }

  if(is.null(transfer)) stop('specify whether this environment is used for transfer (>1) or not (0)')
  if(transfer==0){

    rmm$data$environment$resolution <- terra::res(env)

    rmm$data$environment$extentSet <- terra::ext(env)

    rmm$data$environment$variableNames <- names(env)

    rmm <- .worldclimFill(env,rmm)

  } else {

    rmm$data$transfer[paste0('environment',transfer)][[1]]$resolution <-
      terra::res(env)

    rmm$data$transfer[paste0('environment',transfer)][[1]]$extentSet <-
      terra::ext(env)

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
#' @importFrom BIEN BIEN_metadata_citation
rmmAutofillBIEN <- function(rmm, occurrences){

  rmm$data$occurrence$taxon <- unique(occurrences$scrubbed_species_binomial)

  rmm$data$occurrence$dataType   <- "presence only"

  rmm$data$occurrence$yearMin <- NA
  rmm$data$occurrence$yearMax <- NA

  rmm$data$occurrence$sources <- BIEN::BIEN_metadata_citation(dataframe = occurrences)$references

  rmm$data$occurrence$presenceSampleSize <-
    unlist(lapply(X = rmm$data$occurrence$taxa, FUN = function(x){
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
#' rmm <- rmmTemplate()
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

  rmm$data$occurrence$presenceSampleSize <-
    unlist(lapply(X = rmm$data$occurrence$taxon, FUN = function(x){
    nrow(occ[which(occ$name==x),]) }))

  return(rmm)
}


