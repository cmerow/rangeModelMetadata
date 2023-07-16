
#' @export
#'
.rmmLeftJustify=function(dd1){
  #  for testing
  #  head(dd1[,1:4],20)
  to.move2=is.na(dd1$field2)
  dd1$field2[to.move2]=dd1$entity[to.move2]
  to.move3=is.na(dd1$field3) & !to.move2
  dd1$field3[to.move3]=dd1$entity[to.move3]
  dd1$entity[to.move2 | to.move3]=NA
  dd1

}

#############
.onAttach <- function(libname,pkgname) {
  packageStartupMessage('Type, vignette("rmm_directory") for an overview of functions\n
  vignette("rmm_vignette") for using the functions \n
  vignette("rmm_workflow") for incorporating rmm in a range modeling workflow, or\n
  vignette("rmm_Multispecies") for including multiple species in a single rmm object')
}

###############
#' @title Print supported family names for rmm objects
#'
#' @description Used to see options to for specifying an rmm object template
#'
#' @examples
#' rmmFamilies()
#' @return a vector of characters, indicating which RMMS families are supported
#' @export
#'
rmmFamilies <- function(){

  dd <- utils::read.csv(system.file("extdata/dataDictionary.csv",
                                 package='rangeModelMetadata'),
                     stringsAsFactors=FALSE)

  tmp <- unique(dd$family)

  unique(unlist(lapply(tmp,function(x) strsplit(x,', '))))

}


###############
#' @title Open range model metadata dictionary.
#'
#' @description For viewing only
#'
#' @param excel logical; open in excel?
#' @examples
#' dd=rmmDataDictionary()
#' @return If `excel==FALSE`, returns a data.frame, if `excel==TRUE` it returns nothing but attempts to open the metadata dictionary in excel.
#' @export
#'
rmmDataDictionary <- function(excel=FALSE){

  ddFile <- system.file("extdata/dataDictionary.csv",
                        package='rangeModelMetadata')

  if(!excel) {dd <- utils::read.csv(ddFile,stringsAsFactors=FALSE); return(dd)}

  if(excel) {system(paste0('open ', ddFile, ' -a "Microsoft Excel"')) }
}

#############
