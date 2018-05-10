
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
  packageStartupMessage('Type vignette("rmm_vignette") or vignette("rmm_directory") to get started')
}
###############
#' @title Print supported family names for rmm objects
#'
#' @description Used to see options to for specifying an rmm object template
#'
#' @examples
#' rmmFamilies()
#' @export
#'
rmmFamilies=function(){
  dd=utils::read.csv(system.file("extdata/dataDictionary.csv",
                                 package='rangeModelMetadata'),
                     stringsAsFactors=F)
  unique(dd$family)
}


###############
#' @title Open range model metadata dictionary.
#'
#' @description For viewing only
#'
#' @examples
#' dd=rmmDataDictionary()
#' @export
#'
rmmDataDictionary=function(excel=F){
  ddFile=system.file("extdata/dataDictionary.csv",package='rangeModelMetadata')
  if(!excel) {dd=utils::read.csv(ddFile,stringsAsFactors=F); return(dd)}
  if(excel) {system(paste0('open ', ddFile, ' -a "Microsoft Excel"')) }
}
