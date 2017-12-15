
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
