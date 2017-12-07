#' @title Check if fields are empty in a range model metadata list
#'
#' @description Check if fields are empty in a range model metadata list
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @param obligateOnly logical; only show empty obligat fields
#'
#' @examples
#' rmm=rangeModelMetadataTemplate(useCase='apAll')
#' raster.files=list.files(system.file("extdata/Env_Demo",package='rangeModelMetadata'),full.names = T)
#' env=raster::stack(raster.files)
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=0) # for fitting environment
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=1) # for the first environment that you're transfering to
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=2) # for the second environment that you're transfering to, etc.
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

rmmPrintEmpty=function(rmm,obligateOnly=FALSE){


#
#   rmm1=rmmToCSV(rmm, filename = NULL)
#
#   for(i in 1:length(rmm)){
#     print(!length(rmm[[i]]))
#   }
#     field2=as.character(unique(dd.f2$field2))
#     rmm[[i]]=sapply(field2,function(x) NULL)
#     for(j in 1:length(field2)){
#       if(!useCase=='apAll'){
#         dd.f3=dd.f2[dd.f2$field2==names(rmm[[i]])[j] & dd.f2[,useCase]==1,]
#         #subset(dd.f2,field2==names(rmm[[i]])[j]  & Obligate==1)
#       } else { dd.f3=subset(dd.f2,field2==names(rmm[[i]])[j]) }
#       field3=as.character(unique(dd.f3$field3))
#       if(!all(is.na(field3) | is.null(field3) | field3=='')){
#         rmm[[i]][[j]]=sapply(field3,function(x) NULL)
#         for(k in 1:length(field3)){
#           if(!useCase=='apAll'){
#             dd.f4=dd.f3[dd.f3$field3==names(rmm[[i]][[j]])[k] & dd.f3[,useCase]==1,]
#             #subset(dd.f3,field3==names(rmm[[i]][[j]])[k] & Obligate==1)
#           } else {  dd.f4=subset(dd.f3,field3==names(rmm[[i]][[j]])[k]) }
#           field4=as.character(unique(dd.f4$entity))
#           if(!all(is.na(field4) | is.null(field4) | field4=='')) rmm[[i]][[j]][[k]]=sapply(field4,function(x) NULL)
#         }
#       }
#     }
#   }
  #for(i in 1:legrmm,function(x) !length(rmm))
  ##nametree function by Vincent Zoonekynd
  #nametree <- function(X, prefix = "")
  #  if( is.list(X) )
  #    for( i in seq_along(X) ) {
  #      cat( prefix, names(X)[i], "\n", sep="" )
  #      nametree(X[[i]], paste0(prefix, ""))
  #    }

  # rmm=rangeModelMetadataTemplate()
  # rmm1=rmm
  # is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))
  #
  # for(i in 1:length(rmm)){
  #   #rmm1[[i]]=
  #     is.null(rmm1[[i]])
  #     is.NullOb(rmm1[[i]])
  # }
  #
  # a=list(ww=1,ee=2)
  # a$ww=NULL
  #
  # ## Recursively step down into list, removing all such objects
  # rmNullObs <- function(x) {
  #   x <- Filter(Negate(is.NullOb), x)
  #   lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
  #
  # }
  #
  # rmNullObs(rmm)
  #list_names <- capture.output(nametree(rmm))

  #if(obligateOnly){

  #  dd=read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetadata'),stringsAsFactors=F)
  #  dd<-dd[which(dd$Obligate==1),]
  #  accepted_names<-c(as.character(dd$Field1), as.character(dd$Field2), as.character(dd$Field3), as.character(dd$Field4))
  #  list_names<-list_names[which(list_names%in%accepted_names)]


  #}

  #non_null_names<-rmNullObs(x = rmm)
  #rmm<-rmNullObs(rmm)
  #non_null_names<-capture.output(nametree(rmm))
  #empty_names<-list_names[which(!list_names%in%non_null_names)]

  #return(empty_names)

}

##########################################################################################
##########################################################################################
##########################################################################################
#' @title Check if fields are non-NULL in a range model metadata list
#'
#' @description Check if fields are full in a range model metadata list
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @param obligateOnly logical; only show empty obligat fields
#'
#' @examples
#' rmm=rangeModelMetadataTemplate(useCase='apAll')
#' raster.files=list.files(system.file("extdata/Env_Demo",package='rangeModelMetadata'),full.names = T)
#' env=raster::stack(raster.files)
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=0) # for fitting environment
#' rmm=rmmAutofillPackageCitation(rmm,c('raster','sp'))
#' rmmPrintFull(rmm)
#'
#' @return printout to the console
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

# would better if this didn't have the quotes in the names, but this is fine for viewing

rmmPrintFull=function(rmm){
  out=unlist(rmm)
  names(out)=gsub('.','$',names(out),fixed=T)
  out
}

