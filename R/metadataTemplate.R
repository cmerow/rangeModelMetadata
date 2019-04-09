#' @title Range modeling metadata
#'
#' @description Make an empty metadata list
#'
#' @details
#' See Examples.
#'
#' @param family character string; specifies an application profile (use case) by specifiying the families of entitiies that should be included. Specifying NULL includes all entities. Use `rmmFamilies` to see supported values.
# @param families character vector; an alternative to specifying `family`. Provide a vector of family names to include all entities in a family in the template. Use `rmmFamilyNames` to see supported values.
#' @export
#'
#' @examples
#' rmm1=rmmTemplate()
#' rmm2=rmmTemplate(family=c('base'))
#' str(rmm2)
#'
#' @return a range model metadata list
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

# change obligateOnly to a column name in the data dictionary for a particular template

rmmTemplate=function(family=NULL){
  # for testing
  # family=c('base','poop')


  # could add a check that valid families are specified
  if(any(!(family %in% rmmFamilies()))) stop('Specify a correct family. See options with rmmFamilies()')

  dd=utils::read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetadata'),stringsAsFactors=F)

  # convert to old format that worked with other code
  dd=.rmmLeftJustify(dd)

  #== Level 1 fields
  field1=as.character(unique(dd$field1))
  rmm=sapply(field1,function(x) NULL)
  #== Level 2-4 fields
  for(i in 1:length(rmm)){
    if(!is.null(family)){
      dd.f1=dd[dd$field1==names(rmm)[i],]
      dd.f2=dd.f1[unique(unlist(mapply(function(x) grep(x,dd.f1$family),family))),]
    } else {dd.f2=subset(dd,field1==names(rmm)[i])}
  #paste0(dd.f2[,c('field2','field3','entity')],collapse='$')

    field2=as.character(unique(dd.f2$field2))
    field2=field2[complete.cases(field2)]
    rmm[[i]]=sapply(field2,function(x) NULL)
    for(j in 1:length(field2)){
      if(!is.null(family)){
        dd.f3a=dd.f2[dd.f2$field2==names(rmm[[i]])[j],]
        dd.f3=dd.f3a[unique(unlist(mapply(function(x)
          grep(x,dd.f3a$family),family))),]
          #subset(dd.f2,field2==names(rmm[[i]])[j]  & Obligate==1)
      } else { dd.f3=subset(dd.f2,field2==names(rmm[[i]])[j]) }
      field3=as.character(unique(dd.f3$field3))
      field3=field3[complete.cases(field3)]
      if(!all(is.na(field3) | is.null(field3) | field3=='')){
        rmm[[i]][[j]]=sapply(field3,function(x) NULL)
        for(k in 1:length(field3)){
          if(!is.null(family)){
            dd.f4a=dd.f3[dd.f3$field3==names(rmm[[i]][[j]])[k],]
            dd.f4=dd.f4a[unique(unlist(mapply(function(x)
              grep(x,dd.f4a$family),family))),]
              #subset(dd.f3,field3==names(rmm[[i]][[j]])[k] & Obligate==1)
          } else {  dd.f4=subset(dd.f3,field3==names(rmm[[i]][[j]])[k]) }
          field4=as.character(unique(dd.f4$entity))
          field4=field4[complete.cases(field4)]
          if(!all(is.na(field4) | is.null(field4) | field4=='')) rmm[[i]][[j]][[k]]=sapply(field4,function(x) NULL)
        }
      }
    }
  }

  # label as "rmm" class
  # query for RMM class like this: "RMM" %in% class(rmm)
  class(rmm) <- append(class(rmm),"RMM")

  return(rmm)
}









