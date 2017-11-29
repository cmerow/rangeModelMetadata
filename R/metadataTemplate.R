#' @title
#'
#' @description
#'
#' @details
#' See Examples.
#'
#' @param
#' @export
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

speciesRangeMetaDataTemplate=function(){

  dd=read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetaData'))

  #== Level 1 fields
  field1=as.character(unique(dd$Field1))

  mdo=sapply(field1,function(x) NULL)

  #== Level 2-4 fields
  for(i in 1:length(mdo)){
    dd.f2=subset(dd,Field1==names(mdo)[i])
    field2=as.character(unique(dd.f2$Field2))
    mdo[[i]]=sapply(field2,function(x) NULL)
    for(j in 1:length(field2)){
      dd.f3=subset(dd.f2,Field2==names(mdo[[i]])[j])
      field3=as.character(unique(dd.f3$Field3))
      if(!all(is.na(field3))){
        mdo[[i]][[j]]=sapply(field3,function(x) NULL)
        for(k in 1:length(field3)){
          dd.f4=subset(dd.f3,Field3==names(mdo[[i]][[j]])[k])
          field4=as.character(unique(dd.f4$Field4))
          if(!all(is.na(field4))) mdo[[i]][[j]][[k]]=sapply(field4,function(x) NULL)
        }
      }
    }
  }

  mdo
}









