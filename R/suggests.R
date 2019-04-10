#' @title Suggest inputs for a range model metadata list
#'
#' @description Supply fields to receive suggested inputs
#'
#' @details
#' See Examples.
#'
#' @param charString string referencing fields of the form `field1$field2` or `field1$field2$field3`, etc.
#' @param fullFieldDepth print all fields below the current field depth
#'
# @examples
#' rmm1=rmmTemplate()
#' rmmSuggest('dataPrep',fullFieldDepth=FALSE)
#' rmmSuggest('dataPrep',fullFieldDepth=TRUE)
#' rmmSuggest('dataPrep$errors$duplicateRemoval')
#' rmmSuggest('dataPrep$errors$duplicateRemoval$rule')
#' rmmSuggest('model')
#' rmmSuggest('modelFit$algorithmSettings$')
#' rmmSuggest('modelFit$algorithmSettings$maxent$')
#' rmmSuggest('$modelFit$algorithmSettings$maxent$featureSet')

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

# Need to make this also take objects of the form rmm$data rather than just a character
rmmSuggest=function(charString,fullFieldDepth=FALSE){

  #  for testing
  #  head(dd1[,1:6],20)

  # toss leading $ if it occures
  if(substr(charString,1,1)=='$') charString=substr(charString,2,nchar(charString))

  dd=utils::read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetadata'),stringsAsFactors=F)
  dd=.rmmLeftJustify(dd)

  out=sapply(c('type','suggestions'),function(x) NULL)
  fields=unlist(strsplit(charString,'$',fixed=T))
  dd1=dd
  # subset to get just the rows related to the charString
  for(i in 1:length(fields)) dd1=subset(dd1,dd1[,i]==fields[i])

  if(nrow(dd1)>1){
    if(!fullFieldDepth){
      suggestions=unique(dd1[,i+1])
      suggestions=make.names(suggestions)
      suggestions=gsub('\\.(\\w?)', '\\U\\1', suggestions, perl=T)
      out$suggestions=paste0(charString,"$",suggestions)
    } else {
      out$suggestions=as.vector(apply(dd1[,1:4],1,function(x) gsub('$NA','',paste0(x,collapse='$'),fixed=T)))
    }
    out$type=names(dd1)[i+1]
  } else {
    out$type=dd1$type
    out$suggestions= strsplit(dd1$example,'; ')[[1]]
    out$suggestions=gsub(';','',out$suggestions)
  }


  out
}
