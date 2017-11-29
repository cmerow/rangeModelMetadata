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
#' rmm1=rangeModelMetadataTemplate(FALSE)
#' rmmSuggest('dataPrep')
#' rmmSuggest('dataPrep$errors$duplicateRemoval')
#' rmmSuggest('dataPrep$errors$duplicateRemoval$rule')
#' rmmSuggest('model')
#' rmmSuggest('model$algorithmSettings$')
#' rmmSuggest('model$algorithmSettings$maxent$')
#' rmmSuggest('$model$algorithmSettings$maxent$featureSet')

#' @return
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
#' @note strings should note have a leading $ (fix this to remove it!)
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
rmmSuggest=function(charString){

  # toss leading $ if it occures
  if(substr(charString,1,1)=='$') charString=substr(charString,2,nchar(charString))

  dd=read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetaData'),stringsAsFactors = F)
  out=sapply(c('type','suggestions'),function(x) NULL)
  fields=unlist(strsplit(charString,'$',fixed=T))
  for(i in 1:length(fields)){ dd=subset(dd,dd[,i]==fields[i])}
  if(nrow(dd)>1){
    suggestions=unique(dd[,i+1])
    suggestions=gsub('<','',  suggestions)
    suggestions=gsub('>','',  suggestions)
    suggestions=make.names(suggestions)
    suggestions=gsub('\\.(\\w?)', '\\U\\1', suggestions, perl=T)
    out$type=names(dd)[i+1]
    out$suggestions=paste0(charString,"$",suggestions)
  } else {
    out$type=dd$Type
    out$suggestions= strsplit(dd$Example,'; ')[[1]]
  }


  out
}
