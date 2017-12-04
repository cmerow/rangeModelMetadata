#' @title Create .csv File From rangeModelMetaData Object
#'
#' @description Takes user-input rangeModelMetaData object and from it generates a .csv file that can be used to document range model metadata for a variety of applications.
#'
#' @details
#' See Examples.
#'
#' @param x An object of class \code{\link{rmm}} that the user wishes transposed into a .csv file.
#'
#' @param filename The name of the transcription .csv file.
#'
#'
#' @examples
#' #outFile=paste0(path.package('rangeModelMetadata'),"/inst/extdata/demo_rmmToCSV.csv")
#' \dontrun{
#' outFile='~/Desktop/demo_rmmToCSV.csv'
#' rmmObj=rangeModelMetadataTemplate('apAll')
#' rmmToCSV(rmmObj,filename=outFile)
#' system(paste0('open ', outFile, ' -a "Microsoft Excel"'))
#' }
#' @return
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>, Hannah Owens <hannah.owens@@gmail.com
#' @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

rmmToCSV=function(x = rangeModelMetadataTemplate('apAll'), filename = NULL){
  #Verify user has passed the function an rmm object
  if (!class(x) == "list"){
    warning("Target input invalid. Input must be of class 'list'.\n");
    return(NULL);
  }
  #Verify user has passed the function a string for the .csv name
  if (!is.character(filename)){
    warning("Filename input invalid. Input must be a character string.\n");
    return(NULL);
  }

  #Generate headers for table
  csvTable <- c("field 1", "field 2", "field 3", "entity");

  #Loop through list of lists to fill the table
  for (i in 1:length(x)){
    if(is.null(names(x[[i]]))){ # there's a lonely j here
    #if(is.null(names(x[[i]][j]))){ # there's a lonely j here
      csvTable <- rbind(csvTable, c(names(x)[i],"BLANK", "BLANK", "BLANK"));
    } else {
      for (j in 1:length(x[[i]])){
        if(is.null(names(x[[i]][[j]]))){
        #if(is.null(names(x[[i]][[j]][k]))){
          csvTable <- rbind(csvTable,c(names(x)[i],names(x[[i]])[j], "BLANK", "BLANK"));
        } else {
          for (k in 1:length(x[[i]][[j]])){
            if (is.null(unlist(x[[i]][[j]][k]))){
              csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], names(x[[i]][[j]])[k], "BLANK"));
            } else {
              csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], names(x[[i]][[j]])[k], unlist(x[[1]][[1]][1])));
            }
          } # end k
        }
      }
    }
  }

  #Assign header row
  colnames(csvTable) <- csvTable[1,]
  csvTable <- csvTable[-1,]

  #Write to csv
  write.csv(csvTable, filename, row.names = F);
}
