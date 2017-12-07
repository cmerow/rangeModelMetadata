#' @title Create .csv File From rangeModelMetaData Object
#'
#' @description Takes user-input rangeModelMetaData object and from it generates a .csv file that can be used to document range model metadata for a variety of applications.
#'
#' @details
#' See Examples.
#'
#' @param x An object of class \code{rmm} that the user wishes transposed into a .csv file.
#'
#' @param filename The name of the transcription .csv file.
#'
#'
# @examples
#'
#'
# @return
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>, Hannah Owens <hannah.owens@@gmail.com
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

rmmToCSV=function(x = rangeModelMetadataTemplate(useCase='apAll'), filename = NULL){
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
  csvTable <- c("Field 1", "Field 2", "Field 3", "Entity", "Value");

  #Loop through list of lists to fill the table
  ##Field 1
  for (i in 1:length(x)){
    ##Field 2
    for (j in 1:length(x[[i]])){
      #Check to see if Field 2 is null
      if(!is.list(x[[i]][[j]])){
        if(is.null(unlist(x[[i]][j]))){
          csvTable <- rbind(csvTable,c(names(x)[i],"NA", "NA", names(x[[i]])[j], "EMPTY"));
        }
        else{
          csvTable <- rbind(csvTable,c(names(x)[i],"NA", "NA", names(x[[i]])[j], unlist(x[[i]][j])));
        }
      }
      else{
        ##Field 3
        for (k in 1:length(x[[i]][[j]])){
          #Check to see if Field 3 is null
          if(!is.list(x[[i]][[j]][[k]])){
            if (is.null(unlist(x[[i]][[j]][k]))){
              csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], "NA", names(x[[i]][[j]])[k], "EMPTY"));
            }
            else{
              csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], "NA", names(x[[i]][[j]])[k], unlist(x[[i]][[j]][k])));
            }
          }
          else{
            for (l in 1:length(names(x[[i]][[j]][[k]]))){
              if (is.null(unlist(x[[i]][[j]][[k]][l]))){
                csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], names(x[[i]][[j]])[k], names(x[[i]][[j]][[k]])[l], "EMPTY"));
              }
              else{
                csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], names(x[[i]][[j]])[k], names(x[[i]][[j]][[k]])[l], unlist(x[[i]][[j]][[k]][l])));
              }
            }
          }
        }
      }
    }
  }

  #Assign header row
  colnames(csvTable) <- csvTable[1,]
  csvTable <- csvTable[-1,]

  #Write to csv
  utils::write.csv(csvTable, filename, row.names = F);
}


