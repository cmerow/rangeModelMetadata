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
#' @examples
#' rmm=rangeModelMetadataTemplate(useCase='apAll')
#' r.f=system.file("extdata/Env_Demo",package='rangeModelMetadata')
#' raster.files=list.files(r.f,full.names = TRUE)
#' env=raster::stack(raster.files)
#' # for fitting environment
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=0)
#' # for the first environment that you're transfering to
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=1)
#' # for the second environment that you're transfering to, etc.
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=2)
#' \dontrun{
#' rmmToCSV(rmm,file='somePathOnYourMachine/rmm_example.csv')
#' }
# @return
#' @author Hannah Owens <hannah.owens@@gmail.com>, Cory Merow <cory.merow@@gmail.com>
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
  if (!any(class(x) == "list")){
    warning("Target input invalid. Input must be of class 'list'.\n");
    return(NULL);
  }

  #Generate headers for table
  csvTable <- matrix(ncol = 5)
  colnames(csvTable) <- c("Field 1", "Field 2", "Field 3", "Entity", "Value");
  csvTable <- as.data.frame(csvTable)

  #Loop through list of lists to fill the table
  ##Field 1
  for (i in 1:length(x)){
    ##Field 2
    for (j in 1:length(x[[i]])){
      #Check to see if Field 2 is null
      if(!is.list(x[[i]][[j]])){
        if(is.null(unlist(x[[i]][j]))){
          print(cat(i,'  ',j))
          csvTable <- rbind(csvTable,c(names(x)[i],"NA", "NA", names(x[[i]])[j], "NULL"));
        }
        else{
          print(cat(i,'  ',j))
          csvTable <- rbind(csvTable,c(names(x)[i],"NA", "NA", names(x[[i]])[j], cleanForCSV(x[[i]][[j]])));
        }
      }
      else{
        ##Field 3
        for (k in 1:length(x[[i]][[j]])){
          #Check to see if Field 3 is null
          if(!is.list(x[[i]][[j]][[k]])){
            if (is.null(unlist(x[[i]][[j]][k]))){
              print(cat(i,'  ',j,'  ',k))

              csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], "NA", names(x[[i]][[j]])[k], "NULL"));
            }
            else{
              print(cat(i,'  ',j,'  ',k))
              csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], "NA", names(x[[i]][[j]])[k], cleanForCSV(x[[i]][[j]][[k]])));
            }
          }
          else{
            for (l in 1:length(names(x[[i]][[j]][[k]]))){
              if (is.null(unlist(x[[i]][[j]][[k]][l]))){
                print(cat(i,'  ',j,'  ',k,'  ',l))

                csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], names(x[[i]][[j]])[k], names(x[[i]][[j]][[k]])[l], "NULL"));
              }
              else{
                print(cat(i,'  ',j,'  ',k,'  ',l))
                csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], names(x[[i]][[j]])[k], names(x[[i]][[j]][[k]])[l], cleanForCSV(x[[i]][[j]][[k]][[l]])));
              }
            }
          }
        } # end loop over field 3
      }
    }
  }

  #Assign header row
  csvTable <- csvTable[-1,]

  #Write to csv
  if(!is.null(filename)) utils::write.csv(csvTable, filename, row.names = F)

  return(csvTable)
}
