
#' @title Helper function for non-string metadata in rmmToCSV
#'
#' @description Cleans up metadata instances that get messy if one tries to write them directly to csv tables (i.e. extent objects, bibtex objects.)
#'
#' @details
#' This is a utility function for use by \code{rmmToCSV}.
#'
#' @param x An \code{rmm} entry that returned to the \code{rmmToCSV} function.
#'
# @examples
#'
#' @return Reformatted element for use in \code{rmmToCSV} function.
#'
#' @author Hannah Owens <hannah.owens@@gmail.com>, Cory Merow <cory.merow@@gmail.com>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family csvConversion
#' @export

cleanForCSV <- function(x = NULL){
  y <- unlist(x[1]);
  z <- unlist(x)
  if(class(x)=="Extent"){
    temp <- paste("xmin: ", x[1], "; xmax: ", x[2], "; ymin: ", x[3], "; ymax: ", x[4], sep = "")
  }
  #For elements composed of vectors
  else if(length(z) > 1){
    temp <- paste(z, collapse = "; ")
  }
  #For vectors with a single element
  else{
    temp <- z;
  }
  return(temp);
}

##############################################################################
##############################################################################
##############################################################################

#' @title Create rangeModelMetaData (`rmm`) object from a .csv File
#'
#' @description Takes user-input .csv file and converts it to a rangeModelMetaData (`rmm`) object.
#'
#' @details
#' See Examples.
#'
#' @param csv A character file path to the csv file.
#' @param family character string; specifies an application profile (use case) by specifiying the families of entitiies that should be included. Specifying NULL includes all entities. Use \code{rmmFamilies()} to see supported values.
# @param families character vector; an alternative to specifying `family`. Provide a vector of family names to include all entities in a family in the template. Use `rmmFamilyNames` to see supported values.
#'
#' @examples
#' csv <- "somePathOnYourMachine/rmm_example.csv";
#' \dontrun{temp <- csvToRMM(csv);}
#'
#' @return An \code{rmm} object that was read from the supplied .csv text file.
#' @author Hannah Owens <hannah.owens@@gmail.com>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family csvConversion
#' @export
#'


csvToRMM <- function(csv, family=NULL) {

  # read in csv from path
  dd <- read.csv(csv, stringsAsFactors=FALSE)

  #Creating named values from the .csv file
  values <- mapply(assign, dd$entity, dd$value)

  # create a blank rmm to fill from the values in csv
  rmm <- rmmTemplate(family = family)

  # loop over all rows, determine how many fields are not NA,
  # then fill in the value from csv to the slot in the rmm list
  # NOTE: all values are split to vectors by ";" by default
  for(i in 1:nrow(dd)) {
    curRow <- dd[i,!is.na(dd[i,])]
    if (length(curRow) == 5){
      if(curRow[[5]] != "NULL") {
        rmm[[curRow[[1]]]][[curRow[[2]]]][[curRow[[3]]]][[curRow[[4]]]] <- strsplit(curRow[[5]], "; ")[[1]]
      }
    }
    else if (length(curRow) == 4){
      if(curRow[[4]] != "NULL") {
        rmm[[curRow[[1]]]][[curRow[[2]]]][[curRow[[3]]]] <- strsplit(curRow[[4]], "; ")[[1]]
      }
    }
    else if (length(curRow) == 3){
      if(curRow[[3]] != "NULL") {
        rmm[[curRow[[1]]]][[curRow[[2]]]] <- strsplit(curRow[[3]], "; ")[[1]]
      }
    }
    else if (length(curRow) == 2){
      if(curRow[[2]] != "NULL") {
        rmm[[curRow[[1]]]] <- strsplit(curRow[[2]], "; ")[[1]]
      }
    }
  }

  return(rmm)
}

##############################################################################
##############################################################################
##############################################################################


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
#' rmm=rmmTemplate()
#' rasterFiles=list.files(path=paste(system.file(package='dismo'), '/ex', sep=''),
#'                        pattern='grd', full.names=TRUE)
#' #make a stack of the rasters
#' env=raster::stack(rasterFiles)
#' # for fitting environment
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=0)
#' # for the first environment that you're transfering to
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=1)
#' # for the second environment that you're transfering to, etc.
#' rmm=rmmAutofillEnvironment(rmm,env,transfer=2)
#' \dontrun{
#' tmp=rmmToCSV(rmm,file='somePathOnYourMachine/rmm_example.csv')
#' }
#' @return An data frame containing all the information from an \code{rmm} object.
#' @author Hannah Owens <hannah.owens@@gmail.com>, Cory Merow <cory.merow@@gmail.com>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family csvConversion
#' @export

rmmToCSV=function(x = rmmTemplate(family=NULL), filename = NULL){
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
          csvTable <- rbind(csvTable,c(names(x)[i],"NA", "NA", names(x[[i]])[j], "NULL"));
        }
        else{
          csvTable <- rbind(csvTable,c(names(x)[i],"NA", "NA", names(x[[i]])[j], cleanForCSV(x[[i]][[j]])));
        }
      }
      else{
        ##Field 3
        for (k in 1:length(x[[i]][[j]])){
          #Check to see if Field 3 is null
          if(!is.list(x[[i]][[j]][[k]])){
            if (is.null(unlist(x[[i]][[j]][k]))){
              csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], "NA", names(x[[i]][[j]])[k], "NULL"));
            }
            else{
              csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], "NA", names(x[[i]][[j]])[k], cleanForCSV(x[[i]][[j]][[k]])));
            }
          }
          else{
            for (l in 1:length(names(x[[i]][[j]][[k]]))){
              if (is.null(unlist(x[[i]][[j]][[k]][l]))){
                csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], names(x[[i]][[j]])[k], names(x[[i]][[j]][[k]])[l], "NULL"));
              }
              else{
                csvTable <- rbind(csvTable, c(names(x)[i],names(x[[i]])[j], names(x[[i]][[j]])[k], names(x[[i]][[j]][[k]])[l], cleanForCSV(x[[i]][[j]][[k]][[l]])));
              }
            }
          }
        } # end loop over field 3
      }
    }
  }

  #Assign header row
  csvTable <- csvTable[-1,];

  #Write to csv
  if(!is.null(filename)) utils::write.csv(csvTable, filename, row.names = F);

  return(csvTable);
}

