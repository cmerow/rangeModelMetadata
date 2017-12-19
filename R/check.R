

#' @title Check field names of a range model metadata list against conventions
#'
#' @description Identify nonstandard fields
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#'
#' @examples
#' rmm<-rangeModelMetadataTemplate() # Make an empty template
#' rmm$dataPrep$biological$taxonomicHarmonization$taxonomy_source<-"The Plant List"
#' # Add a new, non-standard field
#' rmmCheckName(rmm)
#' # Checking the names should identify the new, non-standard field we've added ("taxonomy_source")
#'
#'
#' @return A vector of names that are not found in the range model metadata dictionary.
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
#' @note Names returned by this check may be either incorrectly named or correctly named but missing from the data dictionary.
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family check
#' @import stats
#' @import utils
#' @export
rmmCheckName=function(rmm, cutoff_distance = 3, returnData = F ){


  list_elements<-capture.output(rmm)
  list_elements<-list_elements[grep(pattern = "$",x = list_elements,fixed = T)] #remove elements that aren't field names

  #Now, we need to purge the non-terminal list elements.
  #Solution (probably not optimal):
  #Identify elements that are completely contained within another element (but are not identical to that element).
  #Any of these will not be terminal element names

  terminal<-lapply(X = list_elements,FUN = function(x){

    if(length(grep(pattern = x,x = unique(list_elements),fixed = T))>1){output<-FALSE}else{output<-TRUE}
    return(output)
  })
  terminal<-unlist(terminal)
  list_elements<-list_elements[terminal]
  rm(terminal)

  dd=utils::read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetadata'),stringsAsFactors=F)
  dd_names<-NULL
  for(i in 1:nrow(dd)){
    val_i<-dd[i,][unique(c(grep(pattern = "field",x = colnames(dd)),grep(pattern = "entity",x = colnames(dd)))) ]#The complicate indexing ensures that id additional fields (eg field4,field5) are added things won't break
    val_i<-val_i[which(!is.na(val_i))]
    val_i<-paste("$",paste(val_i,collapse = "$"),sep = "")
    dd_names<-c(dd_names,val_i)
  }

  rm(i,val_i)


  name_check_df<-as.data.frame((matrix(ncol=4,nrow = length(list_elements))))
  colnames(name_check_df)<-c("exact_match","partial_match","partial_match_suggestions","not_matched")

  for(i in 1:length(list_elements)){

    element_i<-list_elements[i]


    if(element_i%in%dd_names){
      name_check_df$exact_match[i]<-element_i}else{  #if name is valid, else:

        min_dist<-min(adist(x = element_i,y = dd_names))

        if(min_dist<=cutoff_distance){


          name_check_df$partial_match[i]<-element_i
          name_check_df$partial_match_suggestions[i]<-dd_names[which.min(adist(x = element_i,y = dd_names))]
        }

        if(min_dist>cutoff_distance){
          name_check_df$not_matched[i]<-element_i

        }

      }#if name is NOT valid exactly
  }#i loop

  if(nrow(name_check_df)>0){

    if(length(which(!is.na(name_check_df$exact_match)))>0 ){
      cat(paste("The names ",paste(name_check_df$exact_match[which(!is.na(name_check_df$exact_match))],collapse = ", "), " appear accurate.", sep = ""   ))
      cat(noquote("\n "))
    }

    if(length(which(!is.na(name_check_df$partial_match)))>0 ){
      message(paste("The names ",paste(name_check_df$partial_match[which(!is.na(name_check_df$partial_match))],collapse = ", "), " are similar to suggested names, please verify.  Suggested alternatives include: ",paste(name_check_df$partial_match_suggestions[which(!is.na(name_check_df$partial_match_suggestions))],collapse = ", "), sep = ""   ))
      cat(noquote("\n "))
      }

    if(length(which(!is.na(name_check_df$not_matched)))>0 ){
      message(paste("The names ",paste(name_check_df$not_matched[which(!is.na(name_check_df$not_matched))],collapse = ", "), " are not similar to any suggested names, please verify that these are accurate.", sep = ""   ))
      cat(noquote("\n "))
    }


    if(returnData==T){
      return(name_check_df)
    }

  }#overall fx

}

##############################################################

##############################################################

#' @title Check values of a range model metadata list against commonly used values
#' @export
#'
#' @description Identify nonstandard values
#'
#' @details
#' See Examples.
#' @param rmm a range model metadata list
#' @param cutoff_distance The maximum allowable similarity (Levenshtein (edit) distance) for use in fuzzy matching.
#' @param returnData Should a dataframe containing information on matched and unmatched values be returned?  Default is FALSE
#' @examples
#' rmm<-rangeModelMetadataTemplate() #First, we create an empty rmm template
#' rmm$data$environment$variableNames<- c("bio1", "bio 2", "bio3", "cromulent")
#' #We add 3 of the bioclim layers, including a spelling error (an extra space) in bio2,
#' # and a word that is clearly not a climate layer, 'cromulent'.
#' rmmCheckValue(rmm = rmm)
#' #Now, when we check the values, we see that bio1 and bio2 are reported as exact matches,
#' #while 'bio 2' is flagged as a partial match with a suggested value of 'bio2',
#' # and cromulent is flagged as not matched at all.
#' #If we'd like to return a dataframe containing this information in a perhaps more useful format:
#' rmmCheckValue_output<-rmmCheckValue(rmm = rmm,returnData = T)
#' @return Text describing identical, similar and non-matched values for rmm entities with suggested values.  If returnData = T, a dataframe is returned containing 5 columns: field (the rmm entity), exact_match (values that appear correct), partial_match (values that are partial_match to common values), not_matched( values that are dissimilar from accepted values), partial_match_suggestions (suggested values for partial_match values).
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
#' @note Names returned by this check may be either incorrectly named or correctly named but missing from the data dictionary.
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family check
#' @export
rmmCheckValue <- function( rmm, cutoff_distance = 3, returnData = F ){
  dd<-utils::read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetadata'),stringsAsFactors=F)

  dd_constrained<-dd[which(dd$valuesConstrained!="no"),]

  #For all fields with either a kinda or yes in the valuesConstrained field, check values against those in datadictionary

  #Split values into a "exact_match", "partial_match", and "not partial_match" set.

  #Print each set of values along with a note.


  value_check_df<-as.data.frame((matrix(ncol=5,nrow = nrow(dd_constrained))))
  colnames(value_check_df)<-c("field","exact_match","partial_match","partial_match_suggestions","not_matched")

  for(i in 1:nrow(dd_constrained)){

    dd_i<-dd_constrained[i,][c("field1",'field2','field3','entity')]

    if(length(which(is.na(dd_i)))>0){
      dd_i<-paste(dd_i[-which(is.na(dd_i))],collapse = "$")
    }else{dd_i<-paste(dd_i,collapse = "$")}

    value_check_df[i,1]<-dd_i<-paste("rmm",dd_i,sep = "$")

    constrained_values<-dd_constrained$values[i]
    constrained_values<-unlist(strsplit(x = constrained_values,split = "; "))
    value_i<-eval(parse(text=dd_i))

    if(!is.null(eval(parse(text=dd_i)))){

      for(j in 1:length(eval(parse(text=dd_i)))){

        element_j<-eval(parse(text=dd_i))[j]

        if(element_j%in%constrained_values){
          value_check_df$exact_match[i]<-paste(na.omit(c(value_check_df$exact_match[i],element_j)),sep = "; ",collapse = "; ")}else{

            #get the distance between the value and the potential values.

            min_dist<-min(adist(x = element_j,y = constrained_values))

            if(min_dist<=cutoff_distance){
              constrained_values[which.min(adist(x = element_j,y = constrained_values))]

              value_check_df$partial_match[i]<-paste(na.omit(c(value_check_df$partial_match[i],element_j)),sep = "; ",collapse = "; ")
              value_check_df$partial_match_suggestions[i]<-paste(na.omit(c(value_check_df$partial_match_suggestions[i],constrained_values[which.min(adist(x = element_j,y = constrained_values))])),sep = "; ",collapse = "; ")
            }

            if(min_dist>cutoff_distance){
              value_check_df$not_matched[i]<-paste(na.omit(c(value_check_df$not_matched[i],element_j)),sep = "; ",collapse = "; ")

            }else{

              value_check_df$partial_match[i]<-paste(na.omit(c(value_check_df$not_matched[i],element_j)),sep = "; ",collapse = "; ")

            }
            #take the partial_matchst value,
            #or if distance > something, label it as "not_matched"

          }#if element is not in suggested values


      }#j loop

    }#if the value is not null


  }
  #Print stuff

  #Remove fields consisting of all NAs
  value_check_df<-value_check_df[!apply(is.na(value_check_df[,2:5]),1,all),]

  if(nrow(value_check_df)>0){

    for(r in 1:nrow(value_check_df)){
      cat(noquote("For the field ",value_check_df$field[r],":")  )
      cat(noquote("\n "))

      if(!is.na(value_check_df$exact_match[r])){
        cat(noquote("   The entries",value_check_df$exact_match[r], " appear accurate."   ))
        cat(noquote("\n "))}
      if(!is.na(value_check_df$partial_match[r])){
        message(noquote("   The entries",value_check_df$partial_match[r], " are similar to suggested values, please verify.  Suggested alternatives include: ", value_check_df$partial_match_suggestions[r]   ))
        cat(noquote("\n "))
        }

      if(!is.na(value_check_df$not_matched[r])){
        message(noquote("   The entries",value_check_df$not_matched[r], " are not similar to any suggested values, please verify that these are accurate."   ))
        cat(noquote("\n "))
        }
    }

  }else{  #if there are values to check

    message(noquote("The are no suggested fields to verify in this rmm object"))
    cat(noquote("\n "))

  } #if there are no values to check

  if(returnData==T){
    return(value_check_df)
  }


}#end of fx

#############################

#############################

#' @title Check for missing fields
#'
#' @description Identify obligate fields that are missing
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @param useCase The rmm useCase to check the rmm against
#'
#' @examples
#' rmm<-rangeModelMetadataTemplate() # Make an empty template
#'
#'
#' @return A vector of names that are missing from the rmm object.
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family check
#' @import stats
#' @import utils
#' @export
rmmCheckMissingNames<-function(rmm,useCase="apObligate"){

  list_elements<-capture.output(rmm)
  list_elements<-list_elements[grep(pattern = "$",x = list_elements,fixed = T)] #remove elements that aren't field names

  #Now, we need to purge the non-terminal list elements.
  #Solution (probably not optimal):
  #Identify elements that are completely contained within another element (but are not identical to that element).
  #Any of these will not be terminal element names

  terminal<-lapply(X = list_elements,FUN = function(x){

    if(length(grep(pattern = x,x = unique(list_elements),fixed = T))>1){output<-FALSE}else{output<-TRUE}
    return(output)
  })
  terminal<-unlist(terminal)
  list_elements<-list_elements[terminal]


  dd=utils::read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetadata'),stringsAsFactors=F)
  dd_ob<-dd[which(dd[useCase]==1),]

  obligate_names<-NULL
  for(i in 1:nrow(dd_ob)){
    val_i<-dd_ob[i,][unique(c(grep(pattern = "field",x = colnames(dd_ob)),grep(pattern = "entity",x = colnames(dd_ob)))) ]#The complicate indexing ensures that id additional fields (eg field4,field5) are added things won't break
    val_i<-val_i[which(!is.na(val_i))]
    val_i<-paste("$",paste(val_i,collapse = "$"),sep = "")
    obligate_names<-c(obligate_names,val_i)

  }

  missing_names<-obligate_names[which(!obligate_names%in%list_elements)]
  #list_elements[which(!list_elements%in%obligate_names)]

  if(length(missing_names)==0){cat("All obligate field names are present")}

  return(missing_names)

}


##################################

##################################

#' @title Run a final check of an rmm object
#'
#' @description Check an rmm object for non-standard and missing values and fields
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @param useCase The rmm useCase to check the rmm against
#'
#' @examples
#' rmm<-rangeModelMetadataTemplate() # Make an empty template
#' rmmCheckFinalize(rmm)
#'
#'
#' @return Prints feedback to point out possible errors.
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family check
#' @import stats
#' @import utils
#' @export
rmmCheckFinalize<-function(rmm,useCase="apObligate"){

  names<-rmmCheckName(rmm,returnData = T)

  values<-rmmCheckValue(rmm = rmm,returnData = T)

  missing_names<-rmmCheckMissingNames(rmm,useCase = useCase)



  if(length(na.omit(values$partial_match))==0 & length(na.omit(values$not_matched))==0 & #All values are exactly matched
     length(missing_names)==0 & #No names are missing
     length(na.omit(names$partial_match))==0 & length(na.omit(names$not_matched))==0 #All names are exactly matched
        ){
  print("Everything looks good!")
  }


}


