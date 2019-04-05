
#' @title Check field names of a range model metadata list against conventions
#'
#' @description Identify nonstandard fields
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @param cutoff_distance number of allowed different characters to match standardized names
#' @param returnData logical. If FALSE, the functon will return the (possibly) corrected rmm object.  If TRUE, the function will return a data.frame containing information on incorrect names.
#' @param interactiveCorrections logical. If TRUE, the user will be prompted to indicate whether the proposed correction should be accepted, thereby modifying the `rmm` object. If FALSE, suggestions will just be printed to the screen and users can edit them manually.
#' @examples
#' rmm<-rmmTemplate() # Make an empty template
#' rmm$dataPrep$biological$taxonomicHarmonization$taxonomy_source<-"The Plant List"
#' # Add a new, non-standard field
#' rmm.1=rmmCheckName(rmm)
#' # Checking the names should identify the new, non-standard field we've added ("taxonomy_source")
#'
#'
#' @return Either an rmm list object (returnData=F) or a data.frame containing information on possible name errors (returnData=T).
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

rmmCheckName <- function(rmm,
                         cutoff_distance = 3,
                         returnData = F,
                         interactiveCorrections=FALSE ){

  list_elements<-utils::capture.output(rmm)
  list_elements<-list_elements[grep(pattern = "$",x = list_elements,fixed = T)] #remove elements that aren't field names

  #Now, we need to purge the non-terminal list elements.
  #Solution (probably not optimal):
  #Identify elements that are completely contained within another element (but are not identical to that element).
  #Any of these will not be terminal element names

  terminal<-lapply(X = list_elements,FUN = function(x){

    if(length(grep(pattern = x,x = unique(list_elements),fixed = T))>1){
      output<-FALSE
    }else{
      output<-TRUE
    }
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
  #i=26,12

  name_check_df<-as.data.frame((matrix(ncol=5,nrow = length(list_elements))))
  colnames(name_check_df)<-c("exact_match","partial_match",
                             "partial_match_suggestions","corrected_name","not_matched")

  for(i in 1:length(list_elements)){

    element_i<-list_elements[i]

    if(element_i%in%dd_names){
      name_check_df$exact_match[i]<-element_i}else{  #if name is valid, else:

        min_dist<-min(adist(x = element_i,y = dd_names))

        if(min_dist<=cutoff_distance){

          name_check_df$partial_match[i]<-element_i
          name_check_df$partial_match_suggestions[i]<-
            dd_names[which.min(adist(x = element_i,y = dd_names))]

          # prompt
          cat("\n\n")
          cat("\n Element name '",element_i,"' not found in data dictionary", "!\n Did you mean: '",name_check_df$partial_match_suggestions[i],"'? " ) # prompt
          if(interactiveCorrections){
            cat("Type 'y' or 'n'.")

            take <- scan(n = 1, quiet = TRUE, what = 'raw')

            if (take == 'y' | take == 'Y') {
              #rename list element_i to the partial match suggestion

              el_i<-paste("rmm",element_i,sep = "")
              bad_name<-unlist(strsplit(el_i,"$",fixed = T))[length(unlist(strsplit(el_i,"$",fixed = T)))]
              parent_i<-paste(unlist(strsplit(el_i,"$",fixed = T))[1:length(unlist(strsplit(el_i,"$",fixed = T)))-1],collapse = "$")

              exp_i<-paste("names(",parent_i,")[",which(names(eval(parse(text = parent_i)))==bad_name),"] <- '",unlist(strsplit(name_check_df$partial_match_suggestions[i],split = "$",fixed = T))[length(unlist(strsplit(name_check_df$partial_match_suggestions[i],split = "$",fixed = T)))],"'",sep = "")
              eval(parse(text = exp_i))
              name_check_df$corrected_name[i]<-dd_names[which.min(adist(x = element_i,y = dd_names))]

            }
          } # end interactive corrections
          #end prompt
        }

        if(min_dist>cutoff_distance){
          name_check_df$not_matched[i]<-element_i

        }

      }#if name is NOT valid exactly
  }#i loop

  if(nrow(name_check_df)>0){

    # if(length(which(!is.na(name_check_df$exact_match)))>0 ){
    #   cat(paste("The following names appear accurate:", sep = "",collapse = "\n"))
    #   cat(paste("\n",paste(name_check_df$exact_match[which(!is.na(name_check_df$exact_match))],collapse = "\n"), sep = ""   ))
    #   cat(noquote("\n "))
    # }

    if(length(which(!is.na(name_check_df$corrected_name)))>0 ){
      cat(noquote("\n"))
      cat("The following names were corrected:\n")
      cat(paste("\n",paste(name_check_df$partial_match[which(!is.na(name_check_df$corrected_name))],collapse = "\n"), sep = ""   ))
      cat(noquote("\n "))
    }

    if(length(which(!is.na(name_check_df$partial_match) & is.na(name_check_df$corrected_name)))>0 ){
      cat("The following names are similar to suggested names, please verify:\n")
      cat(paste(paste(name_check_df$partial_match[which(!is.na(name_check_df$partial_match) & is.na(name_check_df$corrected_name) )],collapse = "\n"), sep = ""   ))
      cat(paste("\nSuggested alternatives include: \n",paste(name_check_df$partial_match_suggestions[which(!is.na(name_check_df$partial_match_suggestions) & is.na(name_check_df$corrected_name))],collapse = "\n"), sep = ""   ))
      #cat(paste(paste(name_check_df$partial_match_suggestions[which(!is.na(name_check_df$partial_match_suggestions))],collapse = "\n"), sep = ""   ))
      cat(noquote("\n "))
    }

    if(length(which(!is.na(name_check_df$not_matched)))>0 ){
      cat(paste("The following names are not similar to any suggested names, please verify that these are accurate:\n", sep = ""   ))
      cat(paste(paste(name_check_df$not_matched[which(!is.na(name_check_df$not_matched))],collapse = "\n"), sep = ""   ))
      cat(noquote("\n "))
    }

    if(returnData==T){
      return(name_check_df)
    }

    if(returnData==F){return(rmm)}

  }#overall fx

}

####################################################################
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
#'
#' @examples
#' rmm<-rmmTemplate() #First, we create an empty rmm template
#' rmm$data$environment$variableNames<- c("bio1", "bio 2", "bio3", "cromulent")
#' #We add 3 of the bioclim layers, including a spelling error (an extra space) in bio2,
#' # and a word that is clearly not a climate layer, 'cromulent'.
#' rmmCheckValue(rmm = rmm)
#' #Now, when we check the values, we see that bio1 and bio2 are reported as exact matches,
#' #while 'bio 2' is flagged as a partial match with a suggested value of 'bio2',
#' # and cromulent is flagged as not matched at all.
#' #If we'd like to return a dataframe containing this information in a perhaps more useful format:
#' rmmCheckValue_output<-rmmCheckValue(rmm = rmm,returnData = TRUE)
#'
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

rmmCheckValue <- function(rmm, cutoff_distance = 3, returnData = F ){

  dd<-utils::read.csv(system.file("extdata/dataDictionary.csv",
                                  package='rangeModelMetadata'),
                      stringsAsFactors=F)

  dd_constrained=dd[which(dd$constrainedValues!='NULL'),]

  #For all fields with either a kinda or yes in the valuesConstrained field, check values against those in datadictionary

  #Split values into a "exact_match", "partial_match", and "not partial_match" set.

  #Print each set of values along with a note.


  value_check_df<-as.data.frame((matrix(ncol=5,nrow = nrow(dd_constrained))))
  colnames(value_check_df)<-c("field","exact_match","partial_match",
                              "partial_match_suggestions","not_matched")

  for(i in 1:nrow(dd_constrained)){

    dd_i<-dd_constrained[i,][c("field1",'field2','field3','entity')]

    if(length(which(is.na(dd_i)))>0){
      dd_i<-paste(dd_i[-which(is.na(dd_i))],collapse = "$")
    }else{dd_i<-paste(dd_i,collapse = "$")}

    value_check_df[i,1]<-dd_i<-paste("rmm",dd_i,sep = "$")

    constrained_values<-dd_constrained$constrainedValues[i]
    constrained_values<-unlist(strsplit(x = constrained_values,split = "; "))
    value_i<-eval(parse(text=dd_i))

    if(!is.null(eval(parse(text=dd_i)))){

      for(j in 1:length(eval(parse(text=dd_i)))){

        element_j<-eval(parse(text=dd_i))[j]

        if(element_j %in% constrained_values){
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
      cat('\n==========================================\n')
      cat(noquote(paste("For the field ",value_check_df$field[r],collapse = "\n",sep = ""))  )
      cat(noquote("\n "))

      if(!is.na(value_check_df$exact_match[r])){
        cat(noquote(paste("The following entries appear accurate:\n"   )))
        cat(noquote(paste("\n",value_check_df$exact_match[r])))
        cat(noquote("\n "))
        }
      if(!is.na(value_check_df$partial_match[r])){
        cat(noquote(paste0("The following entries are similar to suggested values, please verify:\n" )))
        cat(noquote(paste(value_check_df$partial_match[r])))
        cat(noquote(paste0( "\n\nSuggested alternatives include: \n",value_check_df$partial_match_suggestions[r]  )))
        cat(noquote("\n "))
        }

      if(!is.na(value_check_df$not_matched[r])){
        cat(noquote(paste("The following entries are not similar to any suggested values, please verify that these are accurate:\n"   )))
        cat(noquote(paste(value_check_df$not_matched[r])))
        cat(noquote("\n "))
        }
    }

  }else{  #if there are values to check

    cat(noquote("There are no suggested fields to verify in this rmm object."))
    cat(noquote("\n "))

  } #if there are no values to check

  if(returnData==T){
    return(value_check_df)
  }


}#end of fx

####################################################################
####################################################################
####################################################################

#' @title Check for missing fields
#'
#' @description Identify obligate fields that are missing
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @param family The rmm family to check the rmm against
#'
#' @examples
#' rmm<-rmmTemplate() # Make an empty template
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

rmmCheckMissingNames<-function(rmm,family=c("base")){

  list_elements<-capture.output(rmm)
  list_elements<-list_elements[grep(pattern = "$",x = list_elements,fixed = T)] #remove elements that aren't field names

  #Now, we need to purge the non-terminal list elements.
  #Solution (probably not optimal):
  #Identify elements that are completely contained within another element (but are not identical to that element).
  #Any of these will not be terminal element names

  terminal<-lapply(X = list_elements,FUN = function(x){

    if(length(grep(pattern = x,x = unique(list_elements),fixed = T))>1){
      output<-FALSE
    }else{
      output<-TRUE
    }
    return(output)
  })
  terminal<-unlist(terminal)
  list_elements<-list_elements[terminal]


  dd=utils::read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetadata'),stringsAsFactors=F)

    keep=unique(unlist(lapply(family,function(fam){grep(fam,dd$family)})))
  dd_ob<-dd[keep,]

  obligate_names<-NULL
  for(i in 1:nrow(dd_ob)){
    val_i<-dd_ob[i,][unique(c(grep(pattern = "field",x = colnames(dd_ob)),grep(pattern = "entity",x = colnames(dd_ob)))) ]#The complicate indexing ensures that id additional fields (eg field4,field5) are added things won't break
    val_i<-val_i[which(!is.na(val_i))]
    val_i<-paste("$",paste(val_i,collapse = "$"),sep = "")
    obligate_names<-c(obligate_names,val_i)

  }

  missing_names<-obligate_names[which(!obligate_names%in%list_elements)]
  #list_elements[which(!list_elements%in%obligate_names)]

  if(length(missing_names)==0){cat("All obligate field names are present.")}

  return(missing_names)

}


####################################################################
####################################################################
####################################################################
#' @title Check an rmm object for empty fields
#'
#' @description Identify empty fields in an rmm object and classify these into obligate and optional fields.
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @param family an rmm family, "base" by default
#'
#' @examples
#' #First, make an empty rmm object:
#' rmm<-rmmTemplate()
#' #Next, we check for emtpy fields:
#' empties1<-rmmCheckEmpty(rmm = rmm)
#' #If looks like there are quite a few empty obligate fields.  Let's populate a few:
#' rmm$data$occurrence$taxon<-"Acer rubrum"
#' rmm$data$environment$variableNames<-"Bio1"
#' #Now, if we run rmmCheckEmpty again, we see there are 2 fewer empty, obligate fields
#' empties2<-rmmCheckEmpty(rmm = rmm)
#'
#'
#' @return A dataframe containing empty fields labelled as obligate, optional, or suggested.
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family check
#' @export

#CM: 7/11/18: note that I removed the optional and suggested options as we don't have those any more.
rmmCheckEmpty<-function(rmm, family=c('base')){

  list_elements<-capture.output(rmm)
  list_elements<-list_elements[grep(pattern = "$",x = list_elements,fixed = T)] #remove elements that aren't field names

  #Now, we need to purge the non-terminal list elements.
  #Solution (probably not optimal):
  #Identify elements that are completely contained within another element (but are not identical to that element).
  #Any of these will not be terminal element names

  terminal<-lapply(X = list_elements,FUN = function(x){
    if(length(grep(pattern = x,x = unique(list_elements),fixed = T))>1){
      output<-FALSE}else{output<-TRUE}
    return(output)
  })
  terminal<-unlist(terminal)
  list_elements<-list_elements[terminal]
  rm(terminal)

  dd=utils::read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetadata'),stringsAsFactors=F)

  #Identify which fields are empty
  nulls<-NULL
  for(i in 1:length(list_elements)){
    # eval(parse(text = paste("rmm",list_elements[i],sep = "",collapse = "")))
    if(is.null(eval(parse(text = paste("rmm",list_elements[i],sep = "",collapse = ""))))){output<-TRUE}else{output<-FALSE}
    nulls<-cbind(nulls,output)  }
  rm(i,output)

  empties<-list_elements[nulls]

  output_data<-as.data.frame(matrix(ncol = 4,nrow = length(empties)))
  colnames(output_data)<-c("Empty_field","Obligate","Suggested","Optional")

  output_data$Empty_field<-empties

  dd=utils::read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetadata'),stringsAsFactors=F)
  dd_names<-NULL
  for(i in 1:nrow(dd)){
    val_i<-dd[i,][unique(c(grep(pattern = "field",x = colnames(dd)),grep(pattern = "entity",x = colnames(dd)))) ]#The complicate indexing ensures that id additional fields (eg field4,field5) are added things won't break
    val_i<-val_i[which(!is.na(val_i))]
    val_i<-paste("$",paste(val_i,collapse = "$"),sep = "")
    dd_names<-c(dd_names,val_i)
  }

  rm(i,val_i)


  #CM: check the use case issue
  for(fam in family){
    output_data$Obligate[which(output_data$Empty_field %in%
                               dd_names[which(dd$family==fam)])]<-1
  }

  # output_data$Obligate[which(output_data$Empty_field %in%
  #                              dd_names[which(dd[family]==1)])]<-1
  # output_data$Optional[which(output_data$Empty_field%in%
  #                              dd_names[which(dd[family]==0)])]<-1
  #output_data$Suggested[which(output_data$Empty_field%in%dd_names[which(dd[family]==2)])]<-1 Add this once we figure out how to label suggested fields


  #If there are missing obligate values, warn the user
  if(sum(na.omit(output_data$Obligate))>0){
    cat('===================================\n')
    cat(paste("There are ",sum(na.omit(output_data$Obligate)), "empty obligate fields:\n" ))
    cat(paste(output_data$Empty_field[which(output_data$Obligate==1)],sep = ", ",collapse = "\n"))
    cat("\n")
  }
  cat('===================================\n')
  if(sum(na.omit(output_data$Suggested))>0){
    cat(paste("There are ",sum(na.omit(output_data$Suggested)), "empty suggested fields." ))
    cat("\n")
  }
  cat('===================================\n')
  if(sum(na.omit(output_data$Optional))>0){
    cat(paste("There are ",sum(na.omit(output_data$Optional)), "empty optional fields." ))
    cat("\n")
  }

  if(nrow(output_data)==0){
    cat("All fields are populated.")
    cat("\n")

  }

  # CM 7/12/18 temporarily toss suggested and optional until/if we use
  output_data=output_data[,1:2]
  return(output_data)

}

#############################################################################
#############################################################################
#############################################################################
#' @title Remove NULL entries range model metadata list
#'
#' @description Check if fields are NULL in a range model metadata list and toss
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#'
#' @examples
#' # see vignette('rmm_vignette')
#' @return printout to the console
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
#' @family check
#' @export

# would better if this didn't have the quotes in the names, but this is fine for viewing

rmmCleanNULLs=function(rmm){
  # from https://stackoverflow.com/questions/26539441/remove-null-elements-from-list-of-lists/26540063
  is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

  ## Recursively step down into list, removing all such objects
  rmNullObs <- function(x) {
    x <- Filter(Negate(is.NullOb), x)
    lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
  }
  rmNullObs(rmm)
}
##################################

#' @title Run a final check of an rmm object
#'
#' @description Check an rmm object for non-standard and missing values and fields
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @param family The rmm family to check the rmm against
#'
#' @examples
#' rmm<-rmmTemplate() # Make an empty template
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

rmmCheckFinalize<-function(rmm,family=c('base')){

  names<-rmmCheckName(rmm,returnData = TRUE)

  values<-rmmCheckValue(rmm = rmm,returnData = TRUE)

  missing_names<-rmmCheckMissingNames(rmm,family = family)

  empty_values<-rmmCheckEmpty(rmm = rmm,family = family)

  if(length(na.omit(values$partial_match))==0 & length(na.omit(values$not_matched))==0 & #All values are exactly matched
     length(missing_names)==0 & #No names are missing
     length(na.omit(names$partial_match))==0 & length(na.omit(names$not_matched))==0 & #All names are exactly matched
     sum(na.omit(empty_values$Obligate))==0
        ){
  cat("Everything looks good!")
  }
}



