

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
#' rmm$dataPrep$biological$taxonomicHarmonization$taxonomy_source<-"The Plant List" # Add a new, non-standard field
#' rmmNameCheck(rmm) # Checking the names should identify the new, non-standard field we've added ("taxonomy_source")
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
#' @export
rmmNameCheck=function(rmm){

  #nametree function by Vincent Zoonekynd
  nametree <- function(X, prefix = "")
    if( is.list(X) )
      for( i in seq_along(X) ) {
        cat( prefix, names(X)[i], "\n", sep="" )
        nametree(X[[i]], paste0(prefix, ""))
      }

  list_names<-utils::capture.output(nametree(rmm))

  dd=utils::read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetadata'),stringsAsFactors=F)

  accepted_names<-unique(c(as.character(dd$field1), as.character(dd$field2), as.character(dd$field3), as.character(dd$entity)))

  questionable_names <- list_names[which(!list_names%in%accepted_names)]

  return(questionable_names)

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
#' rmm$data$environment$variableNames<- c("bio1", "bio 2", "bio3", "cromulent") #We add 3 of the bioclim layers, including a spelling error (an extra space) in bio2, and a word that is clearly not a climate layer, 'cromulent'.
#' rmmValueCheck(rmm = rmm) #Now, when we check the values, we see that bio1 and bio2 are reported as exact matches, while 'bio 2' is flagged as a partial match with a suggested value of 'bio2', and cromulent is flagged as not matched at all.
#' #If we'd like to return a dataframe containing this information in a perhaps more useful format:
#' rmmValueCheck_output<-rmmValueCheck(rmm = rmm,returnData = T)
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
rmmValueCheck <- function( rmm, cutoff_distance = 3, returnData = F ){
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
      print(paste("For the field ",value_check_df$field[r],":")  )

      if(!is.na(value_check_df$exact_match[r])){
        print(paste("   The entries",value_check_df$exact_match[r], " appear accurate."   ))}

      if(!is.na(value_check_df$partial_match[r])){
        print(paste("   The entries",value_check_df$partial_match[r], " are similar to suggested values, please verify.  Suggested alternatives include: ", value_check_df$partial_match_suggestions[r]   ))}

      if(!is.na(value_check_df$not_matched[r])){
        print(paste("   The entries",value_check_df$not_matched[r], " are not similar to any suggested values, please verify that these are accurate."   ))}
    }

  }else{  #if there are values to check

    print(paste("The are no suggested fields to verify in this rmm object"))

  } #if there are no values to check

  if(returnData==T){
    return(value_check_df)
  }


}#end of fx

