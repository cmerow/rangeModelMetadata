
#' @title Check field names of a range model metadata list against conventions
#'
#' @description Identify nonstandard fields
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @export
#'
# @examples
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
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
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

  accepted_names<-c(as.character(dd$Field1), as.character(dd$Field2), as.character(dd$Field3), as.character(dd$Field4))

  questionable_names <- list_names[which(!list_names%in%accepted_names)]

  return(questionable_names)

}

##############################################################

##############################################################


#' @title Check values of a range model metadata list against commonly used values
#'
#' @description Identify nonstandard values
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @param cutoff_distance The maximum allowable similarity (Levenshtein (edit) distance) for use in fuzzy matching.
#' @param returnData Should a dataframe containing information on matched and unmatched values be returned?  Default is FALSE
#' @export
#'
# @examples
#'
#'
#' @return Text describing identical, similar and non-matched values for rmm entities with suggested values.
#' @author Cory Merow <cory.merow@@gmail.com>, Brian Maitner <bmaitner@@gmail.com>,
#' @note Names returned by this check may be either incorrectly named or correctly named but missing from the data dictionary.
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
rmmValueCheck=function(rmm,cutoff_distance=3, returnData=F){

  dd=utils::read.csv(system.file("extdata/dataDictionary.csv",package='rangeModelMetadata'),stringsAsFactors=F)

  dd_constrained<-dd[which(dd$valuesConstrained!="no"),]

  #For all fields with either a kinda or yes in the valuesConstrained field, check values against those in datadictionary

  #Split values into a "good", "close", and "not close" set.

  #Print each set of values along with a note.


  value_check_df<-as.data.frame((matrix(ncol=5,nrow = nrow(dd_constrained))))
  colnames(value_check_df)<-c("field","good","close","close_options","bad")

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
          value_check_df$good[i]<-paste(na.omit(c(value_check_df$good[i],element_j)),sep = "; ",collapse = "; ")}else{

            #get the distance between the value and the potential values.

            min_dist<-min(adist(x = element_j,y = constrained_values))

            if(min_dist<=cutoff_distance){
              constrained_values[which.min(adist(x = element_j,y = constrained_values))]

              value_check_df$close[i]<-paste(na.omit(c(value_check_df$close[i],element_j)),sep = "; ",collapse = "; ")
              value_check_df$close_options[i]<-paste(na.omit(c(value_check_df$close_options[i],constrained_values[which.min(adist(x = element_j,y = constrained_values))])),sep = "; ",collapse = "; ")
            }

            if(min_dist>cutoff_distance){
              value_check_df$bad[i]<-paste(na.omit(c(value_check_df$bad[i],element_j)),sep = "; ",collapse = "; ")

            }else{

              value_check_df$close[i]<-paste(na.omit(c(value_check_df$bad[i],element_j)),sep = "; ",collapse = "; ")

            }
            #take the closest value,
            #or if distance > something, label it as "bad"

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

      if(!is.na(value_check_df$good[r])){
        print(paste("   The entries",value_check_df$good[r], " appear accurate."   ))}

      if(!is.na(value_check_df$close[r])){
        print(paste("   The entries",value_check_df$close[r], " are similar to suggested values, please verify.  Suggested alternatives include: ", value_check_df$close_options[r]   ))}

      if(!is.na(value_check_df$bad[r])){
        print(paste("   The entries",value_check_df$bad[r], " are not similar to any suggested values, please verify that these are accurate."   ))}
    }

  }else{  #if there are values to check

    print(paste("The are no suggested fields to verify in this rmm object"))

  } #if there are no values to check

  if(returnData==T){
    return(value_check_df)
  }


}#end of fx

