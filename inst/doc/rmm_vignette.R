## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(rangeModelMetadata)
library(sp)
library(dplyr)

## ------------------------------------------------------------------------
rmm1=rmmTemplate(family=c('obligate')) 
str(rmm1)

## ------------------------------------------------------------------------
rmm2=rmmTemplate(family=NULL)
str(rmm2)

## ------------------------------------------------------------------------
rmmSuggest('dataPrep',fullFieldDepth=FALSE)
rmmSuggest('dataPrep',fullFieldDepth=TRUE) # for all fields below the specified one
rmmSuggest('dataPrep$errors$duplicateRemoval')
rmmSuggest('dataPrep$errors$duplicateRemoval$rule')

## ------------------------------------------------------------------------
rmmSuggest('model')
rmmSuggest('model$maxent$')
rmmSuggest('$model$maxent$featureSet')

## ------------------------------------------------------------------------
rmm=rmmTemplate()
rmm=rmmAutofillPackageCitation(rmm,c('raster','sp'))
# rmmAutoFillData(rmm,species=)
rasterFiles=list.files(path=paste(system.file(package='dismo'), '/ex', sep=''), pattern='grd', full.names=TRUE)
# make a stack of the rasters
env=raster::stack(rasterFiles)
rmm=rmmAutofillEnvironment(rmm,env,transfer=0) # for fitting environment
rmm=rmmAutofillEnvironment(rmm,env,transfer=1) # for transfer environment 1 (assuming different than for fitting)
rmm=rmmAutofillEnvironment(rmm,env,transfer=2) # for transfer environment 2 (assuming different than for fitting)
# rmmAutoFillMaxent(rmm,species=)
# rmmAutoFillPrediction(rmm,species=,layer=1)
rmmCheckFull(rmm) #print just the non-NULL fields

## ------------------------------------------------------------------------
# rmmFindEmpties(rmm)
# rmmCleanEmpties(rmm)

## ------------------------------------------------------------------------

rmm<-rmmTemplate() # Make an empty template
rmm$dataPrep$biological$taxonomicHarmonization$taxonomy_source<-"The Plant List" # # Add a new, non-standard field
# temporary dev error
rmm=rmmCheckName(rmm) # Checking the names should identify the new, non-standard field we've added ("taxonomy_source")


## ------------------------------------------------------------------------
rmm<-rmmTemplate() #First, we create an empty rmm template
rmm$data$environment$variableNames<- c("bio1", "bio 2", "bio3", "cromulent") #We add 3 of the bioclim layers, including a spelling error (an extra space) in bio2, and a word that is clearly not a climate layer, 'cromulent'.
# TO FIX after chaging the usecase isssue
#rmmCheckValue(rmm = rmm) #Now, when we check the values, we see that bio1 and bio2 are reported as exact matches, while 'bio 2' is flagged as a partial match with a suggested value of 'bio2', and 'cromulent' is flagged as not matched at all.

#If we'd like to return a dataframe containing this information in a perhaps more useful format:
# TO FIX after chaging the usecase isssue
#rmmCheckValue_output<-rmmCheckValue(rmm = rmm,returnData = TRUE)

## ----eval=F--------------------------------------------------------------
#  outFile='~/Desktop/demo_rmmToCSV.csv'
#  rmmObj=rmmTemplate()
#  rmmToCSV(rmmObj,filename=outFile)
#  system(paste0('open ', outFile, ' -a "Microsoft Excel"'))

## ------------------------------------------------------------------------
dd=rmmDataDictionary()
str(dd)
# rmmDataDictionary(excel=TRUE) # try this if you have excel

