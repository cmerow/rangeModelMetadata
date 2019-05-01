## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache = FALSE)
knitr::opts_chunk$set(eval = FALSE)

## ----results='hide',message=FALSE----------------------------------------
#  library(rangeModelMetadata)
#  library(rgbif)
#  library(raster)
#  library(dismo)
#  library(ENMeval)
#  library(dplyr)
#  library(sp)
#  library(spThin)
#  library(jsonlite)

## ------------------------------------------------------------------------
#  # search GBIF for occurrence data
#  bv <- rgbif::occ_search(scientificName ='Bradypus variegatus')
#  # restrict table to coordinates
#  occs <- dplyr::select(bv$data, decimalLongitude, decimalLatitude)
#  # remove duplicate values
#  occs <- occs[!duplicated(occs),]
#  # get environmental rasters from dismo package folder
#  files <- list.files(path=paste(system.file(package='dismo'), '/ex',
#                                 sep=''), pattern='grd', full.names=TRUE)
#  # make a stack of the rasters
#  envs <- raster::stack(files)

## ------------------------------------------------------------------------
#  # thin points to remove spatial autocorrelation
#  occs.thinned <- spThin::thin.algorithm(data.frame(occs), thin.par=20, reps=1)
#  # make a spatial points object for the occurrences
#  occs.sp <- sp::SpatialPoints(occs.thinned[[1]])
#  # get the bounding box
#  bb <- sp::bbox(occs.sp)
#  # make extent object and buffer by 5 degrees
#  bb.buf <- raster::extent(bb[1]-5, bb[3]+5, bb[2]-5, bb[4]+5)
#  # crop environmental rasters by the extent to make the background extent
#  bg.ext <- raster::crop(envs, bb.buf)
#  # generate up to 10,000 random points within the background extent (we'll sample from the "biome" raster as it has more holes)
#  bg <- dismo::randomPoints(bg.ext[[9]], n=10000)

## ------------------------------------------------------------------------
#  # run ENMeval
#  e <- ENMeval::ENMevaluate(occs, bg.ext, bg, method='block', RMvalues=1:3, fc=c('L','LQ'), algorithm='maxent.jar')

## ------------------------------------------------------------------------
#  # find the index number of the model with the highest mean test AUC: this will be the optimal model
#  i <- which(e@results$avg.test.AUC  == max(e@results$avg.test.AUC ))
#  # extract the optimal model object
#  m <- e@models[[i]]

## ------------------------------------------------------------------------
#  kable(e@results[i,])

## ------------------------------------------------------------------------
#  # the prediction rasters produced by ENMeval are "raw" values,
#  # which correspond to the relative occurrence rate (ROR) per cell --
#  # see Merow et al. 2013 (Ecography)
#  p <- e@predictions[[i]]
#  plot(p)

## ------------------------------------------------------------------------
#  # specify extent further south for model transfer (this region
#  # should have different climatic conditions than the region
#  # used to train the model). This could be useful if no observations
#  # are available in this region, but you're interested in determining
#  # if there is any potentially suitable habitat there.
#  pt.bb <- raster::extent(-80,-40,-60,-30)
#  # crop environmental predictors by transfer extent
#  pt.ext <- raster::crop(envs, pt.bb)
#  pt <- predict(m, pt.ext, args=c("outputformat=raw"))

## ------------------------------------------------------------------------
#  # generate an empty range model metadata template
#  rmm=rmmTemplate(family=c('base','dataPrep','maxent','prediction',
#                           'transferEnv1','binaryClassification'))

## ------------------------------------------------------------------------
#  str(rmm,1)

## ------------------------------------------------------------------------
#  str(rmm,2)

## ------------------------------------------------------------------------
#    # in the form Author_Year_Taxa_Model_fw (where the last 2 characters are random)
#  rmm$authorship$rmmName='MerowMaitnerOwensKassEnquistGuralnick_2018_BradypusVariegatus_Maxent_b3'
#    # names are distinct from citations (below) in case a citation does not exist
#  rmm$authorship$names='Merow, Cory and Maitner, Brian and Owens, Hannah and Kass, Jamie and Enquist, Brian and Guralnick, Rob'
#  rmm$authorship$contact='cory.merow@gmail.com'
#  rmm$authorship$relatedReferences='@article{,
#    title={RMMS: Species’ Range Model Metadata Standards },
#    author={Merow, Cory and Maitner, Brian S. and Owens, Hannah L. and Kass,
#            Jamie M. and Enquist, Brian and Guralnick, Robert},
#    journal={Global Ecology and Biogeography},
#    year={2018}}'
#  rmm$authorship$license='CC'
#  rmm$authorship$miscNotes='Funding from NSF grant DBI-1661510 and DBI-1913673.'

## ------------------------------------------------------------------------
#  rmm$studyObjective$purpose='transfer'
#  rmm$studyObjective$rangeType='potential'
#  rmm$studyObjective$transfer='detect unoccupied suitable habitat'

## ------------------------------------------------------------------------
#  # occurrence data
#  rmm <- rmmAutofillspocc(rmm, bv$gbif)
#  # autofill info for environmental rasters used for model training and transfer
#  rmm <- rmmAutofillEnvironment(rmm, bg.ext, transfer=0)
#  rmm <- rmmAutofillEnvironment(rmm, pt.ext, transfer=1)
#  # autofill for ENMeval
#  rmm <- rmmAutofillENMeval(rmm, e,
#                            selectionCriteria="highest mean test AUC",
#                            optimalModelIndex=i)
#  # R packages used
#  rmm <- rmmAutofillPackageCitation(rmm=rmm,
#                                    packages=
#                                      c('rgbif','sp','raster','dismo','ENMeval'))

## ------------------------------------------------------------------------
#  rmm$data$occurrence$sources=lapply(rgbif::gbif_citation(bv),function(x) x$citation$citation)
#  rmm$data$occurrence$presenceSampleSize=length(occs.sp)
#  rmm$data$occurrence$backgroundSampleSize=nrow(bg)
#  rmm$data$occurrence$yearMin=1970
#  rmm$data$occurrence$yearMax=2000

## ------------------------------------------------------------------------
#  rmm$data$environment$yearMin=1970
#  rmm$data$environment$yearMin=2000
#  rmm$data$environment$sources='@ARTICLE{Fick2017-qs,
#    title     = "{WorldClim} 2: new 1‐km spatial resolution climate surfaces for
#                 global land areas",
#    author    = "Fick, S E and Hijmans, R J",
#    journal   = "Int. J. Climatol.",
#    publisher = "Wiley Online Library",
#    year      =  2017}'
#  rmm$data$dataNotes='WorldClim data accessed through dismo v1.1-4'

## ------------------------------------------------------------------------
#  mm=data.frame(rbind(apply(values(envs),2,min,na.rm=T),
#                      apply(values(envs),2,max,na.rm=T)))
#  rmm$data$environment$minVal=toJSON(mm[1,])
#  (rmm$data$environment$maxVal=toJSON(mm[2,])) # printed to show format

## ------------------------------------------------------------------------
#  (ex=extent(envs))
#  # make an extent object a data.frame, because that's easy to use with toJSON
#  tmp=as.data.frame(lapply(slotNames(ex), function(i) slot(ex, i) ))
#  names(tmp)=slotNames(ex)
#  rmm$data$environment$extentSet=toJSON(tmp)

## ------------------------------------------------------------------------
#  rmmSuggest('$data') # note the use of quotes!

## ------------------------------------------------------------------------
#  rmm$data$transfer$environment1$yearMin=1970
#  rmm$data$transfer$environment1$yearMin=2000
#  rmm$data$transfer$environment1$resolution=paste0(res(pt.ext)[1],' degrees')
#  rmm$data$transfer$environment1$sources='@ARTICLE{Fick2017-qs,
#    title     = "{WorldClim} 2: new 1‐km spatial resolution climate surfaces for
#                 global land areas",
#    author    = "Fick, S E and Hijmans, R J",
#    journal   = "Int. J. Climatol.",
#    publisher = "Wiley Online Library",
#    year      =  2017}'
#  
#  mm=data.frame(rbind(apply(values(pt.ext),2,min,na.rm=T),
#                      apply(values(pt.ext),2,max,na.rm=T)))
#  rmm$data$transfer$environment1$minVal=toJSON(mm[1,])
#  (rmm$data$transfer$environment1$maxVal=toJSON(mm[2,])) # printed to show format
#  
#  (ex=extent(pt.ext))
#  # make an extent object a data.frame, because that's easy to use with toJSON
#  tmp=as.data.frame(lapply(slotNames(ex), function(i) slot(ex, i) ))
#  names(tmp)=slotNames(ex)
#  rmm$data$transfer$environment1$extentSet=toJSON(tmp)

## ------------------------------------------------------------------------
#  # duplicated observations removed within the grid cells defined by the environmental layers
#  rmm$dataPrep$biological$duplicateRemoval$rule='one observation per cell'
#  rmm$dataPrep$geographic$spatialThin$rule= "20km used as minimum distance between points"

## ------------------------------------------------------------------------
#  rmm$modelFit$partition$partitionRule='block cross validation: partitions occurrence localities by finding the latitude and longitude that divide the occurrence localities into four groups of (insofar as possible) equal numbers'
#  rmm$modelFit$maxent$featureSet='LQ'
#  rmm$modelFit$maxent$regularizationMultiplierSet=1
#  rmm$modelFit$maxent$samplingBiasRule='ignored'
#  rmm$modelFit$maxent$notes='ENMeval was used to compare models with L and LQ features, each using regularization multipliers of 1,2,3. The best model was selected based on AUC evaluated under cross validation.'
#  rmm$modelFit$maxent$numberParameters=e@results[i,]$parameters

## ------------------------------------------------------------------------
#  # fill in remaining fields for model prediction in background extent
#  rmm$prediction$continuous$units <- "relative occurrence rate"
#  rmm$prediction$continuous$minVal <- cellStats(p, min)
#  rmm$prediction$continuous$maxVal <- cellStats(p, max)
#  # clamping is the default for predict()
#  rmm$prediction$extrapolation <- "clamping"

## ------------------------------------------------------------------------
#  rmm$prediction$transfer$environment1$units <- "relative occurrence rate"
#  rmm$prediction$transfer$environment1$minVal <- cellStats(pt, min)
#  rmm$prediction$transfer$environment1$maxVal <- cellStats(pt, max)
#  # clamping is the default for predict()
#  rmm$prediction$extrapolation <- "clamping"

## ------------------------------------------------------------------------
#  rmm$evaluation$trainingDataStats$AUC=e@results[i,]$trainAUC
#  rmm$evaluation$testingDataStats$AUC=e@results[i,]$avg.test.AUC

## ------------------------------------------------------------------------
#  rmm$code$software='@Manual{
#    title = {R: A Language and Environment for Statistical Computing},
#    author = {{R Core Team}},
#    organization = {R Foundation for Statistical Computing},
#    address = {Vienna, Austria},
#    year = {2017},
#    url = {https://www.R-project.org/},}'
#  rmm$code$vignetteCodeLink='https://github.com/cmerow/rangeModelMetadata/blob/master/vignettes/rmm_workflowWithExampleRangeModel.Rmd'

## ------------------------------------------------------------------------
#  #print just the non-NULL fields
#  rmmCleanNULLs(rmm)

## ------------------------------------------------------------------------
#  #print just the non-NULL fields
#  rmmCleanNULLs(rmm)
#  # you can also use this function to omit the NULLs at the end of your workflow using, so if you're happy with the above...
#  rmm <- rmmCleanNULLs(rmm)

## ------------------------------------------------------------------------
#  rmmCheckFinalize(rmm)

