## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache = FALSE)
knitr::opts_chunk$set(eval = T)

## ----results='hide',message=FALSE---------------------------------------------
library(rangeModelMetadata)
library(rgbif)
library(raster)
library(dismo)
library(ENMeval)
library(dplyr)
library(sp)
library(spThin)
library(jsonlite)

## -----------------------------------------------------------------------------
# search GBIF for occurrence data
bv <- rgbif::occ_search(scientificName ='Bradypus variegatus')
# restrict table to coordinates
occs <- dplyr::select(bv$data, decimalLongitude, decimalLatitude)
# remove duplicate values
occs <- occs[!duplicated(occs),]
# remove NAs
occs <- occs[complete.cases(occs),]
# make a stack of Worldclim v1.4 bioclim rasters 
# Note 1: we are using the Worldclim version that comes with the current
# dismo distribution, which is the older v1.4 -- if you want to use v2.0 or
# a different dataset entirely, download it and place the file path here, 
# instead of list.files(...)
# Note 2: as categorical variable "biome" has cells with NA where other bioclim 
# variables have values, you may see some warnings when the predict functions 
# are run
files <- list.files(path=paste(system.file(package='dismo'), '/ex',
                               sep=''), pattern='grd', full.names=TRUE)
envs <- raster::stack(files)

## -----------------------------------------------------------------------------
# thin points to remove spatial autocorrelation
occs.thinned <- spThin::thin.algorithm(data.frame(occs), thin.par=20, reps=1)
# make a spatial points object for the occurrences
occs.sp <- sp::SpatialPoints(occs.thinned[[1]])
# get the bounding box
bb <- sp::bbox(occs.sp)
# make extent object and buffer by 5 degrees
bb.buf <- raster::extent(bb[1]-5, bb[3]+5, bb[2]-5, bb[4]+5)
# crop environmental rasters by the extent to make the background extent
bg.ext <- raster::crop(envs, bb.buf)
# generate up to 10,000 random points within the background extent (we'll sample from the "biome" raster as it has more holes)
bg <- dismo::randomPoints(bg.ext[[9]], n=10000) 

## -----------------------------------------------------------------------------
# run ENMeval
e <- ENMeval::ENMevaluate(occs, bg.ext, bg, method='block', RMvalues=1:3, fc=c('L','LQ'), algorithm='maxnet')

## -----------------------------------------------------------------------------
# find the index number of the model with the highest mean test AUC: this will be the optimal model
i <- which(e@results$avg.test.AUC  == max(e@results$avg.test.AUC ))
# extract the optimal model object
m <- e@models[[i]]

## -----------------------------------------------------------------------------
e@results[i,]

## -----------------------------------------------------------------------------
# the prediction rasters produced by ENMeval are "raw" values,
# which correspond to the relative occurrence rate (ROR) per cell --
# see Merow et al. 2013 (Ecography)
p <- e@predictions[[i]]
plot(p)

## -----------------------------------------------------------------------------
# specify extent further south for model transfer (this region
# should have different climatic conditions than the region
# used to train the model). This could be useful if no observations 
# are available in this region, but you're interested in determining 
# if there are any potentially suitable areas there.
pt.bb <- raster::extent(-80,-40,-60,-30)
# crop environmental predictors by transfer extent
pt.ext <- raster::crop(envs, pt.bb)
pt <- ENMeval::maxnet.predictRaster(m, pt.ext, type = "exponential", clamp = TRUE)
plot(pt)

## -----------------------------------------------------------------------------
# generate an empty range model metadata template
rmm <- rmmTemplate(family=c('base','dataPrep','maxent','prediction',
                         'transferEnv1','binaryClassification'))

## -----------------------------------------------------------------------------
str(rmm, 1)
str(rmm, 2)

## -----------------------------------------------------------------------------
  # in the form Author_Year_Taxa_Model_fw (where the last 2 characters are random)
rmm$authorship$rmmName <- 'MerowMaitnerOwensKassEnquistJetzGuralnick_2018_BradypusVariegatus_Maxent_b3'
  # names are distinct from citations (below) in case a citation does not exist 
rmm$authorship$names <- 'Merow, Cory and Maitner, Brian and Owens, Hannah and Kass, Jamie and Enquist, Brian and Jetz, Walter and Guralnick, Rob'
rmm$authorship$contact <- 'cory.merow@gmail.com'
rmm$authorship$relatedReferences <- '@article{, 
  title={RMMS: Species’ Range Model Metadata Standards },
  author={Merow, Cory and Maitner, Brian S. and Owens, Hannah L. and Kass, 
          Jamie M. and Enquist, Brian and Jetz, Walter and Guralnick, Robert},
  journal={Global Ecology and Biogeography},
  year={2019}}'
rmm$authorship$license <- 'CC'
rmm$authorship$miscNotes <- 'Funding from NSF grant DBI-1661510 and DBI-1913673.'

## -----------------------------------------------------------------------------
rmm$studyObjective$purpose='transfer'
rmm$studyObjective$rangeType='potential'
rmm$studyObjective$transfer='detect unoccupied suitable habitat'

## -----------------------------------------------------------------------------
# occurrence data
rmm <- rmmAutofillspocc(rmm, bv$gbif) 
# autofill info for environmental rasters used for model training and transfer
rmm <- rmmAutofillEnvironment(rmm, bg.ext, transfer=0) 
rmm <- rmmAutofillEnvironment(rmm, pt.ext, transfer=1)
# autofill for ENMeval
rmm <- rmmAutofillENMeval(rmm, e, 
                          selectionCriteria="highest mean test AUC",
                          optimalModelIndex=i) 
# R packages used
rmm <- rmmAutofillPackageCitation(rmm=rmm,
                                  packages= 
                                    c('rgbif','sp','raster','dismo','ENMeval'))

## -----------------------------------------------------------------------------
rmm$data$occurrence$sources <- lapply(rgbif::gbif_citation(bv),function(x) x$citation$citation)
rmm$data$occurrence$yearMin <- 1970
rmm$data$occurrence$yearMax <- 2000

## -----------------------------------------------------------------------------
rmm$data$environment$yearMin <- 1960
rmm$data$environment$yearMin <- 1990
rmm$data$environment$sources <- '@article{hijmans2005,
  title={Very high resolution interpolated climate surfaces for global land areas},
  author={Hijmans, Robert J and Cameron, Susan E and Parra, Juan L and Jones, Peter G and Jarvis, Andy},
  journal={International Journal of Climatology: A Journal of the Royal Meteorological Society},
  volume={25},
  number={15},
  pages={1965--1978},
  year={2005},
  publisher={Wiley Online Library}
}'
rmm$data$dataNotes <- 'WorldClim v1.4 data accessed through dismo v1.1-4'

## -----------------------------------------------------------------------------
mm <- data.frame(rbind(apply(values(envs),2,min,na.rm=T),
                    apply(values(envs),2,max,na.rm=T)))
rmm$data$environment$minVal <- toJSON(mm[1,])
(rmm$data$environment$maxVal <- toJSON(mm[2,])) # printed to show format

## -----------------------------------------------------------------------------
(ex <- extent(envs))
# make an extent object a data.frame, because that's easy to use with toJSON
tmp <- as.data.frame(lapply(slotNames(ex), function(i) slot(ex, i) )) 
names(tmp) <- slotNames(ex)
rmm$data$environment$extentSet <- toJSON(tmp)

## -----------------------------------------------------------------------------
rmmSuggest('$data') # note the use of quotes!

## -----------------------------------------------------------------------------
rmm$data$transfer$environment1$yearMin <- 1970
rmm$data$transfer$environment1$yearMin <- 2000
rmm$data$transfer$environment1$resolution=paste0(res(pt.ext)[1],' degrees')
rmm$data$transfer$environment1$sources <- '@article{hijmans2005,
  title={Very high resolution interpolated climate surfaces for global land areas},
  author={Hijmans, Robert J and Cameron, Susan E and Parra, Juan L and Jones, Peter G and Jarvis, Andy},
  journal={International Journal of Climatology: A Journal of the Royal Meteorological Society},
  volume={25},
  number={15},
  pages={1965--1978},
  year={2005},
  publisher={Wiley Online Library}
}'

mm.pt <- data.frame(rbind(apply(values(pt.ext),2,min,na.rm=T),
                    apply(values(pt.ext),2,max,na.rm=T)))
rmm$data$transfer$environment1$minVal <- toJSON(mm.pt[1,])
(rmm$data$transfer$environment1$maxVal <- toJSON(mm.pt[2,])) # printed to show format
(ex <- extent(pt.ext))
# make an extent object a data.frame, because that's easy to use with toJSON
tmp <- as.data.frame(lapply(slotNames(ex), function(i) slot(ex, i) )) 
names(tmp) <- slotNames(ex)
rmm$data$transfer$environment1$extentSet <- toJSON(tmp)

## -----------------------------------------------------------------------------
# duplicated observations removed within the grid cells defined by the environmental layers
rmm$dataPrep$biological$duplicateRemoval$rule <- 'one observation per cell' 
rmm$dataPrep$geographic$spatialThin$rule <- "20 km used as minimum distance between points"

## -----------------------------------------------------------------------------
rmm$modelFit$maxent$notes <- 'ENMeval was used to compare models with L and LQ features, each using regularization multipliers of 1,2,3. The best model was selected based on test AUC evaluated with spatial cross validation.'

## -----------------------------------------------------------------------------
# fill in remaining fields for model prediction in background extent
rmm$prediction$continuous$units <- "relative occurrence rate"
rmm$prediction$continuous$minVal <- cellStats(p, min)
rmm$prediction$continuous$maxVal <- cellStats(p, max)
# we chose to clamp our model predictions
rmm$prediction$extrapolation <- "clamping"

## -----------------------------------------------------------------------------
rmm$prediction$transfer$environment1$units <- "relative occurrence rate"
rmm$prediction$transfer$environment1$minVal <- cellStats(pt, min)
rmm$prediction$transfer$environment1$maxVal <- cellStats(pt, max)
rmm$prediction$extrapolation <- "clamping"

## -----------------------------------------------------------------------------
rmm$code$software <- '@Manual{
  title = {R: A Language and Environment for Statistical Computing},
  author = {{R Core Team}},
  organization = {R Foundation for Statistical Computing},
  address = {Vienna, Austria},
  year = {2017},
  url = {https://www.R-project.org/},}'
rmm$code$vignetteCodeLink <- 'https://github.com/cmerow/rangeModelMetadata/blob/master/vignettes/rmm_workflowWithExampleRangeModel.Rmd'

## -----------------------------------------------------------------------------
#print just the non-NULL fields
rmmCleanNULLs(rmm)

## -----------------------------------------------------------------------------
#print just the non-NULL fields
rmmCleanNULLs(rmm)
# you can also use this function to omit the NULLs at the end of your workflow using, so if you're happy with the above...
rmm <- rmmCleanNULLs(rmm)

## -----------------------------------------------------------------------------
rmmCheckFinalize(rmm)

