## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache= FALSE)


## ----results='hide'------------------------------------------------------
library(rangeModelMetadata)
library(spocc)
library(sp)
library(raster)
library(dismo)
library(ENMeval)
library(dplyr)

## ------------------------------------------------------------------------
# search GBIF for occurrence data
bv <- occ('Bradypus variegatus', 'gbif', limit=300, has_coords=TRUE)
# restrict table to coordinates
#occs <- bv$gbif$data$Bradypus_variegatus %>% 
occs=dplyr::select(bv$gbif$data$Bradypus_variegatus, longitude, latitude)
# remove duplicate values
occs <- occs[!duplicated(occs),]
# get environmental rasters from dismo package folder
files <- list.files(path=paste(system.file(package='dismo'), '/ex', sep=''), pattern='grd', full.names=TRUE)
# make a stack of the rasters
envs <- stack(files)

## ------------------------------------------------------------------------
# make a spatial points object for the occurrences
occs.sp <- SpatialPoints(occs)
# get the bounding box
bb <- bbox(occs.sp)
# make extent object and buffer by 5 degrees
bb.buf <- extent(bb[1]-5, bb[3]+5, bb[2]-5, bb[4]+5)
# crop environmental rasters by the extent to make the background extent
bg.ext <- crop(envs, bb.buf)
# generate up to 10,000 random points within the background extent (we'll sample from the "biome" raster as it has more holes)
bg <- randomPoints(bg.ext[[9]], n=10000)

## ------------------------------------------------------------------------
# run ENMeval
e <- ENMeval::ENMevaluate(occs, bg.ext, bg, method='block', RMvalues=1:3, fc=c('L','LQ'), algorithm='maxnet')

## ------------------------------------------------------------------------
# find the index number of the model with the highest mean test AUC: this will be the optimal model
i <- which(e@results$Mean.AUC == max(e@results$Mean.AUC))
# extract the optimal model object
m <- e@models[[i]]

## ------------------------------------------------------------------------
# create a cloglog prediction and plot
#p <- predict(m, bg.ext, args=c("outputformat=cloglog"))
notNA=complete.cases(values(bg.ext))
tmp<- predict(m, values(bg.ext)[notNA,], type='link')
p=bg.ext[[1]]; values(p)=NA; values(p)[notNA]=tmp
plot(p)
# further south extent for model transfer
t.bb <- extent(-80,-40,-60,-30)
# crop environmental predictors by transfer extent
t.ext <- crop(envs, t.bb)

## ------------------------------------------------------------------------
# transfer the model to this new area in cloglog form and plot
notNA.t=complete.cases(values(t.ext))
#p.t <- predict(m, t.ext, args=c("outputformat=cloglog"))
tmp<- predict(m, values(t.ext)[notNA.t,], type='link')
p.t=t.ext[[1]]; values(p.t)=NA; values(p.t)[notNA.t]=tmp
plot(p.t)

## ------------------------------------------------------------------------
# generate an empty range model metadata template
rmm=rmmTemplate() 
# R packages used
rmm=rmmAutofillPackageCitation(rmm,c('spocc','sp','raster','dismo','ENMeval'))
# occurrence data
rmm=rmmAutofillspocc(rmm,bv$gbif) 
# autofill info for environmental rasters used for model training and transfer
rmm=rmmAutofillEnvironment(rmm,bg.ext, transfer=0) 
rmm=rmmAutofillEnvironment(rmm,t.ext, transfer=1)
# autofill for ENMeval
rmm=rmmAutofillENMeval(rmm,e, selectionCriteria="highest mean test AUC", optimalModelIndex=i) 

# print just the non-NULL fields  
#rmmCheckFull(rmm)

## ------------------------------------------------------------------------
# fill in remaining fields for model prediction in background extent
rmm$output$prediction$units <- "relative occurrence rate"
rmm$output$prediction$minVal <- cellStats(p, min)
rmm$output$prediction$maxVal <- cellStats(p, max)
# clamping is the default for predict()
rmm$output$prediction$extrapolation <- "clamping"

