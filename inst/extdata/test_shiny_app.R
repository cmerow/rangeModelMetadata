
# write test files to the inst dir
if(Sys.info()['user']=='ctg') myInstDir='/Users/ctg/Dropbox/Projects/Range_Metadata/rangeModelMetadata/inst/extdata'
if(Sys.info()['user']=='Brian') myInstDir=NULL
if(Sys.info()['user']=='hannah') myInstDir=NULL
if(Sys.info()['user']=='musasabi')  myInstDir=NULL

# rmm1 object 1
rmm1=rangeModelMetadataTemplate(useCase='apAll')
rmm1=rmm1AutofillPackageCitation(rmm1,c('raster','sp'))
# rmm1AutoFillData(rmm1,species=)
raster.files=list.files(system.file("extdata/Env_Demo",package='rangeModelMetadata'),full.names = T)
env=raster::stack(raster.files)
rmm1=rmmAutofillEnvironment(rmm1,env,transfer=0) # for fitting environment
rmm1=rmmAutofillEnvironment(rmm1,env,transfer=1) # for transfer environment 1 (assuming different than for fitting)
rmm1=rmmAutofillEnvironment(rmm1,env,transfer=2) # for transfer environment 2 (assuming different than for fitting)
# save as rdata
# save(rmm1,file=paste0(myInstDir,'/shiny_test_rmm1.rds'))
# save as csv


# rmm1 object 2
rmm2=rmm1
# save as rdata
# save(rmm2,file=paste0(myInstDir,'/shiny_test_rmm2.rds'))
# save as csv

rmms=list(rmm1,rmm2)
save(rmms,file=paste0(myInstDir,'/shiny_test_rmms.rds'))
# rmmCheckShiny()
