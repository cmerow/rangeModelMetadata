library(googlesheets)

d_title=gs_title('Model Metadata Dictionary')
d=as.data.frame(gs_read(d_title),stringsAsFactors=F)


#== Write out the dictionary to the package for use with building the metadata template
if(Sys.info()['user']=='ctg') userPath='/Users/ctg/Dropbox/Projects/Range_Metadata'
if(Sys.info()['user']=='Brian') userPath='C:/Users/Brian/Desktop/current_projects'
if(Sys.info()['user']=='hannah') userPath='/Users/ctg/Dropbox/Projects/Range_Metadata'
if(Sys.info()['user']=='musasabi') userPath='/Users/musasabi/Documents/github'

write.csv(d,paste0(userPath,'/rangeModelMetaData/inst/extdata/dataDictionary.csv'),row.names = F)

