
# generate a set of constrained characters for common worldclim variable names
# values like this are the directly downloaded names: "wc2.0_bio_10m_01"

bioclims=paste0('wc2.0_bio_',apply(expand.grid(c('10m_','5m_','1m_'),c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19')),1,paste0,collapse=''))
paste0(bioclims,collapse=';')
