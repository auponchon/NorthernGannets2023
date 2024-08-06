##Spatial fonctions for kernel UD


#reduce dataset with id, sex, datetime, long and lat
get_df_sexed<-function(data,sex,SEX){
    sexo<-enquo(sex) 
    
df<-data %>% 
    dplyr::filter(UQ(sexo)==SEX) %>% 
    dplyr::select(id,sex,datetime,long,lat) %>% 
    droplevels()
return(df)
}


#get brb raster image and convert it to a proper EstUD

get_brb_raster<-function(ltraj,Dparam,TMAX,LMIN,HMIN,GRID){

KUD.brb<-BRB(ltraj,D=Dparam,Tmax=TMAX,Lmin=LMIN,hmin=HMIN,grid=GRID,same4al=T)
combined_raster<-stack(lapply(KUD.brb, raster))
KUD.all<-mean(combined_raster)
crs(combined_raster)<-projcrs
KUD.all <- as(KUD.all,"SpatialPixelsDataFrame")
KUD.all.final<-new("estUD",KUD.all)
KUD.all.final@vol=F
KUD.all.final@h$meth="Plug-in Bandwith"
KUD.all.final@proj4string@projargs<-projcrs
KUD.vol <- getvolumeUD(KUD.all.final)

return(KUD.vol)

}

