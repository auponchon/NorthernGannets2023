library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)
library(terra)
library(ggspatial)
library(RColorBrewer)
library(gridExtra)
library(adehabitatHR)

source(here::here("R","trip_functions.R"))

load(here("data","NewlyCreatedData","rawLocRouzic2023.Rdata"))

bathym<-terra::rast(here::here("data","RawData","shapefiles","Bathym_FR_UK.tif")) 
bathym[bathym>0]<-0

bathymjuv<-terra::rast(here::here("data","RawData","shapefiles","ETOPO_world.tif")) 
bathymjuv[bathymjuv>0]<-0

ocean.pal <- rev(colorRampPalette(brewer.pal(9,"Blues"))(9))
zbreaks <- c(-4000, -2500, -2000, -1500,-1000,-500,-200,-150,-100,-50,-25,0)


target<-c("France","United Kingdom","Spain","Morocco","Belgium",
          "Netherlands", "Germany","Ireland","Luxembourg","Isle of Man",
          "Jersey" ,"Guernsey"  ,"Åland" ,"Algeria", "Portugal",
          "Mauritania","Mali","W. Sahara")

last.incomplete<-c("201284.24","233660.63",
                   "233664.33","233672.49","235195.30",
                   "235199.42","235202.26")


#contours of countries
shp<-ne_countries(scale = 10, returnclass = 'sf') %>%
    dplyr::filter(name %in% target) 

#coastlines
coast<-ne_coastline(scale = 10, returnclass = "sf") 

coord_rouzic<-colo_coord_rouzic %>% 
    sf::st_as_sf(., coords = c("long","lat")) %>% 
    sf::st_set_crs(4326) %>% 
    write_sf(.,
             dsn=here("data","NewlyCreatedData","shapefiles"),
          layer="rouzic_coord",
          driver="ESRI Shapefile",
          delete_layer =T,
           delete_dsn = T)

extjuv<-c(-18,20,-7,57)

bathymjuv<-bathymjuv %>% 
    terra::crop(., extjuv)

########################################################################################
## adults breeding
########################################################################################
indisf<-raw_rouzic_2023 %>% 
    dplyr::filter(stage=="adult" & lat > 47 & travelNb > 0) %>% 
    dplyr::mutate(Date=as.Date(datetime, format="%F")) %>% 
    st_as_sf(.,coords=c("long","lat")) %>% 
     st_set_crs(st_crs(4326)) #%>% 
    # write_sf(.,
    #          dsn=here("data","NewlyCreatedData","shapefiles"),
    #       layer="rouzic_2023_adults_",
    #       driver="ESRI Shapefile",
    #       delete_layer =T,
    #        delete_dsn = T)

map<-ggplot()+
    tidyterra::geom_spatraster(data = bathym, aes(fill = Bathym_FR_UK))+
    geom_sf(data=shp,fill="grey90") +
    geom_sf(data=coast) +
    geom_sf(data=indisf, aes(color=Date),size=0.7) +
    geom_sf(data = coord_rouzic,
            shape=23, 
            colour="black",
            fill="yellow", 
            size=4) + 
    coord_sf(xlim=c(-7,7),
             ylim=c(47,57),
             expand=F) +
    labs(title="Adults, breeding season")+
    scale_colour_gradientn(colours=viridis(length(unique(indisf$Date))),
                           breaks = seq.Date(min(indisf$Date),
                                             max(indisf$Date),
                                             length.out=4),
                           aesthetics = "colour") +
    scale_fill_gradientn(name="Bathymétrie",
                         colors =ocean.pal,
                         limits = c(- 500, 0))+
    annotation_scale(location = "br",
                     width_hint = 0.15,
                     pad_x = unit(0.7, "cm"),
                     bar_cols = "black") +
    annotation_north_arrow(location = "br",
                           which_north = "true",
                           style = ggspatial::north_arrow_nautical(),
                           pad_y = unit(0.8, "cm")) +
    theme_bw() 

tiff(here::here("outputs","raw-adults_breeding.tif"),
     width=2800, 
     height=2800, 
     res=500,
     compression="lzw")
print(map)
dev.off()

########################################################################################
## adults migration
########################################################################################
indisfad<-raw_rouzic_2023 %>% 
    dplyr::filter(stage=="adult" & travelNb > 0) %>% 
    dplyr::mutate(Date=as.Date(datetime, format="%F")) %>% 
    st_as_sf(.,coords=c("long","lat")) %>% 
    st_set_crs(st_crs(4326)) #%>% 
# write_sf(.,
#          dsn=here("data","NewlyCreatedData","shapefiles"),
#       layer="rouzic_2023_adults_",
#       driver="ESRI Shapefile",
#       delete_layer =T,
#        delete_dsn = T)

mapad<-ggplot()+
    tidyterra::geom_spatraster(data = bathymjuv, aes(fill = ETOPO_world))+
    geom_sf(data=indisfad, aes(color=Date),size=0.7) +
    geom_sf(data=shp,fill="grey90") +
    geom_sf(data=coast) +
    geom_sf(data = coord_rouzic,
            shape=23, 
            colour="black",
            fill="yellow", 
            size=4) + 
    coord_sf(xlim=c(-18,7),
             ylim=c(20,57),
             expand=F) +
    labs(title="Adults, migration")+
    scale_colour_gradientn(colours=viridis(length(unique(indisfad$Date))),
                           breaks = seq.Date(min(indisfad$Date),
                                             max(indisfad$Date),
                                             length.out=4),
                           aesthetics = "colour") +
    scale_fill_gradientn(name="Bathymétrie",
                         colors =ocean.pal,
                         limits = c(- 6500, 0))+
    annotation_scale(location = "br",
                     width_hint = 0.15,
                     pad_x = unit(0.7, "cm"),
                     bar_cols = "black") +
    annotation_north_arrow(location = "br",
                           which_north = "true",
                           style = ggspatial::north_arrow_nautical(),
                           pad_y = unit(0.8, "cm")) +
    theme_bw() 
print(mapad)
########################################################################################
## juveniles migration
########################################################################################
indisfjuv<-raw_rouzic_2023 %>% 
    dplyr::filter(stage=="juvenile" & travelNb > 0) %>% 
    dplyr::mutate(Date=as.Date(datetime, format="%F")) %>% 
    st_as_sf(.,coords=c("long","lat")) %>% 
    st_set_crs(st_crs(4326)) #%>% 
    # write_sf(.,
    #          dsn=here("data","NewlyCreatedData","shapefiles"),
    #          layer="rouzic_2023_juveniles_",
    #          driver="ESRI Shapefile",
    #          delete_layer =T,
    #          delete_dsn = T)
    

mapjuv<-ggplot()+
    tidyterra::geom_spatraster(data = bathymjuv, aes(fill =  ETOPO_world))+
    geom_sf(data=shp,fill="grey90") +
    geom_sf(data=coast) +
    geom_sf(data=indisfjuv, aes(color=Date), shape=17,size=0.7) +
    geom_sf(data = coord_rouzic,
            shape=23, 
            colour="black",
            fill="yellow", 
            size=4) + 
    coord_sf(xlim=c(-18,2),
             ylim=c(20,51),
             expand=F) +
    labs(title="Juveniles, migration") +
    scale_colour_gradientn(colours=magma(length(unique(indisfjuv$Date))),
                           breaks = seq.Date(min(indisfjuv$Date),
                                             max(indisfjuv$Date),
                                             length.out=4),
                           aesthetics = "colour") +
    scale_fill_gradientn(name="Bathymétrie",
                         colors =ocean.pal,
                         limits = c(- 6500, 0))+
    annotation_scale(location = "br",
                     width_hint = 0.15,
                     pad_x = unit(0.7, "cm"),
                     bar_cols = "black") +
    annotation_north_arrow(location = "br",
                           which_north = "true",
                           style = ggspatial::north_arrow_nautical(),
                           pad_y = unit(0.8, "cm")) +
    theme_bw() 

tiff(here::here("outputs","raw_migration_adults_juv.tif"),
     width=4500,
     height=3000,
     res=500,
     compression="lzw")
grid.arrange(mapad,mapjuv,ncol=2)
dev.off()

########################################################################################
## adult kernels
########################################################################################
source(here::here("R","problem_trips.R"))

indisfad_spdf<-indisf %>% 
   st_transform(., projcrs) %>% 
    dplyr::filter(!trip.id %in% migration & 
                      !trip.id %in% last.incomplete) %>% 
    as_Spatial(.)

col<-brewer.pal(n=4,"YlOrRd")

KUD.adBS<- kernelUD(indisfad_spdf[,"site"],
                    same4all=T,
                    h=5000,
                    grid=1000)
KUDvol.adBSk <- getvolumeUD(KUD.adBS)
ver90.rouzic<-getverticeshr(KUDvol.adBSk ,90) %>% 
    st_as_sf(.)
ver75.rouzic<-getverticeshr(KUDvol.adBSk ,75)%>% 
    st_as_sf(.)
ver50.rouzic<-getverticeshr(KUDvol.adBSk ,50)%>% 
    st_as_sf(.)
ver25.rouzic<-getverticeshr(KUDvol.adBSk ,25)%>% 
    st_as_sf(.)

ext.brit<-terra::ext(320000,620000,5290000,5600000)

bathymbrit<-bathym %>% 
    terra::project(.,projcrs) %>% 
    terra::crop(., ext.brit)

#coastlines
coast.proj<-ne_coastline(scale = 10, returnclass = "sf") %>% 
    st_transform(.,projcrs)  %>% 
    st_crop(., ext.brit)

#contours of countries
shp.proj<-ne_countries(scale = 10, returnclass = 'sf') %>%
    dplyr::filter(name %in% target) %>% 
    st_transform(.,projcrs) %>% 
    st_crop(., ext.brit)


##map
kern<-ggplot()+
    tidyterra::geom_spatraster(data = bathymbrit, aes(fill =  Bathym_FR_UK)) +
    geom_sf(data=ver90.rouzic, fill=col[1])+
    geom_sf(data=ver75.rouzic, fill=col[2]) +
    geom_sf(data=ver50.rouzic,  fill=col[3]) +  
    geom_sf(data=ver25.rouzic,  fill=col[4]) + 
    geom_sf(data=shp.proj,fill="grey90") +
    geom_sf(data=coast.proj) +
    geom_sf(data = coord_rouzic,
            shape=23, 
            colour="black",
            fill="yellow", 
            size=4) + 
    coord_sf(xlim=ext.brit[c(1,2)],
             ylim=ext.brit[c(3,4)],
             expand=F) +
    # scale_fill_manual(values = c("90%" = col[1], 
    #                              "75%" = col[2],
    #                              "50%" = col[3],
    #                              "25%" = col[4]),
    #                   name = "Distribution") +
    scale_fill_gradientn(name="Bathymétrie",
                         colors =ocean.pal,
                         limits = c(- 200, 0))+
    annotation_scale(location = "br",
                     width_hint = 0.15,
                     pad_x = unit(0.7, "cm"),
                     bar_cols = "black") +
    annotation_north_arrow(location = "br",
                           which_north = "true",
                           style = ggspatial::north_arrow_nautical(),
                           pad_y = unit(0.8, "cm")) +
    theme_bw() 


tiff(here::here("outputs","adults_distribution_kernel.tif"),
     width=3500,
     height=3500,
     res=500,
     compression="lzw")
print(kern)
dev.off()
