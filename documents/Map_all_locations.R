library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)
library(terra)
library(ggspatial)
library(RColorBrewer)
library(gridExtra)

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
    dplyr::filter(stage=="adult" & lat > 47) %>% 
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
     width=3000, 
     height=3000, 
     res=400,
     compression="lzw")
print(map)
dev.off()

########################################################################################
## adults migration
########################################################################################
indisfad<-raw_rouzic_2023 %>% 
    dplyr::filter(stage=="adult") %>% 
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
    dplyr::filter(stage=="juvenile") %>% 
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
     width=5000,
     height=3500,
     res=500,
     compression="lzw")
grid.arrange(mapad,mapjuv,ncol=2)
dev.off()



indisfad_spdf<-as_Spatial(indisf) %>% 
    spTransform(., projcrs)

KUD.adBS<- kernelUD(indisfad_spdf[,"site"],
                    same4all=T,
                    h=5000,
                    grid=1000)
KUDvol.adBSk <- getvolumeUD(KUD.adBS)
ver90.rouzic<-getverticeshr(KUDvol.adBSk ,90)
ver50.rouzic<-getverticeshr(KUDvol.adBSk ,50)

plot(ver90.rouzic,col="orange")
plot(ver50.rouzic,col="red",add=T)
