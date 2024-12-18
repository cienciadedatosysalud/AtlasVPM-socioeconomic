pacman::p_load(
  tidyverse,
  sf,
  rmapshaper,
  tmap,
  Hmisc,
  openxlsx,
  nngeo,
  ineAtlas
)
## ZBS = PCA ##
## sec_cen = census tracts ##
sec_cen <- ineAtlas::get_tract_geom(year=2022) %>% dplyr::select(tract_code,geom) %>% 
  dplyr::rename(CUSEC=tract_code,geometry=geom)

sec_cen_ <- read.csv("../inputs/SECC_CE_20220101.csv",sep='|',colClasses = c(CUSEC='character',CUMUN='character',
                                                                             CSEC='character',CDIS='character',
                                                                             CMUN='character', CPRO='character', 
                                                                             CCA='character', CUDIS='character',
                                                                             CLAU2='character',CNUT1='character',
                                                                             CNUT2='character',CNUT3='character'))

sec_cen <- left_join(sec_cen,sec_cen_,by='CUSEC')
rm(sec_cen_)


### Remove Ceuta and Melilla (no zbs) ###
sec_cen <- sec_cen %>% filter(CCA %nin% c('18','19'))

zbs <- read_sf("../inputs/ZBS_ESP_4258_orginal_all.shp") %>% 
  dplyr::select(!c(area,provincia,ccaa))

## MOVE CANANARY ISLANDS ## 

sec_cen_can <- sec_cen %>% filter(CCA == "05")
sec_cen <- sec_cen %>% filter(CCA %nin% c("05"))

crs <- st_crs(zbs)

geocanariasco <- sec_cen_can %>%
 st_transform(crs) %>%
 st_geometry()
geocanariasco <- geocanariasco + c(5, 7)

sec_cen_can <- sec_cen_can %>%
 st_set_geometry(geocanariasco) %>%
 st_set_crs(crs)

sec_cen <- st_transform(sec_cen,crs)
sec_cen <- rbind(sec_cen, sec_cen_can)
rm(sec_cen_can,geocanariasco,crs)
######



shape_intersection_points_polygons_4258 <- function(sf_object_low_granularity, sf_object_high_granularity, grouping_by_variable){
  
  sf_object_high_granularity <- {{sf_object_high_granularity}} %>% st_transform(32617) # transform to cal centroid
  
  sf_cent <- st_point_on_surface(sf_object_high_granularity) %>% st_transform(4258) # cal centroid and transform to be able compare
  
  sf_object_high_granularity <- sf_object_high_granularity %>% st_transform(4258) # transform to be able compare
  
  sf_use_s2(FALSE) # convert spheric to planar
  sf_cent <- sf_cent %>% mutate(
    intersection = as.integer((st_intersects(geometry, {{sf_object_low_granularity}}))), 
    !! grouping_by_variable:= as.character(sf_object_low_granularity[intersection, grouping_by_variable][[1]]))
  
  sf_object_high_granularity_aux <- as_tibble(sf_cent) %>% 
    dplyr::select(!geometry) 
  
  
  sf_object_high_granularity <- left_join(x = sf_object_high_granularity, y = sf_object_high_granularity_aux)
  
  return(sf_object_high_granularity)
  
}


sec_cen_intersection <- shape_intersection_points_polygons_4258(zbs, sec_cen, 'codatzbs')

rm(sec_cen)



sec_cen_intersection$codatzbs[sec_cen_intersection$CUSEC == '1101204005'] <- '10903'
sec_cen_intersection$codatzbs[sec_cen_intersection$CUSEC == '1101210001'] <- '10903'
sec_cen_intersection$codatzbs[sec_cen_intersection$CUSEC == '1101210019'] <- '10903'
sec_cen_intersection$codatzbs[sec_cen_intersection$CUSEC == '1102201005'] <- '10305'
sec_cen_intersection$codatzbs[sec_cen_intersection$CUSEC == '3302401016'] <- '30508'
sec_cen_intersection$codatzbs[sec_cen_intersection$CUSEC == '3302407051'] <- '30507'
sec_cen_intersection$codatzbs[sec_cen_intersection$CUSEC == '3302408010'] <- '30506'
sec_cen_intersection$codatzbs[sec_cen_intersection$CUSEC == '3304411009'] <- '30404'
sec_cen_intersection$codatzbs[sec_cen_intersection$CUSEC == '4625011006'] <- '50514'
sec_cen_intersection$codatzbs[sec_cen_intersection$CUSEC == '4625011044'] <- '50516'
sec_cen_intersection$codatzbs[sec_cen_intersection$CUSEC == '4625017001'] <- '50212'


zbs1 <- as_tibble(zbs) %>% dplyr::select(!geometry)
zbs1$codatzbs <- as.character(zbs1$codatzbs)
sec_cen_intersection <- left_join(x = sec_cen_intersection, y = zbs1, by = 'codatzbs')
sec_cen_intersection <- sec_cen_intersection[!duplicated(sec_cen_intersection$CUSEC),]
sec_cen_intersection <- sec_cen_intersection %>% dplyr::select(!intersection)


#### Transform to CSV (no map) ####
sec_cen_csv <- as_tibble(sec_cen_intersection)
sec_cen_csv <- sec_cen_csv %>% dplyr::select(-geometry)
write.table(sec_cen_csv,file="../outputs/1_outputs/data_census_tract_pca_2022_2020.csv", sep = "|", row.names = FALSE)

####

####################################################

## To check for matching area percentages between the intersection sec_cen with 
## zbs of sec_cen, with the perc_area = 100 a perfect match

zbs <- st_make_valid(zbs, geos_method = 'valid_linework', geos_keep_collapsed = FALSE) 
sec_cen_intersection <- st_make_valid(sec_cen_intersection, geos_method = 'valid_linework', geos_keep_collapsed = FALSE) 


sec_cen_intersection$perc_area<-0
for(i in 1:nrow(sec_cen_intersection)){
  zbs_filtered <- zbs %>% filter(codatzbs == sec_cen_intersection$codatzbs[i])
  sec_cen_filtered <- sec_cen_intersection %>% filter(CUSEC == sec_cen_intersection$CUSEC[i])
  intersection <- st_intersection(sec_cen_filtered,zbs_filtered)
  area_intersec <- st_area(intersection)
  
  if((length(area_intersec) == 0)){
    sec_cen_intersection$perc_area[i] <- NA
  }else{
    sec_cen_intersection$perc_area[i] <- (area_intersec/st_area(sec_cen_filtered))*100
    
  }
}

sec_cen_intersection$perc_area[is.na(sec_cen_intersection$perc_area)] <- 0

sec_cen_intersection$perc_area[sec_cen_intersection$CUSEC == '1101204005'] <- 100
sec_cen_intersection$perc_area[sec_cen_intersection$CUSEC == '1101210001'] <- 100
sec_cen_intersection$perc_area[sec_cen_intersection$CUSEC == '1101210019'] <- 100
sec_cen_intersection$perc_area[sec_cen_intersection$CUSEC == '1102201005'] <- 100
sec_cen_intersection$perc_area[sec_cen_intersection$CUSEC == '3302401016'] <- 100
sec_cen_intersection$perc_area[sec_cen_intersection$CUSEC == '3302407051'] <- 100
sec_cen_intersection$perc_area[sec_cen_intersection$CUSEC == '3302408010'] <- 100
sec_cen_intersection$perc_area[sec_cen_intersection$CUSEC == '3304411009'] <- 100
sec_cen_intersection$perc_area[sec_cen_intersection$CUSEC == '4625011006'] <- 100
sec_cen_intersection$perc_area[sec_cen_intersection$CUSEC == '4625011044'] <- 100
sec_cen_intersection$perc_area[sec_cen_intersection$CUSEC == '4625017001'] <- 100


sec_cen_csv <- as_tibble(sec_cen_intersection)
sec_cen_csv <- sec_cen_csv %>% dplyr::select(-geometry)


write.table(sec_cen_csv,file="../outputs/1_outputs/census_tracts_intersection_pca_point_on_surface_with_percent_2022_2020.csv", sep = "|", row.names = FALSE)
rm(sec_cen_csv)

sec_cen_intersection$nuts2 <- paste0(sec_cen_intersection$CNUT0,sec_cen_intersection$CNUT1,sec_cen_intersection$CNUT2)

sec_cen_less95 <- sec_cen_intersection %>% filter(perc_area < 95)


# Reassignment of ZBS to census tract by sampling 1000 points and 
# choosing the mode (most repeated zbs where the point falls).

set.seed(1234)
sf_use_s2(FALSE)

for(i in 1:nrow(sec_cen_less95)){
  df <- data.frame(n=i)
  sec_cen_less95_ <- sec_cen_less95 %>% filter(CUSEC %in% sec_cen_less95$CUSEC[i])
  map_sec_cen1 <- sec_cen_intersection %>% filter(CUSEC %in% sec_cen_less95_$CUSEC) %>% 
    st_make_valid()
  
  
  points <- st_sample(map_sec_cen1, size=1000, exact=TRUE) %>% 
    st_make_valid()
  zbs_map_ <- zbs %>% filter(ccaa_cd %in% sec_cen_less95_$nuts2) %>% 
    st_make_valid()
  
  intersection <- as.integer(unlist(st_intersects(points,zbs_map_)))
  
  
  codatzbs <- as.character(zbs_map_[intersection, 'codatzbs'][[1]])
  x <- DescTools::Mode(codatzbs,na.rm = TRUE)
  
  zbs_filtered <- zbs_map_ %>% filter(codatzbs == x[1])
  if(zbs_filtered$ccaa_cd != sec_cen_less95_$nuts2){
    sec_cen_less95$codatzbs_corrected[i] <- NA
    sec_cen_less95$perc_area_corrected[i] <- NA
  }else{
    intersection <- st_intersection(map_sec_cen1,zbs_filtered)
    area_intersec <- st_area(intersection)
    
    if((length(area_intersec) == 0)){
      sec_cen_less95$codatzbs_corrected[i] <- NA
      sec_cen_less95$perc_area_corrected[i] <- NA
    }else{
      sec_cen_less95$codatzbs_corrected[i] <- as.character(x)
      sec_cen_less95$perc_area_corrected[i] <- (area_intersec/st_area(map_sec_cen1))*100
    }
  }
}



# Test new ZBS assigned intersect more percentage area with sec cen 


test <- sec_cen_less95 %>% dplyr::select(CUSEC,NMUN,codatzbs,perc_area,codatzbs_corrected,perc_area_corrected)
p <- test %>% filter(codatzbs != codatzbs_corrected)
p1 <- p %>% filter(perc_area <= perc_area_corrected)
p2 <- p %>% filter(perc_area > perc_area_corrected)

# Assign new codatzbs only in sec cen with major area intersected

sec_cen_intersection <- sec_cen_intersection %>% dplyr::select(!n_zbs)

sec_cen_intersection$codatzbs[sec_cen_intersection$CUSEC %in% p1$CUSEC] <- p1$codatzbs_corrected
sec_cen_intersection$perc_area[sec_cen_intersection$CUSEC %in% p1$CUSEC] <- p1$perc_area_corrected
zbs_ <- as_tibble(zbs) %>% dplyr::select(codatzbs,n_zbs)


sec_cen_intersection <- left_join(sec_cen_intersection,zbs_,by='codatzbs')


sec_cen_csv <- as_tibble(sec_cen_intersection)
sec_cen_csv <- sec_cen_csv %>% dplyr::select(-geometry)

write.table(sec_cen_csv,file="../outputs/1_outputs/census_tracts_intersection_pca_point_on_surface_with_percent_2022_2020_after_corrected.csv", sep = "|", row.names = FALSE)

####



## New ZBS Map by SEC CEN Map

zbs_ <- ms_dissolve(sec_cen_intersection, field = "codatzbs")
zbs_ <- zbs_ %>% filter(codatzbs!=999999)

zbs_ <- zbs_ %>% st_remove_holes()

zbs <- zbs %>% filter(codatzbs!=999999)

zbs_csv <- as_tibble(zbs) %>% dplyr::select(!geometry)
zbs_ <- left_join(zbs_,zbs_csv,by='codatzbs')

st_write(zbs_, paste0("../outputs/1_outputs/new_PCA_ESP_4258_2022_all.shp"), driver="ESRI Shapefile")
rm(zbs_csv)

#### To observe the coincidence in maps after intersection ####


tmap::tmap_options(check.and.fix = TRUE)


sf_use_s2(FALSE)

tm_shape(zbs) + tm_borders(lwd = 1.2) +
  tm_shape(zbs_) +
  tm_borders("red", alpha = 1, lwd = 0.3)





