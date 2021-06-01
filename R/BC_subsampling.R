library(dplyr)
library(lubridate)
library(hms)
library(chron)
library(stringr)
library(raster)
library(rgdal)
library(readr)
library(sp) 
library(rgeos)
library(sf) 
library(data.table)
setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers")
draw <- 1
dsn   <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/BC_subsampled_layers"
shapefile_roads <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL2_F_Road_type.shp", 
                           layer = "MAL2_F_Road_type")
shapefile_roads <- spTransform(shapefile_roads, CRS("+proj=utm +zone=43 ellps=WGS84"))
road_sf <- st_as_sf(shapefile_roads)
road_sf <- data.frame(road_sf)
files <- list.files("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers", 
                    pattern = "\\.shp$")

while(draw <= 100) {   
  road_df <- road_sf %>%
    dplyr::select(Road_ID) %>%
    mutate_at(c('Road_ID'), as.numeric)
  base_road_df <- road_df %>%
    mutate(num_obs = 0, Magic_num = 0) %>%
    mutate_at(c('Road_ID'), as.numeric)
  row_no_road <- as.numeric(as.character(nrow(base_road_df)))
  per <- floor((5 / 100) * row_no_road)   ## change for lesser then 95 percent 
  big_data_N <- data.frame()
  while(row_no_road >= per) {
    sample_file <- sample(files, 1, replace = FALSE,  prob = NULL)
    files <- files[!grepl(sample_file, unlist(files))] 
    rand_shpf <- readOGR(sample_file)
    rand_shpf <- spTransform(rand_shpf, CRS("+proj=utm +zone=43 ellps=WGS84"))
    sf_rand_shpf <- st_as_sf(rand_shpf) 
    name_rand_file <- str_replace_all(substr(sample_file, 1, 18), "_", "-") 
    BC_name <- paste0(name_rand_file, "_BC")
    sf_rand_shpf$geometry <- NULL
    df_rand_shpf <- sf_rand_shpf %>%
      dplyr::select(Road_ID, BC_mn) %>% # select BC_mn / BC_c_mn corrected
      mutate_at(c('Road_ID', 'BC_mn'), as.numeric)  # select BC_c_mn corrected
    names(df_rand_shpf) <- c("Road_ID", BC_name)
    base_road_df <- left_join(base_road_df, df_rand_shpf, by = "Road_ID")
    setDT(base_road_df)
    base_road_df[, -grep("i$", colnames(base_road_df))]
    base_road_df[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x)))] 
    base_road_df$num_obs <- as.numeric(as.character(base_road_df$num_obs))
    base_road_df$Magic_num <- base_road_df$num_obs - 3
    newdata_N <- subset(base_road_df, Magic_num == 2)  # input the N here
    base_road_df <- subset(base_road_df, Magic_num != 2)# input the N here
    row_no_road <- as.numeric(as.character(nrow(base_road_df)))
    setDT(big_data_N)
    setDT(newdata_N)
    big_data_N <- rbind(big_data_N, newdata_N, fill = TRUE)  
  }
  big_data_N <- rbind(big_data_N, base_road_df, fill = TRUE)  # 95% thing
  setDT(big_data_N)
  big_data_N[, c("num_obs", "Magic_num"):= NULL]
  R <- melt(setDT(big_data_N, keep.rownames = TRUE),
            id.var = c("Road_ID"), na.rm = TRUE) [
              order(Road_ID)][]
  R <- data.frame(R)
  names(R) <- c("Road_ID", "Date", "Mean_BC")
  Final_df <- R %>%
    dplyr::select(Road_ID, Mean_BC) %>%
    group_by(Road_ID) %>%
    summarise(Median_BC = median(Mean_BC))         
  Final_df <- R %>%
    dplyr::select(Road_ID, Mean_BC) %>%
    mutate_at(c('Road_ID'), as.numeric) %>%
    group_by(Road_ID) %>%
    summarise(Median_BC = median(Mean_BC, na.rm = TRUE))         
  sh <- left_join(road_sf, Final_df, by = "Road_ID")
  sh$UID <- paste0("MAL2_", sh$Road_ID)
  View(sh)
  shp_final <- st_as_sf(sh) 
  shp_final <- as(shp_final, 'Spatial')
  layer  <- paste("2", draw, sep = "_") 
  writeOGR(shp_final, dsn, layer, driver = "ESRI Shapefile")
  draw <- as.numeric(as.character(draw)) + 1
}
