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
setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Final_layers")
draw <- 1
dsn   <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/UFPs_subsampled_layers"

while(draw <= 100) {  
  shapefile_roads <- st_read("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL1_F_Road_type.shp")
  shapefile_roads <- st_transform(shapefile_roads, crs = "+proj=utm +zone=43 ellps=WGS84")
  road_sf <- st_as_sf(shapefile_roads)
  road_df <- road_sf %>%
    dplyr::select(Road_ID) %>%
    mutate_at(c('Road_ID'), as.numeric)
  road_df$geometry <- NULL
  base_road_df <- road_df %>%
    mutate(num_obs = 0, Magic_num = 0) %>%
    mutate_at(c('Road_ID'), as.numeric)
  row_no_road <- as.numeric(as.character(nrow(base_road_df)))
  per <- floor((5 / 100) * row_no_road)    
  big_data_N <- data.frame()
  files <- list.files("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Final_layers", 
                      pattern = "\\.shp$")
  while(row_no_road >= per) {
    sample_file <- sample(files, 1, replace = FALSE, prob = NULL)
    files <- files[!grepl(sample_file, unlist(files))] 
    rand_shpf <- st_read(sample_file)
    rand_shpf <- st_transform(rand_shpf, CRS("+proj=utm +zone=43 ellps=WGS84"))
    sf_rand_shpf <- st_as_sf(rand_shpf) 
    name_rand_file <- str_replace_all(substr(sample_file, 1, 18), "_", "-") 
    CPC_name <- paste0(name_rand_file, "_CPC")
    df_rand_shpf <- sf_rand_shpf %>%
      dplyr::select(Road_ID, CPC_mn) %>% # select CPC_mn corrected
      mutate_at(c('Road_ID', 'CPC_mn'), as.numeric)
    names(df_rand_shpf) <- c("Road_ID", CPC_name, "geometry")
    df_rand_shpf$geometry <- NULL
    base_road_df <- left_join(base_road_df, df_rand_shpf, by = "Road_ID")
    setDT(base_road_df)
    base_road_df[, -grep("i$", colnames(base_road_df))]
    base_road_df[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x)))] 
    base_road_df$Magic_num <- as.numeric(as.character(base_road_df$num_obs)) - 3
    newdata_N <- subset(base_road_df, Magic_num == 12)  # input the N here
    base_road_df <- subset(base_road_df, Magic_num != 12)# input the N here
    row_no_road <- as.numeric(as.character(nrow(base_road_df)))
    setDT(big_data_N)
    setDT(newdata_N)
    big_data_N <- rbind(big_data_N, newdata_N, fill = TRUE)  
  }
  big_data_N <- rbind(big_data_N, base_road_df, fill = TRUE)  # 95% thing
  setDT(big_data_N)
  try <- big_data_N
  View(try)
  big_data_N[, c("num_obs", "Magic_num"):= NULL]
  R <- melt(setDT(big_data_N, keep.rownames = TRUE),
            id.var = c("Road_ID"), na.rm = TRUE) [
              order(Road_ID)][]
  R <- data.frame(R)
  names(R) <- c("Road_ID", "Date", "Mean_CPC")
  View(R)
  Final_df <- R %>%
    dplyr::select(Road_ID, Mean_CPC) %>%
    mutate_at(c('Road_ID'), as.numeric) %>%
    group_by(Road_ID) %>%
    summarise(Median_CPC = median(Mean_CPC, na.rm = TRUE))         
  sh <- left_join(road_sf, Final_df, by = "Road_ID")
  sh$UID <- paste0("MAL1_", sh$Road_ID)
  shp_final <- st_as_sf(sh) 
  shp_final <- as(shp_final, 'Spatial')
  layer  <- paste("12", draw, sep = "_") 
  writeOGR(shp_final, dsn, layer, driver = "ESRI Shapefile")
  draw <- as.numeric(as.character(draw)) + 1
}
