# load libraries
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
library(here)
library(tidyverse)

extract_road_type <- function(path = "D:/Dropbox/APMfull/MAL_CNG_Paper",
                              file) {
  shapefile_roads <- st_read(here(path, file))
  shapefile_roads <- st_transform(shapefile_roads, crs = "+proj=utm +zone=43 ellps=WGS84")
  road_sf <- data.frame(shapefile_roads)
}

extract_roaddata <- function(path = "D:/Dropbox/APMfull/MAL_CNG_Paper",
                              file) {
  shapefile_roads <- st_read(here(path, file))
  shapefile_roads <- st_transform(shapefile_roads, crs = "+proj=utm +zone=43 ellps=WGS84")
}


extract_all_data <- function(files) {
  files_sub <- files %>% 
    dplyr::select(UID, Rod_typ, Hghw_US, BC_NR_L_mn, BC_c_mn, 
                  CPC_mn, file_name) %>% 
    mutate(CPC_mn = as.numeric(as.character(CPC_mn)),
           BC_c_mn = as.numeric(as.character(BC_c_mn)),
           BC_NR_L_mn = as.numeric(as.character(BC_NR_L_mn))) 
  bc <- files_sub %>% 
    dplyr::select(UID, Rod_typ, Hghw_US, BC_NR_L_mn, file_name) %>% 
    pivot_wider(names_from = file_name, values_from = BC_NR_L_mn)
  cpc <- files_sub %>% 
    dplyr::select(UID, Rod_typ, Hghw_US, CPC_mn, file_name) %>% 
    pivot_wider(names_from = file_name, values_from = CPC_mn)
  file_list_bc <- names(bc)
  for (i in seq_along(file_list_bc)) {
    if (grepl("\\.shp$", file_list_bc[i])) {
      new_name_bc <- paste0("ride_", i)
      file.rename(file_list_bc[i], new_name_bc)
      file_list_bc[i] <- new_name_bc
    }
  }
  names(bc) <- file_list_bc
  file_list_cpc <- names(cpc)
  for (i in seq_along(file_list_cpc)) {
    if (grepl("\\.shp$", file_list_cpc[i])) {
      new_name_cpc <- paste0("ride_", i)
      file.rename(file_list_cpc[i], new_name_cpc)
      file_list_cpc[i] <- new_name_cpc
    }
  }
  names(cpc) <- file_list_cpc
  return(list(bc, cpc))
}

path <- here("D:/Dropbox/APMfull/MAL_CNG_Paper")

path_mal1 <- here(path, "MAL1", "Final_layers")
dsn_mal1 <- here(path, "MAL1", "BC_subsampled_layers_rd")
files_mal1 <- list.files(path_mal1, pattern = "\\.shp$")

path_mal2 <- here(path, "MAL2", "Final_layers")
dsn_mal2 <- here(path, "MAL2", "BC_subsampled_layers_rd")
files_mal2 <- list.files(path_mal2, pattern = "\\.shp$")

path_kan <- here(path, "KAN", "Final_layers")
dsn_kan <- here(path, "KAN", "BC_subsampled_layers_rd")
files_kan <- list.files(path_kan, pattern = "\\.shp$")

path_cbd <- here(path, "CBD", "Final_layers")
dsn_cbd <- here(path, "CBD", "BC_subsampled_layers_rd")
files_cbd <- list.files(path_cbd, pattern = "\\.shp$")


MAL1_roads <- extract_roaddata(path = "D:/Dropbox/APMfull/MAL_CNG_Paper",
                               file = "Roads/MAL1_F_Road_type.shp")
MAL2_roads <- extract_roaddata(path = "D:/Dropbox/APMfull/MAL_CNG_Paper",
                               file = "Roads/MAL2_F_Road_type.shp")
KAN_roads <- extract_roaddata(path = "D:/Dropbox/APMfull/MAL_CNG_Paper",
                              file = "Roads/KAN_F_Road_type.shp")
CBD_roads <- extract_roaddata(path = "D:/Dropbox/APMfull/MAL_CNG_Paper",
                              file = "Roads/CBD_F_Road_type.shp")


mal1_files <- data.frame()
for(i in files_mal1) {
  mal1_file <- extract_road_type(here(path_mal1), i) %>% 
    mutate(file_name = i)
  mal1_files <- rbind(mal1_file, mal1_files)
}

mal2_files <- data.frame()
for(i in files_mal2) {
  mal2_file <- extract_road_type(here(path_mal2), i) %>% 
    mutate(file_name = i)
  mal2_files <- rbind(mal2_file, mal2_files)
}

kan_files <- data.frame()
for(i in files_kan) {
  kan_file <- extract_road_type(here(path_kan), i) %>% 
    mutate(file_name = i)
  kan_files <- rbind(kan_file, kan_files)
}

cbd_files <- data.frame()
for(i in files_cbd) {
  cbd_file <- extract_road_type(here(path_cbd), i) %>% 
    mutate(file_name = i)
  cbd_files <- rbind(cbd_file, cbd_files)
}


mal1 <- extract_all_data(mal1_files)
mal1_bc <- mal1[[1]]
mal1_cpc <- mal1[[2]] 
  
mal2 <- extract_all_data(mal2_files)
mal2_bc <- mal2[[1]] 
mal2_cpc <- mal2[[2]] 
  

cbd <- extract_all_data(cbd_files)
cbd_bc <- cbd[[1]]
cbd_cpc <- cbd[[2]]
  

kan <- extract_all_data(kan_files)
kan_bc <- kan[[1]] 
kan_cpc <- kan[[2]]

bc <- rbind(setDT(mal1_bc), setDT(mal2_bc), setDT(kan_bc), 
            setDT(cbd_bc), fill = T) %>% 
  mutate(across(-c(UID, Rod_typ, Hghw_US), as.numeric)) %>% 
  janitor::remove_empty(which = c("cols"))
cpc <- rbind(setDT(mal1_cpc), setDT(mal2_cpc), setDT(kan_cpc), 
             setDT(cbd_cpc), fill = T)%>% 
  mutate(across(-c(UID, Rod_typ, Hghw_US), as.numeric)) %>% 
  janitor::remove_empty(which = c("cols"))

sel_road_type <- "Residential"

draw <- 1 
roads_used <- bc %>% 
  filter(Rod_typ == sel_road_type) 
# Highway 12 - 7; 14 - 12; 16 - 17; 18 - 30
# Arterial 16 - 10; 18 - 20
# Residential 16 - 12; 18 > 30 

while(draw <= 100) {   
  upper_limit <- 30 ## Change this 
  no_times <- 18 ## Change this 
  big_data_N <- data.frame() 
  row_no_road <- as.numeric(as.character(nrow(roads_used)))
  per <- floor((upper_limit / 100) * row_no_road) 
  base_road <- roads_used %>%  
    mutate(num_obs = 0, magic_no = 0) %>% 
    dplyr::select(UID, num_obs, magic_no)
  list_names <- names(roads_used %>% 
                        dplyr::select(contains("ride")))
  while(row_no_road >= per) {
    sample_file <- sample(list_names, 1, replace = FALSE,  prob = NULL)
    list_names <- list_names[!grepl(sample_file, unlist(list_names))] 
    selected_ride <- roads_used %>% 
      dplyr::select(UID, sample_file) 
    base_road <- left_join(base_road, selected_ride, by = "UID")
    base_road[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x)))] 
    base_road <- base_road %>% 
      mutate(magic_no = num_obs - 3)
    completed_road_segs <- subset(base_road, magic_no == no_times)  
    remaining_road_segs <- subset(base_road, magic_no <= no_times)
    row_no_road <- as.numeric(as.character(nrow(remaining_road_segs)))
    big_data_N <- rbind(setDT(big_data_N), setDT(completed_road_segs), fill = TRUE)  
  }
  big_data_N <- rbind(big_data_N, remaining_road_segs, fill = TRUE)  # 95% thing
  final_df <- big_data_N %>% 
    dplyr::select(everything(), - magic_no, - num_obs) %>% 
    distinct(across(everything())) %>% 
    pivot_longer(-c(UID), names_to = "ride", values_to = "value") %>% 
    distinct(across(everything())) %>% 
    filter(!is.na(value)) %>% 
    dplyr::select(everything(), - ride) %>% 
    group_by(UID) %>% 
    summarise(median = median(value, na.rm = T))
  write.csv(final_df, here("road_type_sb/BC/Residential/",
                           paste0(no_times, "_", draw, ".csv")))
  draw <- draw + 1
}
beepr::beep()

# Highway 16 - 10; 18 - 25
# Arterial 16 - 10; 18 > 30 so not applied 
# Residential 16 - 7; 18 > 30

draw <- 1 
roads_used <- cpc %>% 
  filter(Rod_typ == sel_road_type) 

while(draw <= 100) {   
  upper_limit <- 30 ## Change this 
  no_times <- 18 ## Change this 
  big_data_N <- data.frame() 
  row_no_road <- as.numeric(as.character(nrow(roads_used)))
  per <- floor((upper_limit / 100) * row_no_road) 
  base_road <- roads_used %>%  
    mutate(num_obs = 0, magic_no = 0) %>% 
    dplyr::select(UID, num_obs, magic_no)
  list_names <- names(roads_used %>% 
                        dplyr::select(contains("ride")))
  while(row_no_road >= per) {
    sample_file <- sample(list_names, 1, replace = FALSE,  prob = NULL)
    list_names <- list_names[!grepl(sample_file, unlist(list_names))] 
    selected_ride <- roads_used %>% 
      dplyr::select(UID, sample_file) 
    base_road <- left_join(base_road, selected_ride, by = "UID")
    base_road[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x)))] 
    base_road <- base_road %>% 
      mutate(magic_no = num_obs - 3)
    completed_road_segs <- subset(base_road, magic_no == no_times)  
    remaining_road_segs <- subset(base_road, magic_no <= no_times)
    row_no_road <- as.numeric(as.character(nrow(remaining_road_segs)))
    big_data_N <- rbind(setDT(big_data_N), setDT(completed_road_segs), fill = TRUE)  
  }
  big_data_N <- rbind(big_data_N, remaining_road_segs, fill = TRUE)  # 95% thing
  final_df <- big_data_N %>% 
    dplyr::select(everything(), - magic_no, - num_obs) %>% 
    distinct(across(everything())) %>% 
    pivot_longer(-c(UID), names_to = "ride", values_to = "value") %>% 
    distinct(across(everything())) %>% 
    filter(!is.na(value)) %>% 
    dplyr::select(everything(), - ride) %>% 
    group_by(UID) %>% 
    summarise(median = median(value, na.rm = T))
  write.csv(final_df, here("road_type_sb/CPC/Residential/", 
                           paste0(no_times, "_", draw, ".csv")))
  draw <- draw + 1
}
beepr::beep()
