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

bc_final <- bc %>% 
  pivot_longer(-c(Rod_typ, UID, Hghw_US), names_to = "ride", values_to = "mean_values") %>% 
  dplyr::select(everything(), -ride) %>% 
  group_by(Rod_typ, UID, Hghw_US) %>% 
  summarise_all(list(~ mean(., na.rm = T), 
                     ~ median(., na.rm = T),
                     ~ sd(., na.rm = T),
                     ~ sum(!is.na(.)),
                     ~ max(., na.rm = T), 
                     ~ min(., na.rm = T)))

cpc_final <- cpc %>% 
  pivot_longer(-c(Rod_typ, UID, Hghw_US), names_to = "ride", values_to = "mean_values") %>% 
  dplyr::select(everything(), -ride) %>% 
  group_by(Rod_typ, UID, Hghw_US) %>% 
  summarise_all(list(~ mean(., na.rm = T), 
                     ~ median(., na.rm = T),
                     ~ sd(., na.rm = T),
                     ~ sum(!is.na(.)),
                     ~ max(., na.rm = T), 
                     ~ min(., na.rm = T)))
write.csv(bc_final, here("road_type_sb/BC/", 
                         "BC_final_layer.csv"))
write.csv(cpc_final, here("road_type_sb/CPC/", 
                          "CPC_final_layer.csv"))

R2_NRMSE_function <- function(path, each_file, ref_layer, la, fi) {
  ref_layer <- ref_layer %>%
    ungroup() %>%
    dplyr::select(UID, "Final_Median" = fi) 
  row_no_ref <- as.numeric(as.character(nrow(ref_layer)))
  individual_file <- read_csv(here(path, each_file)) %>% 
    ungroup() %>%
    dplyr::select(UID, "N_Median" = la)
  final_layer <- left_join(ref_layer, individual_file, by = "UID") 
  sum <- summary(lm(Final_Median ~ N_Median, data = final_layer))
  R_squared <- sum$r.squared
  sum_f <- sum$fstatistic[[1]]
  final_layer$rmsq <- (final_layer$N_Median - final_layer$Final_Median) ^ 2
  rms   <- (sum(final_layer$rmsq, na.rm = TRUE)) / row_no_ref  
  rms1  <- (mean(final_layer$rmsq, na.rm = TRUE))  
  rmse  <- sqrt(rms)
  rmse1 <- sqrt(rms1)
  y_bar  <- (sum(final_layer$N_Median, na.rm = TRUE)) / row_no_ref  
  y_bar1 <- (mean(final_layer$N_Median, na.rm = TRUE))  
  y_bar2 <- (mean(final_layer$Final_Median, na.rm = TRUE)) 
  nrmse_mean_not_sum <- rmse1 / y_bar2
  y_max <- (max(final_layer$N_Median, na.rm = TRUE))
  y_min <- (min(final_layer$N_Median, na.rm = TRUE))
  rang  <- y_max - y_min
  N <- gsub(".csv", "", each_file)
  layer_N <- gsub("_", "", substr(each_file, 1, 2))
  layer_N <- as.numeric(as.character(layer_N))
  new_df <- data.frame(N, layer_N, y_bar, y_bar1, rmse, rmse1, rang, 
                       R_squared, y_min, y_max, sum_f, nrmse_mean_not_sum, y_bar2)
}
theme_MC <- list(stat_smooth(colour = "red", size = 2),  
                 theme_classic(), theme(legend.text = element_text(size = 32),
                                        axis.ticks = element_line(size = 2),
                                        axis.ticks.length = unit(.3, "cm"),
                                        plot.title = element_text(size = 44, face = "bold"), 
                                        axis.title = element_text(size = 44, colour = "black", face = "bold"),
                                        axis.text = element_text(size = 40, colour = "black", face = "bold"), 
                                        panel.border = element_rect(colour = "black", fill = NA, size = 2)))
label_at <- function(n) function(x) ifelse(x %% n == 0, x, "")


sel_road_type <- "Highway"
new_path <- here("road_type_sb/CPC/", sel_road_type)
list_files <- list.files(path = new_path)
df <- data.frame()
for (each_file in (list_files)) {
  new_df <- R2_NRMSE_function(new_path, each_file, subset(cpc_final,
                                                          Rod_typ == sel_road_type), 
                              "median", "median")
  df <- rbind(df, new_df) 
} 
write.csv(df, paste0("CPC_", sel_road_type, "_R2_NRMSE.csv"))

names(df) <- c("Layer", "Numeric Layer", "Y Mean Using Sum", "Y Mean using Mean",
               "RMSE using Sum", "RMSE using Mean", "Ymax-Ymin", "R Squared", 
               "Y min", "Y max", "F_statistic", "NRMSE", "Reference_mean")

plo1 <- ggplot(df, aes(x = as.numeric(as.character(`Numeric Layer`)), 
                              y = as.numeric(as.character(`R Squared`)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 20)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC
plo1
ggsave(here("Plots", paste0("CPC_", sel_road_type, "_R2_smooth.jpg")), 
       width = 30, height = 20, units = "cm")

plo1 <- ggplot(df, aes(x = as.numeric(as.character(`Numeric Layer`)), 
                              y = as.numeric(as.character(`R Squared`)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = label_at(0.5)) + 
  scale_x_continuous(limits = c(0, 20)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC
plo1
ggsave(here("Plots", paste0("CPC_", sel_road_type, "_R2.jpg")), 
       width = 30, height = 20, units = "cm")

plo1 <- ggplot(df, aes(x = as.numeric(as.character(`Numeric Layer`)), 
                             y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_continuous(limits = c(0, 20)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC
plo1
ggsave(here("Plots", paste0("CPC_", sel_road_type, "_NRMSE.jpg")), 
       width = 30, height = 20, units = "cm")

plo1 <- ggplot(df, aes(x = as.numeric(as.character(`Numeric Layer`)), 
                             y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 20)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC
plo1
ggsave(here("Plots", paste0("CPC_", sel_road_type, "_NRMSE_smooth.jpg")), 
       width = 30, height = 20, units = "cm")

sel_road_type <- "Arterial"
new_path <- here("road_type_sb/CPC/", sel_road_type)
list_files <- list.files(path = new_path)
df <- data.frame()
for (each_file in (list_files)) {
  new_df <- R2_NRMSE_function(new_path, each_file, subset(cpc_final,
                                                          Rod_typ == sel_road_type), 
                              "median", "median")
  df <- rbind(df, new_df) 
} 
write.csv(df, paste0("CPC_", sel_road_type, "_R2_NRMSE.csv"))

names(df) <- c("Layer", "Numeric Layer", "Y Mean Using Sum", "Y Mean using Mean",
               "RMSE using Sum", "RMSE using Mean", "Ymax-Ymin", "R Squared", 
               "Y min", "Y max", "F_statistic", "NRMSE", "Reference_mean")

plo1 <- ggplot(df, aes(x = as.numeric(as.character(`Numeric Layer`)), 
                       y = as.numeric(as.character(`R Squared`)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 20)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC
plo1
ggsave(here("Plots", paste0("CPC_", sel_road_type, "_R2_smooth.jpg")), 
       width = 30, height = 20, units = "cm")

plo1 <- ggplot(df, aes(x = as.numeric(as.character(`Numeric Layer`)), 
                       y = as.numeric(as.character(`R Squared`)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = label_at(0.5)) + 
  scale_x_continuous(limits = c(0, 20)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC
plo1
ggsave(here("Plots", paste0("CPC_", sel_road_type, "_R2.jpg")), 
       width = 30, height = 20, units = "cm")

plo1 <- ggplot(df, aes(x = as.numeric(as.character(`Numeric Layer`)), 
                       y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_continuous(limits = c(0, 20)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC
plo1
ggsave(here("Plots", paste0("CPC_", sel_road_type, "_NRMSE.jpg")), 
       width = 30, height = 20, units = "cm")

plo1 <- ggplot(df, aes(x = as.numeric(as.character(`Numeric Layer`)), 
                       y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 20)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC
plo1
ggsave(here("Plots", paste0("CPC_", sel_road_type, "_NRMSE_smooth.jpg")), 
       width = 30, height = 20, units = "cm")

sel_road_type <- "Residential"
new_path <- here("road_type_sb/CPC/", sel_road_type)
list_files <- list.files(path = new_path)
df <- data.frame()
for (each_file in (list_files)) {
  new_df <- R2_NRMSE_function(new_path, each_file, subset(cpc_final,
                                                          Rod_typ == sel_road_type), 
                              "median", "median")
  df <- rbind(df, new_df) 
} 
write.csv(df, paste0("CPC_", sel_road_type, "_R2_NRMSE.csv"))

names(df) <- c("Layer", "Numeric Layer", "Y Mean Using Sum", "Y Mean using Mean",
               "RMSE using Sum", "RMSE using Mean", "Ymax-Ymin", "R Squared", 
               "Y min", "Y max", "F_statistic", "NRMSE", "Reference_mean")

plo1 <- ggplot(df, aes(x = as.numeric(as.character(`Numeric Layer`)), 
                       y = as.numeric(as.character(`R Squared`)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 20)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC
plo1
ggsave(here("Plots", paste0("CPC_", sel_road_type, "_R2_smooth.jpg")), 
       width = 30, height = 20, units = "cm")

plo1 <- ggplot(df, aes(x = as.numeric(as.character(`Numeric Layer`)), 
                       y = as.numeric(as.character(`R Squared`)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = label_at(0.5)) + 
  scale_x_continuous(limits = c(0, 20)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC
plo1
ggsave(here("Plots", paste0("CPC_", sel_road_type, "_R2.jpg")), 
       width = 30, height = 20, units = "cm")

plo1 <- ggplot(df, aes(x = as.numeric(as.character(`Numeric Layer`)), 
                       y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_continuous(limits = c(0, 20)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC
plo1
ggsave(here("Plots", paste0("CPC_", sel_road_type, "_NRMSE.jpg")), 
       width = 30, height = 20, units = "cm")

plo1 <- ggplot(df, aes(x = as.numeric(as.character(`Numeric Layer`)), 
                       y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 20)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC
plo1
ggsave(here("Plots", paste0("CPC_", sel_road_type, "_NRMSE_smooth.jpg")), 
       width = 30, height = 20, units = "cm")


