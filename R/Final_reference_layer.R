source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")

library(extrafont)
extrafont::loadfonts(device = "win")
library(DescTools)


setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/Final_layers")

shapefile_roads <- st_read("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/CBD_F_Road_type.shp")
shapefile_roads <- st_transform(shapefile_roads, CRS("+proj=utm +zone=43 ellps=WGS84"))
q <- st_as_sf(shapefile_roads)
loop <- q %>%
  dplyr::select(Road_ID, UID, Road_type) %>%
  mutate_at(c('Road_ID'), as.numeric)
loop$geometry <- NULL
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/Final_layers"
fi <- list.files(dir, pattern = "\\.shp$")

for (fo in (fi)) {
  my_spdf <- st_read(fo)
  my_spdf <- st_transform(my_spdf, crs = "+proj=utm +zone=43 ellps=WGS84")
  r <- st_as_sf(my_spdf)
  w <-  str_replace_all(substr(fo, 1, 18), "_", "-") 
  BC_name <- paste0(w, "_BC")
  BC_NR_name <- paste0(w, "_BC_NR")
  BC_NR_LC_name <- paste0(w, "_BC_LC")
  BC_c_name <- paste0(w, "_BC_c")
  BC_CF_name <- paste0(w, "_BC_CF")
  CO2_name <- paste0(w, "_CO2")
  CO2_c_name <- paste0(w, "_CO2_c")
  CPC_name <- paste0(w, "_CPC")
  Spd_name <- paste0(w, "_Spd")
  RH_name <- paste0(w, "_RH")
  PM_CF_name <- paste0(w, "_PM_CF")
  PM2_5_name <- paste0(w, "_PM2_5")
  PM_c_name <- paste0(w, "_PM_c")
  r$geometry <- NULL
  addr <- r %>%
    mutate(Sped_mn = Sped_mn * 3.6) %>%
    dplyr::select(Road_ID, UID, Rod_typ, BC_mn, BC_NR_mn, BC_NR_L_mn, BC_c_mn, BC_CF_GM, CO2_mn, 
                  CO2_c_mn, CPC_mn, Sped_mn, PM2_5_mn, PM_c_mn, PM_CF_mn, RH_mn) %>%
    mutate_at(c('Road_ID'), as.numeric)
  # addr <- r %>%
  #   select(Road_ID, UID, Road_type,BC_md, BC_NR_md, BC_NR_L_md, BC_c_md, BC_CF_md, CO2_md, 
  #          CO2_c_md, CPC_md, Sped_md, PM2_5_md, PM_c_md, PM_CF_md, RH_md) %>%
  #   mutate_at(c('Road_ID'), as.numeric)
  names(addr) <- c("Road_ID", "UID", "Road_type", BC_name, BC_NR_name, BC_NR_LC_name, BC_c_name, 
                   BC_CF_name, CO2_name, CO2_c_name, CPC_name, Spd_name, PM2_5_name,
                   PM_c_name,  PM_CF_name, RH_name)
  loop <- left_join(loop, addr, by = c("Road_ID" = "Road_ID", "UID" = "UID", 
                                       "Road_type" = "Road_type"))
}
write.csv(loop, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/All_rides_CBD_30m.csv")
q <- q %>%
  mutate_at(c('Road_ID'), as.numeric)
setDT(q)
setkey(q, Road_ID)
loop_BC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_NR <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_NR")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_NR") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_NR_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_c") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_CF <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_CF")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_CF") %>%
  mutate(date = substr(date, 1, 18))
loop_RH <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_RH")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "RH") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2 <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_PM_CF <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_PM_CF")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "PM_CF") %>%
  mutate(date = substr(date, 1, 18))
loop_PM_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_PM_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "PM_c") %>%
  mutate(date = substr(date, 1, 18))
loop_PM2_5 <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_PM2_5")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "PM2_5") %>%
  mutate(date = substr(date, 1, 18))
loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18))
loop_Spd <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_Spd")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "Spd") %>%
  mutate(date = substr(date, 1, 18))


Final <- list(loop_BC, loop_BC_NR, loop_BC_NR_LC, loop_BC_c, loop_BC_CF, 
              loop_PM_CF, loop_Spd, loop_CPC, loop_PM2_5, loop_PM_c,
              loop_CO2, loop_CO2_c, loop_RH) %>% 
  reduce(full_join, by = c("date" = "date", "Road_ID" = "Road_ID", 
                           "Road_type" = "Road_type")) %>%
  dplyr::select(- ends_with("date"))
# BC_c Diurnal factor corrected 

Final_stats_layer <- Final %>%
  group_by(Road_ID, Road_type) %>%
  summarise_all(funs(ma = max, mi = min, mn = mean, md = median, sd, GSD = GSD1, 
                     q10 = quantile(., .1), q25 = quantile(., .25), CV = CV1, GM = GM1,
                     q75 = quantile(., .75), q90 = quantile(., .9), se = stderr, 
                     n = sum(!is.na(.))), na.rm = TRUE) %>%
  mutate(BC_CO_c = BC_c_md / CO2_c_md, # previously calculated correction factor
         CPC_CO_c = CPC_md / CO2_c_md, 
         BC_CO2 = BC_LC_md / CO2_c_md, # no correction factor
         BC_CO2_f = (BC_LC_md * BC_CF_md) / CO2_c_md) # recently after GM calculated correction factor


setDT(Final_stats_layer)
setkey(Final_stats_layer, Road_ID)
layer_final_df <- Final_stats_layer[q]
layer_final_df$UID <- paste0("CBD_", layer_final_df$Road_ID)
write.csv(layer_final_df, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/Final_layer.csv")
# layer_final <- st_as_sf(layer_final_df) 
# layer_final <- as(layer_final, "Spatial")
# dsn <- "D:/Dropbox/APMfull/MAL_CNG_Paper/Final_layers"
# layer <- "Corrected_Final_CBD_Layer"
# writeOGR(layer_final, dsn, layer, driver = "ESRI Shapefile", overwrite_layer = T)


# windowsFonts(Times = windowsFont("Comic Sans MS"))
# theme_ARU <- list(theme_minimal(),
#                   labs(x = "Road Segment ID"),
#                   geom_errorbar(size = 0.05, colour = "#DCDCDC"),
#                   geom_point(size = 0.05),
#                   theme(legend.text = element_text(size = 18),
#                         text = element_text(family = 'Times'),
#                         plot.title = element_text(size = 18, face = "bold"),
#                         plot.subtitle = element_text(size = 13),
#                         axis.title = element_text(size = 18),
#                         axis.text = element_text(size = 16, face = "bold"),
#                         panel.border = element_rect(colour = "black",
#                                                     fill = NA, size = 1.2)))

# icc_BC <- loop %>%
#   select("Road_ID",  ends_with("_BC_c_md")) %>% 
#   pivot_longer(!Road_ID, names_to = "Ride_day", values_to = "BC") %>% 
#   mutate(Ride_day = as.factor(Ride_day)) %>% 
#   group_by(Road_ID) %>% 
#   summarise(mean_bc = mean(BC, na.rm = TRUE),
#             sd_bc = sd(BC, na.rm = TRUE)) %>% 
#   ggplot(aes(x = Road_ID, y = mean_bc,
#              ymin = mean_bc - 1.96 * (sd_bc / sqrt(nrow(loop))),
#              ymax = mean_bc + 1.96 * (sd_bc / sqrt(nrow(loop))))) +
#   theme_ARU +
#   labs(y = expression(paste("BC" ," (", mu, "g",~m^{-3}, ")")),
#        title = "Mean BC at each road segment in CBD over 22 rides",
#        subtitle = paste0("n = ", nrow(loop), " road segments, Error bars = 95% CI"))

beepr::beep()


mal1 <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Final_layer.csv", sep = ",") %>%
  select(UID, Road_type, name_x, Highway_US, PM2_5_md, BC_c_md, CPC_md, RH_md, CO2_c_md)
mal2 <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layer.csv", sep = ",") %>%
  select(UID, Road_type, name_x, Highway_US, PM2_5_md, BC_c_md, CPC_md, RH_md, CO2_c_md)
cbd <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/Final_layer.csv", sep = ",") %>%
  select(UID, Road_type, name_x, Highway_US, PM2_5_md, BC_c_md, CPC_md, RH_md, CO2_c_md)
kan <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/Final_layer.csv", sep = ",") %>%
  select(UID, Road_type, name_x, Highway_US, PM2_5_md, BC_c_md, CPC_md, RH_md, CO2_c_md)

all <- rbind(mal1, mal2, cbd, kan)
names(all) <- c("UID", "Road_type", "Road_name", "Highway_US", "PM2.5", "BC", "UFPs", "RH", "CO2")
write.csv(all, "D:/Dropbox/APMfull/MAL_CNG_Paper/Final_layer.csv")

all <- rbind(mal1, mal2)
names(all) <- c("UID", "Road_type", "Road_name", "Highway_US", "PM2.5", "BC", "UFPs", "RH", "CO2")
write.csv(all, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Final_layer.csv")
