source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")

MAL1 <- st_read("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL1_F_Road_type.shp")
MAL2 <- st_read("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL2_F_Road_type.shp")

len_MAL1 <- sum(st_length(MAL1))
len_MAL2 <- sum(st_length(MAL2))

MAL1_R <- MAL1 %>%
  dplyr::select(Road_type) 

MAL2_R <- MAL2 %>%
  dplyr::select(Road_type) 

MAL_R <- rbind(MAL1_R, MAL2_R)

MAL_R %>%    
  group_by(Road_type) %>%
  summarise(n())
MAL_R %>%    
  group_by(Road_type) %>%
  summarise((n() / 2142) * 100)

MAL1_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_sec.csv", sep = ",", 
                     header = TRUE) %>%
  mutate(Area = "MAL1")
MAL2_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_sec.csv", sep = ",", 
                     header = TRUE) %>%
  mutate(Area = "MAL2") 

Final_sec <- rbind(MAL1_sec, MAL2_sec)
Final_sec1 <- Final_sec %>%
  mutate(Area = "MAL")
Final_summ <- Final_sec1 %>% 
  group_by(Area) %>% 
  summarise(non_na_BC = sum(!is.na(BC)), 
            non_na_BC_NR = sum(!is.na(BC_NR)),
            non_na_BC_NR_LC = sum(!is.na(BC_NR_LC)))

# ((non_na_BC - non_na_BC_NR_LC) / non_na_BC) * 100

mal1 <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Final_layer.csv", sep = ",") %>%
  dplyr::select(UID, Road_type, name_x, Highway_US, PM2_5_md, BC_LC_md, CPC_md, RH_md, CO2_c_md)
mal2 <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layer.csv", sep = ",") %>%
  dplyr::select(UID, Road_type, name_x, Highway_US, PM2_5_md, BC_LC_md, CPC_md, RH_md, CO2_c_md)

mal <- rbind(mal1, mal2)

mal$Area <- "MAL"

mal_sumry <- mal %>%
  dplyr::select(Area, BC_LC_md, CPC_md, RH_md, CO2_c_md) %>%
  group_by(Area) %>%
  summarise_all(funs(mean, median), na.rm = TRUE)
    
  

MAL1 <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Final_layer.csv", sep = ",") 
MAL2 <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layer.csv", sep = ",")

MAL <- rbind(MAL1, MAL2)

MAL_gsd <- MAL %>%
  dplyr::select(Road_type, contains(c("BC_LC_", "CPC_", "CO2_c_"))) %>%
  dplyr::select(Road_type, contains("GS")) %>%
  group_by(Road_type) %>%
  summarise_all(funs(mean), na.rm = TRUE)

