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

############################### MAL ###########################################

q3 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/All_rides_MAL1_30m.csv", sep = ",", 
            header = TRUE)
q3$Road_ID <- paste0("MAL1_", q3$Road_ID)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")
q4 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/All_rides_MAL2_30m.csv", sep = ",", 
            header = TRUE)
q4$Road_ID <- paste0("MAL2_", q4$Road_ID)


q <- rbind(q3, q4, fill = TRUE)


loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC$BC_LC <- log(loop_BC_LC$BC_LC)
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_type, data = loop_BC_LC)
summary(res.aov)
TukeyHSD(res.aov)

loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c$CO2_c <- as.numeric(as.character(log(loop_CO2_c$CO2_c)))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_type, data = loop_CO2_c)
summary(res.aov)
TukeyHSD(res.aov)

loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC$CPC <- log(loop_CPC$CPC)
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_type, data = loop_CPC)
summary(res.aov)
TukeyHSD(res.aov)



############################### without log MAL ###############################

q3 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/All_rides_MAL1_30m.csv", sep = ",", 
            header = TRUE)
q3$Road_ID <- paste0("MAL1_", q3$Road_ID)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")
q4 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/All_rides_MAL2_30m.csv", sep = ",", 
            header = TRUE)
q4$Road_ID <- paste0("MAL2_", q4$Road_ID)


q <- rbind(q3, q4, fill = TRUE)
loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
loop_BC_LC_sum <- loop_BC_LC %>%
  group_by(Road_ID, Road_type) %>%
  summarise(median_BC_LC = median(BC_LC, na.rm = TRUE))
res.aov <- aov(median_BC_LC ~ Road_type, data = loop_BC_LC_sum)
summary(res.aov)
TukeyHSD(res.aov)

loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
loop_CO2_c_sum <- loop_CO2_c %>%
  group_by(Road_ID, Road_type) %>%
  summarise(median_CO2_c = median(CO2_c, na.rm = TRUE))
res.aov1 <- aov(median_CO2_c ~ Road_type, data = loop_CO2_c_sum)
summary(res.aov1)
TukeyHSD(res.aov1)

loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
loop_CPC_sum <- loop_CPC %>%
  group_by(Road_ID, Road_type) %>%
  summarise(median_CPC = median(CPC, na.rm = TRUE))
res.aov2 <- aov(median_CPC ~ Road_type, data = loop_CPC_sum)
summary(res.aov2)
TukeyHSD(res.aov2)


