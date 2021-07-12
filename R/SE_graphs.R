source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
set.seed(108)
library(here)

################################ CO2 analysis #################################3

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/")  
CBD_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CO2_median_drive_pass_means.csv", sep = ",", 
                 header = TRUE) %>%
  mutate(Area = "CBD")
KAN_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CO2_median_drive_pass_means.csv", sep = ",", 
                 header = TRUE) %>%
  mutate(Area = "KAN")
MAL1_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CO2_median_drive_pass_means.csv", sep = ",", 
                  header = TRUE) %>%
  mutate(Area = "MAL1")
MAL2_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_median_drive_pass_means.csv", sep = ",", 
                  header = TRUE) %>%
  mutate(Area = "MAL2")
Sec <- rbind(CBD_sec, KAN_sec, MAL1_sec, MAL2_sec)
Sec <- Sec %>%
  dplyr::select(UID, Area, No_of_observations, Median_N_Observations, Stddev_M_subsamples,
                StdError_N_Observations, Lower_25_M_subsamples, Upper_975_M_subsamples) %>%
  mutate(SE_median = Stddev_M_subsamples / Median_N_Observations)
MAL1_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL1_F_Road_type.shp", 
                    layer = "MAL1_F_Road_type")
MAL1_Ref <- spTransform(MAL1_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
MAL2_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL2_F_Road_type.shp", 
                    layer = "MAL2_F_Road_type")
MAL2_Ref <- spTransform(MAL2_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
KAN_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/KAN_F_Road_type.shp", 
                   layer = "KAN_F_Road_type")
KAN_Ref <- spTransform(KAN_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
CBD_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/CBD_F_Road_type.shp", 
                   layer = "CBD_F_Road_type")
CBD_Ref <- spTransform(CBD_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))

Ref <- bind(MAL1_Ref, MAL2_Ref, KAN_Ref, CBD_Ref)

Ref <- left_join(st_as_sf(Ref), Sec, by = "UID")
Ref_back <- Ref
Ref1 <- Ref
Ref1$Area <- "All"
Ref2 <- rbind(Ref, Ref1)
Ref2 <- Ref2 %>%
  group_by(Area, Road_type) %>%
  mutate(name = sum(!is.na(Road_type)))
Ref2_sum <- Ref2 %>%
  dplyr::select(Area, Road_type, SE_median) %>%
  group_by(Area, Road_type) %>%
  summarise_all(funs(Median = median, p10 = quantile(., .1), 
                     p90 = quantile(., .9)), na.rm = TRUE)
Ref1 <- data.frame(Ref1)
Ref2_sum1 <- Ref1 %>%
  dplyr::select(Area, SE_median) %>%
  group_by(Area) %>%
  summarise_all(funs(Median = median, p10 = quantile(., .1), 
                     p90 = quantile(., .9)), na.rm = TRUE)
Ref2 <- data.frame(Ref2)
ggplot(Ref2, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste(CO[2], " SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold")) +
  scale_y_continuous() + scale_x_continuous(limits = c(0, 1.25), breaks = c (0, 0.5, 1)) +
  geom_text(aes(label = paste0("n = ", name)), x = 1.0, y = 900, colour = "black", size = 4)


Ref <- Ref %>%
  group_by(Area, Road_type) %>%
  mutate(name = sum(!is.na(Road_type)))
Ref <- data.frame(Ref)
ggplot(Ref, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste(CO[2], " SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black",  face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold")) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 1, y = 400, colour = "black", size = 4)

setwd("D:/Dropbox/ILKConsultancy/MM_paper")
Ref <- Ref %>%
  group_by(Area, Road_type) %>%
  filter(Area != "KAN") %>%
  filter(Area != "CBD") %>%
  mutate(Area = "MAL") %>%
  mutate(name = sum(!is.na(Road_type)))
Ref <- data.frame(Ref)
ggplot(Ref, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste(CO[2], " SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black",  face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text.x = element_blank()) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 0.6, y = 100, colour = "black", size = 6)
ggsave(here("Plots", "SE_CO2_MAL.jpg"), width = 30, height = 20, units = "cm")


Ref <- Ref_back %>%
  group_by(Area, Road_type) %>%
  filter(Area == "KAN") %>%
  mutate(name = sum(!is.na(Road_type)))
Ref <- data.frame(Ref)
ggplot(Ref, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste(CO[2], " SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black",  face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text.x = element_blank()) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 1, y = 400, colour = "black", size = 6)
ggsave(here("Plots", "SE_CO2_KAN.jpg"), width = 30, height = 20, units = "cm")


Ref <- Ref_back %>%
  group_by(Area, Road_type) %>%
  filter(Area == "CBD") %>%
  mutate(name = sum(!is.na(Road_type)))
Ref <- data.frame(Ref)
ggplot(Ref, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste(CO[2], " SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black",  face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text.x = element_blank()) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 0.4, y = 40, colour = "black", size = 6)
ggsave(here("Plots", "SE_CO2_CBD.jpg"), width = 30, height = 20, units = "cm")



############################### BC Analysis ####################################

source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
set.seed(108)
library(here)

################################ BC_LC analysis #################################3

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/")  
CBD_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_LC_median_drive_pass_means.csv", sep = ",", 
                 header = TRUE) %>%
  mutate(Area = "CBD")
KAN_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_LC_median_drive_pass_means.csv", sep = ",", 
                 header = TRUE) %>%
  mutate(Area = "KAN")
MAL1_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_BC_LC_median_drive_pass_means.csv", sep = ",", 
                  header = TRUE) %>%
  mutate(Area = "MAL1")
MAL2_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_LC_median_drive_pass_means.csv", sep = ",", 
                  header = TRUE) %>%
  mutate(Area = "MAL2")
Sec <- rbind(CBD_sec, KAN_sec, MAL1_sec, MAL2_sec)
Sec <- Sec %>%
  dplyr::select(UID, Area, No_of_observations, Median_N_Observations, Stddev_M_subsamples,
                StdError_N_Observations, Lower_25_M_subsamples, Upper_975_M_subsamples) %>%
  mutate(SE_median = Stddev_M_subsamples / Median_N_Observations)
MAL1_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL1_F_Road_type.shp", 
                    layer = "MAL1_F_Road_type")
MAL1_Ref <- spTransform(MAL1_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
MAL2_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL2_F_Road_type.shp", 
                    layer = "MAL2_F_Road_type")
MAL2_Ref <- spTransform(MAL2_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
KAN_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/KAN_F_Road_type.shp", 
                   layer = "KAN_F_Road_type")
KAN_Ref <- spTransform(KAN_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
CBD_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/CBD_F_Road_type.shp", 
                   layer = "CBD_F_Road_type")
CBD_Ref <- spTransform(CBD_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
Ref <- bind(MAL1_Ref, MAL2_Ref, KAN_Ref, CBD_Ref)

Ref <- left_join(st_as_sf(Ref), Sec, by = "UID")
Ref_back <- Ref
Ref1 <- Ref
Ref1$Area <- "All"
Ref2 <- rbind(Ref, Ref1)
Ref2 <- Ref2 %>%
  group_by(Area, Road_type) %>%
  mutate(name = sum(!is.na(Road_type)))
Ref2_sum <- Ref2 %>%
  dplyr::select(Area, Road_type, SE_median) %>%
  group_by(Area, Road_type) %>%
  summarise_all(funs(Median = median, p10 = quantile(., .1), 
                     p90 = quantile(., .9)), na.rm = TRUE)
Ref1 <- data.frame(Ref1)
Ref2_sum1 <- Ref1 %>%
  dplyr::select(Area, SE_median) %>%
  group_by(Area) %>%
  summarise_all(funs(Median = median, p10 = quantile(., .1), 
                     p90 = quantile(., .9)), na.rm = TRUE)
Ref2 <- data.frame(Ref2)
ggplot(Ref2, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste("BC SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold")) +
  scale_y_continuous() + scale_x_continuous(limits = c(0, 1.25), breaks = c (0, 0.5, 1)) +
  geom_text(aes(label = paste0("n = ", name)), x = 1.0, y = 900, colour = "black", size = 4)


Ref <- Ref %>%
  group_by(Area, Road_type) %>%
  mutate(name = sum(!is.na(Road_type)))
Ref <- data.frame(Ref)
ggplot(Ref, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste("BC SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black",  face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold")) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 1, y = 400, colour = "black", size = 4)

setwd("D:/Dropbox/ILKConsultancy/MM_paper")
Ref <- Ref %>%
  group_by(Area, Road_type) %>%
  filter(Area != "KAN") %>%
  filter(Area != "CBD") %>%
  mutate(Area = "MAL") %>%
  mutate(name = sum(!is.na(Road_type)))
Ref <- data.frame(Ref)
ggplot(Ref, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste("BC SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black",  face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text.x = element_blank()) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 1.25, y = 300, colour = "black", size = 6)
ggsave(here("Plots", "SE_BC_LC_MAL.jpg"), width = 30, height = 20, units = "cm")


Ref <- Ref_back %>%
  group_by(Area, Road_type) %>%
  filter(Area == "KAN") %>%
  mutate(name = sum(!is.na(Road_type)))
Ref <- data.frame(Ref)
ggplot(Ref, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste("BC SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black",  face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text.x = element_blank()) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 1.5, y = 300, colour = "black", size = 6)
ggsave(here("Plots", "SE_BC_LC_KAN.jpg"), width = 30, height = 20, units = "cm")


Ref <- Ref_back %>%
  group_by(Area, Road_type) %>%
  filter(Area == "CBD") %>%
  mutate(name = sum(!is.na(Road_type)))
Ref <- data.frame(Ref)
ggplot(Ref, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste("BC SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black",  face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text.x = element_blank()) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 0.75, y = 75, colour = "black", size = 6)
ggsave(here("Plots", "SE_BC_LC_CBD.jpg"), width = 30, height = 20, units = "cm")



################################ CPC analysis #################################3

source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
set.seed(108)
library(here)


setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/")  
CBD_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CPC_median_drive_pass_means.csv", sep = ",", 
                 header = TRUE) %>%
  mutate(Area = "CBD")
KAN_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CPC_median_drive_pass_means.csv", sep = ",", 
                 header = TRUE) %>%
  mutate(Area = "KAN")
MAL1_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CPC_median_drive_pass_means.csv", sep = ",", 
                  header = TRUE) %>%
  mutate(Area = "MAL1")
MAL2_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_median_drive_pass_means.csv", sep = ",", 
                  header = TRUE) %>%
  mutate(Area = "MAL2")
Sec <- rbind(CBD_sec, KAN_sec, MAL1_sec, MAL2_sec)
Sec <- Sec %>%
  dplyr::select(UID, Area, No_of_observations, Median_N_Observations, Stddev_M_subsamples,
                StdError_N_Observations, Lower_25_M_subsamples, Upper_975_M_subsamples) %>%
  mutate(SE_median = Stddev_M_subsamples / Median_N_Observations)
MAL1_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL1_F_Road_type.shp", 
                    layer = "MAL1_F_Road_type")
MAL1_Ref <- spTransform(MAL1_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
MAL2_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL2_F_Road_type.shp", 
                    layer = "MAL2_F_Road_type")
MAL2_Ref <- spTransform(MAL2_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
KAN_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/KAN_F_Road_type.shp", 
                   layer = "KAN_F_Road_type")
KAN_Ref <- spTransform(KAN_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
CBD_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/CBD_F_Road_type.shp", 
                   layer = "CBD_F_Road_type")
CBD_Ref <- spTransform(CBD_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
Ref <- bind(MAL1_Ref, MAL2_Ref, KAN_Ref, CBD_Ref)

Ref <- left_join(st_as_sf(Ref), Sec, by = "UID")
Ref_back <- Ref
Ref1 <- Ref
Ref1$Area <- "All"
Ref2 <- rbind(Ref, Ref1)
Ref2 <- Ref2 %>%
  group_by(Area, Road_type) %>%
  mutate(name = sum(!is.na(Road_type)))
Ref2_sum <- Ref2 %>%
  dplyr::select(Area, Road_type, SE_median) %>%
  group_by(Area, Road_type) %>%
  summarise_all(funs(Median = median, p10 = quantile(., .1), 
                     p90 = quantile(., .9)), na.rm = TRUE)
Ref1 <- data.frame(Ref1)
Ref2_sum1 <- Ref1 %>%
  dplyr::select(Area, SE_median) %>%
  group_by(Area) %>%
  summarise_all(funs(Median = median, p10 = quantile(., .1), 
                     p90 = quantile(., .9)), na.rm = TRUE)
Ref2 <- data.frame(Ref2)
ggplot(Ref2, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste("UFPs SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold")) +
  scale_y_continuous() + scale_x_continuous(limits = c(0, 1.25), breaks = c (0, 0.5, 1)) +
  geom_text(aes(label = paste0("n = ", name)), x = 1.0, y = 900, colour = "black", size = 4)


Ref <- Ref %>%
  group_by(Area, Road_type) %>%
  mutate(name = sum(!is.na(Road_type)))
Ref <- data.frame(Ref)
ggplot(Ref, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste("UFPs SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black",  face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold")) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 1, y = 400, colour = "black", size = 4)

setwd("D:/Dropbox/ILKConsultancy/MM_paper")
Ref <- Ref %>%
  group_by(Area, Road_type) %>%
  filter(Area != "KAN") %>%
  filter(Area != "CBD") %>%
  mutate(Area = "MAL") %>%
  mutate(name = sum(!is.na(Road_type)))
Ref <- data.frame(Ref)
ggplot(Ref, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste("UFPs SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black",  face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text.x = element_blank()) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 0.75, y = 150, colour = "black", size = 6)
ggsave(here("Plots", "SE_CPC_MAL.jpg"), width = 30, height = 20, units = "cm")


Ref <- Ref_back %>%
  group_by(Area, Road_type) %>%
  filter(Area == "KAN") %>%
  mutate(name = sum(!is.na(Road_type)))
Ref <- data.frame(Ref)
ggplot(Ref, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste("UFPs SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black",  face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text.x = element_blank()) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 0.6, y = 200, colour = "black", size = 6)
ggsave(here("Plots", "SE_CPC_KAN.jpg"), width = 30, height = 20, units = "cm")


Ref <- Ref_back %>%
  group_by(Area, Road_type) %>%
  filter(Area == "CBD") %>%
  mutate(name = sum(!is.na(Road_type)))
Ref <- data.frame(Ref)
ggplot(Ref, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = expression(paste("UFPs SE of the  medians / Median (using the drive pass means)"))) +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black",  face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text.x = element_blank()) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 0.75, y = 100, colour = "black", size = 6)
ggsave(here("Plots", "SE_CPC_CBD.jpg"), width = 30, height = 20, units = "cm")