source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
set.seed(108)

############################# MAL CO2 analysis #################################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/")  
MAL1_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CO2_median_drive_pass_means.csv", sep = ",", 
                  header = TRUE) %>%
  mutate(Area = "MAL1")
MAL2_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_median_drive_pass_means.csv", sep = ",", 
                  header = TRUE) %>%
  mutate(Area = "MAL2")

Sec <- rbind(MAL1_sec, MAL2_sec)
Sec <- Sec %>%
  dplyr::select(UID, Area, No_of_observations, Median_N_Observations, Stddev_M_subsamples,
                Median_M_subsamples, StdError_N_Observations, Lower_25_M_subsamples, 
                Upper_975_M_subsamples, Mean_M_subsamples) %>%
  mutate(SE_median = Stddev_M_subsamples / Median_N_Observations)
MAL1_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL1_F_Road_type.shp", 
                    layer = "MAL1_F_Road_type")
MAL1_Ref <- spTransform(MAL1_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
MAL2_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL2_F_Road_type.shp", 
                    layer = "MAL2_F_Road_type")
MAL2_Ref <- spTransform(MAL2_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))

Ref <- bind(MAL1_Ref, MAL2_Ref)

Ref <- left_join(st_as_sf(Ref), Sec, by = "UID")
Ref1 <- Ref
Ref1$Area <- "All"
Ref2 <- rbind(Ref, Ref1)
Ref2 <- Ref2 %>%
  group_by(Area, Road_type) %>%
  mutate(name = sum(!is.na(Road_type)))

Ref2_sum <- Ref2 %>%
  select(Area, Road_type, SE_median) %>%
  group_by(Area, Road_type) %>%
  summarise_all(funs(Median = median, p10 = quantile(., .1), 
                     p90 = quantile(., .9)), na.rm = TRUE)
Ref1 <- data.frame(Ref1)
Ref2_sum1 <- Ref1 %>%
  select(Area, SE_median) %>%
  group_by(Area) %>%
  summarise_all(funs(Median = median, p10 = quantile(., .1), 
                     p90 = quantile(., .9)), na.rm = TRUE)
Ref2 <- data.frame(Ref2)

ggplot(Ref2, aes(x = SE_median)) + 
  geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
                 color = "black", bins = 30) +
  labs(y = "count", x = "CO2- SE of the  medians / Median (using the drive pass means)") +
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
  labs(y = "count", x = "CO2- SE of the  medians / Median (using the drive pass means)") +
  facet_grid(Road_type ~ Area) + theme_minimal() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black", face = "bold"),
        strip.text = element_text(size = 14, colour = "black", face = "bold")) +
  scale_y_continuous() +
  geom_text(aes(label = paste0("n = ", name)), x = 1.7, y = 400, colour = "black", size = 4)



############################### MAL2 Shp of SE #################################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")  

MAL2_BC_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_mean.csv", sep = ",", 
                     header = TRUE) %>%
  select(UID, "BC_Mn" = No_of_observations, "Mn_BC_Mn" = Mean_N_Observations, 
         "Sd_BC_Mn" = Stddev_M_subsamples, "Se_BC_Mn" = StdError_N_Observations,
         "L_BC_Mn" = Lower_25_M_subsamples, "U_BC_Mn" = Upper_975_M_subsamples,
         "Mn_MB_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_BC = Sd_BC_Mn / Mn_BC_Mn)

MAL2_BC_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_mean_drive_pass_means.csv", sep = ",", 
                        header = TRUE) %>%
  select(UID, "BC_DMn" = No_of_observations, "Mn_BC_DMn" = Mean_N_Observations, 
         "Sd_BC_DMn" = Stddev_M_subsamples, "Se_BC_DMn" = StdError_N_Observations,
         "L_BC_DMn" = Lower_25_M_subsamples, "U_BC_DMn" = Upper_975_M_subsamples,
         "Mn_MB_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_BC = Sd_BC_DMn / Mn_BC_DMn)

MAL2_BC_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_median.csv", sep = ",", 
                    header = TRUE) %>%
  select(UID, "BC_Md" = No_of_observations, "Md_BC_Md" = Median_N_Observations, 
         "Sd_BC_Md" = Stddev_M_subsamples, "Se_BC_Md" = StdError_N_Observations,
         "L_BC_Md" = Lower_25_M_subsamples, "U_BC_Md" = Upper_975_M_subsamples, 
         "Md_MB_Md" = Median_M_subsamples, "Mn_MB_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_BC = Sd_BC_Md / Md_BC_Md)

MAL2_BC_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_median_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  select(UID, "BC_DMd" = No_of_observations, "Md_BC_DMd" = Median_N_Observations, 
         "Sd_BC_DMd" = Stddev_M_subsamples, "Se_BC_DMd" = StdError_N_Observations,
         "L_BC_DMd" = Lower_25_M_subsamples, "U_BC_DMd" = Upper_975_M_subsamples, 
         "Md_MB_DMd" = Median_M_subsamples, "Mn_MB_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_BC = Sd_BC_DMd / Md_BC_DMd)




MAL2_C_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_mean.csv", sep = ",", 
                    header = TRUE) %>%
  select(UID, "C_Mn" = No_of_observations, "Mn_C_Mn" = Mean_N_Observations, 
         "Sd_C_Mn" = Stddev_M_subsamples, "Se_C_Mn" = StdError_N_Observations,
         "L_C_Mn" = Lower_25_M_subsamples, "U_C_Mn" = Upper_975_M_subsamples,
         "Mn_MC_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_C = Sd_C_Mn / Mn_C_Mn)

MAL2_C_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_mean_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  select(UID, "C_DMn" = No_of_observations, "Mn_C_DMn" = Mean_N_Observations, 
         "Sd_C_DMn" = Stddev_M_subsamples, "Se_C_DMn" = StdError_N_Observations,
         "L_C_DMn" = Lower_25_M_subsamples, "U_C_DMn" = Upper_975_M_subsamples,
         "Mn_MC_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_C = Sd_C_DMn / Mn_C_DMn)

MAL2_C_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_median.csv", sep = ",", 
                   header = TRUE) %>%
  select(UID, "C_Md" = No_of_observations, "Md_C_Md" = Median_N_Observations, 
         "Sd_C_Md" = Stddev_M_subsamples, "Se_C_Md" = StdError_N_Observations,
         "L_C_Md" = Lower_25_M_subsamples, "U_C_Md" = Upper_975_M_subsamples, 
         "Md_MC_Md" = Median_M_subsamples, "Mn_MC_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_C = Sd_C_Md / Md_C_Md)

MAL2_C_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_median_drive_pass_means.csv", sep = ",", 
                      header = TRUE) %>%
  select(UID, "C_DMd" = No_of_observations, "Md_C_DMd" = Median_N_Observations, 
         "Sd_C_DMd" = Stddev_M_subsamples, "Se_C_DMd" = StdError_N_Observations,
         "L_C_DMd" = Lower_25_M_subsamples, "U_C_DMd" = Upper_975_M_subsamples, 
         "Md_MC_DMd" = Median_M_subsamples, "Mn_MC_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_C = Sd_C_DMd / Md_C_DMd)




MAL2_U_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_mean.csv", sep = ",", 
                    header = TRUE) %>%
  select(UID, "U_Mn" = No_of_observations, "Mn_U_Mn" = Mean_N_Observations, 
         "Sd_U_Mn" = Stddev_M_subsamples, "Se_U_Mn" = StdError_N_Observations,
         "L_U_Mn" = Lower_25_M_subsamples, "U_U_Mn" = Upper_975_M_subsamples,
         "Mn_MU_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_U = Sd_U_Mn / Mn_U_Mn)

MAL2_U_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_mean_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  select(UID, "U_DMn" = No_of_observations, "Mn_U_DMn" = Mean_N_Observations, 
         "Sd_U_DMn" = Stddev_M_subsamples, "Se_U_DMn" = StdError_N_Observations,
         "L_U_DMn" = Lower_25_M_subsamples, "U_U_DMn" = Upper_975_M_subsamples,
         "Mn_MU_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_U = Sd_U_DMn / Mn_U_DMn)

MAL2_U_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_median.csv", sep = ",", 
                   header = TRUE) %>%
  select(UID, "U_Md" = No_of_observations, "Md_U_Md" = Median_N_Observations, 
         "Sd_U_Md" = Stddev_M_subsamples, "Se_U_Md" = StdError_N_Observations,
         "L_U_Md" = Lower_25_M_subsamples, "U_U_Md" = Upper_975_M_subsamples, 
         "Md_MU_Md" = Median_M_subsamples, "Mn_MU_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_U = Sd_U_Md / Md_U_Md)

MAL2_U_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_median_drive_pass_means.csv", sep = ",", 
                      header = TRUE) %>%
  select(UID, "U_DMd" = No_of_observations, "Md_U_DMd" = Median_N_Observations, 
         "Sd_U_DMd" = Stddev_M_subsamples, "Se_U_DMd" = StdError_N_Observations,
         "L_U_DMd" = Lower_25_M_subsamples, "U_U_DMd" = Upper_975_M_subsamples, 
         "Md_MU_DMd" = Median_M_subsamples, "Mn_MU_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_U = Sd_U_DMd / Md_U_DMd)


MAL2 <- list(MAL2_BC_mean, MAL2_BC_mean_dm, MAL2_BC_med, MAL2_BC_med_dm,
            MAL2_U_mean, MAL2_U_mean_dm, MAL2_U_med, MAL2_U_med_dm,
            MAL2_C_mean, MAL2_C_mean_dm, MAL2_C_med, MAL2_C_med_dm) %>% 
  reduce(left_join, by = "UID")

MAL2_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL2_F_Road_type.shp", 
                   layer = "MAL2_F_Road_type")
MAL2_Ref <- spTransform(MAL2_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
Ref <- left_join(st_as_sf(MAL2_Ref), MAL2, by = "UID")
Ref <- as(Ref, 'Spatial')
dsn <- "D:/Dropbox/APMfull/MAL_CNG_Paper/SE_Med_Mean"
layer_name <- "MAL2_SE"
writeOGR(Ref, dsn, layer_name, driver = "ESRI Shapefile")

############################### MAL1 Shp of SE #################################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1")  

MAL1_BC_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_BC_mean.csv", sep = ",", 
                      header = TRUE) %>%
  select(UID, "BC_Mn" = No_of_observations, "Mn_BC_Mn" = Mean_N_Observations, 
         "Sd_BC_Mn" = Stddev_M_subsamples, "Se_BC_Mn" = StdError_N_Observations,
         "L_BC_Mn" = Lower_25_M_subsamples, "U_BC_Mn" = Upper_975_M_subsamples,
         "Mn_MB_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_BC = Sd_BC_Mn / Mn_BC_Mn)

MAL1_BC_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_BC_mean_drive_pass_means.csv", sep = ",", 
                         header = TRUE) %>%
  select(UID, "BC_DMn" = No_of_observations, "Mn_BC_DMn" = Mean_N_Observations, 
         "Sd_BC_DMn" = Stddev_M_subsamples, "Se_BC_DMn" = StdError_N_Observations,
         "L_BC_DMn" = Lower_25_M_subsamples, "U_BC_DMn" = Upper_975_M_subsamples,
         "Mn_MB_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_BC = Sd_BC_DMn / Mn_BC_DMn)

MAL1_BC_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_BC_median.csv", sep = ",", 
                     header = TRUE) %>%
  select(UID, "BC_Md" = No_of_observations, "Md_BC_Md" = Median_N_Observations, 
         "Sd_BC_Md" = Stddev_M_subsamples, "Se_BC_Md" = StdError_N_Observations,
         "L_BC_Md" = Lower_25_M_subsamples, "U_BC_Md" = Upper_975_M_subsamples, 
         "Md_MB_Md" = Median_M_subsamples, "Mn_MB_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_BC = Sd_BC_Md / Md_BC_Md)

MAL1_BC_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_BC_median_drive_pass_means.csv", sep = ",", 
                        header = TRUE) %>%
  select(UID, "BC_DMd" = No_of_observations, "Md_BC_DMd" = Median_N_Observations, 
         "Sd_BC_DMd" = Stddev_M_subsamples, "Se_BC_DMd" = StdError_N_Observations,
         "L_BC_DMd" = Lower_25_M_subsamples, "U_BC_DMd" = Upper_975_M_subsamples, 
         "Md_MB_DMd" = Median_M_subsamples, "Mn_MB_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_BC = Sd_BC_DMd / Md_BC_DMd)




MAL1_C_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CO2_mean.csv", sep = ",", 
                     header = TRUE) %>%
  select(UID, "C_Mn" = No_of_observations, "Mn_C_Mn" = Mean_N_Observations, 
         "Sd_C_Mn" = Stddev_M_subsamples, "Se_C_Mn" = StdError_N_Observations,
         "L_C_Mn" = Lower_25_M_subsamples, "U_C_Mn" = Upper_975_M_subsamples,
         "Mn_MC_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_C = Sd_C_Mn / Mn_C_Mn)

MAL1_C_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CO2_mean_drive_pass_means.csv", sep = ",", 
                        header = TRUE) %>%
  select(UID, "C_DMn" = No_of_observations, "Mn_C_DMn" = Mean_N_Observations, 
         "Sd_C_DMn" = Stddev_M_subsamples, "Se_C_DMn" = StdError_N_Observations,
         "L_C_DMn" = Lower_25_M_subsamples, "U_C_DMn" = Upper_975_M_subsamples,
         "Mn_MC_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_C = Sd_C_DMn / Mn_C_DMn)

MAL1_C_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CO2_median.csv", sep = ",", 
                    header = TRUE) %>%
  select(UID, "C_Md" = No_of_observations, "Md_C_Md" = Median_N_Observations, 
         "Sd_C_Md" = Stddev_M_subsamples, "Se_C_Md" = StdError_N_Observations,
         "L_C_Md" = Lower_25_M_subsamples, "U_C_Md" = Upper_975_M_subsamples, 
         "Md_MC_Md" = Median_M_subsamples, "Mn_MC_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_C = Sd_C_Md / Md_C_Md)

MAL1_C_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CO2_median_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  select(UID, "C_DMd" = No_of_observations, "Md_C_DMd" = Median_N_Observations, 
         "Sd_C_DMd" = Stddev_M_subsamples, "Se_C_DMd" = StdError_N_Observations,
         "L_C_DMd" = Lower_25_M_subsamples, "U_C_DMd" = Upper_975_M_subsamples, 
         "Md_MC_DMd" = Median_M_subsamples, "Mn_MC_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_C = Sd_C_DMd / Md_C_DMd)




MAL1_U_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CPC_mean.csv", sep = ",", 
                     header = TRUE) %>%
  select(UID, "U_Mn" = No_of_observations, "Mn_U_Mn" = Mean_N_Observations, 
         "Sd_U_Mn" = Stddev_M_subsamples, "Se_U_Mn" = StdError_N_Observations,
         "L_U_Mn" = Lower_25_M_subsamples, "U_U_Mn" = Upper_975_M_subsamples,
         "Mn_MU_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_U = Sd_U_Mn / Mn_U_Mn)

MAL1_U_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CPC_mean_drive_pass_means.csv", sep = ",", 
                        header = TRUE) %>%
  select(UID, "U_DMn" = No_of_observations, "Mn_U_DMn" = Mean_N_Observations, 
         "Sd_U_DMn" = Stddev_M_subsamples, "Se_U_DMn" = StdError_N_Observations,
         "L_U_DMn" = Lower_25_M_subsamples, "U_U_DMn" = Upper_975_M_subsamples,
         "Mn_MU_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_U = Sd_U_DMn / Mn_U_DMn)

MAL1_U_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CPC_median.csv", sep = ",", 
                    header = TRUE) %>%
  select(UID, "U_Md" = No_of_observations, "Md_U_Md" = Median_N_Observations, 
         "Sd_U_Md" = Stddev_M_subsamples, "Se_U_Md" = StdError_N_Observations,
         "L_U_Md" = Lower_25_M_subsamples, "U_U_Md" = Upper_975_M_subsamples, 
         "Md_MU_Md" = Median_M_subsamples, "Mn_MU_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_U = Sd_U_Md / Md_U_Md)

MAL1_U_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CPC_median_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  select(UID, "U_DMd" = No_of_observations, "Md_U_DMd" = Median_N_Observations, 
         "Sd_U_DMd" = Stddev_M_subsamples, "Se_U_DMd" = StdError_N_Observations,
         "L_U_DMd" = Lower_25_M_subsamples, "U_U_DMd" = Upper_975_M_subsamples, 
         "Md_MU_DMd" = Median_M_subsamples, "Mn_MU_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_U = Sd_U_DMd / Md_U_DMd)


MAL1 <- list(MAL1_BC_mean, MAL1_BC_mean_dm, MAL1_BC_med, MAL1_BC_med_dm,
             MAL1_U_mean, MAL1_U_mean_dm, MAL1_U_med, MAL1_U_med_dm,
             MAL1_C_mean, MAL1_C_mean_dm, MAL1_C_med, MAL1_C_med_dm) %>% 
  reduce(left_join, by = "UID")

MAL1_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/MAL1_F_Road_type.shp", 
                    layer = "MAL1_F_Road_type")
MAL1_Ref <- spTransform(MAL1_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
Ref <- left_join(st_as_sf(MAL1_Ref), MAL1, by = "UID")
Ref <- as(Ref, 'Spatial')
dsn <- "D:/Dropbox/APMfull/MAL_CNG_Paper/SE_Med_Mean"
layer_name <- "MAL1_SE"
writeOGR(Ref, dsn, layer_name, driver = "ESRI Shapefile")


beepr::beep()


# A sample from population with sample size n.
# Draw a sample from the original sample data with replacement with size n, and replicate B times, each re-sampled sample is called a Bootstrap Sample, and there will totally B Bootstrap Samples.
# Evaluate the statistic of ?? for each Bootstrap Sample, and there will be totally B estimates of ??.
# Construct a sampling distribution with these B Bootstrap statistics and use it to make further statistical inference, such as:
# Estimating the standard error of statistic for ??.
# Obtaining a Confidence Interval for ??.