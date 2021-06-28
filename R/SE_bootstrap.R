source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
set.seed(108)

############################### MAL2 Shp of SE #################################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")  
library(dplyr)
MAL2_BC_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_mean.csv", sep = ",", 
                     header = TRUE) %>%
  dplyr::select(UID, "BC_Mn" = No_of_observations, "Mn_BC_Mn" = Mean_N_Observations, 
         "Sd_BC_Mn" = Stddev_M_subsamples, "Se_BC_Mn" = StdError_N_Observations,
         "L_BC_Mn" = Lower_25_M_subsamples, "U_BC_Mn" = Upper_975_M_subsamples,
         "Mn_MB_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_BC = Sd_BC_Mn / Mn_BC_Mn)

MAL2_BC_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_mean_drive_pass_means.csv", sep = ",", 
                        header = TRUE) %>%
  dplyr::select(UID, "BC_DMn" = No_of_observations, "Mn_BC_DMn" = Mean_N_Observations, 
         "Sd_BC_DMn" = Stddev_M_subsamples, "Se_BC_DMn" = StdError_N_Observations,
         "L_BC_DMn" = Lower_25_M_subsamples, "U_BC_DMn" = Upper_975_M_subsamples,
         "Mn_MB_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_BC = Sd_BC_DMn / Mn_BC_DMn)

MAL2_BC_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_median.csv", sep = ",", 
                    header = TRUE) %>%
  dplyr::select(UID, "BC_Md" = No_of_observations, "Md_BC_Md" = Median_N_Observations, 
         "Sd_BC_Md" = Stddev_M_subsamples, "Se_BC_Md" = StdError_N_Observations,
         "L_BC_Md" = Lower_25_M_subsamples, "U_BC_Md" = Upper_975_M_subsamples, 
         "Md_MB_Md" = Median_M_subsamples, "Mn_MB_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_BC = Sd_BC_Md / Md_BC_Md)

MAL2_BC_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_median_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  dplyr::select(UID, "BC_DMd" = No_of_observations, "Md_BC_DMd" = Median_N_Observations, 
         "Sd_BC_DMd" = Stddev_M_subsamples, "Se_BC_DMd" = StdError_N_Observations,
         "L_BC_DMd" = Lower_25_M_subsamples, "U_BC_DMd" = Upper_975_M_subsamples, 
         "Md_MB_DMd" = Median_M_subsamples, "Mn_MB_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_BC = Sd_BC_DMd / Md_BC_DMd)




MAL2_C_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_mean.csv", sep = ",", 
                    header = TRUE) %>%
  dplyr::select(UID, "C_Mn" = No_of_observations, "Mn_C_Mn" = Mean_N_Observations, 
         "Sd_C_Mn" = Stddev_M_subsamples, "Se_C_Mn" = StdError_N_Observations,
         "L_C_Mn" = Lower_25_M_subsamples, "U_C_Mn" = Upper_975_M_subsamples,
         "Mn_MC_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_C = Sd_C_Mn / Mn_C_Mn)

MAL2_C_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_mean_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  dplyr::select(UID, "C_DMn" = No_of_observations, "Mn_C_DMn" = Mean_N_Observations, 
         "Sd_C_DMn" = Stddev_M_subsamples, "Se_C_DMn" = StdError_N_Observations,
         "L_C_DMn" = Lower_25_M_subsamples, "U_C_DMn" = Upper_975_M_subsamples,
         "Mn_MC_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_C = Sd_C_DMn / Mn_C_DMn)

MAL2_C_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_median.csv", sep = ",", 
                   header = TRUE) %>%
  dplyr::select(UID, "C_Md" = No_of_observations, "Md_C_Md" = Median_N_Observations, 
         "Sd_C_Md" = Stddev_M_subsamples, "Se_C_Md" = StdError_N_Observations,
         "L_C_Md" = Lower_25_M_subsamples, "U_C_Md" = Upper_975_M_subsamples, 
         "Md_MC_Md" = Median_M_subsamples, "Mn_MC_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_C = Sd_C_Md / Md_C_Md)

MAL2_C_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_median_drive_pass_means.csv", sep = ",", 
                      header = TRUE) %>%
  dplyr::select(UID, "C_DMd" = No_of_observations, "Md_C_DMd" = Median_N_Observations, 
         "Sd_C_DMd" = Stddev_M_subsamples, "Se_C_DMd" = StdError_N_Observations,
         "L_C_DMd" = Lower_25_M_subsamples, "U_C_DMd" = Upper_975_M_subsamples, 
         "Md_MC_DMd" = Median_M_subsamples, "Mn_MC_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_C = Sd_C_DMd / Md_C_DMd)




MAL2_U_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_mean.csv", sep = ",", 
                    header = TRUE) %>%
  dplyr::select(UID, "U_Mn" = No_of_observations, "Mn_U_Mn" = Mean_N_Observations, 
         "Sd_U_Mn" = Stddev_M_subsamples, "Se_U_Mn" = StdError_N_Observations,
         "L_U_Mn" = Lower_25_M_subsamples, "U_U_Mn" = Upper_975_M_subsamples,
         "Mn_MU_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_U = Sd_U_Mn / Mn_U_Mn)

MAL2_U_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_mean_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  dplyr::select(UID, "U_DMn" = No_of_observations, "Mn_U_DMn" = Mean_N_Observations, 
         "Sd_U_DMn" = Stddev_M_subsamples, "Se_U_DMn" = StdError_N_Observations,
         "L_U_DMn" = Lower_25_M_subsamples, "U_U_DMn" = Upper_975_M_subsamples,
         "Mn_MU_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_U = Sd_U_DMn / Mn_U_DMn)

MAL2_U_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_median.csv", sep = ",", 
                   header = TRUE) %>%
  dplyr::select(UID, "U_Md" = No_of_observations, "Md_U_Md" = Median_N_Observations, 
         "Sd_U_Md" = Stddev_M_subsamples, "Se_U_Md" = StdError_N_Observations,
         "L_U_Md" = Lower_25_M_subsamples, "U_U_Md" = Upper_975_M_subsamples, 
         "Md_MU_Md" = Median_M_subsamples, "Mn_MU_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_U = Sd_U_Md / Md_U_Md)

MAL2_U_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_median_drive_pass_means.csv", sep = ",", 
                      header = TRUE) %>%
  dplyr::select(UID, "U_DMd" = No_of_observations, "Md_U_DMd" = Median_N_Observations, 
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
  dplyr::select(UID, "BC_Mn" = No_of_observations, "Mn_BC_Mn" = Mean_N_Observations, 
         "Sd_BC_Mn" = Stddev_M_subsamples, "Se_BC_Mn" = StdError_N_Observations,
         "L_BC_Mn" = Lower_25_M_subsamples, "U_BC_Mn" = Upper_975_M_subsamples,
         "Mn_MB_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_BC = Sd_BC_Mn / Mn_BC_Mn)

MAL1_BC_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_BC_mean_drive_pass_means.csv", sep = ",", 
                         header = TRUE) %>%
  dplyr::select(UID, "BC_DMn" = No_of_observations, "Mn_BC_DMn" = Mean_N_Observations, 
         "Sd_BC_DMn" = Stddev_M_subsamples, "Se_BC_DMn" = StdError_N_Observations,
         "L_BC_DMn" = Lower_25_M_subsamples, "U_BC_DMn" = Upper_975_M_subsamples,
         "Mn_MB_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_BC = Sd_BC_DMn / Mn_BC_DMn)

MAL1_BC_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_BC_median.csv", sep = ",", 
                     header = TRUE) %>%
  dplyr::select(UID, "BC_Md" = No_of_observations, "Md_BC_Md" = Median_N_Observations, 
         "Sd_BC_Md" = Stddev_M_subsamples, "Se_BC_Md" = StdError_N_Observations,
         "L_BC_Md" = Lower_25_M_subsamples, "U_BC_Md" = Upper_975_M_subsamples, 
         "Md_MB_Md" = Median_M_subsamples, "Mn_MB_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_BC = Sd_BC_Md / Md_BC_Md)

MAL1_BC_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_BC_median_drive_pass_means.csv", sep = ",", 
                        header = TRUE) %>%
  dplyr::select(UID, "BC_DMd" = No_of_observations, "Md_BC_DMd" = Median_N_Observations, 
         "Sd_BC_DMd" = Stddev_M_subsamples, "Se_BC_DMd" = StdError_N_Observations,
         "L_BC_DMd" = Lower_25_M_subsamples, "U_BC_DMd" = Upper_975_M_subsamples, 
         "Md_MB_DMd" = Median_M_subsamples, "Mn_MB_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_BC = Sd_BC_DMd / Md_BC_DMd)




MAL1_C_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CO2_mean.csv", sep = ",", 
                     header = TRUE) %>%
  dplyr::select(UID, "C_Mn" = No_of_observations, "Mn_C_Mn" = Mean_N_Observations, 
         "Sd_C_Mn" = Stddev_M_subsamples, "Se_C_Mn" = StdError_N_Observations,
         "L_C_Mn" = Lower_25_M_subsamples, "U_C_Mn" = Upper_975_M_subsamples,
         "Mn_MC_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_C = Sd_C_Mn / Mn_C_Mn)

MAL1_C_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CO2_mean_drive_pass_means.csv", sep = ",", 
                        header = TRUE) %>%
  dplyr::select(UID, "C_DMn" = No_of_observations, "Mn_C_DMn" = Mean_N_Observations, 
         "Sd_C_DMn" = Stddev_M_subsamples, "Se_C_DMn" = StdError_N_Observations,
         "L_C_DMn" = Lower_25_M_subsamples, "U_C_DMn" = Upper_975_M_subsamples,
         "Mn_MC_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_C = Sd_C_DMn / Mn_C_DMn)

MAL1_C_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CO2_median.csv", sep = ",", 
                    header = TRUE) %>%
  dplyr::select(UID, "C_Md" = No_of_observations, "Md_C_Md" = Median_N_Observations, 
         "Sd_C_Md" = Stddev_M_subsamples, "Se_C_Md" = StdError_N_Observations,
         "L_C_Md" = Lower_25_M_subsamples, "U_C_Md" = Upper_975_M_subsamples, 
         "Md_MC_Md" = Median_M_subsamples, "Mn_MC_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_C = Sd_C_Md / Md_C_Md)

MAL1_C_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CO2_median_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  dplyr::select(UID, "C_DMd" = No_of_observations, "Md_C_DMd" = Median_N_Observations, 
         "Sd_C_DMd" = Stddev_M_subsamples, "Se_C_DMd" = StdError_N_Observations,
         "L_C_DMd" = Lower_25_M_subsamples, "U_C_DMd" = Upper_975_M_subsamples, 
         "Md_MC_DMd" = Median_M_subsamples, "Mn_MC_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_C = Sd_C_DMd / Md_C_DMd)




MAL1_U_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CPC_mean.csv", sep = ",", 
                     header = TRUE) %>%
  dplyr::select(UID, "U_Mn" = No_of_observations, "Mn_U_Mn" = Mean_N_Observations, 
         "Sd_U_Mn" = Stddev_M_subsamples, "Se_U_Mn" = StdError_N_Observations,
         "L_U_Mn" = Lower_25_M_subsamples, "U_U_Mn" = Upper_975_M_subsamples,
         "Mn_MU_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_U = Sd_U_Mn / Mn_U_Mn)

MAL1_U_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CPC_mean_drive_pass_means.csv", sep = ",", 
                        header = TRUE) %>%
  dplyr::select(UID, "U_DMn" = No_of_observations, "Mn_U_DMn" = Mean_N_Observations, 
         "Sd_U_DMn" = Stddev_M_subsamples, "Se_U_DMn" = StdError_N_Observations,
         "L_U_DMn" = Lower_25_M_subsamples, "U_U_DMn" = Upper_975_M_subsamples,
         "Mn_MU_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_U = Sd_U_DMn / Mn_U_DMn)

MAL1_U_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CPC_median.csv", sep = ",", 
                    header = TRUE) %>%
  dplyr::select(UID, "U_Md" = No_of_observations, "Md_U_Md" = Median_N_Observations, 
         "Sd_U_Md" = Stddev_M_subsamples, "Se_U_Md" = StdError_N_Observations,
         "L_U_Md" = Lower_25_M_subsamples, "U_U_Md" = Upper_975_M_subsamples, 
         "Md_MU_Md" = Median_M_subsamples, "Mn_MU_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_U = Sd_U_Md / Md_U_Md)

MAL1_U_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_Bootstrap_CPC_median_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  dplyr::select(UID, "U_DMd" = No_of_observations, "Md_U_DMd" = Median_N_Observations, 
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


############################## KAN Shp of SE ##################################


setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN")  

KAN_BC_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_mean.csv", sep = ",", 
                      header = TRUE) %>%
  dplyr::select(UID, "BC_Mn" = No_of_observations, "Mn_BC_Mn" = Mean_N_Observations, 
                "Sd_BC_Mn" = Stddev_M_subsamples, "Se_BC_Mn" = StdError_N_Observations,
                "L_BC_Mn" = Lower_25_M_subsamples, "U_BC_Mn" = Upper_975_M_subsamples,
                "Mn_MB_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_BC = Sd_BC_Mn / Mn_BC_Mn)

KAN_BC_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_mean_drive_pass_means.csv", sep = ",", 
                         header = TRUE) %>%
  dplyr::select(UID, "BC_DMn" = No_of_observations, "Mn_BC_DMn" = Mean_N_Observations, 
                "Sd_BC_DMn" = Stddev_M_subsamples, "Se_BC_DMn" = StdError_N_Observations,
                "L_BC_DMn" = Lower_25_M_subsamples, "U_BC_DMn" = Upper_975_M_subsamples,
                "Mn_MB_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_BC = Sd_BC_DMn / Mn_BC_DMn)

KAN_BC_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_median.csv", sep = ",", 
                     header = TRUE) %>%
  dplyr::select(UID, "BC_Md" = No_of_observations, "Md_BC_Md" = Median_N_Observations, 
                "Sd_BC_Md" = Stddev_M_subsamples, "Se_BC_Md" = StdError_N_Observations,
                "L_BC_Md" = Lower_25_M_subsamples, "U_BC_Md" = Upper_975_M_subsamples, 
                "Md_MB_Md" = Median_M_subsamples, "Mn_MB_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_BC = Sd_BC_Md / Md_BC_Md)

KAN_BC_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_median_drive_pass_means.csv", sep = ",", 
                        header = TRUE) %>%
  dplyr::select(UID, "BC_DMd" = No_of_observations, "Md_BC_DMd" = Median_N_Observations, 
                "Sd_BC_DMd" = Stddev_M_subsamples, "Se_BC_DMd" = StdError_N_Observations,
                "L_BC_DMd" = Lower_25_M_subsamples, "U_BC_DMd" = Upper_975_M_subsamples, 
                "Md_MB_DMd" = Median_M_subsamples, "Mn_MB_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_BC = Sd_BC_DMd / Md_BC_DMd)




KAN_C_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CO2_mean.csv", sep = ",", 
                     header = TRUE) %>%
  dplyr::select(UID, "C_Mn" = No_of_observations, "Mn_C_Mn" = Mean_N_Observations, 
                "Sd_C_Mn" = Stddev_M_subsamples, "Se_C_Mn" = StdError_N_Observations,
                "L_C_Mn" = Lower_25_M_subsamples, "U_C_Mn" = Upper_975_M_subsamples,
                "Mn_MC_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_C = Sd_C_Mn / Mn_C_Mn)

KAN_C_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CO2_mean_drive_pass_means.csv", sep = ",", 
                        header = TRUE) %>%
  dplyr::select(UID, "C_DMn" = No_of_observations, "Mn_C_DMn" = Mean_N_Observations, 
                "Sd_C_DMn" = Stddev_M_subsamples, "Se_C_DMn" = StdError_N_Observations,
                "L_C_DMn" = Lower_25_M_subsamples, "U_C_DMn" = Upper_975_M_subsamples,
                "Mn_MC_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_C = Sd_C_DMn / Mn_C_DMn)

KAN_C_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CO2_median.csv", sep = ",", 
                    header = TRUE) %>%
  dplyr::select(UID, "C_Md" = No_of_observations, "Md_C_Md" = Median_N_Observations, 
                "Sd_C_Md" = Stddev_M_subsamples, "Se_C_Md" = StdError_N_Observations,
                "L_C_Md" = Lower_25_M_subsamples, "U_C_Md" = Upper_975_M_subsamples, 
                "Md_MC_Md" = Median_M_subsamples, "Mn_MC_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_C = Sd_C_Md / Md_C_Md)

KAN_C_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CO2_median_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  dplyr::select(UID, "C_DMd" = No_of_observations, "Md_C_DMd" = Median_N_Observations, 
                "Sd_C_DMd" = Stddev_M_subsamples, "Se_C_DMd" = StdError_N_Observations,
                "L_C_DMd" = Lower_25_M_subsamples, "U_C_DMd" = Upper_975_M_subsamples, 
                "Md_MC_DMd" = Median_M_subsamples, "Mn_MC_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_C = Sd_C_DMd / Md_C_DMd)




KAN_U_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CPC_mean.csv", sep = ",", 
                     header = TRUE) %>%
  dplyr::select(UID, "U_Mn" = No_of_observations, "Mn_U_Mn" = Mean_N_Observations, 
                "Sd_U_Mn" = Stddev_M_subsamples, "Se_U_Mn" = StdError_N_Observations,
                "L_U_Mn" = Lower_25_M_subsamples, "U_U_Mn" = Upper_975_M_subsamples,
                "Mn_MU_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_U = Sd_U_Mn / Mn_U_Mn)

KAN_U_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CPC_mean_drive_pass_means.csv", sep = ",", 
                        header = TRUE) %>%
  dplyr::select(UID, "U_DMn" = No_of_observations, "Mn_U_DMn" = Mean_N_Observations, 
                "Sd_U_DMn" = Stddev_M_subsamples, "Se_U_DMn" = StdError_N_Observations,
                "L_U_DMn" = Lower_25_M_subsamples, "U_U_DMn" = Upper_975_M_subsamples,
                "Mn_MU_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_U = Sd_U_DMn / Mn_U_DMn)

KAN_U_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CPC_median.csv", sep = ",", 
                    header = TRUE) %>%
  dplyr::select(UID, "U_Md" = No_of_observations, "Md_U_Md" = Median_N_Observations, 
                "Sd_U_Md" = Stddev_M_subsamples, "Se_U_Md" = StdError_N_Observations,
                "L_U_Md" = Lower_25_M_subsamples, "U_U_Md" = Upper_975_M_subsamples, 
                "Md_MU_Md" = Median_M_subsamples, "Mn_MU_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_U = Sd_U_Md / Md_U_Md)

KAN_U_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CPC_median_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  dplyr::select(UID, "U_DMd" = No_of_observations, "Md_U_DMd" = Median_N_Observations, 
                "Sd_U_DMd" = Stddev_M_subsamples, "Se_U_DMd" = StdError_N_Observations,
                "L_U_DMd" = Lower_25_M_subsamples, "U_U_DMd" = Upper_975_M_subsamples, 
                "Md_MU_DMd" = Median_M_subsamples, "Mn_MU_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_U = Sd_U_DMd / Md_U_DMd)


KAN <- list(KAN_BC_mean, KAN_BC_mean_dm, KAN_BC_med, KAN_BC_med_dm,
             KAN_U_mean, KAN_U_mean_dm, KAN_U_med, KAN_U_med_dm,
             KAN_C_mean, KAN_C_mean_dm, KAN_C_med, KAN_C_med_dm) %>% 
  reduce(left_join, by = "UID")

KAN_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/KAN_F_Road_type.shp", 
                    layer = "KAN_F_Road_type")
KAN_Ref <- spTransform(KAN_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
Ref <- left_join(st_as_sf(KAN_Ref), KAN, by = "UID")
Ref <- as(Ref, 'Spatial')
dsn <- "D:/Dropbox/APMfull/MAL_CNG_Paper/SE_Med_Mean"
layer_name <- "KAN_SE"
writeOGR(Ref, dsn, layer_name, driver = "ESRI Shapefile")


beepr::beep()

############################## CBD Shp of SE ###################################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD")  

CBD_BC_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_mean.csv", sep = ",", 
                     header = TRUE) %>%
  dplyr::select(UID, "BC_Mn" = No_of_observations, "Mn_BC_Mn" = Mean_N_Observations, 
                "Sd_BC_Mn" = Stddev_M_subsamples, "Se_BC_Mn" = StdError_N_Observations,
                "L_BC_Mn" = Lower_25_M_subsamples, "U_BC_Mn" = Upper_975_M_subsamples,
                "Mn_MB_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_BC = Sd_BC_Mn / Mn_BC_Mn)

CBD_BC_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_mean_drive_pass_means.csv", sep = ",", 
                        header = TRUE) %>%
  dplyr::select(UID, "BC_DMn" = No_of_observations, "Mn_BC_DMn" = Mean_N_Observations, 
                "Sd_BC_DMn" = Stddev_M_subsamples, "Se_BC_DMn" = StdError_N_Observations,
                "L_BC_DMn" = Lower_25_M_subsamples, "U_BC_DMn" = Upper_975_M_subsamples,
                "Mn_MB_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_BC = Sd_BC_DMn / Mn_BC_DMn)

CBD_BC_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_median.csv", sep = ",", 
                    header = TRUE) %>%
  dplyr::select(UID, "BC_Md" = No_of_observations, "Md_BC_Md" = Median_N_Observations, 
                "Sd_BC_Md" = Stddev_M_subsamples, "Se_BC_Md" = StdError_N_Observations,
                "L_BC_Md" = Lower_25_M_subsamples, "U_BC_Md" = Upper_975_M_subsamples, 
                "Md_MB_Md" = Median_M_subsamples, "Mn_MB_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_BC = Sd_BC_Md / Md_BC_Md)

CBD_BC_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_median_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  dplyr::select(UID, "BC_DMd" = No_of_observations, "Md_BC_DMd" = Median_N_Observations, 
                "Sd_BC_DMd" = Stddev_M_subsamples, "Se_BC_DMd" = StdError_N_Observations,
                "L_BC_DMd" = Lower_25_M_subsamples, "U_BC_DMd" = Upper_975_M_subsamples, 
                "Md_MB_DMd" = Median_M_subsamples, "Mn_MB_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_BC = Sd_BC_DMd / Md_BC_DMd)




CBD_C_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CO2_mean.csv", sep = ",", 
                    header = TRUE) %>%
  dplyr::select(UID, "C_Mn" = No_of_observations, "Mn_C_Mn" = Mean_N_Observations, 
                "Sd_C_Mn" = Stddev_M_subsamples, "Se_C_Mn" = StdError_N_Observations,
                "L_C_Mn" = Lower_25_M_subsamples, "U_C_Mn" = Upper_975_M_subsamples,
                "Mn_MC_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_C = Sd_C_Mn / Mn_C_Mn)

CBD_C_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CO2_mean_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  dplyr::select(UID, "C_DMn" = No_of_observations, "Mn_C_DMn" = Mean_N_Observations, 
                "Sd_C_DMn" = Stddev_M_subsamples, "Se_C_DMn" = StdError_N_Observations,
                "L_C_DMn" = Lower_25_M_subsamples, "U_C_DMn" = Upper_975_M_subsamples,
                "Mn_MC_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_C = Sd_C_DMn / Mn_C_DMn)

CBD_C_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CO2_median.csv", sep = ",", 
                   header = TRUE) %>%
  dplyr::select(UID, "C_Md" = No_of_observations, "Md_C_Md" = Median_N_Observations, 
                "Sd_C_Md" = Stddev_M_subsamples, "Se_C_Md" = StdError_N_Observations,
                "L_C_Md" = Lower_25_M_subsamples, "U_C_Md" = Upper_975_M_subsamples, 
                "Md_MC_Md" = Median_M_subsamples, "Mn_MC_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_C = Sd_C_Md / Md_C_Md)

CBD_C_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CO2_median_drive_pass_means.csv", sep = ",", 
                      header = TRUE) %>%
  dplyr::select(UID, "C_DMd" = No_of_observations, "Md_C_DMd" = Median_N_Observations, 
                "Sd_C_DMd" = Stddev_M_subsamples, "Se_C_DMd" = StdError_N_Observations,
                "L_C_DMd" = Lower_25_M_subsamples, "U_C_DMd" = Upper_975_M_subsamples, 
                "Md_MC_DMd" = Median_M_subsamples, "Mn_MC_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_C = Sd_C_DMd / Md_C_DMd)




CBD_U_mean <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CPC_mean.csv", sep = ",", 
                    header = TRUE) %>%
  dplyr::select(UID, "U_Mn" = No_of_observations, "Mn_U_Mn" = Mean_N_Observations, 
                "Sd_U_Mn" = Stddev_M_subsamples, "Se_U_Mn" = StdError_N_Observations,
                "L_U_Mn" = Lower_25_M_subsamples, "U_U_Mn" = Upper_975_M_subsamples,
                "Mn_MU_Mn" = Mean_M_subsamples) %>%
  mutate(SE_MN_U = Sd_U_Mn / Mn_U_Mn)

CBD_U_mean_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CPC_mean_drive_pass_means.csv", sep = ",", 
                       header = TRUE) %>%
  dplyr::select(UID, "U_DMn" = No_of_observations, "Mn_U_DMn" = Mean_N_Observations, 
                "Sd_U_DMn" = Stddev_M_subsamples, "Se_U_DMn" = StdError_N_Observations,
                "L_U_DMn" = Lower_25_M_subsamples, "U_U_DMn" = Upper_975_M_subsamples,
                "Mn_MU_DMn" = Mean_M_subsamples) %>%
  mutate(SE_DMN_U = Sd_U_DMn / Mn_U_DMn)

CBD_U_med <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CPC_median.csv", sep = ",", 
                   header = TRUE) %>%
  dplyr::select(UID, "U_Md" = No_of_observations, "Md_U_Md" = Median_N_Observations, 
                "Sd_U_Md" = Stddev_M_subsamples, "Se_U_Md" = StdError_N_Observations,
                "L_U_Md" = Lower_25_M_subsamples, "U_U_Md" = Upper_975_M_subsamples, 
                "Md_MU_Md" = Median_M_subsamples, "Mn_MU_Md" = Mean_M_subsamples) %>%
  mutate(SE_MD_U = Sd_U_Md / Md_U_Md)

CBD_U_med_dm <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CPC_median_drive_pass_means.csv", sep = ",", 
                      header = TRUE) %>%
  dplyr::select(UID, "U_DMd" = No_of_observations, "Md_U_DMd" = Median_N_Observations, 
                "Sd_U_DMd" = Stddev_M_subsamples, "Se_U_DMd" = StdError_N_Observations,
                "L_U_DMd" = Lower_25_M_subsamples, "U_U_DMd" = Upper_975_M_subsamples, 
                "Md_MU_DMd" = Median_M_subsamples, "Mn_MU_DMd" = Mean_M_subsamples) %>%
  mutate(SE_DMD_U = Sd_U_DMd / Md_U_DMd)


CBD <- list(CBD_BC_mean, CBD_BC_mean_dm, CBD_BC_med, CBD_BC_med_dm,
            CBD_U_mean, CBD_U_mean_dm, CBD_U_med, CBD_U_med_dm,
            CBD_C_mean, CBD_C_mean_dm, CBD_C_med, CBD_C_med_dm) %>% 
  reduce(left_join, by = "UID")

CBD_Ref <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Roads/CBD_F_Road_type.shp", 
                   layer = "CBD_F_Road_type")
CBD_Ref <- spTransform(CBD_Ref, CRS("+proj=utm +zone=43 ellps=WGS84"))
Ref <- left_join(st_as_sf(CBD_Ref), CBD, by = "UID")
Ref <- as(Ref, 'Spatial')
dsn <- "D:/Dropbox/APMfull/MAL_CNG_Paper/SE_Med_Mean"
layer_name <- "CBD_SE"
writeOGR(Ref, dsn, layer_name, driver = "ESRI Shapefile")


beepr::beep()
