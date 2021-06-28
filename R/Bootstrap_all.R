source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
set.seed(108)

############################## Mean for 30m data ###############################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers")  
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers"
shp_list <- list.files(dir, pattern = "\\.shp$")
data_table <- data.frame()

for(i in seq_along(shp_list))data_table <- rbind(data_table,
                                                 data.frame(st_read(shp_list[i])))
sec <- data_table %>%
  dplyr::select(UID, "Road_type" = Rod_typ, "BC_c" = BC_c_mn, "CPC" = CPC_mn, "BC_LC" = BC_NR_L_mn,
                "CO2_c" = CO2_c_mn)

boot_list <- sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_BC_LC <- sec %>%
  dplyr::select(UID, BC_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)


boot_list_CPC <- sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_mean_drive_pass_means.csv")
write.csv(boot_list_BC_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_LC_mean_drive_pass_means.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_mean_drive_pass_means.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_mean_drive_pass_means.csv")

############################## Mean for point data #############################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")  

sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_sec.csv", sep = ",", 
                  header = TRUE)
sec <- sec %>%
  dplyr::select(UID, Road_type, BC_c, CPC, CO2_c, BC_NR_LC)

boot_list <- sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_BC_LC <- sec %>%
  dplyr::select(UID, BC_NR_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_NR_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)


boot_list_CPC <- sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_mean.csv")
write.csv(boot_list_BC_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_LC_mean.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_mean.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_mean.csv")


############################ Median for point file #############################
setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")  

MAL2_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_sec.csv", sep = ",", 
                 header = TRUE)
MAL2_sec <- MAL2_sec %>%
  dplyr::select(UID, Road_type, BC_c, CPC, CO2_c, BC_NR_LC)


boot_list <- MAL2_sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_LC <- MAL2_sec %>%
  dplyr::select(UID, BC_NR_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_NR_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_CPC <- MAL2_sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- MAL2_sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_median.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_median.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_median.csv")
write.csv(boot_list_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_LC_median.csv")
# MAL2_sec_707 <- MAL2_sec %>%
#   filter(UID == "MAL2_707")
# median_boot <- function(x, i) {median(x[i], na.rm = TRUE)}
# boots_mean <- boot::boot(MAL2_sec_707$BC_c, statistic = median_boot, R = 10000)
# ggplot(data.frame(boots_mean), aes(x = V1)) + geom_histogram() 


############################ Median for 30m data ###############################
setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers")  
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers"
shp_list <- list.files(dir, pattern = "\\.shp$")
data_table <- data.frame()

### For all shapefiles in a folder
for(i in seq_along(shp_list))data_table <- rbind(data_table,
                                                 data.frame(st_read(shp_list[i])))
MAL2_sec <- data_table %>%
  dplyr::select(UID, "Road_type" = Rod_typ, "BC_c" = BC_c_mn, "CPC" = CPC_mn, "BC_LC" = BC_NR_L_mn,
                "CO2_c" = CO2_c_mn)


boot_list <- MAL2_sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_LC <- MAL2_sec %>%
  dplyr::select(UID, BC_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_CPC <- MAL2_sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- MAL2_sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_median_drive_pass_means.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CPC_median_drive_pass_means.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_CO2_median_drive_pass_means.csv")
write.csv(boot_list_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_Bootstrap_BC_LC_median_drive_pass_means.csv")


beepr::beep()


################################## KAN #########################################


############################## Mean for 30m data ###############################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/Final_layers")  
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/Final_layers"
shp_list <- list.files(dir, pattern = "\\.shp$")
data_table <- data.frame()

for(i in seq_along(shp_list))data_table <- rbind(data_table,
                                                 data.frame(st_read(shp_list[i])))
sec <- data_table %>%
  dplyr::select(UID, "Road_type" = Rod_typ, "BC_c" = BC_c_mn, "CPC" = CPC_mn, "BC_LC" = BC_NR_L_mn,
                "CO2_c" = CO2_c_mn)

boot_list <- sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_BC_LC <- sec %>%
  dplyr::select(UID, BC_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)


boot_list_CPC <- sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_mean_drive_pass_means.csv")
write.csv(boot_list_BC_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_LC_mean_drive_pass_means.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CPC_mean_drive_pass_means.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CO2_mean_drive_pass_means.csv")

############################## Mean for point data #############################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN")  

sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_sec.csv", sep = ",", 
             header = TRUE)
sec <- sec %>%
  dplyr::select(UID, Road_type, BC_c, CPC, CO2_c, BC_NR_LC)

boot_list <- sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_BC_LC <- sec %>%
  dplyr::select(UID, BC_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)


boot_list_CPC <- sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_mean.csv")
write.csv(boot_list_BC_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_LC_mean.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CPC_mean.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CO2_mean.csv")


############################ Median for point file #############################
setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN")  

KAN_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_sec.csv", sep = ",", 
                 header = TRUE)
KAN_sec <- KAN_sec %>%
  dplyr::select(UID, Road_type, BC_c, CPC, CO2_c, BC_NR_LC)


boot_list <- KAN_sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_LC <- KAN_sec %>%
  dplyr::select(UID, BC_NR_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_NR_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_CPC <- KAN_sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- KAN_sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_median.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CPC_median.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CO2_median.csv")
write.csv(boot_list_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_LC_median.csv")
# KAN_sec_707 <- KAN_sec %>%
#   filter(UID == "KAN_707")
# median_boot <- function(x, i) {median(x[i], na.rm = TRUE)}
# boots_mean <- boot::boot(KAN_sec_707$BC_c, statistic = median_boot, R = 10000)
# ggplot(data.frame(boots_mean), aes(x = V1)) + geom_histogram() 


############################ Median for 30m data ###############################
setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/Final_layers")  
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/Final_layers"
shp_list <- list.files(dir, pattern = "\\.shp$")
data_table <- data.frame()

### For all shapefiles in a folder
for(i in seq_along(shp_list))data_table <- rbind(data_table,
                                                 data.frame(st_read(shp_list[i])))
KAN_sec <- data_table %>%
  dplyr::select(UID, "Road_type" = Rod_typ, "BC_c" = BC_c_mn, "CPC" = CPC_mn, "BC_LC" = BC_NR_L_mn,
                "CO2_c" = CO2_c_mn)


boot_list <- KAN_sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_LC <- KAN_sec %>%
  dplyr::select(UID, BC_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_CPC <- KAN_sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- KAN_sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_median_drive_pass_means.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CPC_median_drive_pass_means.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_CO2_median_drive_pass_means.csv")
write.csv(boot_list_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_Bootstrap_BC_LC_median_drive_pass_means.csv")


beepr::beep()

####################################### CBD ####################################

############################## Mean for 30m data ###############################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/Final_layers")  
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/Final_layers"
shp_list <- list.files(dir, pattern = "\\.shp$")
data_table <- data.frame()

for(i in seq_along(shp_list))data_table <- rbind(data_table,
                                                 data.frame(st_read(shp_list[i])))
sec <- data_table %>%
  dplyr::select(UID, "Road_type" = Rod_typ, "BC_c" = BC_c_mn, "CPC" = CPC_mn, "BC_LC" = BC_NR_L_mn,
                "CO2_c" = CO2_c_mn)

boot_list <- sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_BC_LC <- sec %>%
  dplyr::select(UID, BC_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)


boot_list_CPC <- sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_mean_drive_pass_means.csv")
write.csv(boot_list_BC_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_LC_mean_drive_pass_means.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CPC_mean_drive_pass_means.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CO2_mean_drive_pass_means.csv")

############################## Mean for point data #############################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD")  

sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_sec.csv", sep = ",", 
             header = TRUE)
sec <- sec %>%
  dplyr::select(UID, Road_type, BC_c, CPC, CO2_c, BC_NR_LC)

boot_list <- sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_BC_LC <- sec %>%
  dplyr::select(UID, BC_NR_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_NR_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)


boot_list_CPC <- sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_mean.csv")
write.csv(boot_list_BC_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_LC_mean.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CPC_mean.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CO2_mean.csv")


############################ Median for point file #############################
setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD")  

CBD_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_sec.csv", sep = ",", 
                  header = TRUE)
CBD_sec <- CBD_sec %>%
  dplyr::select(UID, Road_type, BC_c, CPC, CO2_c, BC_NR_LC)


boot_list <- CBD_sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_LC <- CBD_sec %>%
  dplyr::select(UID, BC_NR_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_NR_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_CPC <- CBD_sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- CBD_sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_median.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CPC_median.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CO2_median.csv")
write.csv(boot_list_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_LC_median.csv")
# CBD_sec_707 <- CBD_sec %>%
#   filter(UID == "CBD_707")
# median_boot <- function(x, i) {median(x[i], na.rm = TRUE)}
# boots_mean <- boot::boot(CBD_sec_707$BC_c, statistic = median_boot, R = 10000)
# ggplot(data.frame(boots_mean), aes(x = V1)) + geom_histogram() 


############################ Median for 30m data ###############################
setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/Final_layers")  
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/Final_layers"
shp_list <- list.files(dir, pattern = "\\.shp$")
data_table <- data.frame()

### For all shapefiles in a folder
for(i in seq_along(shp_list))data_table <- rbind(data_table,
                                                 data.frame(st_read(shp_list[i])))
CBD_sec <- data_table %>%
  dplyr::select(UID, "Road_type" = Rod_typ, "BC_c" = BC_c_mn, "CPC" = CPC_mn, "BC_LC" = BC_NR_L_mn,
                "CO2_c" = CO2_c_mn)


boot_list <- CBD_sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_LC <- CBD_sec %>%
  dplyr::select(UID, BC_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_CPC <- CBD_sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- CBD_sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_median_drive_pass_means.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CPC_median_drive_pass_means.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_CO2_median_drive_pass_means.csv")
write.csv(boot_list_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_Bootstrap_BC_LC_median_drive_pass_means.csv")


beepr::beep()


############################### MAL ############################################

source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
set.seed(108)

############################## Mean for 30m data ###############################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Final_layers")  
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Final_layers"
shp_list <- list.files(dir, pattern = "\\.shp$")
data_table <- data.frame()

for(i in seq_along(shp_list))data_table <- rbind(data_table,
                                                 data.frame(st_read(shp_list[i])))
sec1 <- data_table %>%
  dplyr::select(UID, "Road_type" = Rod_typ, "BC_c" = BC_c_mn, "CPC" = CPC_mn, "BC_LC" = BC_NR_L_mn,
                "CO2_c" = CO2_c_mn)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers")  
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers"
shp_list <- list.files(dir, pattern = "\\.shp$")
data_table <- data.frame()

for(i in seq_along(shp_list))data_table <- rbind(data_table,
                                                 data.frame(st_read(shp_list[i])))
sec2 <- data_table %>%
  dplyr::select(UID, "Road_type" = Rod_typ, "BC_c" = BC_c_mn, "CPC" = CPC_mn, "BC_LC" = BC_NR_L_mn,
                "CO2_c" = CO2_c_mn)

sec <- rbind(sec1, sec2)

boot_list <- sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_BC_LC <- sec %>%
  dplyr::select(UID, BC_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)


boot_list_CPC <- sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_BC_mean_drive_pass_means.csv")
write.csv(boot_list_BC_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_BC_LC_mean_drive_pass_means.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_CPC_mean_drive_pass_means.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_CO2_mean_drive_pass_means.csv")

############################## Mean for point data #############################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1")  

sec1 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_sec.csv", sep = ",", 
              header = TRUE)
sec1 <- sec1 %>%
  dplyr::select(UID, Road_type, BC_c, CPC, CO2_c, BC_NR_LC)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")  

sec2 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_sec.csv", sep = ",", 
              header = TRUE)
sec2 <- sec2 %>%
  dplyr::select(UID, Road_type, BC_c, CPC, CO2_c, BC_NR_LC)

sec <- rbind(sec1, sec2)

boot_list <- sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_BC_LC <- sec %>%
  dplyr::select(UID, BC_NR_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$BC_NR_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)


boot_list_CPC <- sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Mean_N_Observations" = mn_y, 
                "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_BC_mean.csv")
write.csv(boot_list_BC_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_BC_LC_mean.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_CPC_mean.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_CO2_mean.csv")


############################ Median for point file #############################
setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1")  

MAL_sec1 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_sec.csv", sep = ",", 
                  header = TRUE)
MAL_sec1 <- MAL_sec1 %>%
  dplyr::select(UID, Road_type, BC_c, CPC, CO2_c, BC_NR_LC)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")  

MAL_sec2 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_sec.csv", sep = ",", 
                  header = TRUE)
MAL_sec2 <- MAL_sec2 %>%
  dplyr::select(UID, Road_type, BC_c, CPC, CO2_c, BC_NR_LC)

MAL_sec <- rbind(MAL_sec1, MAL_sec2)

boot_list <- MAL_sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_LC <- MAL_sec %>%
  dplyr::select(UID, BC_NR_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_NR_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_CPC <- MAL_sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- MAL_sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_BC_median.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_CPC_median.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_CO2_median.csv")
write.csv(boot_list_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_BC_LC_median.csv")
# MAL_sec_707 <- MAL_sec %>%
#   filter(UID == "MAL_707")
# median_boot <- function(x, i) {median(x[i], na.rm = TRUE)}
# boots_mean <- boot::boot(MAL_sec_707$BC_c, statistic = median_boot, R = 10000)
# ggplot(data.frame(boots_mean), aes(x = V1)) + geom_histogram() 


############################ Median for 30m data ###############################
setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Final_layers")  
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Final_layers"
shp_list <- list.files(dir, pattern = "\\.shp$")
data_table <- data.frame()

### For all shapefiles in a folder
for(i in seq_along(shp_list))data_table <- rbind(data_table,
                                                 data.frame(st_read(shp_list[i])))
MAL_sec1 <- data_table %>%
  dplyr::select(UID, "Road_type" = Rod_typ, "BC_c" = BC_c_mn, "CPC" = CPC_mn, "BC_LC" = BC_NR_L_mn,
                "CO2_c" = CO2_c_mn)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers")  
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers"
shp_list <- list.files(dir, pattern = "\\.shp$")
data_table <- data.frame()

for(i in seq_along(shp_list))data_table <- rbind(data_table,
                                                 data.frame(st_read(shp_list[i])))
MAL_sec2 <- data_table %>%
  dplyr::select(UID, "Road_type" = Rod_typ, "BC_c" = BC_c_mn, "CPC" = CPC_mn, "BC_LC" = BC_NR_L_mn,
                "CO2_c" = CO2_c_mn)

MAL_sec <- rbind(MAL_sec1, MAL_sec2)


boot_list <- MAL_sec %>%
  dplyr::select(UID, BC_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_LC <- MAL_sec %>%
  dplyr::select(UID, BC_LC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$BC_LC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re, 
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax,
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin)

boot_list_CPC <- MAL_sec %>%
  dplyr::select(UID, CPC) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CPC, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

boot_list_CO2 <- MAL_sec %>%
  dplyr::select(UID, CO2_c) %>%
  nest(-UID) %>% 
  mutate(mapped_data = map(data, ~ boot_var_med(.x$CO2_c, na.rm = TRUE))) %>% 
  unnest(mapped_data) %>%
  dplyr::select(UID, "No_of_observations" = len, "Median_N_Observations" = mn_y, 
                "Median_M_subsamples" = mnd_re, "Mean_M_subsamples" = mn_re,
                "Stddev_M_subsamples" = std_dev, "StdError_N_Observations" = std_er,
                "Lower_25_M_subsamples" = lower_bound, "Y_Max_95" = ymax, 
                "Upper_975_M_subsamples" = upper_bound, "Y_Min_95" = ymin) 

write.csv(boot_list, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_BC_median_drive_pass_means.csv")
write.csv(boot_list_CPC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_CPC_median_drive_pass_means.csv")
write.csv(boot_list_CO2, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_CO2_median_drive_pass_means.csv")
write.csv(boot_list_LC, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_Bootstrap_BC_LC_median_drive_pass_means.csv")


beepr::beep()
