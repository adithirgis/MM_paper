source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Joined_shp")  
data_table <- data.frame()
### For all shapefiles in a folder

### OR try this 
stderr <- function(col, na.rm = FALSE) {
  if (na.rm) col <- na.omit(col)
  sqrt(var(col) / length(col))
}

MAL1_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_sec.csv", sep = ",", 
                     header = TRUE) %>%
  mutate(Area = "MAL1")
MAL2_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_sec.csv", sep = ",", 
                     header = TRUE) %>%
  mutate(Area = "MAL2") 
Final_sec <- rbind(MAL1_sec, MAL2_sec)
Final_sec1 <- Final_sec %>%
  mutate(Area = "All")
Final_sec2 <- rbind(Final_sec, Final_sec1)
Final_sec_stats1 <- Final_sec2 %>%
  dplyr::select(Area, Road_type, BC, BC_NR, BC_NR_LC, BC_c, CO2, CO2_c, CPC, 
                PM2_5, RH, PM_c, Speed, BC_CF, PM_CF) %>%
  group_by(Area, Road_type) %>%
  summarise_all(funs(ma = max, mi = min, mn = mean, md = median, sd,  
                     q10 = quantile(., .1), q25 = quantile(., .25), se = stderr,
                     q75 = quantile(., .75), q90 = quantile(., .9), 
                     n = sum(!is.na(.))), na.rm = TRUE)
Final_sec_stats2 <- Final_sec2 %>%
  dplyr::select(Area, BC, BC_NR, BC_NR_LC, BC_c, CO2, CO2_c, CPC, 
                PM2_5, RH, PM_c, Speed, BC_CF, PM_CF) %>%
  group_by(Area) %>%
  summarise_all(funs(ma = max, mi = min, mn = mean, md = median, sd,  
                     q10 = quantile(., .1), q25 = quantile(., .25), se = stderr,
                     q75 = quantile(., .75), q90 = quantile(., .9), 
                     n = sum(!is.na(.))), na.rm = TRUE)
Final_sec_stats <- rbind(Final_sec_stats1, Final_sec_stats2)
write.csv(Final_sec_stats, "D:/Dropbox/APMfull/MAL_CNG_Paper/Points_stats.csv")
beepr::beep()