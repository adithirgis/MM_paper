source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")

MAL1_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_sec.csv", sep = ",", 
                     header = TRUE) %>%
  mutate(Area = "MAL1")
MAL2_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_sec.csv", sep = ",", 
                     header = TRUE) %>%
  mutate(Area = "MAL2") 
CBD_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_sec.csv", sep = ",", 
                     header = TRUE) %>%
  mutate(Area = "CBD")
KAN_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_sec.csv", sep = ",", 
                     header = TRUE) %>%
  mutate(Area = "KAN") 
Final_sec <- rbind(MAL1_sec, MAL2_sec)
Final_sec1 <- Final_sec %>%
  mutate(Area = "MAL",
         date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kolkata")) %>% 
  mutate(week = strftime(date, format = "%V")) 
check_table <- Final_sec1 %>% 
  mutate(day = as.Date(date)) %>% 
  dplyr::select(week, day) %>% 
  group_by(week)  %>% 
  summarise(count_w = n_distinct(day))

p <- ggplot(data = check_table, aes(x = week, y = count_w)) +
  labs(x = expression(bold(paste("week of the year"))), 
       y = expression(bold(paste("# of drive days")))) +
  theme_ARU + geom_col(color = "black", fill = "steelblue", size = 1, width = 0.7) +
  theme(axis.title = element_text(size = 22, face = "bold"), 
        axis.text = element_text(size = 26, face = "bold")) 
p
ggsave(here("Plots", "MAL1_2_weekly_rides.jpg"), width = 45, height = 25, units = "cm")


Final_all <- rbind(MAL1_sec, MAL2_sec, CBD_sec, KAN_sec, Final_sec1)
Final_all_2 <- Final_all %>%
  mutate(Area = "All")
Final_sec2 <- rbind(Final_all, Final_all_2)
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