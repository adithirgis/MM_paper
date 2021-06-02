source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")

MAL1_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_sec.csv", sep = ",", 
                     header = TRUE) %>%
  mutate(Area = "MAL1")
MAL2_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_sec.csv", sep = ",", 
                     header = TRUE) %>%
  mutate(Area = "MAL2") 

MAL1_min <- MAL1_sec %>%
  mutate(minute = ceiling_date(as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kolkata"), 
                               unit = "minutes")) %>%
  dplyr::select(minute, Speed, Altitude, BC, BC_NR, BC_NR_LC, 
         RH, BC_c, PM2_5, PM_CF, PM_c, CPC, CO2, CO2_c, BC_CF) %>%
  group_by(minute) %>%
  summarise_all(funs(ma = max, mi = min, mn = mean, md = median, sd,  
                     q10 = quantile(., .1), q25 = quantile(., .25), se = stderr,
                     q75 = quantile(., .75), q90 = quantile(., .9), 
                     n = sum(!is.na(.))), na.rm = TRUE)

MAL2_min <- MAL2_sec %>%
  mutate(minute = ceiling_date(date, unit = "minutes")) %>%
  dplyr::select(minute, Speed, Altitude, BC, BC_NR, BC_NR_LC, 
         RH, BC_c, PM2_5, PM_CF, PM_c, CPC, CO2, CO2_c, BC_CF) %>%
  group_by(minute) %>%
  summarise_all(funs(ma = max, mi = min, mn = mean, md = median, sd,  
                     q10 = quantile(., .1), q25 = quantile(., .25), se = stderr,
                     q75 = quantile(., .75), q90 = quantile(., .9), 
                     n = sum(!is.na(.))), na.rm = TRUE)

# write.csv(MAL1_min, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_min.csv")
# write.csv(MAL1_min, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_min.csv")

MAL1 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_min.csv", sep = ",", 
              header = TRUE) %>%
  dplyr::select(Road_type, "BC_c" = BC_c_mn, "CPC" = CPC_mn, "CO2_c" = CO2_c_mn,
                "Speed" = Speed_mn, "BC_LC" = BC_NR_LC_mn)
MAL2 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_min.csv", sep = ",", 
              header = TRUE) %>%
  dplyr::select(Road_type, "BC_c" = BC_c_mn, "CPC" = CPC_mn, "CO2_c" = CO2_c_mn,
                "Speed" = Speed_mn, "BC_LC" = BC_NR_LC_mn)

Final <- rbind(MAL1, MAL2) %>%
  mutate(Speed = Speed * 3.6)

Final <- subset(Final, !is.na(CO2_c))
Final_BC <- subset(Final, !is.na(BC_c))
Final_CPC <- subset(Final, !is.na(CPC))

plot3 <- ggplot(Final_BC, aes(x = Speed, y = as.numeric(as.character(BC_c/CO2_c)), 
                              colour = Road_type)) + 
  geom_point(size = 2, alpha = 0.7) + scale_y_log10() +
  labs(x = "Speed (km/h)", y = "BC/CO2 ") + theme_ARU 
plot3
plot4 <- ggplot(Final_CPC, aes(x = Speed, y = as.numeric(as.character(CPC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^1, 10^5)) + geom_hex(bins = 20) + 
  labs(x = "Speed (km/h)", y = "UFPs/CO2 in log scale") + theme_ARU + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) +
  scale_fill_viridis(option = "plasma", limits = c(0, 250)) 

plot4 %+% annotate("text", x = 60, y = 10, label = "Highway", size = 7) %+% 
  subset(Final_CPC, Road_type %in% c("Highway")) 

plot4 %+% subset(Final_CPC, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 10, label = "Arterial", size = 7)
plot4 %+% subset(Final_CPC, Road_type %in% c("Residential")) %+%
  annotate("text", x = 60, y = 10, label = "Residential", size = 7)

plot41 <- ggplot(Final_CPC, aes(x = Speed, y = as.numeric(as.character(CPC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^1, 10^5)) + geom_hex(bins = 20) + 
  labs(x = "Speed (km/h)", y = "UFPs/CO2 in log scale") + theme_ARU + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) +
  scale_fill_viridis(option = "plasma", limits = c(0, 480)) 
plot41

plot5 <- ggplot(Final_BC, aes(x = Speed, y = as.numeric(as.character(BC_c/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-2, 10^2)) + geom_hex(bins = 20) +
  labs(x = "Speed (km/h)", y = "BC/CO2 in log scale") + theme_ARU +  
  scale_fill_viridis(option = "plasma", limits = c(0, 150)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) 

plot5 %+% annotate("text", x = 60, y = 0.01, label = "Highway", size = 7) %+% 
  subset(Final_BC, Road_type %in% c("Highway")) 
plot5 %+% subset(Final_BC, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 0.01, label = "Arterial", size = 7)
plot5 %+% subset(Final_BC, Road_type %in% c("Residential")) %+%
  annotate("text", x = 60, y = 0.01, label = "Residential", size = 7)


plot51 <- ggplot(Final_BC, aes(x = Speed, y = as.numeric(as.character(BC_c/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-2, 10^2)) + geom_hex(bins = 20) +
  labs(x = "Speed (km/h)", y = "BC/CO2 in log scale") + theme_ARU +  
  scale_fill_viridis(option = "plasma", limits = c(0, 380)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60))
plot51


plot3 <- ggplot(Final, aes(x = Speed, y = CO2_c)) +
  labs(x = "Speed (km/h)", y = "CO2 in log scale") + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^0, 10^4)) + geom_hex(bins = 20) +
  theme_ARU + scale_fill_viridis(option = "plasma", limits = c(0, 250)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60))
plot3 %+% annotate("text", x = 60, y = 1, label = "Highway", size = 7) %+% 
  subset(Final, Road_type %in% c("Highway")) 
plot3 %+% subset(Final, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 1, label = "Arterial", size = 7)
plot3 %+% subset(Final, Road_type %in% c("Residential")) %+%
  annotate("text", x = 60, y = 1, label = "Residential", size = 7)

plot31 <- ggplot(Final, aes(x = Speed, y = CO2_c)) +
  labs(x = "Speed (km/h)", y = "CO2 in log scale") + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^0, 10^4)) + geom_hex(bins = 20) +
  theme_ARU + scale_fill_viridis(option = "plasma", limits = c(0, 500)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60))
plot31