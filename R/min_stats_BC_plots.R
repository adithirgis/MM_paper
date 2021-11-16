source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
library(here)
library(ggside)
library(cowplot)

MAL1_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_sec.csv", sep = ",", 
                     header = TRUE) %>%
  mutate(Area = "MAL1")
MAL2_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_sec.csv", sep = ",", 
                     header = TRUE) %>%
  mutate(Area = "MAL2") 

CBD_sec <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_sec.csv", sep = ",", 
                 header = TRUE) %>%
  mutate(Area = "CBD")
KAN_sec <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_sec.csv", sep = ",", 
                    header = TRUE) %>%
  mutate(Area = "KAN")


MAL1_min <- MAL1_sec %>%
  mutate(minute = ceiling_date(as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kolkata"), 
                               unit = "minutes")) %>%
  dplyr::select(minute, Road_type, Speed, Altitude, BC, BC_NR, BC_NR_LC, 
         RH, BC_c, PM2_5, PM_CF, PM_c, CPC, CO2, CO2_c, BC_CF) %>%
  group_by(minute, Road_type) %>%
  summarise_if(is.numeric, funs(ma = max, mi = min, mn = mean, md = median, sd,  
                     q10 = quantile(., .1), q25 = quantile(., .25), se = stderr,
                     q75 = quantile(., .75), q90 = quantile(., .9), 
                     n = sum(!is.na(.))), na.rm = TRUE)

MAL2_min <- MAL2_sec %>%
  mutate(minute = ceiling_date(as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kolkata"), unit = "minutes")) %>%
  dplyr::select(minute, Road_type, Speed, Altitude, BC, BC_NR, BC_NR_LC, 
         RH, BC_c, PM2_5, PM_CF, PM_c, CPC, CO2, CO2_c, BC_CF) %>%
  group_by(minute, Road_type) %>%
  summarise_if(is.numeric, funs(ma = max, mi = min, mn = mean, md = median, sd,  
                     q10 = quantile(., .1), q25 = quantile(., .25), se = stderr,
                     q75 = quantile(., .75), q90 = quantile(., .9), 
                     n = sum(!is.na(.))), na.rm = TRUE)

KAN_min <- KAN_sec %>%
  mutate(minute = ceiling_date(as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kolkata"), 
                               unit = "minutes")) %>%
  dplyr::select(minute, Road_type, Speed, Altitude, BC, BC_NR, BC_NR_LC, 
                RH, BC_c, PM2_5, PM_CF, PM_c, CPC, CO2, CO2_c, BC_CF) %>%
  group_by(minute, Road_type) %>%
  summarise_if(is.numeric, funs(ma = max, mi = min, mn = mean, md = median, sd,  
                     q10 = quantile(., .1), q25 = quantile(., .25), se = stderr,
                     q75 = quantile(., .75), q90 = quantile(., .9), 
                     n = sum(!is.na(.))), na.rm = TRUE)

CBD_min <- CBD_sec %>%
  mutate(minute = ceiling_date(as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kolkata"), 
                               unit = "minutes")) %>%
  dplyr::select(minute, Road_type, Speed, Altitude, BC, BC_NR, BC_NR_LC, 
                RH, BC_c, PM2_5, PM_CF, PM_c, CPC, CO2, CO2_c, BC_CF) %>%
  group_by(minute, Road_type) %>%
  summarise_if(is.numeric, funs(ma = max, mi = min, mn = mean, md = median, sd,  
                     q10 = quantile(., .1), q25 = quantile(., .25), se = stderr,
                     q75 = quantile(., .75), q90 = quantile(., .9), 
                     n = sum(!is.na(.))), na.rm = TRUE)

# write.csv(MAL1_min, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/MAL1_min.csv")
# write.csv(MAL2_min, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/MAL2_min.csv")
# write.csv(MAL1_min, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/CBD_min.csv")
# write.csv(KAN_min, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/KAN_min.csv")

# Final_sec <- rbind(CBD_sec, KAN_sec, MAL1_sec, MAL2_sec)

CBD <- fread("D:/Dropbox/APMfull/Phase_II/CBD/CBD_min.csv", sep = ",", 
             header = TRUE) %>%
  dplyr::select(Road_type, "BC_c" = BC_c_mean, "CPC" = CPC_mean, "CO2_c" = CO2_c_mean,
                "Speed" = Speed_mean, "BC_NR_LC" = BC_NR_LC_mean) %>%
  mutate(Area = "CBD") %>%
  mutate(Speed = Speed * 3.6)
KAN <- fread("D:/Dropbox/APMfull/Phase_II/KAN/KAN_min.csv", sep = ",", 
             header = TRUE) %>%
  dplyr::select(Road_type, "BC_c" = BC_c_mean, "CPC" = CPC_mean, "CO2_c" = CO2_c_mean,
                "Speed" = Speed_mean, "BC_NR_LC" = BC_NR_LC_mean) %>%
  mutate(Area = "KAN") %>%
  mutate(Speed = Speed * 3.6)
MAL1 <- fread("D:/Dropbox/APMfull/Phase_II/MAL1/MAL1_min.csv", sep = ",", 
              header = TRUE) %>%
  dplyr::select(Road_type, "BC_c" = BC_c_mean, "CPC" = CPC_mean, "CO2_c" = CO2_c_mean,
                "Speed" = Speed_mean, "BC_NR_LC" = BC_NR_LC_mean) %>%
  mutate(Area = "MAL1") %>%
  mutate(Speed = Speed * 3.6)
MAL2 <- fread("D:/Dropbox/APMfull/Phase_II/MAL2/MAL2_min.csv", sep = ",", 
              header = TRUE) %>%
  dplyr::select(Road_type, "BC_c" = BC_c_mean, "CPC" = CPC_mean, "CO2_c" = CO2_c_mean,
                "Speed" = Speed_mean, "BC_NR_LC" = BC_NR_LC_mean) %>%
  mutate(Area = "MAL2") %>%
  mutate(Speed = Speed * 3.6)

Final <- rbind(CBD, MAL1, MAL2, KAN)
Final_rt <- Final %>%
  mutate(Road_type = "All")
Final_MAL <- rbind(MAL1, MAL2) %>%
  mutate(Area = "MAL")
Final_MAL_rt <- Final_MAL %>%
  mutate(Road_type = "All")
Final_all <- Final %>%
  mutate(Area = "All")
Final_all_rt <- Final_all %>%
  mutate(Road_type = "All")
Final <- rbind(CBD, MAL1, MAL2, KAN, Final_MAL, Final_all, Final_rt, Final_MAL_rt,Final_all_rt, fill = TRUE)

Final <- subset(Final, !is.na(CO2_c))
Final_BC <- subset(Final, !is.na(BC_NR_LC))
Final_CPC <- subset(Final, !is.na(CPC))

theme_ARU <- list(theme_classic(),
                  theme(legend.text = element_text(size = 32, colour = "black", face = "bold"),
                        legend.title = element_blank(),
                        plot.subtitle = element_text(size = 32), 
                        plot.title = element_text(size = 44, face = "bold", hjust = 0.5), 
                        axis.title = element_text(size = 44, colour = "black", face = "bold"),
                        axis.text = element_text(size = 40, colour = "black", face = "bold"),
                        panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
                        legend.position = "right",
                        strip.background = element_blank(), strip.text = element_blank()))

plot3 <- ggplot(data = subset(Final_all, CO2_c != 0), aes(x = Speed, y = as.numeric(as.character(BC_NR_LC/CO2_c)), 
                              colour = Road_type)) + 
  geom_point(size = 3, alpha = 0.7) + scale_y_log10() +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), y = expression(paste("BC/", CO[2], "in log10 scale"))) + theme_ARU 
plot3
ggsave(here("Plots", "BC_CO2_point_All.jpg"), width = 45, height = 30, units = "cm")


plot4 <- ggplot(data = subset(Final_all, CO2_c != 0), aes(x = Speed, y = as.numeric(as.character(CPC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^1, 10^5)) + geom_hex(bins = 30) + 
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("UFPs" ," (", ~cm^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) +
  scale_fill_viridis(option = "plasma", limits = c(0, 200)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm"))

plot4 %+% annotate("text", x = 60, y = 10, label = "Highway", size = 20) %+% 
  subset(Final_all, Road_type %in% c("Highway")) +
  annotate(geom = 'text', label = 'a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "UFPs_CO2_high_All.jpg"), width = 45, height = 30, units = "cm")

plot4 %+% subset(Final_all, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 10, label = "Arterial", size = 20) +
  annotate(geom = 'text', label = 'b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "UFPs_CO2_art_All.jpg"), width = 45, height = 30, units = "cm")

plot4 %+% subset(Final_all, Road_type %in% c("Residential")) %+%
  annotate("text", x = 60, y = 10, label = "Residential", size = 20) +
  annotate(geom = 'text', label = 'c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "UFPs_CO2_resi_All.jpg"), width = 45, height = 30, units = "cm")

cols <- c("Highway" = "maroon", "Arterial" = "orange", "Residential" = "steelblue")
plot41 <- ggplot(data = subset(Final_all, CO2_c != 0), aes(x = Speed, 
                                                    y = as.numeric(as.character(CPC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^1, 10^5)) + geom_hex(bins = 35) + 
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("UFPs" ," (", ~cm^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) +
  scale_fill_viridis(option = "plasma", limits = c(0, 200)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) +
  geom_rug(data = subset(Final_all, (CO2_c != 0)), aes(colour = Road_type)) +  
  scale_color_manual(values = cols) +
  annotate(geom = 'text', label = 'd)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plot41
ggsave(here("Plots", "UFPs_CO2_all_All.jpg"), width = 45, height = 30, units = "cm")


plot5 <- ggplot(data = subset(Final_all, CO2_c != 0), aes(x = Speed, 
                                                          y = as.numeric(as.character(BC_NR_LC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-2, 10^2)) + geom_hex(bins = 30) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU +  
  scale_fill_viridis(option = "plasma", limits = c(0, 150)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) 

plot5 %+% annotate("text", x = 60, y = 0.01, label = "Highway", size = 20) %+% 
  subset(Final_all, Road_type %in% c("Highway")) +
  annotate(geom = 'text', label = 'a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "BC_CO2_high_All.jpg"), width = 45, height = 30, units = "cm")

plot5 %+% subset(Final_all, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 0.01, label = "Arterial", size = 20) +
  annotate(geom = 'text', label = 'b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "BC_CO2_art_All.jpg"), width = 45, height = 30, units = "cm")

plot5 %+% subset(Final_all, Road_type %in% c("Residential")) %+%
  annotate("text", x = 60, y = 0.01, label = "Residential", size = 20) +
  annotate(geom = 'text', label = 'c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "BC_CO2_resi_All.jpg"), width = 45, height = 30, units = "cm")



plot51 <- ggplot(data = subset(Final_all, CO2_c != 0), aes(x = Speed, 
                                                           y = as.numeric(as.character(BC_NR_LC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-2, 10^2)) + geom_hex(bins = 35) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU +  
  scale_fill_viridis(option = "plasma", limits = c(0, 150)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) +
  annotate(geom = 'text', label = 'd)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plot51
ggsave(here("Plots", "BC_CO2_all_All.jpg"), width = 45, height = 30, units = "cm")



plot3 <- ggplot(data = subset(Final_all, CO2_c != 0), aes(x = Speed, y = CO2_c)) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste(CO[2], "(ppm)")))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^0, 10^4)) + geom_hex(bins = 30) +
  theme_ARU + scale_fill_viridis(option = "plasma", limits = c(0, 150)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm"))


plot3 %+% annotate("text", x = 60, y = 1, label = "Highway", size = 20) %+% 
  subset(Final_all, Road_type %in% c("Highway")) +
  annotate(geom = 'text', label = 'a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "CO2_high_All.jpg"), width = 45, height = 30, units = "cm")

plot3 %+% subset(Final_all, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 1, label = "Arterial", size = 20) +
  annotate(geom = 'text', label = 'b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "CO2_art_All.jpg"), width = 45, height = 30, units = "cm")

plot3 %+% subset(Final_all, Road_type %in% c("Residential")) %+%
  annotate("text", x = 60, y = 1, label = "Residential", size = 20) +
  annotate(geom = 'text', label = 'c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "CO2_resi_All.jpg"), width = 45, height = 30, units = "cm")

plot31 <- ggplot(data = subset(Final_all, CO2_c != 0), aes(x = Speed, y = CO2_c)) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), y = expression(bold(paste(CO[2], "(ppm)")))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^0, 10^4)) + geom_hex(bins = 45) +
  theme_ARU + scale_fill_viridis(option = "plasma", limits = c(0, 150)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) +
  annotate(geom = 'text', label = 'd)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plot31
ggsave(here("Plots", "CO2_all_All.jpg"), width = 45, height = 30, units = "cm")

################################################################################




plot3 <- ggplot(data = subset(Final_MAL, CO2_c != 0), aes(x = Speed, y = as.numeric(as.character(BC_NR_LC/CO2_c)), 
                                                          colour = Road_type)) + 
  geom_point(size = 3, alpha = 0.7) + scale_y_log10() +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC/", CO[2], "in log10 scale")))) + theme_ARU 
plot3
ggsave(here("Plots", "BC_CO2_point_MAL.jpg"), width = 45, height = 30, units = "cm")


plot4 <- ggplot(data = subset(Final_MAL, CO2_c != 0), aes(x = Speed, 
                                                          y = as.numeric(as.character(CPC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^1, 10^5)) + geom_hex(bins = 30) + 
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("UFPs" ," (", ~cm^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) +
  scale_fill_viridis(option = "plasma", limits = c(0, 100)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm"))

plot4 %+% annotate("text", x = 60, y = 10, label = "Highway", size = 20) %+% 
  subset(Final_MAL, Road_type %in% c("Highway")) +
  annotate(geom = 'text', label = 'a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20) 
ggsave(here("Plots", "UFPs_CO2_high_MAL.jpg"), width = 45, height = 30, units = "cm")

plot4 %+% subset(Final_MAL, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 10, label = "Arterial", size = 20) +
  annotate(geom = 'text', label = 'b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "UFPs_CO2_art_MAL.jpg"), width = 45, height = 30, units = "cm")

plot4 %+% subset(Final_MAL, Road_type %in% c("Residential")) %+%
  annotate("text", x = 60, y = 10, label = "Residential", size = 20) +
  annotate(geom = 'text', label = 'c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "UFPs_CO2_resi_MAL.jpg"), width = 45, height = 30, units = "cm")

plot41 <- ggplot(subset(Final_MAL, CO2_c != 0), aes(x = Speed, 
                                                    y = as.numeric(as.character(CPC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^1, 10^5)) + geom_hex(bins = 35) + 
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("UFPs" ," (", ~cm^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) +
  scale_fill_viridis(option = "plasma", limits = c(0, 100)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) +
  annotate(geom = 'text', label = 'd)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plot41
ggsave(here("Plots", "UFPs_CO2_all_MAL.jpg"), width = 45, height = 30, units = "cm")


plot5 <- ggplot(data = subset(Final_MAL, CO2_c != 0), aes(x = Speed, 
                                                          y = as.numeric(as.character(BC_NR_LC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-2, 10^2)) + geom_hex(bins = 30) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU +  
  scale_fill_viridis(option = "plasma", limits = c(0, 100)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm"))

plot5 %+% annotate("text", x = 60, y = 0.01, label = "Highway", size = 20) %+% 
  subset(Final_MAL, Road_type %in% c("Highway")) +
  annotate(geom = 'text', label = 'a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20) 
ggsave(here("Plots", "BC_CO2_high_MAL.jpg"), width = 45, height = 30, units = "cm")

plot5 %+% subset(Final_MAL, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 0.01, label = "Arterial", size = 20) +
  annotate(geom = 'text', label = 'b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "BC_CO2_art_MAL.jpg"), width = 45, height = 30, units = "cm")

plot5 %+% subset(Final_MAL, Road_type %in% c("Residential")) %+%
  annotate("text", x = 60, y = 0.01, label = "Residential", size = 20) +
  annotate(geom = 'text', label = 'c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "BC_CO2_resi_MAL.jpg"), width = 45, height = 30, units = "cm")


plot51 <- ggplot(data = subset(Final_MAL, CO2_c != 0), aes(x = Speed, 
                                                           y = as.numeric(as.character(BC_NR_LC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-2, 10^2)) + geom_hex(bins = 35) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU +  
  scale_fill_viridis(option = "plasma", limits = c(0, 100)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) +
  annotate(geom = 'text', label = 'd)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plot51
ggsave(here("Plots", "BC_CO2_all_MAL.jpg"), width = 45, height = 30, units = "cm")



plot3 <- ggplot(data = subset(Final_MAL, CO2_c != 0), aes(x = Speed, y = CO2_c)) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste(CO[2], "(ppm)")))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^0, 10^4)) + geom_hex(bins = 35) +
  theme_ARU + scale_fill_viridis(option = "plasma", limits = c(0, 100)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm"))


plot3 %+% annotate("text", x = 60, y = 1, label = "Highway", size = 20) %+% 
  subset(Final_MAL, Road_type %in% c("Highway")) +
  annotate(geom = 'text', label = 'a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "CO2_high_MAL.jpg"), width = 45, height = 30, units = "cm")

plot3 %+% subset(Final_MAL, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 1, label = "Arterial", size = 20) +
  annotate(geom = 'text', label = 'b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "CO2_art_MAL.jpg"), width = 45, height = 30, units = "cm")

plot3 %+% subset(Final_MAL, Road_type %in% c("Residential")) %+%
  annotate("text", x = 60, y = 1, label = "Residential", size = 20) +
  annotate(geom = 'text', label = 'c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "CO2_resi_MAL.jpg"), width = 45, height = 30, units = "cm")

plot31 <- ggplot(data = subset(Final_MAL, CO2_c != 0), aes(x = Speed, y = CO2_c)) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste(CO[2], "(ppm)")))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^0, 10^4)) + geom_hex(bins = 45) +
  theme_ARU + scale_fill_viridis(option = "plasma", limits = c(0, 100)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) +
  annotate(geom = 'text', label = 'd)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plot31
ggsave(here("Plots", "CO2_all_MAL.jpg"), width = 45, height = 30, units = "cm")


################################################################################


plot3 <- ggplot(data = subset(KAN, CO2_c != 0), aes(x = Speed, y = as.numeric(as.character(BC_NR_LC/CO2_c)), 
                                                    colour = Road_type)) + 
  geom_point(size = 3, alpha = 0.7) + scale_y_log10() +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC/", CO[2], "in log10 scale")))) + theme_ARU 
plot3
ggsave(here("Plots", "BC_CO2_point_KAN.jpg"), width = 45, height = 30, units = "cm")


plot4 <- ggplot(data = subset(KAN, CO2_c != 0), aes(x = Speed, 
                                                    y = as.numeric(as.character(CPC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^1, 10^5)) + geom_hex(bins = 30) + 
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("UFPs" ," (", ~cm^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) +
  scale_fill_viridis(option = "plasma", limits = c(0, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm"))

plot4 %+% annotate("text", x = 60, y = 10, label = "Highway", size = 20) %+% 
  subset(KAN, Road_type %in% c("Highway")) +
  annotate(geom = 'text', label = 'a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20) 
ggsave(here("Plots", "UFPs_CO2_high_KAN.jpg"), width = 45, height = 30, units = "cm")

plot4 %+% subset(KAN, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 10, label = "Arterial", size = 20) +
  annotate(geom = 'text', label = 'b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "UFPs_CO2_art_KAN.jpg"), width = 45, height = 30, units = "cm")

plot4 %+% subset(KAN, Road_type %in% c("Residential")) %+%
  annotate("text", x = 60, y = 10, label = "Residential", size = 20) +
  annotate(geom = 'text', label = 'c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "UFPs_CO2_resi_KAN.jpg"), width = 45, height = 30, units = "cm")

plot41 <- ggplot(subset(KAN, CO2_c != 0), aes(x = Speed, y = as.numeric(as.character(CPC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^1, 10^5)) + geom_hex(bins = 30) + 
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("UFPs" ," (", ~cm^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) +
  scale_fill_viridis(option = "plasma", limits = c(0, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) +
  annotate(geom = 'text', label = 'd)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plot41
ggsave(here("Plots", "UFPs_CO2_all_KAN.jpg"), width = 45, height = 30, units = "cm")


plot5 <- ggplot(data = subset(KAN, CO2_c != 0), aes(x = Speed, 
                                                    y = as.numeric(as.character(BC_NR_LC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-2, 10^2)) + geom_hex(bins = 30) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU +  
  scale_fill_viridis(option = "plasma", limits = c(0, 50)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm"))

plot5 %+% annotate("text", x = 60, y = 0.01, label = "Highway", size = 20) %+% 
  subset(KAN, Road_type %in% c("Highway")) +
  annotate(geom = 'text', label = 'a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "BC_CO2_high_KAN.jpg"), width = 45, height = 30, units = "cm")

plot5 %+% subset(KAN, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 0.01, label = "Arterial", size = 20) +
  annotate(geom = 'text', label = 'b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "BC_CO2_art_KAN.jpg"), width = 45, height = 30, units = "cm")

plot5 %+% subset(KAN, Road_type %in% c("Residential")) %+%
  annotate("text", x = 60, y = 0.01, label = "Residential", size = 20) +
  annotate(geom = 'text', label = 'c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "BC_CO2_resi_KAN.jpg"), width = 45, height = 30, units = "cm")



plot51 <- ggplot(data = subset(KAN, CO2_c != 0), aes(x = Speed, 
                                                     y = as.numeric(as.character(BC_NR_LC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-2, 10^2)) + geom_hex(bins = 35) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU +  
  scale_fill_viridis(option = "plasma", limits = c(0, 50)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) +
  annotate(geom = 'text', label = 'd)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plot51
ggsave(here("Plots", "BC_CO2_all_KAN.jpg"), width = 45, height = 30, units = "cm")



plot3 <- ggplot(data = subset(KAN, CO2_c != 0), aes(x = Speed, y = CO2_c)) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste(CO[2], "(ppm)")))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^0, 10^4)) + geom_hex(bins = 30) +
  theme_ARU + scale_fill_viridis(option = "plasma", limits = c(0, 100)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm"))


plot3 %+% annotate("text", x = 60, y = 1, label = "Highway", size = 20) %+% 
  subset(KAN, Road_type %in% c("Highway")) +
  annotate(geom = 'text', label = 'a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "CO2_high_KAN.jpg"), width = 45, height = 30, units = "cm")

plot3 %+% subset(KAN, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 1, label = "Arterial", size = 20) +
  annotate(geom = 'text', label = 'b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "CO2_art_KAN.jpg"), width = 45, height = 30, units = "cm")

plot3 %+% subset(KAN, Road_type %in% c("Residential")) %+%
  annotate("text", x = 60, y = 1, label = "Residential", size = 20) +
  annotate(geom = 'text', label = 'c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "CO2_resi_KAN.jpg"), width = 45, height = 30, units = "cm")

plot31 <- ggplot(data = subset(KAN, CO2_c != 0), aes(x = Speed, y = CO2_c)) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste(CO[2], "(ppm)")))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^0, 10^4)) + geom_hex(bins = 30) +
  theme_ARU + scale_fill_viridis(option = "plasma", limits = c(0, 100)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) +
  annotate(geom = 'text', label = 'd)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plot31
ggsave(here("Plots", "CO2_all_KAN.jpg"), width = 45, height = 30, units = "cm")


################################################################################

plot3 <- ggplot(data = subset(CBD, CO2_c != 0), aes(x = Speed, y = as.numeric(as.character(BC_NR_LC/CO2_c)), 
                                                    colour = Road_type)) + 
  geom_point(size = 3, alpha = 0.7) + scale_y_log10() +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC/", CO[2], "in log10 scale")))) + theme_ARU 
plot3
ggsave(here("Plots", "BC_CO2_point_CBD.jpg"), width = 45, height = 30, units = "cm")


plot4 <- ggplot(data = subset(CBD, CO2_c != 0), aes(x = Speed, y = as.numeric(as.character(CPC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^1, 10^5)) + geom_hex(bins = 35) + 
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("UFPs" ," (", ~cm^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) +
  scale_fill_viridis(option = "plasma", limits = c(0, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) 

plot4 %+% annotate("text", x = 60, y = 10, label = "Highway", size = 20) %+% 
  subset(CBD, Road_type %in% c("Highway")) +
  annotate(geom = 'text', label = 'a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "UFPs_CO2_high_CBD.jpg"), width = 45, height = 30, units = "cm")

plot4 %+% subset(CBD, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 10, label = "Arterial", size = 20) +
  annotate(geom = 'text', label = 'b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "UFPs_CO2_art_CBD.jpg"), width = 45, height = 30, units = "cm")

# plot4 %+% subset(CBD, Road_type %in% c("Residential")) %+%
#   annotate("text", x = 60, y = 10, label = "Residential", size = 20)
# ggsave(here("Plots", "UFPs_CO2_resi_CBD.jpg"), width = 45, height = 30, units = "cm")

plot41 <- ggplot(subset(CBD, CO2_c != 0), aes(x = Speed, y = as.numeric(as.character(CPC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^1, 10^5)) + geom_hex(bins = 35) + 
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("UFPs" ," (", ~cm^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) +
  scale_fill_viridis(option = "plasma", limits = c(0, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) +
  annotate(geom = 'text', label = 'c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plot41
ggsave(here("Plots", "UFPs_CO2_all_CBD.jpg"), width = 45, height = 30, units = "cm")


plot5 <- ggplot(data = subset(CBD, CO2_c != 0), aes(x = Speed, 
                                                    y = as.numeric(as.character(BC_NR_LC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-2, 10^2)) + geom_hex(bins = 30) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU +  
  scale_fill_viridis(option = "plasma", limits = c(0, 70)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) 

plot5 %+% annotate("text", x = 60, y = 0.01, label = "Highway", size = 20) %+% 
  subset(CBD, Road_type %in% c("Highway")) +
  annotate(geom = 'text', label = 'a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "BC_CO2_high_CBD.jpg"), width = 45, height = 30, units = "cm")

plot5 %+% subset(CBD, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 0.01, label = "Arterial", size = 20) +
  annotate(geom = 'text', label = 'b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "BC_CO2_art_CBD.jpg"), width = 45, height = 30, units = "cm")

# plot5 %+% subset(CBD, Road_type %in% c("Residential")) %+%
#   annotate("text", x = 60, y = 0.01, label = "Residential", size = 20)
# ggsave(here("Plots", "BC_CO2_resi_CBD.jpg"), width = 45, height = 30, units = "cm")



plot51 <- ggplot(data = subset(CBD, CO2_c != 0), aes(x = Speed, 
                                                     y = as.numeric(as.character(BC_NR_LC/CO2_c)))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-2, 10^2)) + geom_hex(bins = 30) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")/", CO[2], " (ppm)")))) + theme_ARU +  
  scale_fill_viridis(option = "plasma", limits = c(0, 70)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) +
  annotate(geom = 'text', label = 'c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plot51
ggsave(here("Plots", "BC_CO2_all_CBD.jpg"), width = 45, height = 30, units = "cm")



plot3 <- ggplot(data = subset(CBD, CO2_c != 0), aes(x = Speed, y = CO2_c)) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste(CO[2], "(ppm)")))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^0, 10^4)) + geom_hex(bins = 30) +
  theme_ARU + scale_fill_viridis(option = "plasma", limits = c(0, 70)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm"))


plot3 %+% annotate("text", x = 60, y = 1, label = "Highway", size = 20) %+% 
  subset(CBD, Road_type %in% c("Highway")) +
  annotate(geom = 'text', label = 'a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "CO2_high_CBD.jpg"), width = 45, height = 30, units = "cm")

plot3 %+% subset(CBD, Road_type %in% c("Arterial")) %+%
  annotate("text", x = 60, y = 1, label = "Arterial", size = 20) +
  annotate(geom = 'text', label = 'b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
ggsave(here("Plots", "CO2_art_CBD.jpg"), width = 45, height = 30, units = "cm")

# plot3 %+% subset(CBD, Road_type %in% c("Residential")) %+%
#   annotate("text", x = 60, y = 1, label = "Residential", size = 20)
# ggsave(here("Plots", "CO2_resi_CBD.jpg"), width = 45, height = 30, units = "cm")

plot31 <- ggplot(data = subset(CBD, CO2_c != 0), aes(x = Speed, y = CO2_c)) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste(CO[2], "(ppm)")))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^0, 10^4)) + geom_hex(bins = 30) +
  theme_ARU + scale_fill_viridis(option = "plasma", limits = c(0, 70)) + 
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 20, 40, 60)) + 
  theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) +
  annotate(geom = 'text', label = 'c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plot31
ggsave(here("Plots", "CO2_all_CBD.jpg"), width = 45, height = 30, units = "cm")



###########

y_label_UFPs <- expression(bold(paste("UFPs" ," (", ~cm^{-3}, ")/", CO[2], " (ppm)")))
y_label_BC <- expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")/", CO[2], " (ppm)")))
cols <- c("Highway" = "maroon", "Arterial" = "orange", "Residential" = "steelblue", "All" = "black")

plot_min_speed <- function(Final, Area_type, CPC, S_quartile, UFPs_CO2, UFPs_CO2_m, 
                           label_paper, y_label) {
  Final_for_graph <- Final %>%
    filter(Area == Area_type) %>%
    filter(CO2_c != 0) %>%
    mutate(UFPs_CO2 = as.numeric(as.character(CPC/CO2_c))) %>%
    group_by(Road_type) %>%
    mutate(S_quartile = ntile(Speed, 99)) %>%
    group_by(Road_type, S_quartile) %>%
    summarise(UFPs_CO2_m = mean(UFPs_CO2, na.rm = TRUE),
              Speed_m = mean(Speed, na.rm = TRUE))
  Final_for_graph$Road_type <- factor(Final_for_graph$Road_type, 
                                      levels = c("All", "Highway", "Arterial", "Residential"))
  plot4 <- ggplot(Final_for_graph, aes(x = Speed_m, y = UFPs_CO2_m, colour = Road_type, fill = Road_type)) + 
    geom_line(data = subset(Final_for_graph, (S_quartile <= 90 & S_quartile >= 10)), size = 1.5) + 
    geom_line(data = subset(Final_for_graph, (S_quartile <= 75 & S_quartile >= 25)), size = 3) +
    geom_point(data = subset(Final_for_graph, (S_quartile == 50), colour = Road_type, fill = Road_type), 
               size = 8, shape = 23) +
    labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
         y = y_label) + 
    theme_ARU + scale_color_manual(values = cols) + scale_fill_manual(values = cols) + 
    theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) + 
    annotate(geom = 'text', label = label_paper, x = Inf, y = Inf, hjust = 1, vjust = 1.5, size = 20) 
  # annotate("text", x = 60, y = 10, label = "Highway", size = 20) + 
  plot4
}



plot_min_speed(Final, "All", CPC, 
               S_quartile, UFPs_CO2, UFPs_CO2_m, 'b)', y_label_UFPs)
ggsave(here("Plots", "UFPs_CO2_All.jpg"), width = 45, height = 30, units = "cm")

plot_min_speed(Final, "MAL", CPC, 
               S_quartile, UFPs_CO2, UFPs_CO2_m, 'b)', y_label_UFPs)
ggsave(here("Plots", "UFPs_CO2_MAL.jpg"), width = 45, height = 30, units = "cm")

plot_min_speed_C <- function(Final, Area_type, BC_NR_LC, S_quartile, BC_CO2, BC_CO2_m, 
                           label_paper, y_label) {
  Final_for_graph <- Final %>%
    filter(Area == Area_type) %>%
    filter(CO2_c != 0) %>%
    mutate(BC_CO2 = as.numeric(as.character(BC_NR_LC/CO2_c))) %>%
    group_by(Road_type) %>%
    mutate(S_quartile = ntile(Speed, 99)) %>%
    group_by(Road_type, S_quartile) %>%
    summarise(BC_CO2_m = mean(BC_CO2, na.rm = TRUE),
              Speed_m = mean(Speed, na.rm = TRUE))
  Final_for_graph$Road_type <- factor(Final_for_graph$Road_type, 
                                      levels = c("All", "Highway", "Arterial", "Residential"))
  plot4 <- ggplot(Final_for_graph, aes(x = Speed_m, y = BC_CO2_m, colour = Road_type, fill = Road_type)) + 
    geom_line(data = subset(Final_for_graph, (S_quartile <= 90 & S_quartile >= 10)), size = 1.5) + 
    geom_line(data = subset(Final_for_graph, (S_quartile <= 75 & S_quartile >= 25)), size = 3) +
    geom_point(data = subset(Final_for_graph, (S_quartile == 50), colour = Road_type, fill = Road_type), 
               size = 8, shape = 23) +
    labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
         y = y_label) + 
    theme_ARU + scale_color_manual(values = cols) + scale_fill_manual(values = cols) + 
    theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) + 
    annotate(geom = 'text', label = label_paper, x = Inf, y = Inf, hjust = 1, vjust = 1.5, size = 20) 
  # annotate("text", x = 60, y = 10, label = "Highway", size = 20) + 
  plot4
}

plot_min_speed_C(Final, "All", BC_NR_LC, 
               S_quartile, BC_CO2, BC_CO2_m, 'a)', y_label_BC)
ggsave(here("Plots", "BC_CO2_All.jpg"), width = 45, height = 30, units = "cm")

plot_min_speed_C(Final, "MAL", BC_NR_LC, 
               S_quartile, BC_CO2, BC_CO2_m, 'a)', y_label_BC)
ggsave(here("Plots", "BC_CO2_MAL.jpg"), width = 45, height = 30, units = "cm")



y_label_UFPs <- expression(bold(paste("UFPs" ," (", ~cm^{-3}, ")")))
y_label_BC <- expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")")))


plot_min_speed_BC <- function(Final, Area_type, BC_NR_LC, S_quartile, BC_CO2, BC_CO2_m, 
                           label_paper, y_label) {
  Final_for_graph <- Final %>%
    filter(Area == Area_type) %>%
    group_by(Road_type) %>%
    mutate(S_quartile = ntile(Speed, 99)) %>%
    group_by(Road_type, S_quartile) %>%
    summarise(BC_m = mean(BC_NR_LC, na.rm = TRUE),
              Speed_m = mean(Speed, na.rm = TRUE))
  Final_for_graph$Road_type <- factor(Final_for_graph$Road_type, 
                                      levels = c("All", "Highway", "Arterial", "Residential"))
  plot4 <- ggplot(Final_for_graph, aes(x = Speed_m, y = BC_m, colour = Road_type, fill = Road_type)) + 
    geom_line(data = subset(Final_for_graph, (S_quartile <= 90 & S_quartile >= 10)), size = 1.5) + 
    geom_line(data = subset(Final_for_graph, (S_quartile <= 75 & S_quartile >= 25)), size = 3) +
    geom_point(data = subset(Final_for_graph, (S_quartile == 50), colour = Road_type, fill = Road_type), 
               size = 8, shape = 23) +
    labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
         y = y_label) + 
    theme_ARU + scale_color_manual(values = cols) + scale_fill_manual(values = cols) + 
    theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) + 
    annotate(geom = 'text', label = label_paper, x = Inf, y = Inf, hjust = 1, vjust = 1.5, size = 20) 
  # annotate("text", x = 60, y = 10, label = "Highway", size = 20) + 
  plot4
}

plot_min_speed_BC(Final, "All", BC_NR_LC, 
               S_quartile, BC_CO2, BC_CO2_m, 'a)', y_label_BC)
ggsave(here("Plots", "BC_S_All.jpg"), width = 45, height = 30, units = "cm")

plot_min_speed_BC(Final, "MAL", BC_NR_LC, 
               S_quartile, BC_CO2, BC_CO2_m, 'a)', y_label_BC)
ggsave(here("Plots", "BC_S_MAL.jpg"), width = 45, height = 30, units = "cm")

plot_min_speed_CPC <- function(Final, Area_type, CPC, S_quartile, UFPs_CO2, UFPs_CO2_m, 
                           label_paper, y_label) {
  Final_for_graph <- Final %>%
    filter(Area == Area_type) %>%
    group_by(Road_type) %>%
    mutate(S_quartile = ntile(Speed, 99)) %>%
    group_by(Road_type, S_quartile) %>%
    summarise(CPC_m = mean(CPC, na.rm = TRUE),
              Speed_m = mean(Speed, na.rm = TRUE))
  Final_for_graph$Road_type <- factor(Final_for_graph$Road_type, 
                                      levels = c("All", "Highway", "Arterial", "Residential"))
  plot4 <- ggplot(Final_for_graph, aes(x = Speed_m, y = (CPC_m / 1000), colour = Road_type, fill = Road_type)) + 
    geom_line(data = subset(Final_for_graph, (S_quartile <= 90 & S_quartile >= 10)), size = 1.5) + 
    geom_line(data = subset(Final_for_graph, (S_quartile <= 75 & S_quartile >= 25)), size = 3) +
    geom_point(data = subset(Final_for_graph, (S_quartile == 50), colour = Road_type, fill = Road_type), 
               size = 8, shape = 23) +
    labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
         y = y_label, 
         subtitle = expression(bold(paste(~x10^{3})))) + 
    theme_ARU + scale_color_manual(values = cols) + scale_fill_manual(values = cols) + 
    theme(legend.position = "right", legend.key.height = unit(2.5, "cm")) + 
    annotate(geom = 'text', label = label_paper, x = Inf, y = Inf, hjust = 1, vjust = 1.5, size = 20) 
  # annotate("text", x = 60, y = 10, label = "Highway", size = 20) + 
  plot4
}



plot_min_speed_CPC(Final, "All", CPC, 
               S_quartile, UFPs_CO2, UFPs_CO2_m, 'b)', y_label_UFPs)
ggsave(here("Plots", "UFPs_S_All.jpg"), width = 45, height = 30, units = "cm")

plot_min_speed_CPC(Final, "MAL", CPC, 
               S_quartile, UFPs_CO2, UFPs_CO2_m, 'b)', y_label_UFPs)
ggsave(here("Plots", "UFPs_S_MAL.jpg"), width = 45, height = 30, units = "cm")




theme_ARU <- list(theme_classic(),
                  theme(legend.text = element_text(size = 32, colour = "black", face = "bold"),
                        legend.title = element_blank(),
                        plot.title = element_text(size = 44, face = "bold", hjust = 0.5), 
                        axis.title = element_text(size = 44, colour = "black", face = "bold"),
                        axis.text = element_text(size = 40, colour = "black"),
                        panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
                        legend.position = "right",
                        strip.background = element_blank(), strip.text = element_blank()))

Final$Road_type <- factor(Final$Road_type, 
                          levels = c("All", "Highway", "Arterial", "Residential"))

p <- ggplot(data = subset(Final, Area == "All"), aes(x = Speed, y = as.numeric(as.character(BC_NR_LC/CO2_c)))) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")/", CO[2], " (ppm)")))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits = c(10^-1, 10^1)) +
  theme_ARU + theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm")) + 
  geom_smooth(data = subset(Final, Area == "All"), aes(color = Road_type, fill = Road_type), method = lm, 
              size = 2, se = TRUE, formula = y ~ x) + scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) +  
  annotate(geom = 'text', label = "a) ", x = Inf, y = Inf, hjust = 1, vjust = 1.5, size = 20) +
  geom_xsidedensity(data = subset(Final, Area == "All"), aes(y = stat(density), colour = Road_type, fill = Road_type), alpha = 0.5) +
  geom_ysidedensity(data = subset(Final, Area == "All"), aes(x = stat(density), colour = Road_type, fill = Road_type), alpha = 0.5) +
  scale_xsidey_continuous(breaks = NULL) +
  scale_ysidex_continuous(guide = guide_axis(angle = 90), breaks = NULL) 
p
ggsave(here("Plots", "BC_CO2_All_D.jpg"), width = 45, height = 30, units = "cm")

p <- ggplot(data = subset(Final, Area == "MAL"), aes(x = Speed, y = as.numeric(as.character(BC_NR_LC/CO2_c)))) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")/", CO[2], " (ppm)")))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits = c(10^-1, 10^1)) +
  theme_ARU + theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm")) + 
  geom_smooth(data = subset(Final, Area == "MAL"), aes(color = Road_type, fill = Road_type), method = lm, 
              size = 2, se = TRUE, formula = y ~ x) + scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) +  
  annotate(geom = 'text', label = "a)", x = Inf, y = Inf, hjust = 1, vjust = 1.5, size = 20) +
  geom_xsidedensity(data = subset(Final, Area == "MAL"), aes(y = stat(density), colour = Road_type, fill = Road_type), alpha = 0.5) +
  geom_ysidedensity(data = subset(Final, Area == "MAL"), aes(x = stat(density), colour = Road_type, fill = Road_type), alpha = 0.5) +
  scale_xsidey_continuous(breaks = NULL) +
  scale_ysidex_continuous(guide = guide_axis(angle = 90), breaks = NULL) 
p
ggsave(here("Plots", "BC_CO2_MAL_D.jpg"), width = 45, height = 30, units = "cm")


p <- ggplot(data = subset(Final, Area == "All"), aes(x = Speed, y = as.numeric(as.character(CPC/CO2_c)))) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("UFPs" ," (", ~cm^{-3}, ")/", CO[2], " (ppm)")))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits = c(10^2, 10^4)) +
  theme_ARU + theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm")) + 
  geom_smooth(data = subset(Final, Area == "All"), aes(color = Road_type, fill = Road_type), method = lm, 
              size = 2, se = TRUE, formula = y ~ x) + scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) +  
  annotate(geom = 'text', label = "b)", x = Inf, y = Inf, hjust = 1, vjust = 1.5, size = 20) +
  geom_xsidedensity(data = subset(Final, Area == "All"), aes(y = stat(density), colour = Road_type, fill = Road_type), alpha = 0.5) +
  geom_ysidedensity(data = subset(Final, Area == "All"), aes(x = stat(density), colour = Road_type, fill = Road_type), alpha = 0.5) +
  scale_xsidey_continuous(breaks = NULL) +
  scale_ysidex_continuous(guide = guide_axis(angle = 90), breaks = NULL) 
p
ggsave(here("Plots", "UFPs_CO2_All_D.jpg"), width = 45, height = 30, units = "cm")

p <- ggplot(data = subset(Final, Area == "MAL"), aes(x = Speed, y = as.numeric(as.character(CPC/CO2_c)))) +
  labs(x = expression(bold(paste("Speed (km", ~h^{-1}, ")"))), 
       y = expression(bold(paste("UFPs" ," (", ~cm^{-3}, ")/", CO[2], " (ppm)")))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits = c(10^2, 10^4)) +
  theme_ARU + theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm")) + 
  geom_smooth(data = subset(Final, Area == "MAL"), aes(color = Road_type, fill = Road_type), method = lm, 
              size = 2, se = TRUE, formula = y ~ x) + scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) +  
  annotate(geom = 'text', label = "b) ", x = Inf, y = Inf, hjust = 1, vjust = 1.5, size = 20) +
  geom_xsidedensity(data = subset(Final, Area == "MAL"), aes(y = stat(density), colour = Road_type, fill = Road_type), alpha = 0.5) +
  geom_ysidedensity(data = subset(Final, Area == "MAL"), aes(x = stat(density), colour = Road_type, fill = Road_type), alpha = 0.5) +
  scale_xsidey_continuous(breaks = NULL) +
  scale_ysidex_continuous(guide = guide_axis(angle = 90), breaks = NULL) 
p
ggsave(here("Plots", "UFPs_CO2_MAL_D.jpg"), width = 45, height = 30, units = "cm")
