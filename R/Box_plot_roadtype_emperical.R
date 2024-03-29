source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
library(viridis)
library(here)


mal1 <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Final_layers/Corrected_Final_MAL1_Layer.shp", 
                layer = "Corrected_Final_MAL1_Layer")
mal1 <- spTransform(mal1, CRS("+proj=utm +zone=43 ellps=WGS84"))
mal2 <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Final_layers/Corrected_Final_MAL2_Layer.shp", 
                layer = "Corrected_Final_MAL2_Layer")
mal2 <- spTransform(mal2, CRS("+proj=utm +zone=43 ellps=WGS84"))
cbd <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Final_layers/Corrected_Final_CBD_Layer.shp", 
               layer = "Corrected_Final_CBD_Layer")
cbd <- spTransform(cbd, CRS("+proj=utm +zone=43 ellps=WGS84"))
kan <- readOGR("D:/Dropbox/APMfull/MAL_CNG_Paper/Final_layers/Corrected_Final_KAN_Layer.shp", 
               layer = "Corrected_Final_KAN_Layer")
kan <- spTransform(kan, CRS("+proj=utm +zone=43 ellps=WGS84"))

kan$Rod_typ <- kan$Road_type
cbd$Rod_typ <- cbd$i_Rd_ty
# dsn <- "D:/Dropbox/APMfull/MAL_CNG_Paper/Final_layers"
# layer <- "Corrected_Final_KAN_Layer"
# writeOGR(kan, dsn, layer, driver = "ESRI Shapefile", overwrite_layer = T)



fin <- bind(mal1, mal2)
fin_df <- st_as_sf(fin)
layer_final <- as(fin_df, "Spatial")
# dsn <- "D:/Dropbox/APMfull/MAL_CNG_Paper/Final_layers/Combined_layers"
# layer <- "Corrected_Final_MAL_Layer"
# writeOGR(layer_final, dsn, layer, driver = "ESRI Shapefile", overwrite_layer = T)


fin <- bind(kan, cbd, mal1, mal2)
fin_df <- st_as_sf(fin)
layer_final <- as(fin_df, "Spatial")
# dsn <- "D:/Dropbox/APMfull/MAL_CNG_Paper/Final_layers/Combined_layers"
# layer <- "Corrected_Final_Layer"
# writeOGR(layer_final, dsn, layer, driver = "ESRI Shapefile", overwrite_layer = T)



fin_df_all <- fin_df %>%
  dplyr::select("Road_type" = Rod_typ, UID, "BC" = BC_md, "BC_NR" = BC_NR_md, "BC_NR_LC" = BC_LC_md, 
                "BC_c" = BC_c_md, "BC_CF" = BC_CF_md, "CPC" = CPC_md, "CO2_c" = CO2_c_md, 
                "CO2" = CO2_md, "PM2_5" = PM2_5_md, "PM_c" = PM_c_md, "RH" = RH_md, 
                "Speed" = Spd_md, "PM_CF" = PM_CF_md) %>%
  mutate(Area = gsub("_", "", substr(UID, 1, 4)))
fin_df_all[ , c('geometry', 'UID')] <- list(NULL)
fin_df_all <- data.frame(fin_df_all)
fin_df_1 <- data.frame(fin_df_all) %>%
  mutate(Area = "All")

fin_df_all_a <- as.data.frame(fin_df) %>%
  dplyr::select("Road_type" = Rod_typ, UID, "BC" = BC_md, "BC_NR" = BC_NR_md, "BC_NR_LC" = BC_LC_md, 
                "BC_c" = BC_c_md, "BC_CF" = BC_CF_md, "CPC" = CPC_md, "CO2_c" = CO2_c_md, 
                "CO2" = CO2_md, "PM2_5" = PM2_5_md, "PM_c" = PM_c_md, "RH" = RH_md, 
                "Speed" = Spd_md, "PM_CF" = PM_CF_md) %>%
  mutate(Area = gsub("_", "", substr(UID, 1, 4)),
         BC_CO = BC_c / CO2_c, CPC_CO = CPC / CO2_c) %>% 
  filter(Area == "MAL1" | Area == "MAL2") %>% 
  dplyr::select(Road_type, BC_CO, CPC_CO, Speed, BC_c, CPC) %>% 
  group_by(Road_type) %>% 
  summarise_all(funs(ma = max, mi = min, mn = mean, md = median, sd, CV = CV1,
                q10 = quantile(., .1), q25 = quantile(., .25), se = stderr,
                q75 = quantile(., .75), q90 = quantile(., .9), 
                n = sum(!is.na(.))), na.rm = TRUE)

fin <- rbind(fin_df_all, fin_df_1)
Final_stats1 <- fin %>%
  dplyr::select(Area, Road_type, BC, BC_NR, BC_NR_LC, BC_c, CO2, CO2_c, CPC, 
                PM2_5, RH, PM_c, Speed, BC_CF, PM_CF) %>%
  group_by(Area, Road_type) %>%
  summarise_all(funs(ma = max, mi = min, mn = mean, md = median, sd, CV = CV1,
                     q10 = quantile(., .1), q25 = quantile(., .25), se = stderr,
                     q75 = quantile(., .75), q90 = quantile(., .9), 
                     n = sum(!is.na(.))), na.rm = TRUE)
Final_stats2 <- fin %>%
  dplyr::select(Area, BC, BC_NR, BC_NR_LC, BC_c, CO2, CO2_c, CPC, 
                PM2_5, RH, PM_c, Speed, BC_CF, PM_CF) %>%
  group_by(Area) %>%
  summarise_all(funs(ma = max, mi = min, mn = mean, md = median, sd, CV = CV1,
                     q10 = quantile(., .1), q25 = quantile(., .25), se = stderr,
                     q75 = quantile(., .75), q90 = quantile(., .9), 
                     n = sum(!is.na(.))), na.rm = TRUE)
Final_stats <- rbind(Final_stats1, Final_stats2)
# write.csv(Final_stats, "D:/Dropbox/APMfull/MAL_CNG_Paper/layer30m_stats.csv")

fin_df_all$Road_type <- factor(fin_df_all$Road_type, levels = c("Highway", "Arterial",
                                                                "Residential"))
cols <- c("Highway" = "maroon", "Arterial" = "orange", "Residential" = "steelblue")
ticks <- qnorm(c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
labels <- c(1, 5, 10, 25, 50, 75, 90, 95, 99) 

theme_ARU <- list(theme_classic(),
                  theme(legend.text = element_text(size = 32, colour = "black", face = "bold"),
                        legend.title = element_blank(),
                        plot.title = element_text(size = 44, face = "bold", hjust = 0.5), 
                        plot.subtitle = element_text(size = 32), 
                        axis.title = element_text(size = 44, colour = "black", face = "bold"),
                        axis.text = element_text(size = 40, colour = "black", face = "bold"),
                        panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
                        legend.position = "right",
                        strip.background = element_blank(), strip.text = element_blank()))


p2 <- ggplot(fin_df_all, aes(sample = log(BC_NR_LC), color = Road_type)) + 
  stat_qq(size = 3, geom = 'point') + 
  stat_qq_line(size = 2, linetype = 2) + scale_color_manual(values = cols) + 
  scale_x_continuous(breaks = ticks, labels = labels) + 
  labs(x = "Emperical percentile",
       y = expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")")))) + theme_ARU +
  theme(legend.text = element_text(size = 32)) +
  annotate(geom = 'text', label = '', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
p2
ggsave(here("Plots", "BC_All_empper.jpg"), dpi = 300, width = 45, height = 30, units = "cm")

p2 <- ggplot(data = subset(fin_df_all, Area == "KAN"), aes(sample = log(BC_NR_LC), color = Road_type)) + 
  stat_qq(size = 3, geom = 'point') + 
  stat_qq_line(size = 2, linetype = 2) + scale_color_manual(values = cols) + 
  scale_x_continuous(breaks = ticks, labels = labels) + 
  labs(x = "Emperical percentile",
       y = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")")))) + theme_ARU +
  theme(legend.text = element_text(size = 32))
p2
ggsave(here("Plots", "BC_KAN_empper.jpg"), dpi = 300, width = 45, height = 30, units = "cm")

p2 <- ggplot(data = subset(fin_df_all, Area == "CBD"), aes(sample = log(BC_NR_LC), color = Road_type)) + 
  stat_qq(size = 3, geom = 'point') + 
  stat_qq_line(size = 2, linetype = 2) + scale_color_manual(values = cols) + 
  scale_x_continuous(breaks = ticks, labels = labels) + 
  labs(x = "Emperical percentile",
       y = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")")))) + theme_ARU +
  theme(legend.text = element_text(size = 32))
p2
ggsave(here("Plots", "BC_CBD_empper.jpg"), dpi = 300, width = 45, height = 30, units = "cm")


p2 <- ggplot(data = subset(fin_df_all, Area == "MA1" | Area == "MAL2"), aes(sample = log(BC_NR_LC), color = Road_type)) + 
  stat_qq(size = 3, geom = 'point') + 
  stat_qq_line(size = 2, linetype = 2) + scale_color_manual(values = cols) + 
  scale_x_continuous(breaks = ticks, labels = labels) + 
  labs(x = "Emperical percentile",
       y = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")")))) + theme_ARU +
  theme(legend.text = element_text(size = 32)) +
  annotate(geom = 'text', label = '', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)

p2
ggsave(here("Plots", "BC_MAL_empper.jpg"), dpi = 300, width = 45, height = 30, units = "cm")



p3 <- ggplot(fin_df_all, aes(sample = log(CPC), color = Road_type)) + 
  stat_qq(size = 3, geom = 'point') + 
  stat_qq_line(size = 2, linetype = 2) + scale_color_manual(values = cols) + 
  scale_x_continuous(breaks = ticks, labels = labels) + 
  labs(x = "Emperical percentile",
       y = expression(bold(paste("UFP" ," (#", ~cm^{-3}, ")")))) + 
  theme_ARU +
  theme(legend.text = element_text(size = 32)) +
  annotate(geom = 'text', label = '', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
p3
ggsave(here("Plots", "UFPs_All_empper.jpg"), dpi = 300, width = 45, height = 30, units = "cm")

p3 <- ggplot(data = subset(fin_df_all, Area == "KAN"), aes(sample = log(CPC), color = Road_type)) + 
  stat_qq(size = 3, geom = 'point') + 
  stat_qq_line(size = 2, linetype = 2) + 
  scale_color_manual(values = cols) + 
  scale_x_continuous(breaks = ticks, labels = labels) + 
  labs(x = "Emperical percentile",
       y = expression(bold(paste("UFP" ," (#", ~cm^{-3}, ")")))) + 
  theme_ARU +
  theme(legend.text = element_text(size = 32))
p3
ggsave(here("Plots", "UFPs_KAN_empper.jpg"), dpi = 300, width = 45, height = 30, units = "cm")


p3 <- ggplot(data = subset(fin_df_all, Area == "CBD"), aes(sample = log(CPC), color = Road_type)) + 
  stat_qq(size = 3, geom = 'point') + 
  stat_qq_line(size = 2, linetype = 2) + scale_color_manual(values = cols) + 
  scale_x_continuous(breaks = ticks, labels = labels) + 
  labs(x = "Emperical percentile",
       y = expression(bold(paste("UFP" ," (#", ~cm^{-3}, ")")))) + 
  theme_ARU +
  theme(legend.text = element_text(size = 32))
p3
ggsave(here("Plots", "UFPs_CBD_empper.jpg"), dpi = 300, width = 45, height = 30, units = "cm")


p3 <- ggplot(data = subset(fin_df_all, Area == "MAL1" | Area == "MAL2"), aes(sample = log(CPC), color = Road_type)) + 
  stat_qq(size = 3, geom = 'point') + 
  stat_qq_line(size = 2, linetype = 2) + scale_color_manual(values = cols) + 
  scale_x_continuous(breaks = ticks, labels = labels) + 
  labs(x = "Emperical percentile",
       y = expression(bold(paste("UFP" ," (#", ~cm^{-3}, ")")))) + 
  theme_ARU +
  theme(legend.text = element_text(size = 32)) +
  annotate(geom = 'text', label = '', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)

p3
ggsave(here("Plots", "UFPs_MAL_empper.jpg"), dpi = 300, width = 45, height = 30, units = "cm")


p4 <- ggplot(fin_df_all, aes(sample = log(CO2_c), color = Road_type)) + 
  stat_qq(size = 3, geom = 'point') + 
  stat_qq_line(size = 2, linetype = 2) + scale_color_manual(values = cols) + 
  scale_x_continuous(breaks = ticks, labels = labels) + 
  labs(x = "Emperical percentile",
       y = expression(bold(paste(Delta, CO[2], " (ppm)")))) + theme_ARU +
  theme(legend.text = element_text(size = 32)) +
  annotate(geom = 'text', label = '', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
p4
ggsave(here("Plots", "CO2_All_empper.jpg"), dpi = 300, width = 45, height = 30, units = "cm")


p4 <- ggplot(data = subset(fin_df_all, Area == "KAN"), aes(sample = log(CO2_c), color = Road_type)) + 
  stat_qq(size = 3, geom = 'point') + 
  stat_qq_line(size = 2, linetype = 2) + scale_color_manual(values = cols) + 
  scale_x_continuous(breaks = ticks, labels = labels) + 
  labs(x = "Emperical percentile",
       y = expression(bold(paste(Delta, CO[2], " (ppm)")))) + theme_ARU +
  theme(legend.text = element_text(size = 32))
p4
ggsave(here("Plots", "CO2_KAN_empper.jpg"), dpi = 300, width = 45, height = 30, units = "cm")


p4 <- ggplot(data = subset(fin_df_all, Area == "CBD"), aes(sample = log(CO2_c), color = Road_type)) + 
  stat_qq(size = 3, geom = 'point') + 
  stat_qq_line(size = 2, linetype = 2) + scale_color_manual(values = cols) + 
  scale_x_continuous(breaks = ticks, labels = labels) + 
  labs(x = "Emperical percentile",
       y = expression(bold(paste(Delta, CO[2], " (ppm)")))) + theme_ARU +
  theme(legend.text = element_text(size = 32))
p4
ggsave(here("Plots", "CO2_CBD_empper.jpg"), dpi = 300, width = 45, height = 30, units = "cm")



p4 <- ggplot(data = subset(fin_df_all, Area == "MAL1" | Area == "MAL2"), aes(sample = log(CO2_c), color = Road_type)) + 
  stat_qq(size = 3, geom = 'point') + 
  stat_qq_line(size = 2, linetype = 2) + scale_color_manual(values = cols) + 
  scale_x_continuous(breaks = ticks, labels = labels) + 
  labs(x = "Emperical percentile",
       y = expression(bold(paste(Delta, CO[2], " (ppm)")))) + theme_ARU +
  theme(legend.text = element_text(size = 32)) +
  annotate(geom = 'text', label = '', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)

p4
ggsave(here("Plots", "CO2_MAL_empper.jpg"), dpi = 300, width = 45, height = 30, units = "cm")



p4 + theme(legend.text = element_text(size = 14, colour = "black"),
           legend.position = "bottom", legend.key.width = unit(2.5, "cm"))

cols <- c("Highway" = "maroon", "Arterial" = "orange", "Residential" = "steelblue")
fin_df <- fin_df %>%
  dplyr::select("Road_type" = Rod_typ, BC_LC_md, CPC_md)

MAL_data <- subset(fin_df_all, (Area == "MAL1" | Area == "MAL2"))
model_hw <- mgcv::gam(CPC ~ BC_NR_LC, data = subset(MAL_data, Road_type == "Highway"))
summary(model_hw)
model_art <- mgcv::gam(CPC ~ BC_NR_LC, data = subset(MAL_data, Road_type == "Arterial"))
summary(model_art)
model_res <- mgcv::gam(CPC ~ BC_NR_LC, data = subset(MAL_data, Road_type == "Residential"))
summary(model_res)

plot <- ggplot(fin_df_all, aes(x = BC_NR_LC, y = as.numeric(CPC) / 1000)) +
  geom_point(size = 2, alpha = 0.7) + 
  labs(x = expression(bold(paste("BC" ," (", mu, "g", ~m^{-3}, ")"))), 
       y = expression(bold(paste("UFP (#", ~cm^{-3}, ")"))), 
       subtitle = expression(bold(paste(~x10^{3})))) +
  scale_y_continuous() +
  theme_ARU + theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm")) +  geom_hex(bins = 30) + 
  scale_fill_viridis(option = "plasma", limits = c(0, 250)) + 
  geom_smooth(aes(color = Road_type), method = mgcv::gam, size = 2, se = TRUE, formula = y ~ s(x)) +
  scale_color_manual(values = cols) 
plot
ggsave(here("Plots", "BC_vs_UFPs_All_hex.jpg"), dpi = 300, width = 45, height = 30, units = "cm")

plot <- ggplot(data = subset(fin_df_all, Area == "KAN"), aes(x = BC_NR_LC, y = as.numeric(CPC) / 1000)) +
  geom_point(size = 2, alpha = 0.7) + 
  labs(x = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")"))), 
       y = expression(bold(paste("UFP (#", ~cm^{-3}, ")"))), 
       subtitle = expression(bold(paste(~x10^{3})))) +
  scale_y_continuous() +
  theme_ARU + theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm")) +  geom_hex(bins = 30) + 
  scale_fill_viridis(option = "plasma", limits = c(0, 250)) + 
  geom_smooth(data = subset(fin_df_all, Area == "KAN"), aes(color = Road_type), 
              method = mgcv::gam, size = 2, se = TRUE, formula = y ~ s(x)) +
  scale_color_manual(values = cols) 
plot
ggsave(here("Plots", "BC_vs_UFPs_KAN_hex.jpg"), dpi = 300, width = 45, height = 30, units = "cm")


plot <- ggplot(data = subset(fin_df_all, Area == "CBD"), aes(x = BC_NR_LC, y = as.numeric(CPC) / 1000)) +
  geom_point(size = 2, alpha = 0.7) + 
  labs(x = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")"))), 
       y = expression(bold(paste("UFP (#", ~cm^{-3}, ")"))), 
       subtitle = expression(bold(paste(~x10^{3})))) +
  scale_y_continuous() +
  theme_ARU + theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm")) +  geom_hex(bins = 30) + 
  scale_fill_viridis(option = "plasma", limits = c(0, 250)) + 
  geom_smooth(data = subset(fin_df_all, Area == "CBD"), aes(color = Road_type), 
              method = mgcv::gam, size = 2, se = TRUE, formula = y ~ s(x)) +
  scale_color_manual(values = cols) 
plot
ggsave(here("Plots", "BC_vs_UFPs_CBD_hex.jpg"), dpi = 300, width = 45, height = 30, units = "cm")


plot <- ggplot(data = subset(fin_df_all, Area == "MAL1" | Area == "MAL2"), 
               aes(x = BC_NR_LC, y = as.numeric(CPC) / 1000)) +
  geom_point(size = 2, alpha = 0.7) + 
  labs(x = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")"))), 
       y = expression(bold(paste("UFP (#", ~cm^{-3}, ")"))), 
       subtitle = expression(bold(paste(~x10^{3})))) +
  scale_y_continuous() +
  theme_ARU + theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm")) +  
  geom_hex(bins = 30) + 
  scale_fill_viridis(option = "plasma") + 
  geom_smooth(data = subset(fin_df_all, Area == "MAL1" | Area == "MAL2"), 
              aes(color = Road_type), 
              method = mgcv::gam, size = 2, se = TRUE, formula = y ~ s(x)) +
  scale_color_manual(values = cols) 
plot
ggsave(here("Plots", "BC_vs_UFPs_MAL_hex.jpg"), dpi = 300, width = 45, height = 40, units = "cm")

plot1 <- ggplot(data = subset(fin_df_all, Area == "MAL1" | Area == "MAL2"), 
               aes(x = BC_NR_LC, y = as.numeric(CPC) / 1000)) +
  geom_point(size = 2, alpha = 0.7) + 
  labs(x = expression(bold(paste("log 10 BC" ," (", mu, "g",~m^{-3}, ")"))), 
       y = expression(bold(paste("log 10 UFP (#", ~cm^{-3}, ")")))) +
  scale_y_log10() + scale_x_log10() +
  theme_ARU + theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm")) +  
  geom_hex(bins = 30) + 
  scale_fill_viridis(option = "plasma") + 
  geom_smooth(data = subset(fin_df_all, Area == "MAL1" | Area == "MAL2"), 
              aes(color = Road_type), 
              method = lm, size = 2, se = TRUE, formula = y ~ x) +
  scale_color_manual(values = cols) 
plot1
ggsave(here("Plots", "BC_vs_UFPs_MAL_hex_log_lm.jpg"), dpi = 300, width = 45, height = 30, units = "cm")


plot2 <- ggplot(data = subset(fin_df_all, Area == "MAL1" | Area == "MAL2"), 
                aes(x = BC_NR_LC, y = as.numeric(CPC) / 1000)) +
  geom_point(size = 2, alpha = 0.7) + 
  labs(x = expression(bold(paste("log 10 BC" ," (", mu, "g",~m^{-3}, ")"))), 
       y = expression(bold(paste("log 10 UFP (#", ~cm^{-3}, ")")))) +
  scale_y_log10() + scale_x_log10() +
  theme_ARU + theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm")) +  
  geom_hex(bins = 30) + 
  scale_fill_viridis(option = "plasma") + 
  geom_smooth(data = subset(fin_df_all, Area == "MAL1" | Area == "MAL2"), 
              aes(color = Road_type), 
              method = mgcv::gam, size = 2, se = TRUE, formula = y ~ s(x)) +
  scale_color_manual(values = cols) 
plot2
ggsave(here("Plots", "BC_vs_UFPs_MAL_hex_log.jpg"), dpi = 300, width = 45, height = 30, units = "cm")



datah <- fin_df_all 

plo1 <- ggplot(datah, aes(Road_type, BC_NR_LC))+ 
  labs(x = "", y = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, 130), breaks = c(0, 25, 50, 75, 100, 125), 
                     expand = c(0, 0)) + theme_ARU 
plo1
ggsave(here("Plots", "BC_All_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")

plo1 <- ggplot(data = subset(datah, Area == "KAN"), aes(Road_type, BC_NR_LC))+ 
  labs(x = "", y = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, 130), breaks = c(0, 25, 50, 75, 100, 125), 
                     expand = c(0, 0)) + theme_ARU 
plo1
ggsave(here("Plots", "BC_KAN_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")

plo1 <- ggplot(data = subset(datah, Area == "CBD"), aes(Road_type, BC_NR_LC))+ 
  labs(x = "", y = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, 130), breaks = c(0, 25, 50, 75, 100, 125), 
                     expand = c(0, 0)) + theme_ARU 
plo1
ggsave(here("Plots", "BC_CBD_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")

plo1 <- ggplot(data = subset(datah, (Area == "MAL1" | Area == "MAL2")), aes(Road_type, BC_NR_LC))+ 
  labs(x = "", y = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, 130), breaks = c(0, 25, 50, 75, 100, 125), 
                     expand = c(0, 0)) + theme_ARU +
  annotate(geom = 'text', label = '  a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20) 
plo1
ggsave(here("Plots", "BC_MAL_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")



plo2 <- ggplot(datah, aes(Road_type, as.numeric(CPC) / 1000))+ 
  labs(x = "", y = expression(bold(paste("UFP (#", ~cm^{-3}, ")"))), 
       subtitle = expression(bold(paste(~x10^{3})))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, 275), breaks = c(0, 50, 100, 150, 200, 250), 
                     expand = c(0, 0)) + theme_ARU 
plo2
ggsave(here("Plots", "UFPs_All_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")


plo2 <- ggplot(data = subset(datah, Area == "KAN"), aes(Road_type, as.numeric(CPC) / 1000)) + 
  labs(x = "", y = expression(bold(paste("UFP (#", ~cm^{-3}, ")"))), 
       subtitle = expression(bold(paste(~x10^{3})))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, 275), breaks = c(0, 50, 100, 150, 200, 250), 
                     expand = c(0, 0)) + theme_ARU 
plo2
ggsave(here("Plots", "UFPs_KAN_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")


plo2 <- ggplot(data = subset(datah, Area == "CBD"), aes(Road_type, as.numeric(CPC) / 1000))+ 
  labs(x = "", y = expression(bold(paste("UFP (#", ~cm^{-3}, ")"))), 
       subtitle = expression(bold(paste(~x10^{3})))) + 
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, 275), breaks = c(0, 50, 100, 150, 200, 250), 
                     expand = c(0, 0)) + theme_ARU 
plo2
ggsave(here("Plots", "UFPs_CBD_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")

plo2 <- ggplot(data = subset(datah, Area == "MAL1" | Area == "MAL2"), aes(Road_type, as.numeric(CPC) / 1000))+ 
  labs(x = "", y = expression(bold(paste("UFP (#", ~cm^{-3}, ")"))), 
       subtitle = expression(bold(paste(~x10^{3})))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, 275), breaks = c(0, 50, 100, 150, 200, 250), 
                     expand = c(0, 0)) + theme_ARU  +
  annotate(geom = 'text', label = '  b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plo2
ggsave(here("Plots", "UFPs_MAL_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")


plo3 <- ggplot(datah, aes(Road_type, CO2_c))+ 
  labs(x = "", y = expression(bold(paste(Delta, CO[2], " (ppm)")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, 150), breaks = c(0, 25, 50, 75, 100, 125), 
                     expand = c(0, 0)) + theme_ARU
plo3
ggsave(here("Plots", "CO2_All_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")

plo3 <- ggplot(data = subset(datah, Area == "KAN"), aes(Road_type, CO2_c))+ 
  labs(x = "", y = expression(bold(paste(Delta, CO[2], " (ppm)")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, 150), breaks = c(0, 25, 50, 75, 100, 125), 
                     expand = c(0, 0)) + theme_ARU
plo3
ggsave(here("Plots", "CO2_KAN_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")


plo3 <- ggplot(data = subset(datah, Area == "CBD"), aes(Road_type, CO2_c))+ 
  labs(x = "", y = expression(bold(paste(Delta, CO[2], " (ppm)")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, 150), breaks = c(0, 25, 50, 75, 100, 125), 
                     expand = c(0, 0)) + theme_ARU
plo3
ggsave(here("Plots", "CO2_CBD_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")


plo3 <- ggplot(data = subset(datah, Area == "MAL1" | Area == "MAL2"), aes(Road_type, CO2_c))+ 
  labs(x = "", y = expression(bold(paste(Delta, CO[2], " (ppm)")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, 150), breaks = c(0, 25, 50, 75, 100, 125), 
                     expand = c(0, 0)) + theme_ARU +
  annotate(geom = 'text', label = '  c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plo3
ggsave(here("Plots", "CO2_MAL_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")


fin_mal <- bind(mal1, mal2)
fin_df_mal <- st_as_sf(fin_mal)
layer_final <- as(fin_df_mal, "Spatial")
# dsn <- "D:/Dropbox/APMfull/MAL_CNG_Paper/Final_layers/Combined_layers"
# layer <- "Corrected_Final_Layer"
# writeOGR(layer_final, dsn, layer, driver = "ESRI Shapefile", overwrite_layer = T)



fin_df_mal_p <- fin_df_mal %>%
  dplyr::select("Road_type" = Rod_typ, UID, "BC" = BC_md, "BC_NR" = BC_NR_md, "BC_NR_LC" = BC_LC_md, 
                "BC_c" = BC_c_md, "BC_CF" = BC_CF_md, "CPC" = CPC_md, "CO2_c" = CO2_c_md, 
                "CO2" = CO2_md, "PM2_5" = PM2_5_md, "PM_c" = PM_c_md, "RH" = RH_md, 
                "Speed" = Spd_md, "PM_CF" = PM_CF_md) %>%
  mutate(Area = gsub("_", "", substr(UID, 1, 3)))
fin_df_mal_p[ , c('geometry', 'UID')] <- list(NULL)
fin_df_mal_p <- data.frame(fin_df_mal_p)
fin_df_mal_p$Road_type <- factor(fin_df_mal_p$Road_type, levels = c("Highway", "Arterial",
                                                                    "Residential"))


plo3 <- ggplot(data = subset(fin_df_mal_p, Area == "MAL"), aes(x = Road_type, y = BC_NR_LC/CO2_c))+ 
  labs(x = "", y = expression(bold(paste("BC /", Delta, CO[2])))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA)) + theme_ARU +
  annotate(geom = 'text', label = '  d)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plo3
ggsave(here("Plots", "BC_CO2_MAL_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")


plo3 <- ggplot(data = subset(fin_df_mal_p, Area == "MAL"), aes(x = Road_type, y = CPC/CO2_c))+ 
  labs(x = "", y = expression(bold(paste("UFP /", Delta, CO[2])))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA)) + theme_ARU +
  annotate(geom = 'text', label = '  e)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plo3
ggsave(here("Plots", "UFPs_CO2_MAL_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")


plo4 <- ggplot(data = subset(fin_df_mal_p, Area == "MAL"), aes(x = Road_type, y = CPC/BC_NR_LC))+ 
  labs(x = "", y = expression(bold(paste("UFP / BC")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(breaks = c(0, 2000, 4000, 6000), limits = c(0, 6000)) + theme_ARU +
  annotate(geom = 'text', label = '  f)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plo4
ggsave(here("Plots", "UFPs_BC_MAL_boxplot.jpg"), dpi = 300, width = 30, height = 20, units = "cm")

# http://www.sthda.com/english/articles/40-regression-analysis/162-nonlinear-regression-essentials-in-r-polynomial-and-spline-regression-models/

fin_df_stats <- as.data.frame(fin) %>%
  mutate(BC_CO = BC_c / CO2_c, CPC_CO = CPC / CO2_c, CPC_BC = CPC / BC_c) %>% 
  dplyr::select(Road_type, BC_CO, CPC_CO, Area, CPC_BC) %>% 
  group_by(Road_type, Area) %>% 
  summarise_all(funs(ma = max, mi = min, mn = mean, md = median, sd, CV = CV1,
                     q05 = quantile(., .05), q10 = quantile(., .1), 
                     q25 = quantile(., .25), se = stderr,
                     q75 = quantile(., .75), q90 = quantile(., .9), q95 = quantile(., .95),
                     n = sum(!is.na(.))), na.rm = TRUE)
write.csv(fin_df_stats, "quasi_emission_factors.csv")

fin_df_stats_n <- as.data.frame(fin) %>%
  mutate(BC_CO = BC_c / CO2_c, CPC_CO = CPC / CO2_c, CPC_BC = CPC / BC_c) %>% 
  dplyr::select(BC_CO, CPC_CO, Area, CPC_BC) %>% 
  group_by(Area) %>% 
  summarise_all(funs(ma = max, mi = min, mn = mean, md = median, sd, CV = CV1,
                     q05 = quantile(., .05), q10 = quantile(., .1), 
                     q25 = quantile(., .25), se = stderr,
                     q75 = quantile(., .75), q90 = quantile(., .9), q95 = quantile(., .95),
                     n = sum(!is.na(.))), na.rm = TRUE)
write.csv(fin_df_stats_n, "quasi_emission_factors_all.csv")
