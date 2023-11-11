library(dplyr)
library(lubridate)
library(hms)
library(chron)
library(stringr)
library(raster)
library(rgdal)
library(readr)
library(sp) 
library(rgeos)
library(sf) 
library(data.table)

mal1_path <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Final_layers"
mal1_name <- "MAL1"
files <- list.files(mal1_path, pattern = "\\.shp$")
mal1 <- data.frame()
for(i in files) {
  mal1_sf <- st_read(paste0(mal1_path, "/", i)) %>% 
    mutate(month = month.abb[as.numeric(substr(i, 6, 7))], 
           Area = mal1_name)
  mal1 <- rbind(mal1, mal1_sf)
}

mal2_path <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers"
mal2_name <- "MAL2"
files <- list.files(mal2_path, pattern = "\\.shp$")
mal2 <- data.frame()
for(i in files) {
  mal2_sf <- st_read(paste0(mal2_path, "/", i)) %>% 
    mutate(month = month.abb[as.numeric(substr(i, 6, 7))], 
           Area = mal2_name)
  mal2 <- rbind(mal2, mal2_sf)
}

cbd_path <- "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/Final_layers"
cbd_name <- "CBD"
files <- list.files(cbd_path, pattern = "\\.shp$")
cbd <- data.frame()
for(i in files) {
  cbd_sf <- st_read(paste0(cbd_path, "/", i)) %>% 
    mutate(month = month.abb[as.numeric(substr(i, 6, 7))], 
           Area = cbd_name)
  cbd <- rbind(cbd, cbd_sf)
}

kan_path <- "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/Final_layers"
kan_name <- "KAN"
files <- list.files(kan_path, pattern = "\\.shp$")
kan <- data.frame()
for(i in files) {
  kan_sf <- st_read(paste0(kan_path, "/", i)) %>% 
    mutate(month = month.abb[as.numeric(substr(i, 6, 7))], 
           Area = kan_name)
  kan <- rbind(kan, kan_sf)
}

fin_df_all <- rbind(setDT(mal1), setDT(mal2), setDT(kan), setDT(cbd), fill = T)
datah <- fin_df_all 
datah$month <- factor(datah$month, levels = month.abb)

plo1 <- ggplot(datah, aes(month, BC_NR_L_mn))+ 
  labs(x = "", y = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = c(0, 0)) + theme_ARU 
plo1
ggsave(here("Plots", "month_BC_All_boxplot.jpg"), width = 30, height = 20, units = "cm")
ggsave(here("Plots", "month_BC_All_boxplot.pdf"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(data = subset(datah, Area == "KAN"), aes(month, BC_NR_L_mn))+ 
  labs(x = "", y = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA),
                     expand = c(0, 0)) + theme_ARU 
plo1
ggsave(here("Plots", "month_BC_KAN_boxplot.jpg"), width = 30, height = 20, units = "cm")
ggsave(here("Plots", "month_BC_KAN_boxplot.pdf"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(data = subset(datah, Area == "CBD"), aes(month, BC_NR_L_mn))+ 
  labs(x = "", y = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = c(0, 0)) + theme_ARU 
plo1
ggsave(here("Plots", "month_BC_CBD_boxplot.jpg"), width = 30, height = 20, units = "cm")
ggsave(here("Plots", "month_BC_CBD_boxplot.pdf"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(data = subset(datah, (Area == "MAL1" | Area == "MAL2")), aes(month, BC_NR_L_mn))+ 
  labs(x = "", y = expression(bold(paste("BC" ," (", mu, "g",~m^{-3}, ")")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA), # breaks = c(0, 50, 100, 150, 200), 
                     expand = c(0, 0)) + theme_ARU +
  annotate(geom = 'text', label = '  a)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20) 
plo1
ggsave(here("Plots", "month_BC_MAL_boxplot.jpg"), width = 30, height = 20, units = "cm")
ggsave(here("Plots", "month_BC_MAL_boxplot.pdf"), width = 30, height = 20, units = "cm")



plo2 <- ggplot(datah, aes(month, as.numeric(CPC_mn) / 1000))+ 
  labs(x = "", y = expression(bold(paste("UFP (#", ~cm^{-3}, ")"))), 
       subtitle = expression(bold(paste(~x10^{3})))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA),  
                     expand = c(0, 0)) + theme_ARU 
plo2
ggsave(here("Plots", "month_UFPs_All_boxplot.jpg"), width = 30, height = 20, units = "cm")
ggsave(here("Plots", "month_UFPs_All_boxplot.pdf"), width = 30, height = 20, units = "cm")


plo2 <- ggplot(data = subset(datah, Area == "KAN"), aes(month, as.numeric(CPC_mn) / 1000)) + 
  labs(x = "", y = expression(bold(paste("UFP (#", ~cm^{-3}, ")"))), 
       subtitle = expression(bold(paste(~x10^{3})))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = c(0, 0)) + theme_ARU 
plo2
ggsave(here("Plots", "month_UFPs_KAN_boxplot.jpg"), width = 30, height = 20, units = "cm")
ggsave(here("Plots", "month_UFPs_KAN_boxplot.pdf"), width = 30, height = 20, units = "cm")


plo2 <- ggplot(data = subset(datah, Area == "CBD"), aes(month, as.numeric(CPC_mn) / 1000))+ 
  labs(x = "", y = expression(bold(paste("UFP (#", ~cm^{-3}, ")"))), 
       subtitle = expression(bold(paste(~x10^{3})))) + 
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = c(0, 0)) + theme_ARU 
plo2
ggsave(here("Plots", "month_UFPs_CBD_boxplot.jpg"), width = 30, height = 20, units = "cm")
ggsave(here("Plots", "month_UFPs_CBD_boxplot.pdf"), width = 30, height = 20, units = "cm")

plo2 <- ggplot(data = subset(datah, Area == "MAL1" | Area == "MAL2"), aes(month, as.numeric(CPC_mn) / 1000))+ 
  labs(x = "", y = expression(bold(paste("UFP (#", ~cm^{-3}, ")"))), 
       subtitle = expression(bold(paste(~x10^{3})))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA), # breaks = c(0, 50, 100, 150, 200, 250, 300), 
                     expand = c(0, 0)) + theme_ARU  +
  annotate(geom = 'text', label = '  b)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plo2
ggsave(here("Plots", "month_UFPs_MAL_boxplot.jpg"), width = 30, height = 20, units = "cm")
ggsave(here("Plots", "month_UFPs_MAL_boxplot.pdf"), width = 30, height = 20, units = "cm")


plo3 <- ggplot(datah, aes(month, CO2_c_mn))+ 
  labs(x = "", y = expression(bold(paste(Delta, CO[2], " (ppm)")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = c(0, 0)) + theme_ARU
plo3
ggsave(here("Plots", "month_CO2_All_boxplot.jpg"), width = 30, height = 20, units = "cm")
ggsave(here("Plots", "month_CO2_All_boxplot.pdf"), width = 30, height = 20, units = "cm")

plo3 <- ggplot(data = subset(datah, Area == "KAN"), aes(month, CO2_c_mn))+ 
  labs(x = "", y = expression(bold(paste(Delta, CO[2], " (ppm)")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = c(0, 0)) + theme_ARU
plo3
ggsave(here("Plots", "month_CO2_KAN_boxplot.jpg"), width = 30, height = 20, units = "cm")
ggsave(here("Plots", "month_CO2_KAN_boxplot.pdf"), width = 30, height = 20, units = "cm")


plo3 <- ggplot(data = subset(datah, Area == "CBD"), aes(month, CO2_c_mn))+ 
  labs(x = "", y = expression(bold(paste(Delta, CO[2], " (ppm)")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA), 
                     expand = c(0, 0)) + theme_ARU
plo3
ggsave(here("Plots", "month_CO2_CBD_boxplot.jpg"), width = 30, height = 20, units = "cm")
ggsave(here("Plots", "month_CO2_CBD_boxplot.pdf"), width = 30, height = 20, units = "cm")


plo3 <- ggplot(data = subset(datah, Area == "MAL1" | Area == "MAL2"), aes(month, CO2_c_mn))+ 
  labs(x = "", y = expression(bold(paste(Delta, CO[2], " (ppm)")))) +
  stat_summary(fun.data = f, geom = "boxplot", width = 0.2, size = 1.5) +  
  stat_summary(fun.y = mean, colour = "black", geom = "point",size = 4) +
  scale_y_continuous(limits = c(0, NA), # breaks = c(0, 25, 50, 75, 100, 125, 150), 
                     expand = c(0, 0)) + theme_ARU +
  annotate(geom = 'text', label = '  c)', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)
plo3
ggsave(here("Plots", "month_CO2_MAL_boxplot.jpg"), width = 30, height = 20, units = "cm")
ggsave(here("Plots", "month_CO2_MAL_boxplot.pdf"), width = 30, height = 20, units = "cm")
