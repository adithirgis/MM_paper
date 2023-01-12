source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
library(here)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/UFPs_subsampled_layers")

df <- data.frame()
directory <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/UFPs_subsampled_layers"
list_files <- list.files(directory, pattern = "\\.shp$")
# Final reference road shapefile of the concerned pollutant from 
# which error needs to be determined 
Final_reference <- st_read("D:/Dropbox/APMfull/MAL_CNG_Paper/Final_layers/Corrected_Final_MAL2_Layer.shp") 
# Final reference road shapefile should be converted to a projected coordinate 
# system after which it can be converted to a shapefile 
Final_reference <- st_transform(Final_reference, crs = "+proj=utm +zone=43 ellps=WGS84")
Final_reference <- st_as_sf(Final_reference)
row_no_ref <- as.numeric(as.character(nrow(Final_reference)))
Final_reference <- data.frame(Final_reference)
Final_reference$geometry <- NULL

for (each_file in (list_files)) {
  # Each shapefile of the pollutant also needs to be treated the same way 
  new_df <- R2_NRMSE_function(each_file, Final_reference, "Median_CPC", "CPC_md")
  df <- rbind(df, new_df) 
} 

# Plots
plo <- ggplot(df, aes(x = as.numeric(as.character(layer_N)), 
                      y = as.numeric(as.character(nrmse_mean_not_sum)))) + 
  stat_smooth() + geom_point(colour = "blue", size = 1.5) + 
  xlab("number of drive days") + scale_x_continuous(limits = c(0, 20)) + 
  ylab("NRMSE") + ggtitle("NRMSE in Data Only Analysis in Malleshwaram 2 for UFP") + 
  theme(legend.text = element_text(size = 18),
        plot.title = element_text(size = 24, face = "bold"), 
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, face = "bold"))

plo

plo1 <- ggplot(df, aes(x = as.numeric(as.character(layer_N)), 
                       y = as.numeric(as.character(R_squared)), 
                       color = expression(bold(paste(~R^{2}))))) + 
  stat_smooth() + geom_point(colour = "blue", size = 1.5) + 
  theme_minimal() + scale_y_continuous(limits = c(0,1)) + 
  scale_x_continuous(limits = c(0,25)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + ggtitle("Monte Carlo Analysis in Malleshwaram 2 for UFP") + 
  theme(legend.text = element_text(size=18),
        plot.title = element_text(size =18, face = "bold"), 
        axis.title = element_text(size=20),
        axis.text = element_text(size = 18, face = "bold"), 
        panel.border = element_rect(colour = "black", fill = NA, size = 1))

plo1


# ggsave("KM_PM2.5_NRMSE.jpeg", plot = plo, dpi = 300)
# ggsave("KM_PM2.5_Rsq.jpeg", plot =plo1, dpi =  300)

###### Saving it for further use ######
names(df) <- c("Layer", "Numeric Layer", "Y Mean Using Sum", "Y Mean using Mean",
               "RMSE using Sum", "RMSE using Mean", "Ymax-Ymin", "R Squared", 
               "Y min", "Y max", "F_statistic", "NRMSE", "Reference_mean")
myOutFile <- paste("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Analysis_UFPs_MAL2_Corrected.csv")
write.csv(df, file = myOutFile)
message("Code Completed", domain = NULL)
beepr::beep()

# y <- aggregate(poly) # to dissolve polygons
# dsn <- "D:/Adithi/PRiMER 2019/Mapping/Data/India Layers - DIVA/IND_adm/"
# layer <-"India_Full"
# writeOGR(y, dsn, layer, driver = "ESRI Shapefile")

################################# R-sq UFPs ####################################
theme_MC <- list(stat_smooth(colour = "red", size = 2),  
                 theme_classic(), theme(legend.text = element_text(size = 32),
                                        axis.ticks = element_line(size = 2),
                                        plot.title = element_text(size = 44, face = "bold"), 
                                        axis.title = element_text(size = 44, colour = "black", face = "bold"),
                                        axis.text = element_text(size = 40, colour = "black", face = "bold"), 
                                        panel.border = element_rect(colour = "black", fill = NA, size = 2)))
label_at <- function(n) function(x) ifelse(x %% n == 0, x, "")

CBD_UFPs <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/Analysis_UFPs_CBD_Corrected.csv", sep = ",")

plo1 <- ggplot(CBD_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "UFPs_CBD_MC_Rsq_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(CBD_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "UFPs_CBD_MC_Rsq.jpg"), width = 30, height = 20, units = "cm")


KAN_UFPs <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/Analysis_UFPs_KAN_Corrected.csv", sep = ",")

plo1 <- ggplot(KAN_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "UFPs_KAN_MC_Rsq_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(KAN_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "UFPs_KAN_MC_Rsq.jpg"), width = 30, height = 20, units = "cm")


MAL1_UFPs <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Analysis_UFPs_MAL1_Corrected.csv", sep = ",")

plo1 <- ggplot(MAL1_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                              y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "UFPs_MAL1_MC_Rsq_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(MAL1_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                              y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "UFPs_MAL1_MC_Rsq.jpg"), width = 30, height = 20, units = "cm")



MAL2_UFPs <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Analysis_UFPs_MAL2_Corrected.csv", sep = ",")

plo1 <- ggplot(MAL2_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                              y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "UFPs_MAL2_MC_Rsq_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(MAL2_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                              y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "UFPs_MAL2_MC_Rsq.jpg"), width = 30, height = 20, units = "cm")


MAL_UFPs <- rbind(MAL1_UFPs, MAL2_UFPs)

plo1 <- ggplot(MAL_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC + stat_smooth(method = "loess", colour = "red", size = 2)


plo1
ggsave(here("Plots", "UFPs_MAL_MC_Rsq_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(MAL_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC + stat_smooth(method = "loess", colour = "red", size = 2) +
  annotate(geom = 'text', label = '  c) UFP', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)


plo1
ggsave(here("Plots", "UFPs_MAL_MC_Rsq.jpg"), width = 30, height = 20, units = "cm")


All_UFPs <- rbind(MAL1_UFPs, MAL2_UFPs, KAN_UFPs, CBD_UFPs)

plo1 <- ggplot(All_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC 


plo1
ggsave(here("Plots", "UFPs_All_MC_Rsq_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(All_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC +
  annotate(geom = 'text', label = '  c) UFP', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)

plo1
ggsave(here("Plots", "UFPs_All_MC_Rsq.jpg"), width = 30, height = 20, units = "cm")




################################# NRMSE UFPs ###################################

plo1 <- ggplot(CBD_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "UFPs_CBD_MC_NRMSE_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(CBD_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "UFPs_CBD_MC_NRMSE.jpg"), width = 30, height = 20, units = "cm")



plo1 <- ggplot(KAN_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "UFPs_KAN_MC_NRMSE_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(KAN_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "UFPs_KAN_MC_NRMSE.jpg"), width = 30, height = 20, units = "cm")



plo1 <- ggplot(MAL1_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                              y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC

plo1
ggsave(here("Plots", "UFPs_MAL1_MC_NRMSE_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(MAL1_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                              y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "UFPs_MAL1_MC_NRMSE.jpg"), width = 30, height = 20, units = "cm")




plo1 <- ggplot(MAL2_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                              y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "UFPs_MAL2_MC_NRMSE_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(MAL2_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                              y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC 


plo1
ggsave(here("Plots", "UFPs_MAL2_MC_NRMSE.jpg"), width = 30, height = 20, units = "cm")


plo1 <- ggplot(MAL_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC + stat_smooth(method = "loess", colour = "red", size = 2)


plo1
ggsave(here("Plots", "UFPs_MAL_MC_NRMSE_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(MAL_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC + stat_smooth(method = "loess", colour = "red", size = 2) +
  annotate(geom = 'text', label = 'd) UFP  ', x = Inf, y = Inf, hjust = 1.0, vjust = 1.5, size = 20)


plo1
ggsave(here("Plots", "UFPs_MAL_MC_NRMSE.jpg"), width = 30, height = 20, units = "cm")


plo1 <- ggplot(All_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC 


plo1
ggsave(here("Plots", "UFPs_All_MC_NRMSE_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(All_UFPs, aes(x = as.numeric(as.character(Numeric.Layer)), 
                             y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC +
  annotate(geom = 'text', label = 'd) UFP  ', x = Inf, y = Inf, hjust = 1.0, vjust = 1.5, size = 20)

plo1
ggsave(here("Plots", "UFPs_All_MC_NRMSE.jpg"), width = 30, height = 20, units = "cm")


################################# R-sq BC ######################################

CBD_BC <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/Analysis_BC_CBD_Corrected.csv", sep = ",")

plo1 <- ggplot(CBD_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "BC_CBD_MC_Rsq_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(CBD_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "BC_CBD_MC_Rsq.jpg"), width = 30, height = 20, units = "cm")


KAN_BC <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/Analysis_BC_KAN_Corrected.csv", sep = ",")

plo1 <- ggplot(KAN_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "BC_KAN_MC_Rsq_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(KAN_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC

plo1
ggsave(here("Plots", "BC_KAN_MC_Rsq.jpg"), width = 30, height = 20, units = "cm")


MAL1_BC <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Analysis_BC_MAL1_Corrected.csv", sep = ",")

plo1 <- ggplot(MAL1_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                            y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "BC_MAL1_MC_Rsq_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(MAL1_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                            y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "BC_MAL1_MC_Rsq.jpg"), width = 30, height = 20, units = "cm")



MAL2_BC <- read.csv("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Analysis_BC_MAL2_Corrected.csv", sep = ",")

plo1 <- ggplot(MAL2_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                            y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "BC_MAL2_MC_Rsq_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(MAL2_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                            y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC


plo1
ggsave(here("Plots", "BC_MAL2_MC_Rsq.jpg"), width = 30, height = 20, units = "cm")


MAL_BC <- rbind(MAL1_BC, MAL2_BC)

plo2 <- ggplot(MAL_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC + stat_smooth(method = "loess", colour = "red", size = 2)


plo2
ggsave(here("Plots", "BC_MAL_MC_Rsq_smooth.jpg"), width = 30, height = 20, units = "cm")

plo3 <- ggplot(MAL_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC + stat_smooth(method = "loess", colour = "red", size = 2) +
  annotate(geom = 'text', label = '  a) BC', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)


plo3
ggsave(here("Plots", "BC_MAL_MC_Rsq.jpg"), width = 30, height = 20, units = "cm")


All_BC <- rbind(MAL1_BC, MAL2_BC, KAN_BC, CBD_BC)

plo4 <- ggplot(All_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC + stat_smooth(method = "loess", colour = "red", size = 2)


plo4
ggsave(here("Plots", "BC_All_MC_Rsq_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(All_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(R.Squared)))) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = label_at(0.5)) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab(expression(bold(paste(~R^{2})))) + theme_MC + stat_smooth(method = "loess", colour = "red", size = 2) +
  annotate(geom = 'text', label = '  a) BC', x = -Inf, y = Inf, hjust = 0, vjust = 1.5, size = 20)



plo1
ggsave(here("Plots", "BC_All_MC_Rsq.jpg"), width = 30, height = 20, units = "cm")




################################# NRMSE BC ###################################

plo1 <- ggplot(CBD_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "BC_CBD_MC_NRMSE_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(CBD_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "BC_CBD_MC_NRMSE.jpg"), width = 30, height = 20, units = "cm")



plo1 <- ggplot(KAN_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "BC_KAN_MC_NRMSE_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(KAN_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "BC_KAN_MC_NRMSE.jpg"), width = 30, height = 20, units = "cm")



plo1 <- ggplot(MAL1_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                            y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "BC_MAL1_MC_NRMSE_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(MAL1_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                            y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "BC_MAL1_MC_NRMSE.jpg"), width = 30, height = 20, units = "cm")




plo1 <- ggplot(MAL2_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                            y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "BC_MAL2_MC_NRMSE_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(MAL2_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                            y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC


plo1
ggsave(here("Plots", "BC_MAL2_MC_NRMSE.jpg"), width = 30, height = 20, units = "cm")


plo6 <- ggplot(MAL_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC + stat_smooth(method = "loess", colour = "red", size = 2)


plo6
ggsave(here("Plots", "BC_MAL_MC_NRMSE_smooth.jpg"), width = 30, height = 20, units = "cm")

plo7 <- ggplot(MAL_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC + stat_smooth(method = "loess", colour = "red", size = 2) +
  annotate(geom = 'text', label = 'b) BC  ', x = Inf, y = Inf, hjust = 1.0, vjust = 1.5, size = 20)


plo7
ggsave(here("Plots", "BC_MAL_MC_NRMSE.jpg"), width = 30, height = 20, units = "cm")


plo1 <- ggplot(All_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) + geom_point(colour = "blue", size = 1.5) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC + stat_smooth(method = "loess", colour = "red", size = 2)

plo1
ggsave(here("Plots", "BC_All_MC_NRMSE_smooth.jpg"), width = 30, height = 20, units = "cm")

plo1 <- ggplot(All_BC, aes(x = as.numeric(as.character(Numeric.Layer)), 
                           y = as.numeric(as.character(NRMSE)))) + 
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, 22)) + xlab("number of drive days") +
  ylab("NRMSE") + theme_MC + stat_smooth(method = "loess", colour = "red", size = 2) +
  annotate(geom = 'text', label = 'b) BC  ', x = Inf, y = Inf, hjust = 1.0, vjust = 1.5, size = 20)

plo1
ggsave(here("Plots", "BC_All_MC_NRMSE.jpg"), width = 30, height = 20, units = "cm")