source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")


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
  stat_smooth() + geom_point(colour = "blue", size = 0.8) + 
  xlab("N: No of unique drive days") + scale_x_continuous(limits = c(0, 20)) + 
  ylab("NRMSE") + ggtitle("NRMSE in Data Only Analysis in Malleshwaram 2 for UFPs") + 
  theme(legend.text = element_text(size = 18),
        plot.title = element_text(size = 24, face = "bold"), 
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, face = "bold"))

plo

plo1 <- ggplot(df, aes(x = as.numeric(as.character(layer_N)), 
                       y = as.numeric(as.character(R_squared)), 
                       color = "R Squared")) + 
  stat_smooth() + geom_point(colour = "blue", size = 0.8) + 
  theme_minimal() + scale_y_continuous(limits = c(0,1)) + 
  scale_x_continuous(limits = c(0,25)) + xlab("N: No of unique drive days") +
  ylab("R Squared") + ggtitle("Monte Carlo Analysis in Malleshwaram 2 for UFPs") + 
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
