source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/UFPs_subsampled_layers")

directory <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/UFPs_subsampled_layers"
shp_list <- list.files(directory, pattern = "\\.shp$")
# The final reference Means of median layer file 
Final_Ref <- st_read("D:/Dropbox/APMfull/MAL_CNG_Paper/Final_layers/Corrected_Final_MAL1_Layer.shp", 
                     layer = "Corrected_Final_MAL1_Layer")
Final_Ref <- st_transform(Final_Ref, crs = "+proj=utm +zone=43 ellps=WGS84")
Final_Reference <- st_as_sf(Final_Ref)
Final_Reference <- data.frame(Final_Reference)
Final_Reference <- Final_Reference %>%
  dplyr::select(Road_ID, everything()) %>%
  mutate_at(c('Road_ID'), as.numeric)
Final_Reference$geometry <- NULL

for (shp in (shp_list)) {
  Residual_layer <- Residual_function(shp, "Median_CPC", Final_Reference, "CPC_md", "MAL1_", "CPC") 
  Residual_layer <- st_as_sf(Residual_layer) 
  Residual_layer <- as(Residual_layer, 'Spatial')
  dsn <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/UFPs_residual_layers"
  layer_name <- gsub(".shp", "", paste("Residual", shp , sep = "_"))
  writeOGR(Residual_layer, dsn, layer_name, driver = "ESRI Shapefile")
} 

beepr::beep()
