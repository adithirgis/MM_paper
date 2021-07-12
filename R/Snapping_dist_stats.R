library(lubridate)
library(hms)
library(stringr)
library(rgdal)
library(readr)
library(sp)
library(tidyverse)
library(sf)
library(dplyr)
library(data.table)


# For mean of the points to road conversion: join the attributes of the point shapefile to that of the road using mean of all the points lying on that 30m road segment(OSM)

setwd("D:/Dropbox/APMfull/Phase_II/MAL1/Joined_snp")
files <- list.files("D:/Dropbox/APMfull/Phase_II/MAL1/Joined_snp", pattern = "\\.shp$")
data1 <- data.frame()
for (each_file in (files)) {
  point_shp <- st_read(each_file)
  point_shp <- st_transform(point_shp, crs = "+proj=utm +zone=43 ellps=WGS84")
  point_df <- data.frame(point_shp) %>%
    dplyr::select("snapped_dist" = snp_dst) %>%
    mutate(Area = "MAL1")
  data1 <- rbind(data1, point_df)
}
setwd("D:/Dropbox/APMfull/Phase_II/MAL2/Joined_snp")
files <- list.files("D:/Dropbox/APMfull/Phase_II/MAL2/Joined_snp", pattern = "\\.shp$")
data2 <- data.frame()
for (each_file in (files)) {
  point_shp <- st_read(each_file)
  point_shp <- st_transform(point_shp, crs = "+proj=utm +zone=43 ellps=WGS84") 
  point_df <- data.frame(point_shp) %>%
    dplyr::select("snapped_dist" = snp_dst) %>%
    mutate(Area = "MAL2") 
  data2 <- rbind(data2, point_df)
}

data4 <- rbind(data1, data2)

data5 <- data4 %>%
  dplyr::select(snapped_dist) %>%
  mutate_at(c('snapped_dist'), as.numeric) %>%
  summarise_all(funs(max, min, mean, median, sd, q25 = quantile(., .25), 
                     q75 = quantile(., .75)), na.rm = TRUE) 
data5$Area <- "Study_Area" 
data6 <- data4 %>%
  dplyr::select(Area, snapped_dist) %>%
  mutate_at(c('snapped_dist'), as.numeric) %>%
  group_by(Area) %>%
  summarise_all(funs(max, min, mean, median, sd, q25 = quantile(., .25), 
                     q75 = quantile(., .75)), na.rm = TRUE) 

data7 <- rbind(data6, data5)

### Chunk for calculating the change the correction factor for Black carbon

CF_BC <- read.csv("D:/Data to share with sir and jonathan/Peenya/MM_CF_2020.csv")
CF_BC <- CF_BC %>%
  mutate(date = as.POSIXct(date, format = "%d-%b-%Y %H:%M:%S", 
                           tz = 'Asia/Kolkata')) %>%
  mutate(month = format(date, "%Y-%m"),
         hour = format(date, "%H")) 
CF_BC <- CF_BC %>%
  mutate_at(c('hour'), as.numeric)
CF_BC[ , c('date', 'X')] <- list(NULL)
CF_BC <- subset(CF_BC, hour <= 13 & hour >= 09)
mean_BC_CF <- mean(CF_BC$BC_CF, na.rm = TRUE)








