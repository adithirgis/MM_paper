source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Final_layers")
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/Final_layers"
shp_list <- list.files(dir, pattern = "\\.shp$")
box_all <- data.frame()
theme_ARU <- list(theme_classic(),
                  stat_summary(fun.data = f, geom = "boxplot", width = 0.4, size = 1.5),  
                  stat_summary(fun.y = mean, colour = "black", geom = "point", size = 2),
                  theme(legend.text = element_text(size = 14), 
                        axis.title = element_text(size = 28, face = "bold"), 
                        axis.text = element_text(size = 28, colour = "black",face = "bold"),
                        panel.border = element_rect(colour = "black", fill = NA, size = 1.2) 
                  ))
for (fo in shp_list) {
  r <- st_read(fo)
  addr <- r %>%
    dplyr::select(UID, Rod_typ, BC_md, BC_CF_md, BC_NR_md, BC_NR_L_md, BC_c_md)
  w <- str_replace_all(substr(fo, 1, 10), "_", "-") 
  addr$date <- as.Date(w, format = '%Y-%m-%d', tz = "Asia/Kolkata")
  addr$month <- as.numeric(as.character(format(addr$date, "%m")))
  box_all <- rbind(box_all, addr)
}

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers")
dir <- "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/Final_layers"
shp_list <- list.files(dir, pattern = "\\.shp$")
box1 <- data.frame()

for (fo in shp_list) {
  r <- st_read(fo)
  addr <- r %>%
    dplyr::select(UID, Rod_typ, BC_md, BC_CF_md, BC_NR_md, BC_NR_L_md, BC_c_md)
  w <- str_replace_all(substr(fo, 1, 10), "_", "-") 
  addr$date <- as.Date(w, format = '%Y-%m-%d', tz = "Asia/Kolkata")
  addr$month <- as.numeric(as.character(format(addr$date, "%m")))
  box1 <- rbind(box1, addr)
}

box <- rbind(box1, box_all)

box$Date <- as.Date(box$date, format = '%Y-%m-%d', tz = "Asia/Kolkata")
box_jan <- subset(box, Date >= as.Date("2020-01-01") & Date <= as.Date("2020-02-27"))
box <- subset(box, Date < as.Date("2020-01-01"))
box_jan$month <- case_when(box_jan$month == "1" ~ "Jan",
                           box_jan$month == "2" ~ "Feb",
                           TRUE ~ as.character(box_jan$month))
box$month <- case_when(box$month == "7" ~ "Jul",
                       box$month == "8" ~ "Aug",
                       box$month == "9" ~ "Sep",
                       box$month == "10" ~ "Oct",
                       box$month == "11" ~ "Nov",
                       box$month == "12" ~ "Dec",
                       TRUE ~ as.character(box$month))
box <- rbind(box, box_jan)

box$size_f = factor(box$month, levels = c('Jul', 'Aug', 'Sep', 'Oct',
                                          'Nov', 'Dec', 'Jan', 'Feb'))

p1 <- ggplot(box, aes(size_f, BC_NR_L_md)) + 
  labs(x = "", y = expression(paste("BC" ," (", mu, "g",~m^{-3}, ")"))) + theme_ARU + theme(axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(0, 130), expand = c(0, 0),breaks = seq(0, 130, by = 50)) + 
  annotate("text", label = "On Road", x = "Sep", y = 120, size = 12, face = "bold")
p1


setwd("D:/Dropbox/APMfull/Colocation CSTEP/CSTEP_co-location_2020/Exp_9_2020_01_15/AE33_CSTEP")
ae33_cstep <- data.frame()
dir <- "D:/Dropbox/APMfull/Colocation CSTEP/CSTEP_co-location_2020/Exp_9_2020_01_15/AE33_CSTEP/"
dat_list <- list.files(dir, pattern = "\\.dat$")
### For all shapefiles in a folder
for(i in seq_along(dat_list))ae33_cstep <- rbind(ae33_cstep, 
                                                 data.frame(read.table(dat_list[i], 
                                                                       header = TRUE, 
                                                                       skip = 4)))
ae33_cstep$date <- with(ae33_cstep, as.POSIXct(paste(as.Date(Date.yyyy.MM.dd.., 
                                                             format = '%Y/%m/%d',
                                                             tz = "Asia/Kolkata"), 
                                                     Time.hh.mm.ss..)))
ae33_cstep1 <- ae33_cstep
ae33_cstep <- ae33_cstep %>%
  mutate(AE33_cstep = BC6. / 1000,
         UVPM_cstep = BC1. / 1000,
         hour = as.numeric(as.character(format(date, "%H"))),
         month = as.numeric(as.character(format(date, "%m"))))
ae33_cstep <- subset(ae33_cstep, AE33_cstep > 0)
ae33_cstep <- subset(ae33_cstep, UVPM_cstep > 0)
ae33_cstep <- subset(ae33_cstep, hour <= 13 & hour >= 9)
ae33_cstep <- subset(ae33_cstep, Status. == 0 | Status. == 256 | Status. == 128)
ae33_cstep <- dplyr::select(ae33_cstep, date, AE33_cstep, UVPM_cstep, month)
ae33_cstep$Date <- as.Date(ae33_cstep$date, format = '%Y-%m-%d', tz ="Asia/Kolkata")
ae33_cstep <- ae33_cstep %>%
  filter(Date != as.Date("2019-08-13") & Date != as.Date("2019-08-18") &
           Date != as.Date("2019-08-14") & Date != as.Date("2019-08-19") &
           Date != as.Date("2019-08-20") & Date != as.Date("2019-08-21") &
           Date != as.Date("2019-08-30") & Date != as.Date("2019-08-31") &
           Date != as.Date("2019-09-01") & Date != as.Date("2019-09-02"))
ae33_cstep_jan <- subset(ae33_cstep, Date >= as.Date("2020-01-01"))
ae33_cstep_jan$month <- case_when(ae33_cstep_jan$month == "1" ~ "Jan",
                                  ae33_cstep_jan$month == "2" ~ "Feb",
                                  TRUE ~ as.character(ae33_cstep_jan$month))
ae33_cstep$month <- case_when(ae33_cstep$month == "7" ~ "Jul",
                              ae33_cstep$month == "8" ~ "Aug",
                              ae33_cstep$month == "9" ~ "Sep",
                              ae33_cstep$month == "10" ~ "Oct",
                              ae33_cstep$month == "11" ~ "Nov",
                              ae33_cstep$month == "12" ~ "Dec",
                              ae33_cstep$month == "1" ~ "Jan",
                              ae33_cstep$month == "2" ~ "Feb",
                              TRUE ~ as.character(ae33_cstep$month))
ae33_cstep <- rbind(ae33_cstep, ae33_cstep_jan)
beepr::beep()


ae33_cstep$size_f1 <- factor(ae33_cstep$month, levels = c('Jul', 'Aug', 'Sep', 'Oct', 
                                                          'Nov', 'Dec', 'Jan', 'Feb'))

p3 <- ggplot(ae33_cstep, aes(size_f1, AE33_cstep))+ 
  labs(x = "", y = expression(paste("BC" ," (", mu, "g",~m^{-3}, ")"))) +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0),breaks = seq(0, 10, by = 5)) +
  theme_ARU +
  annotate("text", label = "Ambient", x = "Sep", y = 14, size = 12, face = "bold")
p3

ylab <- p1$labels$y
p1$labels$y <- p3$labels$y <- " "
library(patchwork)
p1 / p3
grid::grid.draw(grid::textGrob(ylab, x = 0.0, rot = 90))

# write.csv(box, "BC_MM.csv")
# write.csv(ae33_cstep, "BC_CSTEP.csv")
beepr::beep()