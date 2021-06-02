library(lubridate)
library(scales)
library(zoo)
library(caTools)
library(xts)
library(tidyverse)
library(dplyr)
library(data.table)
library(ggplot2)
library(reshape)
library(hms)
library(stringr)
library(rgdal)
library(readr)
library(sp)
library(sf)
library(foreach)
library(doParallel)
library(raster)
library(rJava)
library(mailR) # to send mail of the results
library(devtools)
library(chron)
library(openair)
library(RPostgreSQL)
library(DBI)
library(raster)
library(viridis)
library(DescTools)


':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}


BC_file_analysis <- function(z) {
  trial<- read.csv(z, header = TRUE, skip = 16, sep = ",")
  trial <- trial %>%
    drop_na(BC)
  date <- substr(sub(".csv$", "", z), 1, 10)
  date <- gsub("_", "-", date)
  trial$date <- with(trial, as.POSIXct(paste(as.Date(Date,  
                                                     tryFormats = c("%d-%m-%Y", 
                                                                    "%Y/%m/%d",
                                                                    "%m/%d/%Y"),
                                                     optional = FALSE, 
                                                     tz = "Asia/Kolkata"), Time),
                                       tz = "Asia/Kolkata"))
  trial$Date <- trial$date  #"%Y/%m/%d", "%d-%m-%Y"
  ef_file <- trial %>%
    filter(Status == 0) %>%
    select(Date, ATN, BC) %>%
    mutate(BC1 = (BC / 1000), LD = BC1 - rollapply(BC1, FUN = mean,
                                                   width = 30, align = "center",
                                                   partial = TRUE)) %>%
    mutate(LD75 = runquantile(LD, 300, 0.75, type = 2,
                              endrule = c("NA")),
           LD25 = runquantile(LD, 300, 0.25, type = 2,
                              endrule = c("NA")),
           BC2 = BC1, BC3 = BC1) %>%
    mutate(BC2 = ifelse(BC2 >= 0, 0, BC2),
           BC2 = ifelse(BC2 < 0, 1, BC2)) %>%
    mutate(BC2 = rollapply(BC2, FUN = mean, width = 5,
                           align = "center",  partial = TRUE),
           cev1 = ifelse((LD > 5 * LD75) |
                           (LD < 5 * LD25), BC1, NA))
  ATN <- ef_file[1, 2]
  ef_file$ATN <- ef_file$ATN-(ATN) # for new filter
  BC_Final <- ef_file
  CEV <- data.frame(ef_file$Date, ef_file$cev1)
  CEV$ef_file.cev1[!is.na(CEV$ef_file.cev1)] <- 1
  CEV <- data.frame(CEV)
  date_file <- data.frame(ef_file$Date, ef_file$BC2, ef_file$BC3)
  CEV <- CEV %>%
    drop_na(ef_file.cev1)
  setDT(CEV)
  setDT(date_file)
  cev_file <- date_file[CEV, on = c('ef_file.Date')]
  cev_file <- cev_file[!(cev_file$ef_file.BC2 == 0), ]
  cev_file <- xts(cev_file, order.by = cev_file$ef_file.Date)
  ef_file  <- data.frame(ef_file)
  CE <- data.frame(index(CEV))
  i <- index(cev_file)
  i_old <- index(cev_file)
  i <- index(cev_file) + 1
  j <- index(cev_file) + 2
  k <- index(cev_file) - 2
  l <- index(cev_file) - 1
  i <- cbind(as.character(i), as.character(i_old), as.character(j),
             as.character(k), as.character(l))
  Date_cev <- data.frame(i)
  remove_cev <- data.frame(Date = unlist(Date_cev, use.names = FALSE))
  Date_Table <- unique(remove_cev[c("Date")])
  if (nrow(Date_Table) == 0 | is.na(nrow(Date_Table))) {
    BC <- ef_file %>%
      mutate(BC_Factor = 1)
  } else {
    Date_Table <- Date_Table %>%
      mutate(BC_Factor = 0,
             Date = as.POSIXct(Date, tz = "Asia/Kolkata"))
    setDT(Date_Table)
    setDT(ef_file)
    BC <- Date_Table[ef_file, on = c('Date')]
    BC$BC_Factor[is.na(BC$BC_Factor)] <- 1
  }
  BC <- BC %>%
    mutate(BC_Fi = BC_Factor * BC1)
  BC$BC_Fi[BC$BC_Fi == 0] <- NA
  BC <- BC %>%
    mutate(Tr = exp(- ATN / 100),
           CF = (1 / (0.88 * Tr + 0.12)),
           BC_Final = BC_Fi * CF)
  BC$BC_Fi[BC$BC_Fi < 0] <- NA
  BC$BC_Fi[is.na(BC$BC_Fi)] <- " "
  BC$BC_Final[BC$BC_Final < 0] <- NA
  BC$BC_Final[is.na(BC$BC_Final)] <- " "
  BC_Final <- BC %>%
    select("date" = Date, "BC" = BC3, "BC_NR" = BC_Fi, "BC_NR_LC" = BC_Final) %>%
    mutate_at(c('BC', 'BC_NR', 'BC_NR_LC'), as.numeric)
  return(list(BC_Final, trial))
}


GPS_file_analysis <- function(z) {
  GPS_f <- read.csv(z, sep = ",")
  GPS_f$date <- gsub(" 00:", " 12:", GPS_f$date, fixed = TRUE)
  GPS_f$date <- strptime(GPS_f$date, "%m/%d/%Y %I:%M:%OS %p")
  GPS_f$date <- as.POSIXct(GPS_f$date, format = '%Y-%m-%d %H:%M:%S', 
                           tz = "Asia/Kolkata")
  GPS_f <- GPS_f %>%
    dplyr::select(date, latitude, longitude, speed, satellites, altitude, 
                  accuracy)
  names(GPS_f) <- c("date", "Latitude", "Longitude", "Speed", "Satellites", 
                    "Altitude", "Accuracy")
  GPS_f
}


CPC_file_analysis <- function(z) {
  CPC_f <- read.csv(z, sep = ",", skip = 17) #CPC
  u <- stringr::str_extract(substr(z, 1, 23), "[0-9]{4}\\_[0-9]{2}\\_[0-9]{2}")
  w <- str_replace_all(u, "_", "-") 
  CPC_f <- CPC_f %>%
    dplyr::select(Time, "Particle_count" = `Concentration....cm³.`) %>%
    mutate(CPC = Particle_count * 5.5,
           date = ymd_hms(paste(w, Time), tz = "Asia/Kolkata")) %>%
    select(date, CPC)
}


CO2_file_analysis <- function(z) {
  CO2_f <- read.delim(z, skip = 1, sep = ",",
                      header = TRUE, row.names = NULL,
                      stringsAsFactors = FALSE)
  CO2_f <- CO2_f[ , 1:ncol(CO2_f)]
  names(CO2_f) <- c("Date", "Time", "CO2", "H2O", "CO2_A", "H2O_A", "Flow_CO2")
  CO2_f <- CO2_f %>%
    mutate(date = as.POSIXct(paste(as.Date(Date, tryFormats = c("%d-%m-%Y",
                                                                "%m/%d/%Y"),
                                           optional = FALSE, tz = "Asia/Kolkata"), Time),
                             tz = "Asia/Kolkata"),
           CO2_c = CO2 - min(CO2, na.rm = TRUE)) %>%
    dplyr::select(date, CO2, CO2_c, Flow_CO2)
}


DT_file_analysis <- function(z) {
  DT_f <- read.delim(z, header = TRUE, sep = ",", row.names = NULL, skip = 28)
  names(DT_f) <- c("Date", "Time", "PM2.5")
  DT_f$Date <- as.Date(DT_f$Date, tryFormats = c("%d-%m-%Y", "%m/%d/%Y"),
                       optional = FALSE, tz = "Asia/Kolkata")
  DT_f$date <- as.POSIXct(strptime(paste(DT_f$Date, DT_f$Time),
                                   format = '%Y-%m-%d %H:%M:%S',
                                   tz = "Asia/Kolkata"))
  DT_f <- DT_f %>%
    mutate_at(c('PM2.5'), as.numeric) %>%
    mutate(PM2_5 = PM2.5 * 1000) %>%
    dplyr::select(date, PM2_5)
}

RH_file_analysis <- function(z) {
  RH_f <- read.csv(z, header = TRUE, sep = ",", skip = 6, row.names = NULL)
  RH_f_Date <- RH_f[ ,2]
  RH_f_Time <- RH_f[ ,3]
  RH <- RH_f[ , grepl("RH", names(RH_f))]
  RH_f <- data.frame(RH_f_Date, RH_f_Time, RH)
  RH_f <- RH_f %>%
    select("LogDate" = RH_f_Date, "LogTime" = RH_f_Time, RH)
  RH_f$LogTime <- gsub(".", ":", RH_f$LogTime, fixed = TRUE)
  RH_f$LogTime <- gsub("AM", "", RH_f$LogTime, fixed = TRUE)
  RH_f$LogTime <- gsub("PM", "", RH_f$LogTime, fixed = TRUE)
  RH_f <- RH_f %>%
    mutate(date = as.POSIXct(paste(LogDate, LogTime), format = '%d-%m-%Y %H:%M:%S', 
                             tz = "Asia/Kolkata")) %>%
    dplyr::select(date, RH) %>%
    mutate_at(c('RH'), as.numeric)
}


snapPointsToLines1 <- function (points, lines, maxDist = NA, withAttrs = TRUE, 
                                idField = NA) {
  if (rgeosStatus()) {
    if (!requireNamespace("rgeos", quietly = TRUE)) 
      stop("package rgeos required for snapPointsToLines")
  }
  else stop("rgeos not installed")
  if (is(points, "SpatialPointsDataFrame") == FALSE && missing(withAttrs)) 
    withAttrs = FALSE 
  if (is(points, "SpatialPointsDataFrame") == FALSE && withAttrs == TRUE) 
    stop("A SpatialPointsDataFrame object is needed! Please set withAttrs as FALSE.")
  d = rgeos::gDistance(points, lines, byid = TRUE)
  if (!is.na(maxDist)) {
    distToLine <- apply(d, 2, min, na.rm = TRUE)
    validPoints <- distToLine <= maxDist
    distToPoint <- apply(d, 1, min, na.rm = TRUE)
    validLines <- distToPoint <= maxDist
    points <- points[validPoints, ]
    lines = lines[validLines, ]
    d = d[validLines, validPoints, drop = FALSE]
    distToLine <- distToLine[validPoints]
    if (!any(validPoints)) {
      if (is.na(idField)) {
        idCol = character(0)
      } else {
        idCol = lines@data[, idField][0]
      }
      newCols = data.frame(nearest_line_id = idCol, snap_dist = numeric(0))
      if (withAttrs) 
        df <- cbind(points@data, newCols)
      else df <- newCols
      res <- SpatialPointsDataFrame(points, data = df, 
                                    proj4string = CRS(proj4string(points)), 
                                    match.ID = FALSE)
      return(res)
    }
  } else {
    distToLine = apply(d, 2, min, na.rm = TRUE)
  }
  nearest_line_index = apply(d, 2, which.min)
  coordsLines = coordinates(lines)
  coordsPoints = coordinates(points)
  mNewCoords = vapply(1:length(points), function(x) 
    nearestPointOnLine(coordsLines[[nearest_line_index[x]]][[1]],
                       coordsPoints[x, ]), FUN.VALUE = c(0, 0))
  if (!is.na(idField)) {
    nearest_line_id = lines@data[, idField][nearest_line_index]
  } else {
    nearest_line_id = sapply(slot(lines, "lines"), 
                             function(i) slot(i, "ID"))[nearest_line_index]
  }
  if (withAttrs) 
    df = cbind(points@data, data.frame(nearest_line_id, snap_dist = distToLine))
  else 
    df = data.frame(nearest_line_id, snap_dist = distToLine, 
                    row.names = names(nearest_line_index))
  SpatialPointsDataFrame(coords = t(mNewCoords), data = df, 
                         proj4string = CRS(proj4string(points)))
}



Snap_analysis <- function(file, dis, Area_name) {
  shp_point <- readOGR(file)
  shp_point_proj <- spTransform(shp_point, CRS("+proj=utm +zone=43 ellps=WGS84"))
  Snapped_Points <- snapPointsToLines1(shp_point_proj, shp_road_proj, 
                                       maxDist = dis, 
                                       withAttrs = TRUE)
  Area <- Area_name
  data_diff <- data.frame(Area) %>%
    mutate(diff = nrow(as.data.frame(shp_point)) -  
             nrow(as.data.frame(Snapped_Points)),
           Point_row = nrow(as.data.frame(shp_point)),
           SnapPoint_row = nrow(as.data.frame(Snapped_Points)),
           percent = (diff / nrow(as.data.frame(shp_point))) * 100,
           layer = gsub(".shp", "", file))
  return(list(Snapped_Points, data_diff)) 
}

GSD1 <- function(col, na.rm = TRUE) {
  GSD_col <- exp(sd(log(col), na.rm = TRUE))
  return(GSD_col)
}

GM1 <- function(col, na.rm = TRUE) {
  GM_col <- exp(mean(log(col), na.rm = TRUE)) 
  return(GM_col)
}

CV1 <- function(col, na.rm = TRUE) {
  cv <- mean(col, na.rm = TRUE) / sd(col, na.rm = TRUE)
  return(cv)
}

stderr <- function(col, na.rm = FALSE) {
  if (na.rm) col <- na.omit(col)
  sqrt(var(col) / length(col))
}

st <- function(x, na.rm = FALSE) {
  if (na.rm) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

boot_var_med <- function(BC_c, na.rm = FALSE) {
  if (na.rm) BC_c <- na.omit(BC_c)
  y <- as.vector(as.numeric(as.character(BC_c))) 
  data_all <- data.frame()
  len <- length(y)
  M <- 10000
  b <- 1
  result_vec <- vector(length = M)
  for(b in 1:M) {
    bc_sample <- sample(y, size = length(y), replace = TRUE)
    mn <- median(bc_sample, na.rm = TRUE)
    result_vec[b] <- mn
  }
  std_dev <- sd(result_vec, na.rm = TRUE)
  std_er <- st(y, na.rm = TRUE)
  mn_y <- median(y, na.rm = TRUE)
  mnd_re <- median(result_vec, na.rm = TRUE)
  mn_re <- mean(result_vec, na.rm = TRUE)
  lower_bound <- (quantile(result_vec, probs = c(0.025), na.rm = TRUE))[[1]]
  upper_bound <- (quantile(result_vec, probs = c(0.975), na.rm = TRUE))[[1]]
  ymin <- mn_y - 1.96*(std_er)
  ymax <- mn_y + 1.96*(std_er)
  data_sample <- data.frame(len, mn_y, mn_re, mnd_re, std_dev, std_er, lower_bound, upper_bound,
                            ymin, ymax)
  return(data_sample)
}


### Bootstrap function
boot_var <- function(BC_c, na.rm = FALSE) {
  if (na.rm) BC_c <- na.omit(BC_c)
  y <- as.vector(as.numeric(as.character(BC_c))) 
  data_all <- data.frame()
  len <- length(y)
  M <- 10000
  b <- 1
  result_vec <- vector(length = M)
  for(b in 1:M) {
    bc_sample <- sample(y, size = length(y), replace = TRUE)
    mn <- mean(bc_sample, na.rm = TRUE)
    result_vec[b] <- mn
  }
  std_dev <- sd(result_vec, na.rm = TRUE)
  std_er <- st(y, na.rm = TRUE)
  mn_y <- mean(y, na.rm = TRUE)
  mn_re <- mean(result_vec, na.rm = TRUE)
  lower_bound <- (quantile(result_vec, probs = c(0.025), na.rm = TRUE))[[1]]
  upper_bound <- (quantile(result_vec, probs = c(0.975), na.rm = TRUE))[[1]]
  ymin <- mn_y - 1.96 * (std_er)
  ymax <- mn_y + 1.96 * (std_er)
  data_sample <- data.frame(len, mn_y, mn_re, std_dev, std_er, lower_bound, upper_bound,
                            ymin, ymax)
  return(data_sample)
}

R2_NRMSE_function <- function(each_file, Final_reference, la, fi) {
  Final_ref <- Final_reference %>%
    dplyr::select(Road_ID, "Final_Median" = fi) %>% # add fi
    mutate_at(c('Road_ID'), as.numeric)
  my_spdf <- st_read(each_file)
  my_spdf <- st_transform(my_spdf, crs = "+proj=utm +zone=43 ellps=WGS84")
  my_spdf <- st_as_sf(my_spdf) 
  my_spdf$geometry <- NULL
  spdf <- my_spdf %>%
    dplyr::select(Road_ID, "N_Median" = la) %>% # add la
    mutate_at(c('Road_ID'), as.numeric)
  names(spdf) <- c("Road_ID", "N_Median", "geometry")  ## here change or remove
  spdf$geometry <- NULL
  # Join both tables for easy calculations 
  Final <- left_join(Final_ref, spdf, by = "Road_ID") 
  # To calculate R squared
  sum <- summary(lm(Final_Median ~ N_Median, data = Final))
  R_squared <- sum$r.squared
  sum_f <- sum$fstatistic[[1]]
  # To calculate RMSE: Observed - expected square the difference
  Final$rmsq <- (Final$N_Median - Final$Final_Median) ^ 2
  # To calculate RMSE: Mean of the squared difference between observed and 
  # expected and then its square root ######
  rms   <- (sum(Final$rmsq, na.rm = TRUE)) / row_no_ref  
  rms1  <- (mean(Final$rmsq, na.rm = TRUE))  
  rmse  <- sqrt(rms)
  rmse1 <- sqrt(rms1)
  # To calculate Mean of the observed: Sum of values / no of observations 
  y_bar  <- (sum(Final$N_Median, na.rm = TRUE)) / row_no_ref  
  # Sum total and then divide by the number of rows than the mean uniformity 
  # about the number of observations
  y_bar1 <- (mean(Final$N_Median, na.rm = TRUE))  
  y_bar2 <- (mean(Final$Final_Median, na.rm = TRUE)) 
  nrmse_mean_not_sum <- rmse1 / y_bar2
  # the mean uniformity abot the number of observations
  # To calculate min/max; na.rm = TRUE ignores the NA values 
  y_max <- (max(Final$N_Median, na.rm = TRUE))
  y_min <- (min(Final$N_Median, na.rm = TRUE))
  rang  <- y_max - y_min
  # String manipulations to identify each layer 
  N <- each_file
  layer_N <- gsub("_", "", substr(N, 1, 2))
  layer_N <- as.numeric(as.character(layer_N))
  # Make a new data frame of the variables 
  new_df <- data.frame(N, layer_N, y_bar, y_bar1, rmse, rmse1, rang, 
                       R_squared, y_min, y_max, sum_f, nrmse_mean_not_sum, y_bar2)
}


# Med_BC in Final reference Layer and the other Median BC is from each layer
Residual_function <- function(shp, la, Final_Reference, fi, Area, para) {
  # Each shapefile converted to a dataframe my_spdf: my spatial data frame 
  my_spdf <- st_read(shp)
  my_spdf <- st_transform(my_spdf, crs = "+proj=utm +zone=43 ellps=WGS84")
  my_spdf_sf <- st_as_sf(my_spdf) 
  my_spdf_sf$geometry <- NULL
  spdf <- my_spdf_sf %>%
    dplyr::select(Road_ID, "N_Median" = la) %>%   
    mutate_at(c('Road_ID', 'N_Median'), as.numeric)
  names(spdf) <- c("Road_ID", "N_Median", "geometry")  ## here change or remove
  spdf$geometry <- NULL
  # Make a dataframe with both the reference and each rides data to get the 
  # difference also called as residual and convert it into a shapefile using 
  # any one of the shapefiles
  Residual_layer <- left_join(Final_Reference, spdf, by = "Road_ID")
  Residual_C <- paste0("Residual_", para)
  Residual_layer <- Residual_layer %>%
    dplyr::select(Road_ID, Rod_typ, N_Median, "Reference_Median" = fi, everything(), 
                  -contains(c("_mn", "10", "90", "25", "75", "_ma", "_mi", "cat", "_n", "_CO",
                              "_sd", "_GS", "_1", "_CV", "GM", "_9", "se", "_7"))) %>%
    mutate(!!Residual_C := N_Median - Reference_Median,
           UID = paste0(Area, Road_ID)) 
}

theme_ARU <- list(theme_classic(),
                  theme(legend.text = element_text(size = 14),
                        legend.title = element_blank(),
                        plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
                        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), 
                        axis.title = element_text(size = 18, colour = "black", face = "bold"),
                        axis.text = element_text(size = 18, colour = "black", face = "bold"),
                        panel.border = element_rect(colour = "black", fill = NA, size = 1), 
                        legend.position = "right",
                        strip.background = element_blank(), strip.text = element_blank()))
