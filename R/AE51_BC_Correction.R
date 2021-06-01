library(lubridate) # dates manipulation
library(scales)
library(zoo)
library(caTools)
library(xts)
library(tidyverse)
library(dplyr)
library(data.table)
library(ggplot2)
library(reshape)
setwd("D:/Dropbox/APMfull/Phase_II/AE51")
dir <- "D:/Dropbox/APMfull/Phase_II/AE51"
Area <- "SA"
lt <- list.files(dir, pattern = "\\.csv$")

data_removed <- data.frame()
for (z in (lt)) 
{
  trial<- read.csv(z, header = TRUE, skip = 16, sep = ",")
  trial <- trial %>%
    drop_na(BC) %>%
    mutate_at(c('Date', 'Time'), as.character)
  trial$date <- with(trial, as.POSIXct(paste(as.Date(Date, format = "%Y/%m/%d"), Time),
                                       tz = "Asia/Kolkata"))
  if (is.null(trial$date)) {
    trial$date <- with(trial, as.POSIXct(paste(as.Date(Date, 
                                                       format = "%d-%m-%Y"), 
                                               Time), tz = "Asia/Kolkata"))
  } else if(is.null(trial$date)) {
    trial$date <- with(trial, as.POSIXct(paste(as.Date(Date, 
                                                       format = "%m/%d/%Y"), 
                                               Time), tz = "Asia/Kolkata"))
  } else if(is.null(trial$date)) {
    trial$date <- with(trial, as.POSIXct(paste(as.Date(Date, 
                                                       format = "%d/%m/%Y"), 
                                               Time), tz = "Asia/Kolkata"))
  }
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
  BC_na <- subset(BC_Final, !is.na(BC_NR_LC))
  data_remove <- data.frame(Area) %>%
    mutate(diff = nrow(as.data.frame(trial)) -  
             nrow(as.data.frame(BC_na)),
           Point_row = nrow(as.data.frame(trial)),
           corrected_row = nrow(as.data.frame(BC_na)),
           percent = (diff / nrow(as.data.frame(trial))) * 100,
           layer = gsub(".csv", "", z))
  data_removed <- rbind(data_removed, data_remove)
  myOutFile <- paste("D:/Dropbox/APMfull/Phase_II/AE51/Corrected/Corrected_BC_",z)
  write.csv(BC_Final, file = myOutFile)
  
}

print("Files Saved")
