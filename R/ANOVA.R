source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
library(extrafont)
library(DescTools)
library(tidyr)
ICC_table <- data.frame()

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")
q <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/All_rides_MAL2_30m.csv", sep = ",", 
           header = TRUE)
q$Road_ID <- paste0("MAL2_", q$Road_ID)
loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC$BC_LC <- log(loop_BC_LC$BC_LC)
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_ID, data = loop_BC_LC)
ICC_BC_LC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
               (summary(res.aov)[1][[1]][[3]][[1]] + 
                  summary(res.aov)[1][[1]][[3]][[2]]))

loop_BC_LC_highway <- loop_BC_LC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_highway)
ICC_BC_LC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                       summary(res_aov_high)[1][[1]][[3]][[2]]))


loop_BC_LC_arterial <- loop_BC_LC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_arterial)
ICC_BC_LC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                      summary(res_aov_art)[1][[1]][[3]][[2]]))


loop_BC_LC_resi <- loop_BC_LC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_resi)
ICC_BC_LC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                      summary(res_aov_res)[1][[1]][[3]][[2]]))

# loop_BC_LC2 <- loop_BC_LC_resi %>%
#   dplyr::select(Road_ID, Road_type, date, BC_LC) %>%
#   pivot_wider(names_from = c(Road_ID, Road_type), values_from = BC_LC) %>%
#   unnest(everything())

# loop_BC_LC1 <- loop_BC_LC %>%
#   nest(data = -c(Road_type)) %>%
#   mutate(aov_model = map(data, ~aov(BC_LC ~ Road_ID, data = .),
#                          tidy_aov = map(aov, ~broom::tidy(.)))) %>%
#   pull(tidy_aov)

loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c$CO2_c <- as.numeric(as.character(log(loop_CO2_c$CO2_c)))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_ID, data = loop_CO2_c)
ICC_CO2_c <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_CO2_c_highway <- loop_CO2_c %>%
  filter(Road_type == "Highway")
loop_CO2_c_highway <- subset(loop_CO2_c, Road_type == "Highway")
res_aov_high <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_highway)
ICC_CO2_c_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CO2_c_arterial <- loop_CO2_c %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_arterial)
summary(res_aov_art)
ICC_CO2_c_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CO2_c_resi <- loop_CO2_c %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_resi)
ICC_CO2_c_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC$CPC <- log(loop_CPC$CPC)
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_ID, data = loop_CPC)
ICC_CPC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
              (summary(res.aov)[1][[1]][[3]][[1]] + 
                 summary(res.aov)[1][[1]][[3]][[2]]))

loop_CPC_highway <- loop_CPC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(CPC ~ Road_ID, data = loop_CPC_highway)
ICC_CPC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                      summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CPC_arterial <- loop_CPC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CPC ~ Road_ID, data = loop_CPC_arterial)
ICC_CPC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                     summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CPC_resi <- loop_CPC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CPC ~ Road_ID, data = loop_CPC_resi)
ICC_CPC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                     summary(res_aov_res)[1][[1]][[3]][[2]]))

ICC_table_MAL2 <- data.frame(Area = c("MAL2", "MAL2", "MAL2"), Pollutant = c("BC", "UFPs", "CO2"),
                             ICC = c(ICC_BC_LC, ICC_CPC, ICC_CO2_c), 
                             ICC_Highway = c(ICC_BC_LC_high, ICC_CPC_high, ICC_CO2_c_high),
                             ICC_Arterial = c(ICC_BC_LC_art, ICC_CPC_art, ICC_CO2_c_art),
                             ICC_Residential = c(ICC_BC_LC_res, ICC_CPC_res, ICC_CO2_c_res))
ICC_table <- rbind(ICC_table, ICC_table_MAL2)
beepr::beep()
write.csv(ICC_table, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/ICC_log.csv")



############################### MAL1 ###########################################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1")
q <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/All_rides_MAL1_30m.csv", sep = ",", 
           header = TRUE)
q$Road_ID <- paste0("MAL1_", q$Road_ID)
loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC$BC_LC <- log(loop_BC_LC$BC_LC)
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_ID, data = loop_BC_LC)
ICC_BC_LC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
               (summary(res.aov)[1][[1]][[3]][[1]] + 
                  summary(res.aov)[1][[1]][[3]][[2]]))

loop_BC_LC_highway <- loop_BC_LC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_highway)
ICC_BC_LC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                       summary(res_aov_high)[1][[1]][[3]][[2]]))


loop_BC_LC_arterial <- loop_BC_LC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_arterial)
ICC_BC_LC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                      summary(res_aov_art)[1][[1]][[3]][[2]]))


loop_BC_LC_resi <- loop_BC_LC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_resi)
ICC_BC_LC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                      summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c$CO2_c <- as.numeric(as.character(log(loop_CO2_c$CO2_c)))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_ID, data = loop_CO2_c)
ICC_CO2_c <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_CO2_c_highway <- loop_CO2_c %>%
  filter(Road_type == "Highway")
loop_CO2_c_highway <- subset(loop_CO2_c, Road_type == "Highway")
res_aov_high <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_highway)
ICC_CO2_c_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CO2_c_arterial <- loop_CO2_c %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_arterial)
summary(res_aov_art)
ICC_CO2_c_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CO2_c_resi <- loop_CO2_c %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_resi)
ICC_CO2_c_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC$CPC <- log(loop_CPC$CPC)
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_ID, data = loop_CPC)
ICC_CPC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
              (summary(res.aov)[1][[1]][[3]][[1]] + 
                 summary(res.aov)[1][[1]][[3]][[2]]))

loop_CPC_highway <- loop_CPC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(CPC ~ Road_ID, data = loop_CPC_highway)
ICC_CPC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                      summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CPC_arterial <- loop_CPC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CPC ~ Road_ID, data = loop_CPC_arterial)
ICC_CPC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                     summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CPC_resi <- loop_CPC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CPC ~ Road_ID, data = loop_CPC_resi)
ICC_CPC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                     summary(res_aov_res)[1][[1]][[3]][[2]]))

ICC_table_MAL1 <- data.frame(Area = c("MAL1", "MAL1", "MAL1"), Pollutant = c("BC", "UFPs", "CO2"),
                             ICC = c(ICC_BC_LC, ICC_CPC, ICC_CO2_c), 
                             ICC_Highway = c(ICC_BC_LC_high, ICC_CPC_high, ICC_CO2_c_high),
                             ICC_Arterial = c(ICC_BC_LC_art, ICC_CPC_art, ICC_CO2_c_art),
                             ICC_Residential = c(ICC_BC_LC_res, ICC_CPC_res, ICC_CO2_c_res))
ICC_table <- rbind(ICC_table, ICC_table_MAL1)
beepr::beep()
write.csv(ICC_table, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/ICC_log.csv")


########################### without log ######################################

ICC_table <- data.frame()

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")
q <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/All_rides_MAL2_30m.csv", sep = ",", 
           header = TRUE)
q$Road_ID <- paste0("MAL2_", q$Road_ID)
loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_ID, data = loop_BC_LC)
ICC_BC_LC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
               (summary(res.aov)[1][[1]][[3]][[1]] + 
                  summary(res.aov)[1][[1]][[3]][[2]]))

loop_BC_LC_highway <- loop_BC_LC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_highway)
ICC_BC_LC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                       summary(res_aov_high)[1][[1]][[3]][[2]]))


loop_BC_LC_arterial <- loop_BC_LC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_arterial)
ICC_BC_LC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                      summary(res_aov_art)[1][[1]][[3]][[2]]))


loop_BC_LC_resi <- loop_BC_LC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_resi)
ICC_BC_LC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                      summary(res_aov_res)[1][[1]][[3]][[2]]))

loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_ID, data = loop_CO2_c)
ICC_CO2_c <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_CO2_c_highway <- loop_CO2_c %>%
  filter(Road_type == "Highway")
loop_CO2_c_highway <- subset(loop_CO2_c, Road_type == "Highway")
res_aov_high <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_highway)
ICC_CO2_c_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CO2_c_arterial <- loop_CO2_c %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_arterial)
summary(res_aov_art)
ICC_CO2_c_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CO2_c_resi <- loop_CO2_c %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_resi)
ICC_CO2_c_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_ID, data = loop_CPC)
ICC_CPC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
              (summary(res.aov)[1][[1]][[3]][[1]] + 
                 summary(res.aov)[1][[1]][[3]][[2]]))

loop_CPC_highway <- loop_CPC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(CPC ~ Road_ID, data = loop_CPC_highway)
ICC_CPC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                      summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CPC_arterial <- loop_CPC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CPC ~ Road_ID, data = loop_CPC_arterial)
ICC_CPC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                     summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CPC_resi <- loop_CPC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CPC ~ Road_ID, data = loop_CPC_resi)
ICC_CPC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                     summary(res_aov_res)[1][[1]][[3]][[2]]))

ICC_table_MAL2 <- data.frame(Area = c("MAL2", "MAL2", "MAL2"), Pollutant = c("BC", "UFPs", "CO2"),
                             ICC = c(ICC_BC_LC, ICC_CPC, ICC_CO2_c), 
                             ICC_Highway = c(ICC_BC_LC_high, ICC_CPC_high, ICC_CO2_c_high),
                             ICC_Arterial = c(ICC_BC_LC_art, ICC_CPC_art, ICC_CO2_c_art),
                             ICC_Residential = c(ICC_BC_LC_res, ICC_CPC_res, ICC_CO2_c_res))
ICC_table <- rbind(ICC_table, ICC_table_MAL2)
beepr::beep()
write.csv(ICC_table, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/ICC.csv")

############################### without log MAL1 ###############################

q <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/All_rides_MAL1_30m.csv", sep = ",", 
           header = TRUE)
q$Road_ID <- paste0("MAL1_", q$Road_ID)
loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_ID, data = loop_BC_LC)
ICC_BC_LC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
               (summary(res.aov)[1][[1]][[3]][[1]] + 
                  summary(res.aov)[1][[1]][[3]][[2]]))

loop_BC_LC_highway <- loop_BC_LC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_highway)
ICC_BC_LC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                       summary(res_aov_high)[1][[1]][[3]][[2]]))


loop_BC_LC_arterial <- loop_BC_LC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_arterial)
ICC_BC_LC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                      summary(res_aov_art)[1][[1]][[3]][[2]]))


loop_BC_LC_resi <- loop_BC_LC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_resi)
ICC_BC_LC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                      summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_ID, data = loop_CO2_c)
ICC_CO2_c <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_CO2_c_highway <- loop_CO2_c %>%
  filter(Road_type == "Highway")
loop_CO2_c_highway <- subset(loop_CO2_c, Road_type == "Highway")
res_aov_high <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_highway)
ICC_CO2_c_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CO2_c_arterial <- loop_CO2_c %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_arterial)
summary(res_aov_art)
ICC_CO2_c_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CO2_c_resi <- loop_CO2_c %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_resi)
ICC_CO2_c_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_ID, data = loop_CPC)
ICC_CPC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
              (summary(res.aov)[1][[1]][[3]][[1]] + 
                 summary(res.aov)[1][[1]][[3]][[2]]))

loop_CPC_highway <- loop_CPC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(CPC ~ Road_ID, data = loop_CPC_highway)
ICC_CPC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                      summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CPC_arterial <- loop_CPC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CPC ~ Road_ID, data = loop_CPC_arterial)
ICC_CPC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                     summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CPC_resi <- loop_CPC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CPC ~ Road_ID, data = loop_CPC_resi)
ICC_CPC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                     summary(res_aov_res)[1][[1]][[3]][[2]]))

ICC_table_MAL1 <- data.frame(Area = c("MAL1", "MAL1", "MAL1"), Pollutant = c("BC", "UFPs", "CO2"),
                             ICC = c(ICC_BC_LC, ICC_CPC, ICC_CO2_c), 
                             ICC_Highway = c(ICC_BC_LC_high, ICC_CPC_high, ICC_CO2_c_high),
                             ICC_Arterial = c(ICC_BC_LC_art, ICC_CPC_art, ICC_CO2_c_art),
                             ICC_Residential = c(ICC_BC_LC_res, ICC_CPC_res, ICC_CO2_c_res))
ICC_table <- rbind(ICC_table, ICC_table_MAL1)
beepr::beep()
write.csv(ICC_table, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/ICC.csv")

########################################### KAN ################################

source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
library(extrafont)
library(DescTools)
library(tidyr)
ICC_table <- data.frame()

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN")
q <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/All_rides_KAN_30m.csv", sep = ",", 
           header = TRUE)
q$Road_ID <- paste0("KAN_", q$Road_ID)
loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC$BC_LC <- log(loop_BC_LC$BC_LC)
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_ID, data = loop_BC_LC)
ICC_BC_LC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_BC_LC_highway <- loop_BC_LC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_highway)
ICC_BC_LC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))


loop_BC_LC_arterial <- loop_BC_LC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_arterial)
ICC_BC_LC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))


loop_BC_LC_resi <- loop_BC_LC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_resi)
ICC_BC_LC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))

# loop_BC_LC2 <- loop_BC_LC_resi %>%
#   dplyr::select(Road_ID, Road_type, date, BC_LC) %>%
#   pivot_wider(names_from = c(Road_ID, Road_type), values_from = BC_LC) %>%
#   unnest(everything())

# loop_BC_LC1 <- loop_BC_LC %>%
#   nest(data = -c(Road_type)) %>%
#   mutate(aov_model = map(data, ~aov(BC_LC ~ Road_ID, data = .),
#                          tidy_aov = map(aov, ~broom::tidy(.)))) %>%
#   pull(tidy_aov)

loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c$CO2_c <- as.numeric(as.character(log(loop_CO2_c$CO2_c)))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_ID, data = loop_CO2_c)
ICC_CO2_c <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_CO2_c_highway <- loop_CO2_c %>%
  filter(Road_type == "Highway")
loop_CO2_c_highway <- subset(loop_CO2_c, Road_type == "Highway")
res_aov_high <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_highway)
ICC_CO2_c_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CO2_c_arterial <- loop_CO2_c %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_arterial)
summary(res_aov_art)
ICC_CO2_c_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CO2_c_resi <- loop_CO2_c %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_resi)
ICC_CO2_c_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC$CPC <- log(loop_CPC$CPC)
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_ID, data = loop_CPC)
ICC_CPC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
              (summary(res.aov)[1][[1]][[3]][[1]] + 
                 summary(res.aov)[1][[1]][[3]][[2]]))

loop_CPC_highway <- loop_CPC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(CPC ~ Road_ID, data = loop_CPC_highway)
ICC_CPC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                      summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CPC_arterial <- loop_CPC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CPC ~ Road_ID, data = loop_CPC_arterial)
ICC_CPC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                     summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CPC_resi <- loop_CPC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CPC ~ Road_ID, data = loop_CPC_resi)
ICC_CPC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                     summary(res_aov_res)[1][[1]][[3]][[2]]))

ICC_table_KAN <- data.frame(Area = c("KAN", "KAN", "KAN"), Pollutant = c("BC", "UFPs", "CO2"),
                            ICC = c(ICC_BC_LC, ICC_CPC, ICC_CO2_c), 
                            ICC_Highway = c(ICC_BC_LC_high, ICC_CPC_high, ICC_CO2_c_high),
                            ICC_Arterial = c(ICC_BC_LC_art, ICC_CPC_art, ICC_CO2_c_art),
                            ICC_Residential = c(ICC_BC_LC_res, ICC_CPC_res, ICC_CO2_c_res))
ICC_table <- rbind(ICC_table, ICC_table_KAN)
beepr::beep()
write.csv(ICC_table, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/ICC_log.csv")

############################### CBD ###########################################

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD")
q <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/All_rides_CBD_30m.csv", sep = ",", 
           header = TRUE)
q$Road_ID <- paste0("CBD_", q$Road_ID)
loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC$BC_LC <- log(loop_BC_LC$BC_LC)
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_ID, data = loop_BC_LC)
ICC_BC_LC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_BC_LC_highway <- loop_BC_LC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_highway)
ICC_BC_LC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))


loop_BC_LC_arterial <- loop_BC_LC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_arterial)
ICC_BC_LC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))


loop_BC_LC_resi <- loop_BC_LC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_resi)
ICC_BC_LC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c$CO2_c <- as.numeric(as.character(log(loop_CO2_c$CO2_c)))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_ID, data = loop_CO2_c)
ICC_CO2_c <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_CO2_c_highway <- loop_CO2_c %>%
  filter(Road_type == "Highway")
loop_CO2_c_highway <- subset(loop_CO2_c, Road_type == "Highway")
res_aov_high <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_highway)
ICC_CO2_c_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CO2_c_arterial <- loop_CO2_c %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_arterial)
summary(res_aov_art)
ICC_CO2_c_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CO2_c_resi <- loop_CO2_c %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_resi)
ICC_CO2_c_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC$CPC <- log(loop_CPC$CPC)
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_ID, data = loop_CPC)
ICC_CPC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
              (summary(res.aov)[1][[1]][[3]][[1]] + 
                 summary(res.aov)[1][[1]][[3]][[2]]))

loop_CPC_highway <- loop_CPC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(CPC ~ Road_ID, data = loop_CPC_highway)
ICC_CPC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                      summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CPC_arterial <- loop_CPC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CPC ~ Road_ID, data = loop_CPC_arterial)
ICC_CPC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                     summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CPC_resi <- loop_CPC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CPC ~ Road_ID, data = loop_CPC_resi)
ICC_CPC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                     summary(res_aov_res)[1][[1]][[3]][[2]]))

ICC_table_CBD <- data.frame(Area = c("CBD", "CBD", "CBD"), Pollutant = c("BC", "UFPs", "CO2"),
                            ICC = c(ICC_BC_LC, ICC_CPC, ICC_CO2_c), 
                            ICC_Highway = c(ICC_BC_LC_high, ICC_CPC_high, ICC_CO2_c_high),
                            ICC_Arterial = c(ICC_BC_LC_art, ICC_CPC_art, ICC_CO2_c_art),
                            ICC_Residential = c(ICC_BC_LC_res, ICC_CPC_res, ICC_CO2_c_res))
ICC_table <- rbind(ICC_table, ICC_table_CBD)
beepr::beep()
write.csv(ICC_table, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/ICC_log.csv")


########################### without log ######################################

ICC_table <- data.frame()

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN")
q <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/All_rides_KAN_30m.csv", sep = ",", 
           header = TRUE)
q$Road_ID <- paste0("KAN_", q$Road_ID)
loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_ID, data = loop_BC_LC)
ICC_BC_LC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_BC_LC_highway <- loop_BC_LC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_highway)
ICC_BC_LC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))


loop_BC_LC_arterial <- loop_BC_LC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_arterial)
ICC_BC_LC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))


loop_BC_LC_resi <- loop_BC_LC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_resi)
ICC_BC_LC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))

loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_ID, data = loop_CO2_c)
ICC_CO2_c <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_CO2_c_highway <- loop_CO2_c %>%
  filter(Road_type == "Highway")
loop_CO2_c_highway <- subset(loop_CO2_c, Road_type == "Highway")
res_aov_high <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_highway)
ICC_CO2_c_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CO2_c_arterial <- loop_CO2_c %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_arterial)
summary(res_aov_art)
ICC_CO2_c_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CO2_c_resi <- loop_CO2_c %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_resi)
ICC_CO2_c_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_ID, data = loop_CPC)
ICC_CPC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
              (summary(res.aov)[1][[1]][[3]][[1]] + 
                 summary(res.aov)[1][[1]][[3]][[2]]))

loop_CPC_highway <- loop_CPC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(CPC ~ Road_ID, data = loop_CPC_highway)
ICC_CPC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                      summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CPC_arterial <- loop_CPC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CPC ~ Road_ID, data = loop_CPC_arterial)
ICC_CPC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                     summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CPC_resi <- loop_CPC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CPC ~ Road_ID, data = loop_CPC_resi)
ICC_CPC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                     summary(res_aov_res)[1][[1]][[3]][[2]]))

ICC_table_KAN <- data.frame(Area = c("KAN", "KAN", "KAN"), Pollutant = c("BC", "UFPs", "CO2"),
                            ICC = c(ICC_BC_LC, ICC_CPC, ICC_CO2_c), 
                            ICC_Highway = c(ICC_BC_LC_high, ICC_CPC_high, ICC_CO2_c_high),
                            ICC_Arterial = c(ICC_BC_LC_art, ICC_CPC_art, ICC_CO2_c_art),
                            ICC_Residential = c(ICC_BC_LC_res, ICC_CPC_res, ICC_CO2_c_res))
ICC_table <- rbind(ICC_table, ICC_table_KAN)
beepr::beep()
write.csv(ICC_table, "D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/ICC.csv")

############################### without log CBD ###############################

q <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/All_rides_CBD_30m.csv", sep = ",", 
           header = TRUE)
q$Road_ID <- paste0("CBD_", q$Road_ID)
loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_ID, data = loop_BC_LC)
ICC_BC_LC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_BC_LC_highway <- loop_BC_LC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_highway)
ICC_BC_LC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))


loop_BC_LC_arterial <- loop_BC_LC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_arterial)
ICC_BC_LC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))


loop_BC_LC_resi <- loop_BC_LC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_resi)
ICC_BC_LC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_ID, data = loop_CO2_c)
ICC_CO2_c <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_CO2_c_highway <- loop_CO2_c %>%
  filter(Road_type == "Highway")
loop_CO2_c_highway <- subset(loop_CO2_c, Road_type == "Highway")
res_aov_high <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_highway)
ICC_CO2_c_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CO2_c_arterial <- loop_CO2_c %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_arterial)
summary(res_aov_art)
ICC_CO2_c_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CO2_c_resi <- loop_CO2_c %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_resi)
ICC_CO2_c_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_ID, data = loop_CPC)
ICC_CPC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
              (summary(res.aov)[1][[1]][[3]][[1]] + 
                 summary(res.aov)[1][[1]][[3]][[2]]))

loop_CPC_highway <- loop_CPC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(CPC ~ Road_ID, data = loop_CPC_highway)
ICC_CPC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                      summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CPC_arterial <- loop_CPC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CPC ~ Road_ID, data = loop_CPC_arterial)
ICC_CPC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                     summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CPC_resi <- loop_CPC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CPC ~ Road_ID, data = loop_CPC_resi)
ICC_CPC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                     summary(res_aov_res)[1][[1]][[3]][[2]]))

ICC_table_CBD <- data.frame(Area = c("CBD", "CBD", "CBD"), Pollutant = c("BC", "UFPs", "CO2"),
                            ICC = c(ICC_BC_LC, ICC_CPC, ICC_CO2_c), 
                            ICC_Highway = c(ICC_BC_LC_high, ICC_CPC_high, ICC_CO2_c_high),
                            ICC_Arterial = c(ICC_BC_LC_art, ICC_CPC_art, ICC_CO2_c_art),
                            ICC_Residential = c(ICC_BC_LC_res, ICC_CPC_res, ICC_CO2_c_res))
ICC_table <- rbind(ICC_table, ICC_table_CBD)
beepr::beep()
write.csv(ICC_table, "D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/ICC.csv")


############################### MAL ###########################################

q3 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/All_rides_MAL1_30m.csv", sep = ",", 
            header = TRUE)
q3$Road_ID <- paste0("MAL1_", q3$Road_ID)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")
q4 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/All_rides_MAL2_30m.csv", sep = ",", 
            header = TRUE)
q4$Road_ID <- paste0("MAL2_", q4$Road_ID)


q <- rbind(q3, q4, fill = TRUE)


loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC$BC_LC <- log(loop_BC_LC$BC_LC)
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_ID, data = loop_BC_LC)
ICC_BC_LC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_BC_LC_highway <- loop_BC_LC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_highway)
ICC_BC_LC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))


loop_BC_LC_arterial <- loop_BC_LC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_arterial)
ICC_BC_LC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))


loop_BC_LC_resi <- loop_BC_LC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_resi)
ICC_BC_LC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c$CO2_c <- as.numeric(as.character(log(loop_CO2_c$CO2_c)))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_ID, data = loop_CO2_c)
ICC_CO2_c <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_CO2_c_highway <- loop_CO2_c %>%
  filter(Road_type == "Highway")
loop_CO2_c_highway <- subset(loop_CO2_c, Road_type == "Highway")
res_aov_high <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_highway)
ICC_CO2_c_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CO2_c_arterial <- loop_CO2_c %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_arterial)
summary(res_aov_art)
ICC_CO2_c_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CO2_c_resi <- loop_CO2_c %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_resi)
ICC_CO2_c_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC$CPC <- log(loop_CPC$CPC)
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_ID, data = loop_CPC)
ICC_CPC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
              (summary(res.aov)[1][[1]][[3]][[1]] + 
                 summary(res.aov)[1][[1]][[3]][[2]]))

loop_CPC_highway <- loop_CPC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(CPC ~ Road_ID, data = loop_CPC_highway)
ICC_CPC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                      summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CPC_arterial <- loop_CPC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CPC ~ Road_ID, data = loop_CPC_arterial)
ICC_CPC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                     summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CPC_resi <- loop_CPC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CPC ~ Road_ID, data = loop_CPC_resi)
ICC_CPC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                     summary(res_aov_res)[1][[1]][[3]][[2]]))

ICC_table_MAL <- data.frame(Area = c("MAL", "MAL", "MAL"), Pollutant = c("BC", "UFPs", "CO2"),
                            ICC = c(ICC_BC_LC, ICC_CPC, ICC_CO2_c), 
                            ICC_Highway = c(ICC_BC_LC_high, ICC_CPC_high, ICC_CO2_c_high),
                            ICC_Arterial = c(ICC_BC_LC_art, ICC_CPC_art, ICC_CO2_c_art),
                            ICC_Residential = c(ICC_BC_LC_res, ICC_CPC_res, ICC_CO2_c_res))
ICC_table <- rbind(ICC_table, ICC_table_MAL)
beepr::beep()
write.csv(ICC_table, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_ICC_log.csv")

############################### without log MAL ###############################

q3 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/All_rides_MAL1_30m.csv", sep = ",", 
            header = TRUE)
q3$Road_ID <- paste0("MAL1_", q3$Road_ID)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")
q4 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/All_rides_MAL2_30m.csv", sep = ",", 
            header = TRUE)
q4$Road_ID <- paste0("MAL2_", q4$Road_ID)


q <- rbind(q3, q4, fill = TRUE)
loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_ID, data = loop_BC_LC)
ICC_BC_LC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_BC_LC_highway <- loop_BC_LC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_highway)
ICC_BC_LC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))


loop_BC_LC_arterial <- loop_BC_LC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_arterial)
ICC_BC_LC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))


loop_BC_LC_resi <- loop_BC_LC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_resi)
ICC_BC_LC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_ID, data = loop_CO2_c)
ICC_CO2_c <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_CO2_c_highway <- loop_CO2_c %>%
  filter(Road_type == "Highway")
loop_CO2_c_highway <- subset(loop_CO2_c, Road_type == "Highway")
res_aov_high <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_highway)
ICC_CO2_c_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CO2_c_arterial <- loop_CO2_c %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_arterial)
summary(res_aov_art)
ICC_CO2_c_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CO2_c_resi <- loop_CO2_c %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_resi)
ICC_CO2_c_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_ID, data = loop_CPC)
ICC_CPC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
              (summary(res.aov)[1][[1]][[3]][[1]] + 
                 summary(res.aov)[1][[1]][[3]][[2]]))

loop_CPC_highway <- loop_CPC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(CPC ~ Road_ID, data = loop_CPC_highway)
ICC_CPC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                      summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CPC_arterial <- loop_CPC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CPC ~ Road_ID, data = loop_CPC_arterial)
ICC_CPC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                     summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CPC_resi <- loop_CPC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CPC ~ Road_ID, data = loop_CPC_resi)
ICC_CPC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                     summary(res_aov_res)[1][[1]][[3]][[2]]))

ICC_table_MAL <- data.frame(Area = c("MAL", "MAL", "MAL"), Pollutant = c("BC", "UFPs", "CO2"),
                            ICC = c(ICC_BC_LC, ICC_CPC, ICC_CO2_c), 
                            ICC_Highway = c(ICC_BC_LC_high, ICC_CPC_high, ICC_CO2_c_high),
                            ICC_Arterial = c(ICC_BC_LC_art, ICC_CPC_art, ICC_CO2_c_art),
                            ICC_Residential = c(ICC_BC_LC_res, ICC_CPC_res, ICC_CO2_c_res))
ICC_table <- rbind(ICC_table, ICC_table_MAL)
beepr::beep()
write.csv(ICC_table, "D:/Dropbox/APMfull/MAL_CNG_Paper/MAL_ICC.csv")

############################ All ###############################################

source("D:/Dropbox/ILKConsultancy/MM_paper/R/Paper_functions.R")
library(extrafont)
library(DescTools)
library(tidyr)
ICC_table <- data.frame()

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN")
q1 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/All_rides_KAN_30m.csv", sep = ",", 
            header = TRUE)
q1$Road_ID <- paste0("KAN_", q1$Road_ID)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD")
q2 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/All_rides_CBD_30m.csv", sep = ",", 
            header = TRUE)
q2$Road_ID <- paste0("CBD_", q2$Road_ID)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1")
q3 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/All_rides_MAL1_30m.csv", sep = ",", 
            header = TRUE)
q3$Road_ID <- paste0("MAL1_", q3$Road_ID)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")
q4 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/All_rides_MAL2_30m.csv", sep = ",", 
            header = TRUE)
q4$Road_ID <- paste0("MAL2_", q4$Road_ID)


q <- rbind(q1, q2, q3, q4, fill = TRUE)
loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC$BC_LC <- log(loop_BC_LC$BC_LC)
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_ID, data = loop_BC_LC)
ICC_BC_LC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_BC_LC_highway <- loop_BC_LC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_highway)
ICC_BC_LC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))


loop_BC_LC_arterial <- loop_BC_LC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_arterial)
ICC_BC_LC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))


loop_BC_LC_resi <- loop_BC_LC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_resi)
ICC_BC_LC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))

# loop_BC_LC2 <- loop_BC_LC_resi %>%
#   dplyr::select(Road_ID, Road_type, date, BC_LC) %>%
#   pivot_wider(names_from = c(Road_ID, Road_type), values_from = BC_LC) %>%
#   unnest(everything())

# loop_BC_LC1 <- loop_BC_LC %>%
#   nest(data = -c(Road_type)) %>%
#   mutate(aov_model = map(data, ~aov(BC_LC ~ Road_ID, data = .),
#                          tidy_aov = map(aov, ~broom::tidy(.)))) %>%
#   pull(tidy_aov)

loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c$CO2_c <- as.numeric(as.character(log(loop_CO2_c$CO2_c)))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_ID, data = loop_CO2_c)
ICC_CO2_c <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_CO2_c_highway <- loop_CO2_c %>%
  filter(Road_type == "Highway")
loop_CO2_c_highway <- subset(loop_CO2_c, Road_type == "Highway")
res_aov_high <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_highway)
ICC_CO2_c_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CO2_c_arterial <- loop_CO2_c %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_arterial)
summary(res_aov_art)
ICC_CO2_c_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CO2_c_resi <- loop_CO2_c %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_resi)
ICC_CO2_c_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC$CPC <- log(loop_CPC$CPC)
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_ID, data = loop_CPC)
ICC_CPC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
              (summary(res.aov)[1][[1]][[3]][[1]] + 
                 summary(res.aov)[1][[1]][[3]][[2]]))

loop_CPC_highway <- loop_CPC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(CPC ~ Road_ID, data = loop_CPC_highway)
ICC_CPC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                      summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CPC_arterial <- loop_CPC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CPC ~ Road_ID, data = loop_CPC_arterial)
ICC_CPC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                     summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CPC_resi <- loop_CPC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CPC ~ Road_ID, data = loop_CPC_resi)
ICC_CPC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                     summary(res_aov_res)[1][[1]][[3]][[2]]))

ICC_table_All <- data.frame(Area = c("All", "All", "All"), Pollutant = c("BC", "UFPs", "CO2"),
                            ICC = c(ICC_BC_LC, ICC_CPC, ICC_CO2_c), 
                            ICC_Highway = c(ICC_BC_LC_high, ICC_CPC_high, ICC_CO2_c_high),
                            ICC_Arterial = c(ICC_BC_LC_art, ICC_CPC_art, ICC_CO2_c_art),
                            ICC_Residential = c(ICC_BC_LC_res, ICC_CPC_res, ICC_CO2_c_res))
ICC_table <- rbind(ICC_table, ICC_table_All)
beepr::beep()
write.csv(ICC_table, "D:/Dropbox/APMfull/MAL_CNG_Paper/ICC_log.csv")

########################### without log ######################################

ICC_table <- data.frame()

q1 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/KAN/All_rides_KAN_30m.csv", sep = ",", 
            header = TRUE)
q1$Road_ID <- paste0("KAN_", q1$Road_ID)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD")
q2 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/CBD/All_rides_CBD_30m.csv", sep = ",", 
            header = TRUE)
q2$Road_ID <- paste0("CBD_", q2$Road_ID)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1")
q3 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL1/All_rides_MAL1_30m.csv", sep = ",", 
            header = TRUE)
q3$Road_ID <- paste0("MAL1_", q3$Road_ID)

setwd("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2")
q4 <- fread("D:/Dropbox/APMfull/MAL_CNG_Paper/MAL2/All_rides_MAL2_30m.csv", sep = ",", 
            header = TRUE)
q4$Road_ID <- paste0("MAL2_", q4$Road_ID)


q <- rbind(q1, q2, q3, q4, fill = TRUE)
loop <- q %>%
  mutate_at(c('Road_ID'), as.factor)
q_high <- q %>%
  filter(Road_type == "Highway")
q_art <- q %>%
  filter(Road_type == "Arterial")
q_resi <- q %>%
  filter(Road_type == "Residential")

loop_BC_LC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_BC_LC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "BC_LC") %>%
  mutate(date = substr(date, 1, 18))
loop_BC_LC <- loop_BC_LC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(BC_LC ~ Road_ID, data = loop_BC_LC)
ICC_BC_LC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_BC_LC_highway <- loop_BC_LC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_highway)
ICC_BC_LC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))


loop_BC_LC_arterial <- loop_BC_LC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_arterial)
ICC_BC_LC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))


loop_BC_LC_resi <- loop_BC_LC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(BC_LC ~ Road_ID, data = loop_BC_LC_resi)
ICC_BC_LC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))

loop_CO2_c <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CO2_c")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CO2_c") %>%
  mutate(date = substr(date, 1, 18))
loop_CO2_c <- loop_CO2_c %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CO2_c ~ Road_ID, data = loop_CO2_c)
ICC_CO2_c <- (summary(res.aov)[1][[1]][[3]][[1]] / 
                (summary(res.aov)[1][[1]][[3]][[1]] + 
                   summary(res.aov)[1][[1]][[3]][[2]]))

loop_CO2_c_highway <- loop_CO2_c %>%
  filter(Road_type == "Highway")
loop_CO2_c_highway <- subset(loop_CO2_c, Road_type == "Highway")
res_aov_high <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_highway)
ICC_CO2_c_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                     (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                        summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CO2_c_arterial <- loop_CO2_c %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_arterial)
summary(res_aov_art)
ICC_CO2_c_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                       summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CO2_c_resi <- loop_CO2_c %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CO2_c ~ Road_ID, data = loop_CO2_c_resi)
ICC_CO2_c_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                    (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                       summary(res_aov_res)[1][[1]][[3]][[2]]))


loop_CPC <- loop %>%
  dplyr::select(Road_ID, Road_type, ends_with("_CPC")) %>%
  pivot_longer(!c(Road_ID, Road_type), names_to = "date", values_to = "CPC") %>%
  mutate(date = substr(date, 1, 18)) 
loop_CPC <- loop_CPC %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
res.aov <- aov(CPC ~ Road_ID, data = loop_CPC)
ICC_CPC <- (summary(res.aov)[1][[1]][[3]][[1]] / 
              (summary(res.aov)[1][[1]][[3]][[1]] + 
                 summary(res.aov)[1][[1]][[3]][[2]]))

loop_CPC_highway <- loop_CPC %>%
  filter(Road_type == "Highway")
res_aov_high <- aov(CPC ~ Road_ID, data = loop_CPC_highway)
ICC_CPC_high <- (summary(res_aov_high)[1][[1]][[3]][[1]] / 
                   (summary(res_aov_high)[1][[1]][[3]][[1]] + 
                      summary(res_aov_high)[1][[1]][[3]][[2]]))

loop_CPC_arterial <- loop_CPC %>%
  filter(Road_type == "Arterial")
res_aov_art <- aov(CPC ~ Road_ID, data = loop_CPC_arterial)
ICC_CPC_art <- (summary(res_aov_art)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_art)[1][[1]][[3]][[1]] + 
                     summary(res_aov_art)[1][[1]][[3]][[2]]))

loop_CPC_resi <- loop_CPC %>%
  filter(Road_type == "Residential")
res_aov_res <- aov(CPC ~ Road_ID, data = loop_CPC_resi)
ICC_CPC_res <- (summary(res_aov_res)[1][[1]][[3]][[1]] / 
                  (summary(res_aov_res)[1][[1]][[3]][[1]] + 
                     summary(res_aov_res)[1][[1]][[3]][[2]]))

ICC_table_All <- data.frame(Area = c("All", "All", "All"), Pollutant = c("BC", "UFPs", "CO2"),
                            ICC = c(ICC_BC_LC, ICC_CPC, ICC_CO2_c), 
                            ICC_Highway = c(ICC_BC_LC_high, ICC_CPC_high, ICC_CO2_c_high),
                            ICC_Arterial = c(ICC_BC_LC_art, ICC_CPC_art, ICC_CO2_c_art),
                            ICC_Residential = c(ICC_BC_LC_res, ICC_CPC_res, ICC_CO2_c_res))
ICC_table <- rbind(ICC_table, ICC_table_All)
beepr::beep()
write.csv(ICC_table, "D:/Dropbox/APMfull/MAL_CNG_Paper/ICC.csv")
