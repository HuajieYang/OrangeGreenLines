# This R script prepare data of the Orange Line for analysis
# Settings
if (!require("pacman")) {install.packages("pacman"); library(pacman)}
p_load(dplyr, lubridate, ggplot2, stargazer, stringr, foreign)

# Weather data ====
# 1803651 location: PORTLAND INTERNATIONAL AIRPORT, OR US （45.5958	-122.6093）
wd <- read.csv("data/weather/1803651.csv", stringsAsFactors = F) 

wd <- wd %>% 
      select(date=DATE, PRCP, TMAX, TMIN, TAVG)  %>% 
      mutate(date=as.Date(date),
             TMAX90=ifelse(TMAX>90, 1,0))

# Travel time speed data ====
Powell_E_TTSP <- read.delim("data/Travel time and speed/SE Powell Blvd (E).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE)
Powell_W_TTSP <- read.delim("data/Travel time and speed/SE Powell Blvd (W).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 

Holgate_E_TTSP <- read.delim("data/Travel time and speed/SE Holgate Blvd (E).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 
Holgate_W_TTSP <- read.delim("data/Travel time and speed/SE Holgate Blvd (W).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 

McLoughlin_N_TTSP <- read.delim("data/Travel time and speed/SE McLoughlin Blvd (N).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 
McLoughlin_S_TTSP <- read.delim("data/Travel time and speed/SE McLoughlin Blvd (S).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 

Milwaukie_17th_N_TTSP <- read.delim("data/Travel time and speed/SE Milwaukie AveSE 17th (N).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 
Milwaukie_17th_S_TTSP <- read.delim("data/Travel time and speed/SE Milwaukie AveSE 17th (S).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 

Division_E_TTSP <- read.delim("data/Travel time and speed/SE Division St (E).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 
Division_W_TTSP <- read.delim("data/Travel time and speed/SE Division St (W).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE)

# Reorganze data 
Powell_E_TTSP <- Powell_E_TTSP %>% mutate(roadway="Powell_E")
Powell_W_TTSP <- Powell_W_TTSP %>% mutate(roadway="Powell_W")

Holgate_E_TTSP <- Holgate_E_TTSP %>% mutate(roadway="Holgate_E")
Holgate_W_TTSP <- Holgate_W_TTSP %>% mutate(roadway="Holgate_W")

McLoughlin_N_TTSP <- McLoughlin_N_TTSP %>% mutate(roadway="McLoughlin_N")
McLoughlin_S_TTSP <- McLoughlin_S_TTSP %>% mutate(roadway="McLoughlin_S")

Milwaukie_17th_N_TTSP <- Milwaukie_17th_N_TTSP %>% mutate(roadway="Milwaukie_17th_N")
Milwaukie_17th_S_TTSP <- Milwaukie_17th_S_TTSP %>% mutate(roadway="Milwaukie_17th_S")

Division_E_TTSP <- Division_E_TTSP %>% mutate(roadway="Division_E")
Division_W_TTSP <- Division_W_TTSP %>% mutate(roadway="Division_W")

data_combined_5 <- rbind(Powell_E_TTSP, Powell_W_TTSP, Holgate_E_TTSP, Holgate_W_TTSP, McLoughlin_N_TTSP, 
                         McLoughlin_S_TTSP, Milwaukie_17th_N_TTSP, Milwaukie_17th_S_TTSP, Division_E_TTSP, Division_W_TTSP)

# Peak period from Giuliano's paper: the AM peak is 7 to 10 a.m., and the PM peak is 4 to 7 p.m.
# Wiki (https://en.wikipedia.org/wiki/Rush_hour#United_States):  6–10 am (6:00–10:00) and 4–8 pm (16:00–20:00)
data_combined_5 <- data_combined_5 %>%
  mutate(date_time=mdy_hm(X5.Minutes),
         date=as.Date(date_time),
         TTime=as.numeric(Avg.Travel.Time..mins.),
         AvgSpeed=as.numeric(Average.Speed..mph.)) %>% # Some observations of Avg.Travel.Time..mins are "closed", generating NAs 
  filter(date > "2012-09-11", date < "2018-09-12") %>%
  mutate(day_week=weekdays(date),
         before_after=ifelse(date < ymd("2015-09-12"), 0, 1),
         time_hour=hour(date_time),
         min5=minute(date_time),
         min15=ifelse(min5 %in% c(0,  5,  10), 1, NA),
         min15=ifelse(min5 %in% c(15, 20, 25), 2, min15),
         min15=ifelse(min5 %in% c(30, 35, 40), 3, min15),
         min15=ifelse(min5 %in% c(45, 50, 55), 4, min15),
         AM_PM=ifelse(time_hour %in% c(7, 8, 9), "AM", "NonPeak"), 
         AM_PM=ifelse(time_hour %in% c(16, 17, 18), "PM", AM_PM),
         ec_group=ifelse(roadway %in% c("Powell_E", "Powell_W", "Holgate_E", "Holgate_W", "Division_E", "Division_W"), 
                         "control", "experimental"),
         direction=ifelse(roadway %in% c("Division_E", "Holgate_E", "McLoughlin_S", 
                                         "Milwaukie_17th_S", "Powell_E"), "Outbound", "Inbound")
         ) 

data_combined_15 <- data_combined_5 %>%
                    group_by(roadway, date, time_hour, min15) %>% 
                    summarise(TTime=mean(TTime, na.rm=TRUE),
                              AvgSpeed=mean(AvgSpeed, na.rm=TRUE),
                              AM_PM=first(AM_PM),
                              day_week=first(day_week),
                              before_after=first(before_after),
                              ec_group=first(ec_group))  %>%
                    ungroup() %>% 
                    left_join(wd, by=c("date"))

data_combined_day <- data_combined_5 %>%
                      group_by(roadway, date, AM_PM) %>% 
                      summarise(TTime=mean(TTime, na.rm=TRUE),
                                # SDSpeed=sd(AvgSpeed, na.rm=TRUE),
                                AvgSpeed=mean(AvgSpeed, na.rm=TRUE),
                                day_week=first(day_week),
                                before_after=first(before_after),
                                ec_group=first(ec_group))  %>%
                      ungroup() %>% 
                      left_join(wd, by=c("date"))

# Select the observations during weekday peak periods
data_combined_15_wdpeak <- data_combined_15 %>%
                           filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))                           

data_combined_day_wdpeak <- data_combined_day %>%
                            filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))  

# Calculate early morning (2:00 am - 3:59 am) speed on weekdays 
em_summary <- data_combined_5 %>%
              filter(time_hour %in% c(2, 3), !(day_week %in% c("Saturday", "Sunday")))  %>%
              group_by(roadway, date)  %>%
              summarise(em_TTime=mean(TTime, na.rm=TRUE),
                        em_AvgSpeed=mean(AvgSpeed, na.rm=TRUE)) %>%
              ungroup() %>%
              group_by(roadway)  %>%
              summarise(his_em_AvgSpeed_min=min(em_AvgSpeed, na.rm = TRUE),
                        his_em_AvgSpeed_median=median(em_AvgSpeed, na.rm = TRUE),
                        his_em_AvgSpeed_mean=mean(em_AvgSpeed, na.rm = TRUE),
                        his_em_AvgSpeed_max=max(em_AvgSpeed, na.rm = TRUE),
                        his_em_AvgSpeed_sd=sd(em_AvgSpeed, na.rm = TRUE)) %>%
              ungroup()
              
# Calculate historical average speed
his_avg_wdpeak <- data_combined_day_wdpeak %>%
                  group_by(roadway) %>%
                  summarise(his_peak_avg_sp=mean(AvgSpeed, na.rm=TRUE),
                           his_peak_avg_tt=mean(TTime, na.rm=TRUE))

data_combined_day_wdpeak <- data_combined_day_wdpeak %>%
                            left_join(em_summary) %>%
                            left_join(his_avg_wdpeak)  %>%
                            mutate(DDV_his_peak_avg=AvgSpeed - his_peak_avg_sp, # day-to-day variation (DDV): keep DDV to be positive
                                   DDV_his_em_avg=his_em_AvgSpeed_mean - AvgSpeed,
                                   month_num=month(date),
                                   year_num=year(date),
                                   year_1st=ifelse(date>="2015-09-12"&date<="2016-09-11", 1, 0),
                                   year_2nd=ifelse(date>="2016-09-12"&date<="2017-09-11", 1, 0),
                                   year_3rd=ifelse(date>="2017-09-12"&date<="2018-09-11", 1, 0)) 

save(data_combined_day_wdpeak, file="output/intermediate/data_combined_day_wdpeak.RData")

# Average flow of vehicel data====
# Load average flow of vehicel data 
# Traffic flow:  This is the number of vehicles per time period of granularity. For routes there are typically multiple data 
#                sources providing flows, so the various data sources’ flows are averaged to provide the average flow on the 
#                route for the specified time period.

Powell_E_FV <- read.delim("data/PeMS/SE Powell Blvd (E)/Average Flow & Veh-hours of Delay SL.txt", header = TRUE, sep="\t", stringsAsFactors=FALSE)
Powell_W_FV <- read.delim("data/PeMS/SE Powell Blvd (W)/Average Flow & Veh-hours of delay.txt", header = TRUE, sep="\t", stringsAsFactors=FALSE)

Holgate_E_FV <- read.delim("data/PeMS/SE Holgate Blvd (E)/Average Flow & Veh-hours of delay.txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 
Holgate_W_FV <- read.delim("data/PeMS/SE Holgate Blvd (W)/Average Flow & Veh-hours of delay SL.txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 

McLoughlin_N_FV <- read.delim("data/PeMS/SE McLoughlin Blvd (N)/Average flow and Veh-hours of delay SL.txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 
McLoughlin_S_FV <- read.delim("data/PeMS/SE McLoughlin Blvd (S)/Average Flow & Veh-hours of delay SL.txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 

Milwaukie_17th_N_FV <- read.delim("data/PeMS/SE Milwaukie AveSE 17th (N)/Average Flow & Veh-hours of delay.txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 
Milwaukie_17th_S_FV <- read.delim("data/PeMS/SE Milwaukie AveSE 17th (S)/Average Flow & Veh-hours of delay SL.txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 

Division_E_FV <- read.delim("data/PeMS/SE Division St (E)/Average Flow & Veh-hours of delay SL.txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 
Division_W_FV <- read.delim("data/PeMS/SE Division St (W)/Average Flow & Veh-hours of delay.txt", header = TRUE, sep="\t", stringsAsFactors=FALSE)

Powell_E_FV <- Powell_E_FV %>% mutate(roadway="Powell_E")
Powell_W_FV <- Powell_W_FV %>% mutate(roadway="Powell_W")

Holgate_E_FV <- Holgate_E_FV %>% mutate(roadway="Holgate_E")
Holgate_W_FV <- Holgate_W_FV %>% mutate(roadway="Holgate_W")

McLoughlin_N_FV <- McLoughlin_N_FV %>% mutate(roadway="McLoughlin_N")
McLoughlin_S_FV <- McLoughlin_S_FV %>% mutate(roadway="McLoughlin_S")

Milwaukie_17th_N_FV <- Milwaukie_17th_N_FV %>% mutate(roadway="Milwaukie_17th_N")
Milwaukie_17th_S_FV <- Milwaukie_17th_S_FV %>% mutate(roadway="Milwaukie_17th_S")

Division_E_FV <- Division_E_FV %>% mutate(roadway="Division_E")
Division_W_FV <- Division_W_FV %>% mutate(roadway="Division_W")

FV_all <- rbind(Powell_E_FV, Powell_W_FV, Holgate_E_FV, Holgate_W_FV, McLoughlin_N_FV, McLoughlin_S_FV, 
                Milwaukie_17th_N_FV, Milwaukie_17th_S_FV, Division_E_FV, Division_W_FV)

# Peak period from Giuliano's paper: the AM peak is 7 to 10 a.m., and the PM peak is 4 to 7 p.m.
# Wiki (https://en.wikipedia.org/wiki/Rush_hour#United_States):  6–10 am (6:00–10:00) and 4–8 pm (16:00–20:00)
FV_all_5 <- FV_all %>%
  mutate(date_time=mdy_hm(X5.Minutes),
         date=as.Date(date_time),
         FVeh=as.numeric(Average.Flow..Vehs.)) %>%
  filter(date > "2012-09-11", date < "2018-09-12") %>%
  mutate(day_week=weekdays(date),
         time_hour=hour(date_time),
         min5=minute(date_time),
         min15=ifelse(min5 %in% c(0,  5,  10), 1, NA),
         min15=ifelse(min5 %in% c(15, 20, 25), 2, min15),
         min15=ifelse(min5 %in% c(30, 35, 40), 3, min15),
         min15=ifelse(min5 %in% c(45, 50, 55), 4, min15),
         AM_PM=ifelse(time_hour %in% c(7, 8, 9), "AM", "NonPeak"), 
         AM_PM=ifelse(time_hour %in% c(16, 17, 18), "PM", AM_PM)) 

FV_all_15 <- FV_all_5 %>%
  group_by(roadway, date, time_hour, min15) %>% 
  summarise(FVeh=mean(FVeh, na.rm=TRUE),
            AM_PM=first(AM_PM),
            day_week=first(day_week))  %>%
  ungroup() 

FV_all_day <- FV_all_5 %>%
  group_by(roadway, date, AM_PM) %>% 
  summarise(FVeh=mean(FVeh, na.rm=TRUE),
            day_week=first(day_week))  %>%
  ungroup() 

# Select the obsersvations during peak hours of weekday 
FV_all_15_wdpeak <- FV_all_15 %>%
  filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))                           

FV_all_wdpeak <- FV_all_day %>%
  filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))  

# Flow of vehicle is not useful: there are no data for non-freeway roadways; 
# data of freeway varies by day of week (Mondays: 97.3; Tuesday: 136.7)
test <- FV_all_wdpeak %>% filter(AM_PM=="AM", roadway %in% c("McLoughlin_S", "Powell_E"))
View(test)


# Transit ridership data ====
ridership <- read.csv("data/Passenger Census Dump - All.csv", as.is=TRUE)

month_df <- data.frame(month_num=c(1:12), 
                       month_char=c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))

ridership_w <- ridership %>% 
               filter(SERVICE_KEY=="W") %>%
               mutate(SUMMARY_BEGIN_DATE=as.character(SUMMARY_BEGIN_DATE),
                      YEAR=as.numeric(substr(SUMMARY_BEGIN_DATE, 6,9)),
                      MONTH=substr(SUMMARY_BEGIN_DATE, 3,5),
                      DAY=as.numeric(substr(SUMMARY_BEGIN_DATE, 1,2)),
                      TIME=substr(SUMMARY_BEGIN_DATE, 11, 18),
                      rt_dir_stopdesc=paste(ROUTE_NUMBER, DIRECTION, PUBLIC_LOCATION_DESCRIPTION, sep="_")
                      )  %>%
              left_join(month_df, by=c("MONTH"="month_char")) %>%
              mutate(date=ymd(paste(YEAR, month_num, DAY, sep="-")))
              

# Experimental and control stops ====
# Analyze GIS files 
buslines <-  read.dbf("output/intermediate/buslines.dbf", as.is=TRUE)
busstops <-  read.dbf("output/intermediate/busstops.dbf", as.is=TRUE)

# Load GIS bus stop data 
control_busstops <-  read.dbf("data/GIS/Control_Corridor_BusStops.dbf", as.is=TRUE) 
experimental_busstops <- read.dbf("data/GIS/Experimental_Corridor_BusStops.dbf", as.is=TRUE) 

control_busstops <- control_busstops %>%
                    filter(ROUTE!="MAX")  %>%
                    mutate(ROUTE=as.numeric(ROUTE),
                           rt_dir_stopdesc=paste(ROUTE, DIR, LOCATION, sep="_"))

experimental_busstops <- experimental_busstops %>%
                          filter(!(ROUTE %in% c("MAX", "STREETCAR"))) %>%
                          mutate(ROUTE=as.numeric(ROUTE),                           
                                 rt_dir_stopdesc=paste(ROUTE, DIR, LOCATION, sep="_"))

# extract ridership of control stops from ridership data 
ridership_w_rt_control <- ridership_w %>%
                          filter(rt_dir_stopdesc %in% control_busstops$rt_dir_stopdesc)

# all rt_dir_stopdesc are available from ridership dataset 
# sort(unique(ridership_w_rt_control$rt_dir_stopdesc))==sort(unique(control_busstops$rt_dir_stopdesc)) 

ridership_w_rt_experimental <- ridership_w %>%
                               filter(rt_dir_stopdesc %in% experimental_busstops$rt_dir_stopdesc)

# sort(unique(ridership_w_rt_experimental$rt_dir_stopdesc))==sort(unique(experimental_busstops$rt_dir_stopdesc))

# There are five stops that do not appear in ridership dataset 
# setdiff(sort(unique(experimental_busstops$rt_dir_stopdesc)), sort(unique(ridership_w_rt_experimental$rt_dir_stopdesc)))

# test <- experimental_busstops %>%
#   filter(rt_dir_stopdesc %in% c("196_0_SE Grand & Mill", "196_0_SE Water/OMSI (Streetcar)", "196_1_SE M L King & Mill",
#                                 "196_1_SE Water/OMSI (Streetcar)", "291_0_SE McLoughlin & Park Ave"))
# 
# 
# test1 <- ridership_w %>% # filter(PUBLIC_LOCATION_DESCRIPTION %in% c("SE Grand & Mill", "SE Water/OMSI (Streetcar)",
#   #                                          "SE M L King & Mill", "SE Water/OMSI (Streetcar)",
#   #                                          "SE McLoughlin & Park Ave")) %>%
#   filter(ROUTE_NUMBER %in% c(196, 291), YEAR==2019) 
# 
# test2 <- experimental_busstops %>% filter(ROUTE==291)

# Post DID regression analysis ====
spec_mx <- matrix(c(1, 1, 1, 1, 1, 1,
                    0, 1, 0, 1, 0, 1,
                    0, 0, 1, 1, 1, 1, 
                    0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE)

# major arterial speed base models
sp_amo_day_ma_coef_mx <- t(as.matrix(summary(sp_amo_day_ma)$coefficient[, 1]))
sp_ami_day_ma_coef_mx <- t(as.matrix(summary(sp_ami_day_ma)$coefficient[, 1]))
sp_pmo_day_ma_coef_mx <- t(as.matrix(summary(sp_pmo_day_ma)$coefficient[, 1]))
sp_pmi_day_ma_coef_mx <- t(as.matrix(summary(sp_pmi_day_ma)$coefficient[, 1]))

sp_did_ma_df <- data.frame(Group=rep(c("Control", "Treated", "Counterfactual"), each=2), 
                           Before_after=rep(c("before", "after"), 3),
                           sp_amo_day_ma_speed=as.numeric(sp_amo_day_ma_coef_mx %*% spec_mx),
                           sp_ami_day_ma_speed=as.numeric(sp_ami_day_ma_coef_mx %*% spec_mx),
                           sp_pmo_day_ma_speed=as.numeric(sp_pmo_day_ma_coef_mx %*% spec_mx),
                           sp_pmi_day_ma_speed=as.numeric(sp_pmi_day_ma_coef_mx %*% spec_mx)) 

save(sp_did_ma_df, file="output/intermediate/sp_did_ma_df.RData")

# major arterial day-to-day variation base models 
ddv_amo_day_ma_coef_mx <- t(as.matrix(summary(ddv_amo_day_ma)$coefficient[, 1]))
ddv_ami_day_ma_coef_mx <- t(as.matrix(summary(ddv_ami_day_ma)$coefficient[, 1]))
ddv_pmo_day_ma_coef_mx <- t(as.matrix(summary(ddv_pmo_day_ma)$coefficient[, 1]))
ddv_pmi_day_ma_coef_mx <- t(as.matrix(summary(ddv_pmi_day_ma)$coefficient[, 1]))


ddv_did_ma_df <- data.frame(Group=rep(c("Control", "Treated", "Counterfactual"), each=2), 
                            Before_after=rep(c("before", "after"), 3),
                            ddv_amo_day_ma_speed=as.numeric(ddv_amo_day_ma_coef_mx %*% spec_mx),
                            ddv_ami_day_ma_speed=as.numeric(ddv_ami_day_ma_coef_mx %*% spec_mx),
                            ddv_pmo_day_ma_speed=as.numeric(ddv_pmo_day_ma_coef_mx %*% spec_mx),
                            ddv_pmi_day_ma_speed=as.numeric(ddv_pmi_day_ma_coef_mx %*% spec_mx)) 

save(ddv_did_ma_df, file="output/intermediate/ddv_did_ma_df.RData")


# local roadway speed base models
sp_amo_day_lr_coef_mx <- t(as.matrix(summary(sp_amo_day_lr)$coefficient[, 1]))
sp_ami_day_lr_coef_mx <- t(as.matrix(summary(sp_ami_day_lr)$coefficient[, 1]))
sp_pmo_day_lr_coef_mx <- t(as.matrix(summary(sp_pmo_day_lr)$coefficient[, 1]))
sp_pmi_day_lr_coef_mx <- t(as.matrix(summary(sp_pmi_day_lr)$coefficient[, 1]))

sp_did_lr_df <- data.frame(Group=rep(c("Control", "Treated", "Counterfactual"), each=2), 
                           Before_after=rep(c("before", "after"), 3),
                           sp_amo_day_lr_speed=as.numeric(sp_amo_day_lr_coef_mx %*% spec_mx),
                           sp_ami_day_lr_speed=as.numeric(sp_ami_day_lr_coef_mx %*% spec_mx),
                           sp_pmo_day_lr_speed=as.numeric(sp_pmo_day_lr_coef_mx %*% spec_mx),
                           sp_pmi_day_lr_speed=as.numeric(sp_pmi_day_lr_coef_mx %*% spec_mx)) 

save(sp_did_lr_df, file="output/intermediate/sp_did_lr_df.RData")

# local roadway day-to-day variation base models 
ddv_amo_day_lr_coef_mx <- t(as.matrix(summary(ddv_amo_day_lr)$coefficient[, 1]))
ddv_ami_day_lr_coef_mx <- t(as.matrix(summary(ddv_ami_day_lr)$coefficient[, 1]))
ddv_pmo_day_lr_coef_mx <- t(as.matrix(summary(ddv_pmo_day_lr)$coefficient[, 1]))
ddv_pmi_day_lr_coef_mx <- t(as.matrix(summary(ddv_pmi_day_lr)$coefficient[, 1]))


ddv_did_lr_df <- data.frame(Group=rep(c("Control", "Treated", "Counterfactual"), each=2), 
                            Before_after=rep(c("before", "after"), 3),
                            ddv_amo_day_lr_speed=as.numeric(ddv_amo_day_lr_coef_mx %*% spec_mx),
                            ddv_ami_day_lr_speed=as.numeric(ddv_ami_day_lr_coef_mx %*% spec_mx),
                            ddv_pmo_day_lr_speed=as.numeric(ddv_pmo_day_lr_coef_mx %*% spec_mx),
                            ddv_pmi_day_lr_speed=as.numeric(ddv_pmi_day_lr_coef_mx %*% spec_mx)) 

save(ddv_did_lr_df, file="output/intermediate/ddv_did_lr_df.RData")


