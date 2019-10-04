# This R script analyzes data of the Orange Line
# Settings
if (!require("pacman")) {install.packages("pacman"); library(pacman)}
p_load(dplyr, lubridate, ggplot2, stargazer, stringr, foreign)

# Weather data ====
# 1803651 location: PORTLAND INTERNATIONAL AIRPORT, OR US （45.5958	-122.6093）
wd <- read.csv("data/weather/1803651.csv", stringsAsFactors = F) %>% 
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
         AvgSpeed=as.numeric(Average.Speed..mph.)) %>%
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
            SDSpeed=sd(AvgSpeed, na.rm=TRUE),
            AvgSpeed=mean(AvgSpeed, na.rm=TRUE),
            day_week=first(day_week),
            before_after=first(before_after),
            ec_group=first(ec_group))  %>%
  ungroup() %>% 
  left_join(wd, by=c("date"))


# Select the obsersvations during peak hours of weekday 
data_combined_15_wdpeak <- data_combined_15 %>%
                           filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))                           

data_combined_day_wdpeak <- data_combined_day %>%
                            filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))  

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



