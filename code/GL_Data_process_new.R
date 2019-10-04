# This script analyzes 5-min interval travel data for the MAX Green Line 
# Settings
if (!require("pacman")) {install.packages("pacman"); library(pacman)}
p_load(dplyr, lubridate, ggplot2, stargazer)

# Load data ====
# Weather data 
# 1803651 location: PORTLAND INTERNATIONAL AIRPORT, OR US （45.5958	-122.6093）
# 1803661 location: PORTLAND WEATHER FORECAST OFFICE, OR US （(45.5605	-122.5383)

wd1 <- read.csv("data/weather/1803651.csv", stringsAsFactors = F) %>% 
      select(date=DATE, PRCP, TMAX, TMIN, TAVG)  %>% 
      mutate(date=as.Date(date),
             TMAX90=ifelse(TMAX>90, 1,0))

# wd2 <- read.csv("data/weather/1803661.csv", stringsAsFactors = F) %>% select(DATE, PRCP, TMAX, TMIN, TOBS)

# Detecor data 
data_5min_0405 <- read.csv("data/data_5min_040912_050912.csv", stringsAsFactors = F) 
data_5min_0506 <- read.csv("data/data_5min_050912_060912.csv", stringsAsFactors = F) 
data_5min_0607 <- read.csv("data/data_5min_060912_070912.csv", stringsAsFactors = F) 
data_5min_0708 <- read.csv("data/data_5min_070912_080912.csv", stringsAsFactors = F) 
data_5min_0809 <- read.csv("data/data_5min_080912_090912.csv", stringsAsFactors = F) 
data_5min_0910 <- read.csv("data/data_5min_090912_100912.csv", stringsAsFactors = F) 
data_5min_1011 <- read.csv("data/data_5min_100912_110912.csv", stringsAsFactors = F) 
data_5min_1112 <- read.csv("data/data_5min_110912_120912.csv", stringsAsFactors = F) 
data_5min_1213 <- read.csv("data/data_5min_120912_130912.csv", stringsAsFactors = F) 
data_5min_1314 <- read.csv("data/data_5min_130912_140912.csv", stringsAsFactors = F) 

# Metadata 
detector_meta <- read.csv("data/metadata/detectors_metadata.csv", stringsAsFactors = F) # detector metadata
stations_meta <- read.csv("data/metadata/stations_metadata.csv", stringsAsFactors = F) # station metadata
highways_meta <- read.csv("data/metadata/highways_metadata.csv", stringsAsFactors = F) # highway metadata 

# Identify stationids ====
# I-84
# good on I-84:1055, 1056, 1057, 1059, 1060, 1061, 1062, 1127
# outside of experimental zone: 1058, 1097, 1145 (it is not ramp but looks like a ramp) 
# ramp stations (5000) with lat&lon: 5061, whose lat&lon is far away from the freeway
stations_I84 <- stations_meta %>% 
                  filter(highwayid %in% c(7, 8), start_date=="2004-01-01 00:00:00-08") %>% 
                  select(stationid, lat, lon, highwayid, locationtext, start_date, end_date) %>% 
                  filter(stationid %in% c(1055, 1056, 1057, 1059, 1060, 1061, 1062, 1127))  %>% 
                  arrange(stationid)

# I-205
# highway id for I-205: 3, 4
# good on I-205 within experimental zone: lat&lon of 1142 is a little far away from the freeway
# Outside of experimental zone: lat <= 45.42950 (stationid 1124), lat >= 45.56085 (stationid 1141)
# ramp stations (5000) with lat&lon: 5126, 5098, whose lat&lon is far away from the freeway
stations_I205 <- stations_meta %>% 
                  filter(highwayid %in% c(3, 4), start_date=="2004-01-01 00:00:00-08") %>% 
                  select(stationid,lat, lon, highwayid, locationtext, start_date, end_date) %>%
                  filter(!is.na(lat) & (lat > 45.4296 & lat < 45.5608) & stationid<5000)  %>%
                  arrange(stationid)


# highway id for  I-5: 1, 2
# Outside of experimental zone: lat <= 45.37870 (stationid 1040), lat >= 45.51715 (stationid 1016)
# 3114 and 3197 are not included in data_5min 
stations_I5 <- stations_meta %>% 
                filter(highwayid %in% c(1, 2), start_date=="2004-01-01 00:00:00-08") %>% 
                select(stationid, lat, lon, highwayid, locationtext, start_date, end_date)%>%
                filter(!is.na(lat)&(lat < 45.5171 & lat > 45.3788))  %>%
                arrange(stationid)

stations_green <- rbind(stations_I84, stations_I205, stations_I5)

# Identify highwayid, stationid, lanenumber for each detector====
# Select data for stations identified for the Green Line
detector_meta_sub <- detector_meta %>%
                     select(highwayid, stationid, detector_id=detectorid, lanenumber) 

data_5min_0405_green <- data_5min_0405 %>%
                         left_join(detector_meta_sub) %>%
                         filter(stationid %in% stations_green$stationid)  

data_5min_0506_green <- data_5min_0506 %>%
                        left_join(detector_meta_sub) %>%
                        filter(stationid %in% stations_green$stationid)  

data_5min_0607_green <- data_5min_0607 %>%
                        left_join(detector_meta_sub) %>%
                        filter(stationid %in% stations_green$stationid)  

data_5min_0708_green <- data_5min_0708 %>%
                        left_join(detector_meta_sub) %>%
                        filter(stationid %in% stations_green$stationid)  

data_5min_0809_green <- data_5min_0809 %>%
                        left_join(detector_meta_sub) %>%
                        filter(stationid %in% stations_green$stationid)  

data_5min_0910_green <- data_5min_0910 %>%
                        left_join(detector_meta_sub) %>%
                        filter(stationid %in% stations_green$stationid)  

data_5min_1011_green <- data_5min_1011 %>%
                        left_join(detector_meta_sub) %>%
                        filter(stationid %in% stations_green$stationid)  

data_5min_1112_green <- data_5min_1112 %>%
                        left_join(detector_meta_sub) %>%
                        filter(stationid %in% stations_green$stationid)  

data_5min_1213_green <- data_5min_1213 %>%
                        left_join(detector_meta_sub) %>%
                        filter(stationid %in% stations_green$stationid)  

data_5min_1314_green <- data_5min_1314 %>%
                        left_join(detector_meta_sub) %>%
                        filter(stationid %in% stations_green$stationid)  

save(data_5min_0405_green, file="output/intermediate/data_5min_0405_green.RData")
save(data_5min_0506_green, file="output/intermediate/data_5min_0506_green.RData")
save(data_5min_0607_green, file="output/intermediate/data_5min_0607_green.RData")
save(data_5min_0708_green, file="output/intermediate/data_5min_0708_green.RData")
save(data_5min_0809_green, file="output/intermediate/data_5min_0809_green.RData")
save(data_5min_0910_green, file="output/intermediate/data_5min_0910_green.RData")
save(data_5min_1011_green, file="output/intermediate/data_5min_1011_green.RData")
save(data_5min_1112_green, file="output/intermediate/data_5min_1112_green.RData")
save(data_5min_1213_green, file="output/intermediate/data_5min_1213_green.RData")
save(data_5min_1314_green, file="output/intermediate/data_5min_1314_green.RData")


# Load data 
load("~/OrangeGreenLines/output/intermediate/data_5min_0405_green.RData")
load("~/OrangeGreenLines/output/intermediate/data_5min_0506_green.RData")
load("~/OrangeGreenLines/output/intermediate/data_5min_0607_green.RData")
load("~/OrangeGreenLines/output/intermediate/data_5min_0708_green.RData")
load("~/OrangeGreenLines/output/intermediate/data_5min_0809_green.RData")
load("~/OrangeGreenLines/output/intermediate/data_5min_0910_green.RData")
load("~/OrangeGreenLines/output/intermediate/data_5min_1011_green.RData")
load("~/OrangeGreenLines/output/intermediate/data_5min_1112_green.RData")
load("~/OrangeGreenLines/output/intermediate/data_5min_1213_green.RData")
load("~/OrangeGreenLines/output/intermediate/data_5min_1314_green.RData")


# Analysis===== 
# Remove 09-12 00:00:00-07 for each year because it exisits in the following year dataset
# Remove # detector ids in differenent year: 1949 1950 1951 1953 1954 1955 appears since 2006-2007; 
# 100369 100370 100374 100375 100376 100435 100436 100437 appears since 2013-2014
data_5min_0405_green <- data_5min_0405_green %>%
  filter(!(starttime %in% c("2004-12-31 23:55:00-08", "2005-09-12 00:00:00-07")))

data_5min_0506_green <- data_5min_0506_green %>%
  filter(starttime!="2006-09-12 00:00:00-07")

data_5min_0607_green <- data_5min_0607_green %>%
  filter(starttime!="2007-09-12 00:00:00-07") %>% 
  filter(!(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955)))

data_5min_0708_green <- data_5min_0708_green %>%
  filter(starttime!="2005-09-12 00:00:00-07") %>% 
  filter(!(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955)))

data_5min_0809_green <- data_5min_0809_green %>%
  filter(starttime!="2009-09-12 00:00:00-07") %>% 
  filter(!(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955)))

data_5min_0910_green <- data_5min_0910_green %>%
  filter(starttime!="2010-09-12 00:00:00-07") %>% 
  filter(!(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955)))

data_5min_1011_green <- data_5min_1011_green %>%
  filter(starttime!="2011-09-12 00:00:00-07") %>% 
  filter(!(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955)))

data_5min_1112_green <- data_5min_1112_green %>%
  filter(starttime!="2012-09-12 00:00:00-07") %>% 
  filter(!(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955)))

data_5min_1213_green <- data_5min_1213_green %>%
  filter(starttime!="2013-09-12 00:00:00-07") %>% 
  filter(!(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955)))

data_5min_1314_green <- data_5min_1314_green %>%
  filter(starttime!="2014-09-12 00:00:00-07") %>% 
  filter(!(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955, 
                            100369, 100370, 100374, 100375, 100376, 
                            100435, 100436, 100437)))


# 07, 08 in starttime means time zone, which does not matter after checking
#  highwayid stationid lanenumber detector_id              starttime           date_time       date
#          1      1004          2        1026 2005-10-30 01:50:00-07 2005-10-30 01:50:00 2005-10-30
#          1      1004          2        1026 2005-10-30 01:55:00-07 2005-10-30 01:55:00 2005-10-30
#          1      1004          2        1026 2005-10-30 02:00:00-08 2005-10-30 02:00:00 2005-10-30
#          1      1004          2        1026 2005-10-30 02:05:00-08 2005-10-30 02:05:00 2005-10-30

#  highwayid stationid ec_group lanenumber detector_id              starttime           date_time       date
#          1      1004  control          2        1026 2006-04-02 01:45:00-08 2006-04-02 01:45:00 2006-04-02
#          1      1004  control          2        1026 2006-04-02 01:50:00-08 2006-04-02 01:50:00 2006-04-02
#          1      1004  control          2        1026 2006-04-02 03:00:00-07 2006-04-02 03:00:00 2006-04-02
#          1      1004  control          2        1026 2006-04-02 03:05:00-07 2006-04-02 03:05:00 2006-04-02

# the peak period is defined the same as commuting time 
data_5min_0414_green <- rbind(data_5min_0405_green, data_5min_0506_green, data_5min_0607_green, data_5min_0708_green,
                              data_5min_0809_green, data_5min_0910_green, data_5min_1011_green, data_5min_1112_green,       
                              data_5min_1213_green, data_5min_1314_green) %>%
                        mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
                               date=as.Date(starttime),
                               day_week=weekdays(date),
                               before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                               time_hour=hour(date_time),
                               AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
                               AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM),
                               ec_group=ifelse(highwayid %in% c(1,2), "control", "experimental")) %>%
                        select(highwayid, ec_group, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, time_hour, before_after, 
                               AM_PM, speed, volume)  %>% #  
                        arrange(highwayid, stationid, lanenumber, detector_id, date_time)  

# Convert 5 minutes interval data to 15 minutes interval data 
data_15min_0414_green <-  data_5min_0414_green %>%
                          mutate(hour=hour(date_time),
                                 min5=minute(date_time),
                                 min15=ifelse(min5 %in% c(0,  5,  10), 1, NA),
                                 min15=ifelse(min5 %in% c(15, 20, 25), 2, min15),
                                 min15=ifelse(min5 %in% c(30, 35, 40), 3, min15),
                                 min15=ifelse(min5 %in% c(45, 50, 55), 4, min15)) %>%
                          group_by (highwayid, stationid, lanenumber, detector_id, date, hour, min15)  %>%
                          summarise(speed=mean(speed, na.rm=TRUE),
                                    volume=mean(volume, na.rm=TRUE),
                                    AM_PM=first(AM_PM),
                                    day_week=first(day_week),
                                    before_after=first(before_after),
                                    ec_group=first(ec_group)
                                    )  

save(data_15min_0414_green, file="output/intermediate/data_15min_0414_green.RData")

# 15-min interval at the station level ====
load("output/intermediate/data_15min_0414_green.RData")
data_15min_0414_green_wdpeak <- data_15min_0414_green %>%
                               filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))

save(data_15min_0414_green_wdpeak, file="output/intermediate/data_15min_0414_green_wdpeak.RData")

load("output/intermediate/data_15min_0414_green_wdpeak.RData")

data_15min_0414_green_wdpeak <- data_15min_0414_green_wdpeak %>%
                                left_join(wd1, by=c("date"))  %>%
                                mutate(PRCP=as.numeric(PRCP))

# T-test 
# I-84 
t.test(data_15min_0414_green_wdpeak %>% filter(highwayid==7, AM_PM=="AM", before_after==0) %>% select(speed),
       data_15min_0414_green_wdpeak %>% filter(highwayid==7, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_15min_0414_green_wdpeak %>% filter(highwayid==8, AM_PM=="AM", before_after==0) %>% select(speed),
       data_15min_0414_green_wdpeak %>% filter(highwayid==8, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_15min_0414_green_wdpeak %>% filter(highwayid==7, AM_PM=="PM", before_after==0) %>% select(speed),
       data_15min_0414_green_wdpeak %>% filter(highwayid==7, AM_PM=="PM", before_after==1) %>% select(speed))

t.test(data_15min_0414_green_wdpeak %>% filter(highwayid==8, AM_PM=="PM", before_after==0) %>% select(speed),
       data_15min_0414_green_wdpeak %>% filter(highwayid==8, AM_PM=="PM", before_after==1) %>% select(speed))

# I-205
t.test(data_15min_0414_green_wdpeak %>% filter(highwayid==3, AM_PM=="AM", before_after==0) %>% select(speed),
       data_15min_0414_green_wdpeak %>% filter(highwayid==3, AM_PM=="AM", before_after==1) %>% select(speed))


t.test(data_15min_0414_green_wdpeak %>% filter(highwayid==3, AM_PM=="PM", before_after==0) %>% select(speed),
       data_15min_0414_green_wdpeak %>% filter(highwayid==3, AM_PM=="PM", before_after==1) %>% select(speed))


t.test(data_15min_0414_green_wdpeak %>% filter(highwayid==4, AM_PM=="AM", before_after==0) %>% select(speed),
       data_15min_0414_green_wdpeak %>% filter(highwayid==4, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_15min_0414_green_wdpeak %>% filter(highwayid==4, AM_PM=="PM", before_after==0) %>% select(speed),
       data_15min_0414_green_wdpeak %>% filter(highwayid==4, AM_PM=="PM", before_after==1) %>% select(speed))  

# I-5
t.test(data_15min_0414_green_wdpeak %>% filter(highwayid==1, AM_PM=="AM", before_after==0) %>% select(speed),
       data_15min_0414_green_wdpeak %>% filter(highwayid==1, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_15min_0414_green_wdpeak %>% filter(highwayid==1, AM_PM=="PM", before_after==0) %>% select(speed),
       data_15min_0414_green_wdpeak %>% filter(highwayid==1, AM_PM=="PM", before_after==1) %>% select(speed))

t.test(data_15min_0414_green_wdpeak %>% filter(highwayid==2, AM_PM=="AM", before_after==0) %>% select(speed),
       data_15min_0414_green_wdpeak %>% filter(highwayid==2, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_15min_0414_green_wdpeak %>% filter(highwayid==2, AM_PM=="PM", before_after==0) %>% select(speed),
       data_15min_0414_green_wdpeak %>% filter(highwayid==1, AM_PM=="PM", before_after==1) %>% select(speed))

# DID regression 
  sp_amo_station_15 <- lm(speed ~ before_after + ec_group + before_after*ec_group, 
                          data=data_15min_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(2, 4, 7)))
  
  sp_ami_station_15 <- lm(speed ~ before_after + ec_group + before_after*ec_group, 
                          data=data_15min_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(1, 3, 8)))
  
  sp_pmo_station_15 <- lm(speed ~ before_after + ec_group + before_after*ec_group, 
                          data=data_15min_0414_green_wdpeak %>% filter(AM_PM=="PM", highwayid %in% c(2, 4, 7)))
  
  sp_pmi_station_15 <- lm(speed ~ before_after + ec_group + before_after*ec_group, 
                          data=data_15min_0414_green_wdpeak %>% filter(AM_PM=="PM", highwayid %in% c(1, 3, 8)))
  
  stargazer(sp_amo_station_15, sp_ami_station_15, sp_pmo_station_15, sp_pmi_station_15, 
            column.labels = c("AM Outward", "AM Inward", "PM Outward",  "PM Inward"),
            dep.var.labels=c("Travel Speed"),
            covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                               "Time Period × Group"),
            type="text", out="output/intermediate/gl_station_15min_sp.htm")

# Add weather data 
  sp_amo_station_15_weather <- lm(speed ~ before_after + ec_group + PRCP + TMAX90 + before_after*ec_group,  
                                  data=data_15min_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(2, 4, 7)))
  
  sp_ami_station_15_weather <- lm(speed ~ before_after + ec_group + PRCP + TMAX90 + before_after*ec_group, 
                                  data=data_15min_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(1, 3, 8)))
  
  sp_pmo_station_15_weather <- lm(speed ~ before_after + ec_group + PRCP + TMAX90 + before_after*ec_group, 
                                  data=data_15min_0414_green_wdpeak %>% filter(AM_PM=="PM", highwayid %in% c(2, 4, 7)))
  
  sp_pmi_station_15_weather <- lm(speed ~ before_after + ec_group + PRCP + TMAX90 + before_after*ec_group, 
                                  data=data_15min_0414_green_wdpeak %>% filter(AM_PM=="PM", highwayid %in% c(1, 3, 8)))
  
  stargazer(sp_amo_station_15_weather, sp_ami_station_15_weather, sp_pmo_station_15_weather, sp_pmi_station_15_weather, 
            column.labels = c("AM Outward", "AM Inward", "PM Outward",  "PM Inward"),
            dep.var.labels=c("Travel Speed"),
            covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                               "Time Period × Group"),
            type="text", out="output/intermediate/gl_station_15min_sp.htm")
  

# Aggregate to AM/PM peak period at station level =====
data_station_day_0414_green_wdpeak <- data_15min_0414_green_wdpeak %>%
                                      group_by(highwayid, stationid, date, AM_PM) %>%
                                      summarise(speed=mean(speed, na.rm=TRUE),
                                                volume=mean(volume, na.rm=TRUE),
                                                before_after=first(before_after),
                                                ec_group=first(ec_group)) %>% 
                                      ungroup() 


# highwayid 1 I-5 North / highwayid 2 I-5 South 
# highwayid 3 I-205 North / highwayid 4 I-205 South
# highwayid 7 I-84 East/ highwayid 8 I-84 West 
sp_amo_station_day <- lm(speed ~ before_after + ec_group + before_after*ec_group, 
                         data=data_station_day_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(2, 4, 7)))

sp_ami_station_day <- lm(speed ~ before_after + ec_group + before_after*ec_group, 
                         data=data_station_day_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(1, 3, 8)))

sp_pmo_station_day <- lm(speed ~ before_after + ec_group + before_after*ec_group, 
                         data=data_station_day_0414_green_wdpeak %>% filter(AM_PM=="PM", highwayid %in% c(2, 4, 7)))

sp_pmi_station_day <- lm(speed ~ before_after + ec_group + before_after*ec_group, 
                         data=data_station_day_0414_green_wdpeak %>% filter(AM_PM=="PM", highwayid %in% c(1, 3, 8)))


stargazer(sp_amo_station_day, sp_ami_station_day, sp_pmo_station_day, sp_pmi_station_day, 
          column.labels = c("AM Outward", "AM Inward", "PM Outward",  "PM Inward"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "Time Period × Group"),
          type="text", out="output/intermediate/gl_station_day_sp.htm")

sp_amo_station_day_weather <- lm(speed ~ before_after + ec_group + PRCP + TMAX90 + before_after*ec_group, 
                                 data=data_station_day_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(2, 4, 7)))

sp_ami_station_day_weather <- lm(speed ~ before_after + ec_group + PRCP + TMAX90 + before_after*ec_group, 
                                 data=data_station_day_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(1, 3, 8)))

sp_pmo_station_day_weather <- lm(speed ~ before_after + ec_group + PRCP + TMAX90 + before_after*ec_group, 
                                 data=data_station_day_0414_green_wdpeak %>% filter(AM_PM=="PM", highwayid %in% c(2, 4, 7)))

sp_pmi_station_day_weather <- lm(speed ~ before_after + ec_group + PRCP + TMAX90 + before_after*ec_group, 
                                 data=data_station_day_0414_green_wdpeak %>% filter(AM_PM=="PM", highwayid %in% c(1, 3, 8)))


stargazer(sp_amo_station_day_weather, sp_ami_station_day_weather, sp_pmo_station_day_weather, sp_pmi_station_day_weather, 
          column.labels = c("AM Outward", "AM Inward", "PM Outward",  "PM Inward"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "Time Period × Group"),
          type="text", out="output/intermediate/gl_station_day_sp_weather.htm")
  
# aggregate to AM/PM peak periods at corridor level ====
# Aggregate 15-minute data to daily datat: the percentage of NA decareases from 3.81% to 0.07% 
data_corridor_day_0414_green_wdpeak <- data_15min_0414_green_wdpeak %>%
                                        group_by(highwayid, date, AM_PM) %>%
                                        summarise(speed=mean(speed, na.rm=TRUE),
                                                  volume=mean(volume, na.rm=TRUE),
                                                  before_after=first(before_after),
                                                  ec_group=first(ec_group)) %>% 
                                        ungroup() 

# highwayid 1 I-5 North / highwayid 2 I-5 South 
# highwayid 3 I-205 North / highwayid 4 I-205 South
# highwayid 7 I-84 East/ highwayid 8 I-84 West 
sp_amo_corridor_day <- lm(speed ~ before_after + ec_group + before_after*ec_group, 
                         data=data_corridor_day_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(2, 4, 7)))

sp_ami_corridor_day <- lm(speed ~ before_after + ec_group + before_after*ec_group, 
                         data=data_corridor_day_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(1, 3, 8)))

sp_pmo_corridor_day <- lm(speed ~ before_after + ec_group + before_after*ec_group, 
                         data=data_corridor_day_0414_green_wdpeak %>% filter(AM_PM=="PM", highwayid %in% c(2, 4, 7)))

sp_pmi_corridor_day <- lm(speed ~ before_after + ec_group + before_after*ec_group, 
                         data=data_corridor_day_0414_green_wdpeak %>% filter(AM_PM=="PM", highwayid %in% c(1, 3, 8)))

stargazer(sp_amo_corridor_day, sp_ami_corridor_day, sp_pmo_corridor_day, sp_pmi_corridor_day, 
          column.labels = c("AM Outward", "AM Inward", "PM Outward",  "PM Inward"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "Time Period × Group"),
          type="text", out="output/intermediate/gl_corridor_day_sp.htm")

# Add weahter variable 


# Plot ====
# Aggregate data at the daily level  
data_day_0414_green_wdpeak <- data_15min_0414_green_wdpeak %>%
  group_by(highwayid, date, AM_PM) %>%
  summarise(speed=mean(speed, na.rm=TRUE),
            volume=mean(volume, na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(Roadways=factor(highwayid, levels=c(1, 2, 3, 4, 7, 8),
                         labels=c("I-5 Northbound", "I-5 Southbound", "I-205 Northbound",
                                  "I-205 Southbound", "I-84 Eastbound", "I-84 Westbound"))) %>%
  as.data.frame()

# I-84
PI84_0414_east <- ggplot(data=data_day_0414_green_wdpeak %>% filter(highwayid==7), aes(x=date, y=speed, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
  ylim(20, 70) + labs(title="Eastbound average speed of I-84 highway during peak periods", 
                      x="Date", y="Average speed (mph)", colour="Peak period") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "output/plots/04-14/PI84_0414_east.png", PI84_0414_east, width = 8, height = 4)

PI84_0414_west <- ggplot(data=data_day_0414_green_wdpeak %>% filter(highwayid==8), aes(x=date, y=speed, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
  ylim(20, 70) + labs(title="Westbound average speed of I-84 highway during peak periods", 
                      x="Date", y="Average speed (mph)", colour="Peak period") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "output/plots/04-14/PI84_0414_west.png", PI84_0414_west, width = 8, height = 4)

# I-205 
PI205_0414_north <-  ggplot(data=data_day_0414_green_wdpeak %>% filter(highwayid==3), aes(x=date, y=speed, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
  ylim(20, 70) + labs(title="Northbound average speed of I-205 highway during peak periods", 
                      x="Date", y="Average speed (mph)", colour="Peak period") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "output/plots/04-14/PI205_0414_north.png", PI205_0414_north, width = 8, height = 4)

PI205_0414_south <-  ggplot(data=data_day_0414_green_wdpeak %>% filter(highwayid==4), aes(x=date, y=speed, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
  ylim(20, 70) + labs(title="Southbound average speed of I-205 highway during peak periods", 
                      x="Date", y="Average speed (mph)", colour="Peak period")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "output/plots/04-14/PI205_0414_south.png", PI205_0414_south, width = 8, height = 4)

# I-5 
PI5_0414_north <-  ggplot(data=data_day_0414_green_wdpeak %>% filter(highwayid==1), aes(x=date, y=speed, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
  ylim(20, 70) + labs(title="Northbound average speed of I-5 highway during peak periods", 
                      x="Date", y="Average speed (mph)", colour="Peak period")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "output/plots/04-14/PI5_0414_north.png", PI5_0414_north, width = 8, height = 4)


PI5_0414_south <-  ggplot(data=data_day_0414_green_wdpeak %>% filter(highwayid==2), aes(x=date, y=speed, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +  ylim(20, 70) + 
  labs(title="Southbound average speed of I-5 highway during peak periods", 
       x="Date", y="Average speed (mph)", colour="Peak period") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "output/plots/04-14/PI5_0414_south.png", PI5_0414_south, width = 8, height = 4)

# Put experimental roadway and control roadway in the same plot. 
PPM_I5_I84_I205  <- ggplot(data=data_day_0414_green_wdpeak %>% filter(AM_PM=="PM", highwayid %in% c(2, 4, 7)), 
                           aes(x=date, y=speed, group=Roadways)) +
  geom_point(aes(colour=Roadways)) +  geom_smooth(aes(linetype=Roadways), colour="yellow", size=0.75) + 
  geom_vline(xintercept=ymd("2009-09-12"), size=0.5) +  ylim(20, 70) + 
  labs( x="Date", y="Average speed (mph)") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "output/plots/04-14/PPM_I5_I84_I205.png", PPM_I5_I84_I205, width = 8, height = 4)

PAM_I5_I84_I205  <- ggplot(data=data_day_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(1, 3, 8)), 
                           aes(x=date, y=speed, group=Roadways)) +
  geom_point(aes(colour=Roadways)) + geom_smooth(aes(linetype=Roadways), colour="yellow", size=0.75) + 
  geom_vline(xintercept=ymd("2009-09-12"), size=0.5) +  ylim(20, 70) + 
  labs( x="Date", y="Average speed (mph)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "output/plots/04-14/PAM_I5_I84_I205.png", PAM_I5_I84_I205, width = 8, height = 4)
















