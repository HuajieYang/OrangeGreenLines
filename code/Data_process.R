# Settings
if (!require("pacman")) {install.packages("pacman"); library(pacman)}
p_load(dplyr, lubridate, ggplot2, stargazer)


# Organized code =====
# Identify stations for I-84, I-205 and I-5 #####

# Load data
data_5min_o <- read.csv("data/data_5min_new.csv", stringsAsFactors = F) # 5 min interval data 
detector_meta <- read.csv("data/metadata/detectors_metadata.csv", stringsAsFactors = F) # detector metadata
stations_meta <- read.csv("data/metadata/stations_metadata.csv", stringsAsFactors = F) # station metadata
highways_meta <- read.csv("data/metadata/highways_metadata.csv", stringsAsFactors = F) # highway metadata 

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


# Analysze travel speed data 
# Identify highwayid, stationid, lanenumber for each detector
detector_meta_sub <- detector_meta %>%
  select(highwayid, stationid, detectorid, lanenumber) %>% 
  rename(detector_id=detectorid) %>%
  semi_join(data_5min_o) %>% 
  arrange(highwayid, stationid, detector_id, lanenumber)

data_5min <- data_5min_o %>%
  left_join(detector_meta_sub) %>%
  rename(detectorid=detector_id) 

# I-84
# time zone 07/08 does not matter: 2009-11-01 00:55:00-07 to 2009-11-01 01:00:00-08
data_I84 <- data_5min %>%
  filter(stationid %in% stations_I84$stationid)  %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), # 07/08 means DST changes which does not affect analysis
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), # the peak period is defined the same as commuting time 
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detectorid, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detectorid, date_time)  %>%
  mutate(ec_group="experimental")

# I-205
data_I205 <- data_5min %>%
  filter(stationid %in% stations_I205$stationid)  %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)),
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"),
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detectorid, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detectorid, date_time)  %>%
  mutate(ec_group="experimental")

# I-5  
data_I5 <- data_5min %>%
  filter(stationid %in% stations_I5$stationid)  %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)),
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"),
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detectorid, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detectorid, date_time) %>%
  mutate(ec_group="control")


# Analysis 
# I-84  
data_I84_wpeak <- data_I84 %>%
  filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))

data_I84_wpeak %>%
  group_by(highwayid, AM_PM, before_after) %>%
  summarise(speed_avg=mean(speed, na.rm=TRUE),
            speed.sd=sd(speed, na.rm=TRUE),
            volume_avg=mean(volume))

# T-test
t.test(data_I84_wpeak %>% filter(highwayid==7, AM_PM=="AM", before_after==0) %>% select(speed),
       data_I84_wpeak %>% filter(highwayid==7, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_I84_wpeak %>% filter(highwayid==8, AM_PM=="AM", before_after==0) %>% select(speed),
       data_I84_wpeak %>% filter(highwayid==8, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_I84_wpeak %>% filter(highwayid==7, AM_PM=="PM", before_after==0) %>% select(speed),
       data_I84_wpeak %>% filter(highwayid==7, AM_PM=="PM", before_after==1) %>% select(speed))

t.test(data_I84_wpeak %>% filter(highwayid==8, AM_PM=="PM", before_after==0) %>% select(speed),
       data_I84_wpeak %>% filter(highwayid==8, AM_PM=="PM", before_after==1) %>% select(speed))

# I-205
data_I205_wdpeak <- data_i205 %>%
  filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday"))) 

data_I205_wdpeak %>%
  group_by(highwayid, AM_PM, before_after) %>%
  summarise(speed_avg=mean(speed, na.rm=TRUE),
            speed.sd=sd(speed, na.rm=TRUE),
            volume_avg=mean(volume))
# T-test
t.test(data_I205_wdpeak %>% filter(highwayid==3, AM_PM=="AM", before_after==0) %>% select(speed),
       data_I205_wdpeak %>% filter(highwayid==3, AM_PM=="AM", before_after==1) %>% select(speed))


t.test(data_I205_wdpeak %>% filter(highwayid==3, AM_PM=="PM", before_after==0) %>% select(speed),
       data_I205_wdpeak %>% filter(highwayid==3, AM_PM=="PM", before_after==1) %>% select(speed))


t.test(data_I205_wdpeak %>% filter(highwayid==4, AM_PM=="AM", before_after==0) %>% select(speed),
       data_I205_wdpeak %>% filter(highwayid==4, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_I205_wdpeak %>% filter(highwayid==4, AM_PM=="PM", before_after==0) %>% select(speed),
       data_I205_wdpeak %>% filter(highwayid==4, AM_PM=="PM", before_after==1) %>% select(speed))

# I-5
data_I5_wdpeak <- data_i5 %>%  
  filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))

data_I5_wdpeak %>%
  group_by(highwayid, AM_PM, before_after) %>%
  summarise(speed_avg=mean(speed, na.rm=TRUE),
            speed.sd=sd(speed, na.rm=TRUE),
            volume_avg=mean(volume))

# T-test
t.test(data_I5_wdpeak %>% filter(highwayid==1, AM_PM=="AM", before_after==0) %>% select(speed),
       data_I5_wdpeak %>% filter(highwayid==1, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_I5_wdpeak %>% filter(highwayid==1, AM_PM=="PM", before_after==0) %>% select(speed),
       data_I5_wdpeak %>% filter(highwayid==1, AM_PM=="PM", before_after==1) %>% select(speed))

t.test(data_I5_wdpeak %>% filter(highwayid==2, AM_PM=="AM", before_after==0) %>% select(speed),
       data_I5_wdpeak %>% filter(highwayid==2, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_I5_wdpeak %>% filter(highwayid==2, AM_PM=="PM", before_after==0) %>% select(speed),
       data_I5_wdpeak %>% filter(highwayid==1, AM_PM=="PM", before_after==1) %>% select(speed))

# DID regression 
stations_3hw <- rbind(stations_i84, stations_i205, stations_i5)


data_3hw_wdpeak <- rbind(data_i84, data_i205, data_i5) %>%
  filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))  %>%
  group_by(highwayid, date, AM_PM)  %>%
  summarise(speed_avg=mean(speed, na.rm=TRUE),
            before_after=first(before_after),
            ec_group=first(ec_group))

m_pool <- lm(speed_avg ~ before_after + ec_group + before_after*ec_group, data=data_3hw_wdpeak)
summary(m_pool)

m_AMPeak <- lm(speed_avg ~ before_after + ec_group + before_after*ec_group, data=data_3hw_wdpeak %>% filter(AM_PM=="AM"))
summary(m_AMPeak)

m_PMPeak <- lm(speed_avg ~ before_after + ec_group + before_after*ec_group, data=data_3hw_wdpeak %>% filter(AM_PM=="PM"))
summary(m_PMPeak)


stargazer(m_pool, m_AMPeak, m_PMPeak, type="text")
# Plot 
# Plots 
data_i84_day <- data_i84 %>%
  filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday"))) %>%
  group_by(highwayid, date, AM_PM) %>%
  summarise(speed_avg=mean(speed, na.rm=TRUE),
            volume_avg=mean(volume)) %>% 
  ungroup()  %>%
  as.data.frame()

PI84_east <- ggplot(data=data_i84_day %>% filter(highwayid==7), aes(x=date, y=speed_avg, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
  ylim(20, 70) + labs(title="Eastbound average speed of I-84 highway during peak periods", x="Date", y="Average speed (mph)", colour="Peak period") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "output/plots/PI84_east.png", PI84_east, width = 8, height = 4)


PI84_west <- ggplot(data=data_i84_day %>% filter(highwayid==8), aes(x=date, y=speed_avg, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
  ylim(20, 70) + labs(title="Westbound average speed of I-84 highway during peak periods", x="Date", y="Average speed (mph)", colour="Peak period") +
  theme(plot.title = element_text(hjust = 0.5))
PI84_west

ggsave(filename = "output/plots/PI84_west.png", PI84_west, width = 8, height = 4)

data_i205_day <- data_i205 %>%
  filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday"))) %>%
  group_by(highwayid, date, AM_PM) %>%
  summarise(speed_avg=mean(speed, na.rm=TRUE),
            volume_avg=mean(volume)) %>%
  as.data.frame()

PI205_north <-  ggplot(data=data_i205_day %>% filter(highwayid==3), aes(x=date, y=speed_avg, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
  ylim(20, 70) + labs(title="Northbound average speed of I-205 highway during peak periods", x="Date", y="Average speed (mph)", colour="Peak period") +
  theme(plot.title = element_text(hjust = 0.5))
PI205_north
ggsave(filename = "output/plots/PI205_north.png", PI205_north, width = 8, height = 4)


PI205_south <-  ggplot(data=data_i205_day %>% filter(highwayid==4), aes(x=date, y=speed_avg, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
  ylim(20, 70) + labs(title="Southbound average speed of I-205 highway during peak periods", x="Date", y="Average speed (mph)", colour="Peak period")+
  theme(plot.title = element_text(hjust = 0.5))
PI205_south
ggsave(filename = "output/plots/PI205_south.png", PI205_south, width = 8, height = 4)


data_i5_day <- data_i5 %>%  
  filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday"))) %>%
  group_by(highwayid, date, AM_PM) %>%
  summarise(speed_avg=mean(speed, na.rm=TRUE),
            volume_avg=mean(volume)) %>%
  as.data.frame()


PI5_north <-  ggplot(data=data_i5_day %>% filter(highwayid==1), aes(x=date, y=speed_avg, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
  ylim(20, 70) + labs(title="Northbound average speed of I-5 highway during peak periods", x="Date", y="Average speed (mph)", colour="Peak period")+
  theme(plot.title = element_text(hjust = 0.5))
PI5_north
ggsave(filename = "output/plots/PI5_north.png", PI5_north, width = 8, height = 4)


PI5_south <-  ggplot(data=data_i5_day %>% filter(highwayid==2), aes(x=date, y=speed_avg, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +  ylim(20, 70) + 
  labs(title="Southbound average speed of I-5 highway during peak periods", x="Date", y="Average speed (mph)", colour="Peak period") +
  theme(plot.title = element_text(hjust = 0.5))
PI5_south
ggsave(filename = "output/plots/PI5_south.png", PI5_south, width = 8, height = 4)


# Put experimental roadway and control roadway in the same plot. 
PM_I5_I84_I205 <- data_2hw_day %>%
  filter(AM_PM=="PM", highwayid %in% c(2, 4, 7))  %>%
  mutate(Roadways=ifelse(highwayid==2, "I-5 Southbound", "I-84 Eastbound"),
         Roadways=ifelse(highwayid==4, "I-205 Southbound", Roadways),
  )

PPM_I5_I84_I205  <- ggplot(data=PM_I5_I84_I205, aes(x=date, y=speed_avg, group=Roadways)) +
  geom_point(aes(colour=Roadways)) +  geom_smooth(aes(linetype=Roadways), colour="yellow", size=0.75) + 
  geom_vline(xintercept=ymd("2009-09-12"), size=0.5) +  ylim(20, 70) + 
  labs( x="Date", y="Average speed (mph)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "output/plots/PPM_I5_I84_I205.png", PPM_I5_I84_I205, width = 8, height = 4)


AM_I5_I84_I205 <- data_2hw_day %>%
  filter(AM_PM=="AM", highwayid %in% c(1, 3, 8))  %>%
  mutate(Roadways=ifelse(highwayid==1, "I-5 Northbound", "I-84 Westbound"),
         Roadways=ifelse(highwayid==3, "I-205 Northbound", Roadways)
  )

PAM_I5_I84_I205  <- ggplot(data=AM_I5_I84_I205, aes(x=date, y=speed_avg, group=Roadways)) +
  geom_point(aes(colour=Roadways)) + geom_smooth(aes(linetype=Roadways), colour="yellow", size=0.75) + 
  geom_vline(xintercept=ymd("2009-09-12"), size=0.5) +  ylim(20, 70) + 
  labs( x="Date", y="Average speed (mph)") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(filename = "output/plots/PAM_I5_I84_I205.png", PAM_I5_I84_I205, width = 8, height = 4)


# Initial analysis ==== 
# Load data 
# 5 minutes interval data 
  data_5min_o <- read.csv("data/data_5min_new.csv", stringsAsFactors = F)

# highway <- read.csv("data/highways.csv", stringsAsFactors = F)
# OR_station <- read.csv("data/OR_stations.csv", stringsAsFactors = F)
# up_station <- read.csv("data/update_stations.csv", stringsAsFactors = F)

# meta data
  detector_meta <- read.csv("data/metadata/detectors_metadata.csv", stringsAsFactors = F)
  detector_startend <- read.csv("data/metadata/detector_start_end.csv", stringsAsFactors = F)
  highways_meta <- read.csv("data/metadata/highways_metadata.csv", stringsAsFactors = F)
  stations_meta <- read.csv("data/metadata/stations_metadata.csv", stringsAsFactors = F)

# Identify highwayid, stationid, lanenumber for each detector
  detector_meta_sub <- detector_meta %>%
    select(highwayid, stationid, detectorid, lanenumber) %>% 
    rename(detector_id=detectorid) %>%
    semi_join(data_5min_o) %>% 
    arrange(highwayid, stationid, detector_id, lanenumber)
  
  data_5min <- data_5min_o %>%
               left_join(detector_meta_sub) %>%
               rename(detectorid=detector_id) 

  
# 10 stations are not included in the data_5min
# The stations included in stations_green.csv are the same as the stations of highwayid %in% c(1, 2, 3, 4, 7, 8) in stations_metadata.csv
# 10 stations that are in stations_green.csv are not in data_5min: 3114 and 3197 are useful for I-5, other stations are not wihin analysis zone
  stations_green <- read.csv("output/intermediate/stations_green.csv", stringsAsFactors = F)
  stations_green <- sort(unique(stations_green$stationid))
  
  stations_meta_green <- stations_meta %>%
    filter(highwayid %in% c(1, 2, 3, 4, 7, 8), start_date=="2004-01-01 00:00:00-08") 
  
  stations_meta_green <- sort(stations_meta_green$stationid)
  stations_green == stations_meta_green
  
  stations_5min <- sort(unique(data_5min$stationid))
  stationid_diff <- setdiff(stations_meta_green, stations_5min)
  stationid_diff

  
# Identify lat, lon for each station 
# 1, 2, 3, 4, 7, 8 are highwayids for I-5, I-205, I-84
stations_meta_green <- stations_meta %>%
                       filter(highwayid %in% c(1, 2, 3, 4, 7, 8), start_date=="2004-01-01 00:00:00-08") 

length(unique(detector_meta_sub$stationid))
length(unique(data_5min$stationid))      
sort(unique(detector_meta_sub$stationid))==sort(unique(data_5min$stationid))

stations_meta_sub <- stations_meta %>%
                     select(highwayid, stationid, milepost, locationtext, lat, lon, start_date, active_dates)  %>%
                     filter(highwayid %in% c(1, 2, 3, 4, 7, 8), stationid %in% detector_meta_sub$stationid)  %>%
                     filter(!is.na(lat))  %>%
                     group_by(highwayid, stationid)  %>%
                     summarise(lat=first(lat),
                               lon=first(lon),
                               start_date=first(start_date),
                               active_dates=first(active_dates),
                               milepost=first(milepost),
                               locationtext=first(locationtext))

length(unique(stations_meta_sub$stationid))
sort(unique(stations_meta_sub$stationid))

# stationid 1145 
table(stations_meta_sub$stationid)
str(stations_meta_sub)
nrow(stations_meta_sub)

# Some stations do not have lat, lon information 
length(unique(stations_meta_sub$stationid))
length(unique(detector_meta_sub$stationid))

nrow(stations_meta_sub)
table(stations_meta_sub$highwayid)

stations_meta_sub


# Non-ramp stationid with NA lat/lon: 2022 and 7068 
# 5000 stations are ramp stations 
station_na_lat <- stations_meta %>%
                  select(highwayid, stationid, lat, lon)  %>%
                  filter(stationid %in% detector_meta_sub$stationid)  %>%
                  filter(is.na(lat))
   
length(unique(station_na_lat$stationid))
# write.csv(test, file="output/intermediate/stations_no_lat.csv", row.names = FALSE)

# I-84 
# Locations of stations:
# do not locate along I-84:
# 7	1055	45.52525	-122.6601/7	1056	45.52816	-122.6496/8	1059	45.53369	-122.5808/8	1060	45.52729	-122.6053/
# 8	1061	45.53405	-122.6251/8	1062	45.53450	-122.6308/8	1145	45.53119	-122.5648
# locate along I-84
# 7	1057	45.53284	-122.6223/7	1127	45.52888	-122.6017
# locate far away from I-84
# 7	1097	45.52048	-122.6653/8	5061	45.53853	-122.6188
stations_i84 <- stations_meta_sub %>%
                filter(highwayid %in% c(7, 8))

# time zone 07/08 does not matter: 2009-11-01 00:55:00-07 to 2009-11-01 01:00:00-08
data_i84 <- data_5min %>%
            filter(stationid %in% stations_i84$stationid)  %>%
            mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)),
                   date=as.Date(starttime),
                   day_week=weekdays(date),
                   before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                   time_hour=hour(date_time),
                   AM_PM=ifelse(time_hour %in% c(7, 8, 9), "AM", "NonPeak"),
                   AM_PM=ifelse(time_hour %in% c(16, 17, 18), "PM", AM_PM)
                   ) %>%
            select(highwayid, stationid, lanenumber, detectorid, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
            arrange(highwayid, stationid, lanenumber, detectorid, date_time)  %>%
            mutate(ec_group="experimental")

head(data_i84)

test <- data_i84 %>% filter(!(day_week %in% c("Saturday", "Sunday"))) %>% mutate(time_hour=hour(date_time)) 

table(test$day_week)
head(test, 100)
test[200:400, ]
table(test$time_hour)

hour(test$date_time)

data_i84_wpeak <- data_i84 %>%
                  filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))

data_i84_wpeak %>%
      group_by(highwayid, AM_PM, before_after) %>%
      summarise(speed_avg=mean(speed, na.rm=TRUE),
                speed.sd=sd(speed, na.rm=TRUE),
                volume_avg=mean(volume))


# T-test
t.test(data_i84_wpeak %>% filter(highwayid==7, AM_PM=="AM", before_after==0) %>% select(speed),
       data_i84_wpeak %>% filter(highwayid==7, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_i84_wpeak %>% filter(highwayid==8, AM_PM=="AM", before_after==0) %>% select(speed),
       data_i84_wpeak %>% filter(highwayid==8, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_i84_wpeak %>% filter(highwayid==7, AM_PM=="PM", before_after==0) %>% select(speed),
       data_i84_wpeak %>% filter(highwayid==7, AM_PM=="PM", before_after==1) %>% select(speed))

t.test(data_i84_wpeak %>% filter(highwayid==8, AM_PM=="PM", before_after==0) %>% select(speed),
       data_i84_wpeak %>% filter(highwayid==8, AM_PM=="PM", before_after==1) %>% select(speed))


summary(data_i84_wpeak)
nrow(data_i84_wpeak)
table(data_i84_wpeak$before_after)

min(data_i84_wpeak$date)
max(data_i84_wpeak$date)
t.test(data_i84_wpeak %>% filter(), )


table(data_i84_wpeak$day_week)

t.test(data_i84_wpeak %>% filter(highwayid==7, AM_PM=="AM", before_after==0) %>% select(volume),
       data_i84_wpeak %>% filter(highwayid==7, AM_PM=="AM", before_after==1) %>% select(volume), paired=TRUE)


summary(data_i84)





# I-205
# select 16 stations out of 39 stations 
stations_i205 <- stations_meta_sub %>%
                 filter(highwayid %in% c(3, 4),
                        lat < 45.53 & lat > 45.43)  # Lat > 45.534258 or Lat < 45.43138 is outside of experimental corridor 
                 
data_i205 <- data_5min %>%
            filter(stationid %in% stations_i205$stationid)  %>%
            mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)),
                   date=as.Date(starttime),
                   day_week=weekdays(date),
                   before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                   time_hour=hour(date_time),
                   AM_PM=ifelse(time_hour %in% c(7, 8, 9), "AM", "NonPeak"),
                   AM_PM=ifelse(time_hour %in% c(16, 17, 18), "PM", AM_PM)
            ) %>%
            select(highwayid, stationid, lanenumber, detectorid, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
            arrange(highwayid, stationid, lanenumber, detectorid, date_time)  %>%
            mutate(ec_group="experimental")


data_i205_wdpeak <- data_i205 %>%
                    filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday"))) 

data_i205_wdpeak %>%
    group_by(highwayid, AM_PM, before_after) %>%
    summarise(speed_avg=mean(speed, na.rm=TRUE),
              speed.sd=sd(speed, na.rm=TRUE),
              volume_avg=mean(volume))
# T-test
t.test(data_i205_wdpeak %>% filter(highwayid==3, AM_PM=="AM", before_after==0) %>% select(speed),
       data_i205_wdpeak %>% filter(highwayid==3, AM_PM=="AM", before_after==1) %>% select(speed))


t.test(data_i205_wdpeak %>% filter(highwayid==3, AM_PM=="PM", before_after==0) %>% select(speed),
       data_i205_wdpeak %>% filter(highwayid==3, AM_PM=="PM", before_after==1) %>% select(speed))


t.test(data_i205_wdpeak %>% filter(highwayid==4, AM_PM=="AM", before_after==0) %>% select(speed),
       data_i205_wdpeak %>% filter(highwayid==4, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_i205_wdpeak %>% filter(highwayid==4, AM_PM=="PM", before_after==0) %>% select(speed),
       data_i205_wdpeak %>% filter(highwayid==4, AM_PM=="PM", before_after==1) %>% select(speed))


# I-5
stations_i5 <- stations_meta_sub %>%
               filter(highwayid %in% c(1, 2),
                      lat < 45.51 & lat > 45.38) # Lat > 45.51 or Lat < 45.38 is outside of control corridor
head(stations_i5)

data_i5 <- data_5min %>%
           filter(stationid %in% stations_i5$stationid)  %>%
           mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)),
                   date=as.Date(starttime),
                   day_week=weekdays(date),
                   before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                   time_hour=hour(date_time),
                   AM_PM=ifelse(time_hour %in% c(7, 8, 9), "AM", "NonPeak"),
                   AM_PM=ifelse(time_hour %in% c(16, 17, 18), "PM", AM_PM)
            ) %>%
            select(highwayid, stationid, lanenumber, detectorid, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
            arrange(highwayid, stationid, lanenumber, detectorid, date_time) %>%
            mutate(ec_group="control")

data_i5_wdpeak <- data_i5 %>%  
                  filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))

data_i5_wdpeak %>%
        group_by(highwayid, AM_PM, before_after) %>%
        summarise(speed_avg=mean(speed, na.rm=TRUE),
                  speed.sd=sd(speed, na.rm=TRUE),
                  volume_avg=mean(volume))

# T-test
t.test(data_i5_wdpeak %>% filter(highwayid==1, AM_PM=="AM", before_after==0) %>% select(speed),
       data_i5_wdpeak %>% filter(highwayid==1, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_i5_wdpeak %>% filter(highwayid==1, AM_PM=="PM", before_after==0) %>% select(speed),
       data_i5_wdpeak %>% filter(highwayid==1, AM_PM=="PM", before_after==1) %>% select(speed))

t.test(data_i5_wdpeak %>% filter(highwayid==2, AM_PM=="AM", before_after==0) %>% select(speed),
       data_i5_wdpeak %>% filter(highwayid==2, AM_PM=="AM", before_after==1) %>% select(speed))

t.test(data_i5_wdpeak %>% filter(highwayid==2, AM_PM=="PM", before_after==0) %>% select(speed),
       data_i5_wdpeak %>% filter(highwayid==1, AM_PM=="PM", before_after==1) %>% select(speed))


# DID regression 
stations_3hw <- rbind(stations_i84, stations_i205, stations_i5)


data_3hw_wdpeak <- rbind(data_i84, data_i205, data_i5) %>%
                   filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))  %>%
                   group_by(highwayid, date, AM_PM)  %>%
                   summarise(speed_avg=mean(speed, na.rm=TRUE),
                             before_after=first(before_after),
                             ec_group=first(ec_group))
head()

head(data_3hw_wdpeak)            
table(data_3hw$AM_PM)



m_pool <- lm(speed_avg ~ before_after + ec_group + before_after*ec_group, data=data_3hw_wdpeak)
summary(m_pool)

m_AMPeak <- lm(speed_avg ~ before_after + ec_group + before_after*ec_group, data=data_3hw_wdpeak %>% filter(AM_PM=="AM"))
summary(m_AMPeak)

m_PMPeak <- lm(speed_avg ~ before_after + ec_group + before_after*ec_group, data=data_3hw_wdpeak %>% filter(AM_PM=="PM"))
summary(m_PMPeak)


stargazer(m_pool, m_AMPeak, m_PMPeak, type="text")


# Plots 
data_i84_day <- data_i84 %>%
                filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday"))) %>%
                group_by(highwayid, date, AM_PM) %>%
                summarise(speed_avg=mean(speed, na.rm=TRUE),
                          volume_avg=mean(volume)) %>% 
                ungroup()  %>%
                as.data.frame()

PI84_east <- ggplot(data=data_i84_day %>% filter(highwayid==7), aes(x=date, y=speed_avg, group=AM_PM)) +
              geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
              ylim(20, 70) + labs(title="Eastbound average speed of I-84 highway during peak periods", x="Date", y="Average speed (mph)", colour="Peak period") +
              theme(plot.title = element_text(hjust = 0.5))
  
ggsave(filename = "output/plots/PI84_east.png", PI84_east, width = 8, height = 4)


PI84_west <- ggplot(data=data_i84_day %>% filter(highwayid==8), aes(x=date, y=speed_avg, group=AM_PM)) +
              geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
              ylim(20, 70) + labs(title="Westbound average speed of I-84 highway during peak periods", x="Date", y="Average speed (mph)", colour="Peak period") +
              theme(plot.title = element_text(hjust = 0.5))
PI84_west

ggsave(filename = "output/plots/PI84_west.png", PI84_west, width = 8, height = 4)

data_i205_day <- data_i205 %>%
                 filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday"))) %>%
                 group_by(highwayid, date, AM_PM) %>%
                 summarise(speed_avg=mean(speed, na.rm=TRUE),
                          volume_avg=mean(volume)) %>%
                 as.data.frame()

PI205_north <-  ggplot(data=data_i205_day %>% filter(highwayid==3), aes(x=date, y=speed_avg, group=AM_PM)) +
                geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
                ylim(20, 70) + labs(title="Northbound average speed of I-205 highway during peak periods", x="Date", y="Average speed (mph)", colour="Peak period") +
                theme(plot.title = element_text(hjust = 0.5))
PI205_north
ggsave(filename = "output/plots/PI205_north.png", PI205_north, width = 8, height = 4)


PI205_south <-  ggplot(data=data_i205_day %>% filter(highwayid==4), aes(x=date, y=speed_avg, group=AM_PM)) +
                geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
                ylim(20, 70) + labs(title="Southbound average speed of I-205 highway during peak periods", x="Date", y="Average speed (mph)", colour="Peak period")+
                theme(plot.title = element_text(hjust = 0.5))
PI205_south
ggsave(filename = "output/plots/PI205_south.png", PI205_south, width = 8, height = 4)


data_i5_day <- data_i5 %>%  
               filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday"))) %>%
               group_by(highwayid, date, AM_PM) %>%
               summarise(speed_avg=mean(speed, na.rm=TRUE),
                         volume_avg=mean(volume)) %>%
               as.data.frame()
          

PI5_north <-  ggplot(data=data_i5_day %>% filter(highwayid==1), aes(x=date, y=speed_avg, group=AM_PM)) +
                geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +
                ylim(20, 70) + labs(title="Northbound average speed of I-5 highway during peak periods", x="Date", y="Average speed (mph)", colour="Peak period")+
                theme(plot.title = element_text(hjust = 0.5))
PI5_north
ggsave(filename = "output/plots/PI5_north.png", PI5_north, width = 8, height = 4)


PI5_south <-  ggplot(data=data_i5_day %>% filter(highwayid==2), aes(x=date, y=speed_avg, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +  ylim(20, 70) + 
  labs(title="Southbound average speed of I-5 highway during peak periods", x="Date", y="Average speed (mph)", colour="Peak period") +
  theme(plot.title = element_text(hjust = 0.5))
PI5_south
ggsave(filename = "output/plots/PI5_south.png", PI5_south, width = 8, height = 4)


# Put experimental roadway and control roadway in the same plot. 
PM_I5_I84_I205 <- data_2hw_day %>%
  filter(AM_PM=="PM", highwayid %in% c(2, 4, 7))  %>%
  mutate(Roadways=ifelse(highwayid==2, "I-5 Southbound", "I-84 Eastbound"),
         Roadways=ifelse(highwayid==4, "I-205 Southbound", Roadways),
  )

PPM_I5_I84_I205  <- ggplot(data=PM_I5_I84_I205, aes(x=date, y=speed_avg, group=Roadways)) +
  geom_point(aes(colour=Roadways)) +  geom_smooth(aes(linetype=Roadways), colour="yellow", size=0.75) + 
  geom_vline(xintercept=ymd("2009-09-12"), size=0.5) +  ylim(20, 70) + 
  labs( x="Date", y="Average speed (mph)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "output/plots/PPM_I5_I84_I205.png", PPM_I5_I84_I205, width = 8, height = 4)


AM_I5_I84_I205 <- data_2hw_day %>%
  filter(AM_PM=="AM", highwayid %in% c(1, 3, 8))  %>%
  mutate(Roadways=ifelse(highwayid==1, "I-5 Northbound", "I-84 Westbound"),
         Roadways=ifelse(highwayid==3, "I-205 Northbound", Roadways)
  )

PAM_I5_I84_I205  <- ggplot(data=AM_I5_I84_I205, aes(x=date, y=speed_avg, group=Roadways)) +
  geom_point(aes(colour=Roadways)) + geom_smooth(aes(linetype=Roadways), colour="yellow", size=0.75) + 
  geom_vline(xintercept=ymd("2009-09-12"), size=0.5) +  ylim(20, 70) + 
  labs( x="Date", y="Average speed (mph)") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(filename = "output/plots/PAM_I5_I84_I205.png", PAM_I5_I84_I205, width = 8, height = 4)


# Examine stations ====

table(highway$highwayname)

# highway id for I-84: 7, 8
highways_meta %>% filter(highwayname=="I-84")

table(stations_meta$start_date)

# Locations of each station (check lat and lon one by one at google map)

# I-84
# good on I-84:1055, 1056, 1057, 1059, 1060, 1061, 1062, 1127
# outside of experimental zone: 1058, 1097, 1145 
# ramp stations (5000) with lat&lon: 5061, whose lat&lon is far away from the freeway
stations_I84 <- stations_meta %>% 
                filter(highwayid %in% c(7, 8), start_date=="2004-01-01 00:00:00-08") %>% 
                select(stationid, lat, lon, opposite_stationid, highwayid, locationtext, start_date, end_date, active_dates) %>% 
                filter(stationid %in% c(1055, 1056, 1057, 1059, 1060, 1061, 1062, 1127))  %>% 
                arrange(lat)

# Wrong lat
stations_I84 %>% 
  filter(stationid==5061) %>% 
  select(stationid,opposite_stationid, highwayid, locationtext, start_date, end_date, lat, lon) 

# highway id for I-205: 3, 4
# good on I-205 within experimental zone: lat&lon of 1142 is a little far away from the freeway
# Outside of experimental zone: lat <= 45.42950 (stationid 1124), lat >= 45.56085 (stationid 1141)
# ramp stations (5000) with lat&lon: 5126, 5098, whose lat&lon is far away from the freeway
  stations_I205 <- stations_meta %>% 
                   filter(highwayid %in% c(3, 4), start_date=="2004-01-01 00:00:00-08") %>% 
                   select(stationid,lat, lon, highwayid, locationtext, start_date, end_date) %>%
                   filter(!is.na(lat) & (lat > 45.4296 & lat < 45.5608)&stationid<5000)  %>%
                   arrange(lat)

# highway id for  I-5: 1, 2
# Outside of experimental zone: lat <= 45.37870 (stationid 1040), lat >= 45.51715 (stationid 1016)
  stations_I5 <- stations_meta %>% 
                 filter(highwayid %in% c(1, 2), start_date=="2004-01-01 00:00:00-08") %>% 
                 select(stationid, lat, lon, opposite_stationid, highwayid, locationtext, start_date, end_date)%>%
                 filter(!is.na(lat)&(lat < 45.517 & lat > 45.3788))  %>%
                 arrange(lat)





# highway id for US 26: 11, 12
highway %>% filter(highwayname=="US26")

station %>% 
  filter(highwayid %in% c(11, 12), start_date=="2004-01-01 00:00:00-08") %>% 
  select(stationid, highwayid, locationtext, start_date, end_date, lat, lon)  %>% 
  arrange(lon)

stations_Green <- station %>% 
  filter(highwayid %in% c(1, 2, 3, 4, 7, 8), start_date=="2004-01-01 00:00:00-08") %>% 
  select(stationid, highwayid, locationtext, start_date, end_date, lat, lon)  %>% 
  arrange(lon)
head(stations_Green)                 
nrow(test)

write.csv(stations_Green, file="output/intermediate/stations_green.csv", row.names = FALSE)

# highway id for 99E: 15, 16
station %>% 
  filter(highwayid %in% c(15, 16)) %>% # , start_date=="2004-01-01 00:00:00-08"
  select(stationid,opposite_stationid, highwayid, locationtext, start_date, end_date, lat, lon)

# 99W, 30 

table(station$start_date)
table(station$highwayid)














detector_sub <- detector %>%
  select(highwayid, stationid, lanenumber,  detectorid)  %>%
  # filter(highwayid %in% c(1, 2, 3, 4, 7, 8))   %>%
  # filter(stationid %in% stations_Green$stationid)   %>%
  rename(detector_id=detectorid)


data_detctor <- data %>%
  group_by(detector_id) %>%
  summarise(freq=n())

test <- data_detctor %>%
  left_join(detector, by=c("detector_id"="detectorid"))
summary(test)
table(test$highwayid)

filter(detector_id %in% detector$detectorid)


data_comb <- data %>%
  left_join(detector_sub)


data_sub <- data %>%
  filter(detector_id %in% detector_sub$detector_id)

data_comb <- data %>%
  left_join(detector_sub)

head(data_comb)
head(data)
# I-84
# highway id for I-84: 7, 8
highway %>% filter(highwayname=="I-84")

table(station$start_date)

stations_I84 <- station %>% 
  filter(highwayid %in% c(7, 8), start_date=="2004-01-01 00:00:00-08") %>% 
  select(stationid,opposite_stationid, highwayid, locationtext, start_date, end_date, lat, lon)  %>% 
  filter(lat > 45.526500 & lat < 45.537240, lon > -122.670262&lon < -122.559737)

# Latitude range: 45.526500 ~ 45.537240
# Longitude range:  -122.670262 ~ -122.559737 


data_comb__I84 <- data_comb %>%
  filter(highwayid %in% c(7, 8))

filter(stationid %in% stations_I84$stationid)
table(data_comb$highwayid)
head(data_comb__I84)




# Maybe useful in the future ====

  # detectors_5min <- data_5min %>%
  #                   rename(detectorid = detector_id) %>%
  #                   distinct(detectorid) 

# Analysis 
# list of detectors in data_5min

getwd()
stations_green <- read.csv("output/intermediate/stations_green.csv", stringsAsFactors = F)
head(stations_green)
length(unique(stations_green$stationid))
stations_green <- sort(unique(stations_green$stationid))

stations_meta_green <- stations_meta %>%
  filter(highwayid %in% c(1, 2, 3, 4, 7, 8), start_date=="2004-01-01 00:00:00-08") 
stations_meta_green <- sort(stations_meta_green$stationid)
stations_5min <- sort(unique(data_5min$stationid))
stationid_diff <- setdiff(stations_meta_green, stations_5min)
stations_green == stations_meta_green
head(stations_meta)  


test <- stations_meta %>%
        filter(stationid %in% stationid_diff) %>%
        select(stationid, locationtext, start_date, end_date)  %>%
        arrange(stationid)

sort(unique(test$stationid))  
  
head(detector_startend)

head(data_5min)
head(stations_meta)
summary(stations_meta)

head(stations_meta)
head(OR_station)
head(detector_meta)
head(detectors_5min)
str(detectors_5min)


length(unique(stations_meta$stationid))
nrow(stations_meta)
summary(stations_meta_sub)


up_station_sub <- up_station %>%
  select(stationid, highwayid, lon, lat)


head(up_station)
summary(up_station)
summary(stations_meta)


nrow(detector_meta_sub)

up_station %>%
  select(highwayid, stationid, lon, lat)  %>% filter(stationid==2022)

stations_meta %>%
  select(highwayid, stationid, lon, lat)  %>% filter(stationid==2022)


stations_meta %>% select(highwayid, stationid, lon, lat) %>% filter(stationid==1089)

stations_meta_sub %>% filter(stationid==1089)

detectors_5min %>% filter(stationid==1089)
summary()

stations_meta_sub <- up_station %>%
  select(highwayid, stationid, lon, lat)  %>%
  semi_join(detector_meta_sub)   %>%
  arrange(highwayid, stationid)

summary(stations_meta_sub)
nrow(stations_meta_sub)

nrow()
left_join(up_station_sub) %>% 
  filter(detectorid %in% detectors_5min$detectorid)  %>% 
  filter(!is.na(lon))

summary(detector_meta_sub)                     

length(unique(detector_meta$detectorid))                     
nrow(detector_meta_sub)  
nrow(detector_meta)  


# , active_dates

length(unique(detector_meta_sub$stationid))

# 
test1 <- detector_meta_sub %>%
  
  
  
  head(up_station)      
head(stations_meta)      

stations_meta %>% select(highwayid, stationid, lat, lon) %>% filter(stationid == 5116)

up_station %>% select(highwayid, stationid, lat, lon) %>% filter(stationid == 5116)

detector_meta_sub %>% filter(stationid == 5116)


write.csv(stations_Green, file="output/intermediate/stations_green.csv", row.names = FALSE)

stations_meta_sub <- stations_meta %>%
  select(highwayid, stationid, lat, lon) 






head(data_5min)
nrow(detector_meta_sub)
length(unique(stations_green$stationid))


















