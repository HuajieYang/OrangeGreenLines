# This script convert original PORTAL data (station level) to corridor level

# Use data from 2005-2007 as an example
# Load data 
load("~/OrangeGreenLines/output/intermediate/data_5min_0506_green.RData")
load("~/OrangeGreenLines/output/intermediate/data_5min_0607_green.RData")

data_5min_0506_green <- data_5min_0506_green %>%
                        filter(starttime!="2006-09-12 00:00:00-07")

data_5min_0607_green <- data_5min_0607_green %>%
                        filter(starttime!="2007-09-12 00:00:00-07") %>% 
                        filter(!(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955)))

data_5min_0507_green <- rbind(data_5min_0506_green, data_5min_0607_green) %>%
                        mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
                               date=as.Date(starttime),
                               day_week=weekdays(date),
                               before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                               time_hour=hour(date_time),
                               time_min5=minute(date_time),
                               AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
                               AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM),
                               ec_group=ifelse(highwayid %in% c(1,2), "control", "experimental")) %>%
                        select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, time_hour, time_min5, 
                               AM_PM, speed)  %>% #  
                        arrange(highwayid, stationid, lanenumber, detector_id, date_time)  

# I-84 east bound stations 
# 1097 is too close to downtown 
# , locationtext, length, upstream, 
# downstream, numberlanes, length_mid, downstream_mile, 
# upstream_mile, opposite_stationid, start_date, end_date
stations_meta <- read.csv("data/metadata/stations_metadata.csv", stringsAsFactors = F) # station metadata

# East bound 
stations_I84 <- stations_meta %>% 
  filter(highwayid %in% c(7, 8), start_date=="2004-01-01 00:00:00-08") 

stations_I84_7 <- stations_I84 %>%
                  filter(highwayid %in% c(7), stationid %in% c(1055, 1056, 1057, 1127)) %>%
                  select(stationid, highwayid, milepost)

# West bound 
# stattionid: milestone of 1147 is not right
stations_I84_8 <- stations_I84 %>%
  filter(highwayid %in% c(8), stationid %in% c(1059, 1060, 1061, 1062)) %>%
  select(stationid, highwayid, milepost, locationtext, length, upstream, 
         downstream, numberlanes, length_mid, downstream_mile, 
         upstream_mile, opposite_stationid, start_date, end_date)

# 5-minute interval ====
# Explore one lane 
test <- data_5min_0507_green %>%
        filter(stationid %in% c(1055, 1056, 1057, 1127))  %>%
        filter(lanenumber==1, AM_PM %in% c("AM", "PM")) %>%
        select(-date_time) %>%
        left_join(stations_I84_7) %>%
        arrange(date, time_hour, time_min5, stationid)%>%
        mutate(id=paste(highwayid, date, time_hour, time_min5, sep="_"))

test1 <- test[1:12, ]

# No missing vales 
test3 <- test1 %>%
          mutate(travel_time=ifelse(stationid==1055, (0.81/2)/speed, NA),
                 travel_time=ifelse(stationid==1056, (0.81/2 + 1.27/2)/speed, travel_time),
                 travel_time=ifelse(stationid==1057, (1.27/2 + 1.09/2)/speed, travel_time),
                 travel_time=ifelse(stationid==1127, (1.09/2)/speed, travel_time)) %>%
          group_by(highwayid,  date, time_hour, time_min5) %>%
          summarise(speed_seg=(3.69-0.52)/sum(travel_time))

# One missing value 
test4_3 <- test4 %>%
           filter(freq==3) 

test5 <- test %>%
  filter(id %in% test4_3$id) 

full_df <- data.frame(highwayid=7, 
                      stationid=rep(c(1055, 1056, 1057, 1127), 5),
                      id=rep(c("7_2005-09-17_19_15", "7_2005-09-17_19_20", 
                               "7_2005-09-17_19_25", "7_2005-09-20_16_0",
                               "7_2005-09-20_16_5"), each=4)) %>%
            left_join(test6) %>%
            select(id, stationid, speed)


# Convert to segment speed when there only one missing value 
convert_segment_3 <- function (full_df){
  
  id_vec <- unique(full_df$id)
  full_df_new <- data.frame(id=NA, speed_seg=NA)
  
  for (id in id_vec) {
    full_df_sub = full_df[full_df$id==id, ]
    
    print(full_df_sub)
    
    sp1=full_df_sub[1, 3]; print(sp1)
    sp2=full_df_sub[2, 3]; print(sp2)
    sp3=full_df_sub[3, 3]; print(sp3)
    sp4=full_df_sub[4, 3]; print(sp4)
    
    if(is.na(sp1)) {
      speed_seg=3.17/((0.81+1.27/2)/sp2 + (1.27/2+1.09/2)/sp3 + (1.09/2)/sp4)
    }
    
    if(is.na(sp2)) {
      speed_seg=3.17/((0.81)/sp1 + (1.27+1.09/2)/sp3 + (1.09/2)/sp4)
    }
    
    if(is.na(sp3)) {
      speed_seg=3.17/((0.81/2)/sp1 + (0.81/2+1.27)/sp2 + (1.09)/sp4)
    }
    
    if(is.na(sp4)) {
      speed_seg=3.17/((0.81/2)/sp1 + (0.81/2+1.27/2)/sp2 + (1.27/2+1.09)/sp3)
    }
    print(speed_seg)
    
    full_df_sub_new <- data.frame(id=id, speed_seg=speed_seg)
    full_df_new <- rbind(full_df_new, full_df_sub_new)
  }
  
  return(full_df_new)
}

convert_segment(full_df)

# 2 missing values
test4_2 <- test4 %>%
  filter(freq==2) 

test6 <- test %>%
  filter(id %in% test4_2$id) 

# 15-minute interval 
data_15min_0507_green <-  data_5min_0507_green %>%
  mutate(time_min15=ifelse(time_min5 %in% c(0,  5,  10), 1, NA),
         time_min15=ifelse(time_min5 %in% c(15, 20, 25), 2, time_min15),
         time_min15=ifelse(time_min5 %in% c(30, 35, 40), 3, time_min15),
         time_min15=ifelse(time_min5 %in% c(45, 50, 55), 4, time_min15)) %>%
  group_by (highwayid, stationid, lanenumber, detector_id, date, time_hour, time_min15)  %>%
  summarise(speed=mean(speed, na.rm=TRUE),
            AM_PM=first(AM_PM))  

head(data_15min_0507_green)

test7 <- data_15min_0507_green %>%
  filter(stationid %in% c(1055, 1056, 1057, 1127))  %>%
  filter(lanenumber==1, AM_PM %in% c("AM", "PM")) %>%
  left_join(stations_I84_7) %>%
  arrange(date, time_hour, time_min15, stationid)


# 42 percent of observations have missing values 
test8 <- test7 %>%
  group_by(highwayid,  date, time_hour, time_min15) %>%
  summarise(freq=n()) 

table(test8$freq)

# daily 
data_day_0507_green <-  data_5min_0507_green %>%
  mutate(time_min15=ifelse(time_min5 %in% c(0,  5,  10), 1, NA),
         time_min15=ifelse(time_min5 %in% c(15, 20, 25), 2, time_min15),
         time_min15=ifelse(time_min5 %in% c(30, 35, 40), 3, time_min15),
         time_min15=ifelse(time_min5 %in% c(45, 50, 55), 4, time_min15)) %>%
  group_by (highwayid, stationid, lanenumber, detector_id, date, AM_PM)  %>%
  summarise(speed=mean(speed, na.rm=TRUE))  


testd1 <- data_day_0507_green %>%
  filter(stationid %in% c(1055, 1056, 1057, 1127))  %>%
  filter(lanenumber==1, AM_PM %in% c("AM", "PM")) %>%
  left_join(stations_I84_7) %>%
  arrange(date, AM_PM, stationid)

testd2 <- testd1 %>%
  group_by(highwayid,  date, AM_PM) %>%
  summarise(freq=n()) 

table(testd2$freq)

