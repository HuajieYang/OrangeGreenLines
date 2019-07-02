# Load travel time and speed data 
getwd()
test <- read.delim("data/Travel time and speed/SE Division St (E).txt", sep = "\t")
head(test, 100)
str(test)


22-3
nrow(data_5min)

nchar("2009-03-12 10:00:00-07")

substr("2009-03-12 10:00:00-07", nchar("2009-03-12 10:00:00-07")-1, nchar("2009-03-12 10:00:00-07"))

substr("2009-03-12 10:00:00-07", 1, nchar("2009-03-12 10:00:00-07")-3)


test <- data_5min %>%
        mutate(timezone=substr(starttime, nchar(starttime)-1, nchar(starttime)))

head(test)
table(test$timezone)
max(data_I84$date)

data_I84 %>% filter(date=="2009-11-01")



data_I84[350:400, ]

summary(data_I84)
test <- data_5min %>%
  filter(stationid %in% stations_I84$stationid)
summary(test)

table(data_I84$detectorid)
table(data_I84$time_hour)
table(data_I84$AM_PM)


head(data_5min_0405_green)


data_5min_0405_green %>%
  arrange()

data_5min_0405_green[1:100, ]


head(data_5min_0405_green)
tail(data_5min_0405_green)


head(data_5min_0607_green)
tail(data_5min_0607_green)


ls(data_5min_0405_green)

head(data_5min_0405_green)





object.size(data_5min_0405_green)





test <- data_5min_0405_green %>%
        select(detector_id, starttime, volume, speed, occupancy, countreadings, vmt, vht, delay, traveltime, resolution)
object.size(test)



# 0405 dataset 
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

head()






length(unique(data_5min_0506_green$detector_id))
length(unique(data_5min_0405_green$detector_id))
length(unique(data_5min_0607_green$detector_id))
length(unique(data_5min_0708_green$detector_id))

ls()

head(data_5min_0506)


head(data_5min_0506_green)


## 0405 analysis 
test <- data_5min_0405_green %>%
        filter(stationid %in% stations_I84$stationid)  %>%
        mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), # 07/08 means DST changes which does not affect analysis
               date=as.Date(starttime),
               day_week=weekdays(date),
               before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
               time_hour=hour(date_time),
               AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), # the peak period is defined the same as commuting time 
               AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
        ) %>%
        select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
        arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
        mutate(ec_group="experimental")



head(test)

min(test$date)
summary(test)
str(test)



test1 <- test %>%
         select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week)
head(test1)

table(test1$date)

test1 %>% filter(date == "2005-09-11")

## 

test <- data_5min_0506_green %>%
        filter(stationid %in% stations_I84$stationid)  %>%
        mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), # 07/08 means DST changes which does not affect analysis
               date=as.Date(starttime),
               day_week=weekdays(date),
               before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
               time_hour=hour(date_time),
               AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), # the peak period is defined the same as commuting time 
               AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
        ) %>%
        select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
        arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
        mutate(ec_group="experimental")


test1 <- test %>%
         select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week)

table(test1$date)

test1 %>% filter(date == "2006-09-12")





stations_I84_1 <- stations_meta %>% 
                  filter( start_date=="2004-01-01 00:00:00-08") %>% 
                  select(stationid, lat, lon, highwayid, locationtext, start_date, end_date) %>% 
                  filter(stationid %in% c(1055, 1056, 1057, 1059, 1060, 1061, 1062, 1127))  %>% 
                  arrange(stationid)

head(data_5min_0405_green)
head(data_5min_0506_green)



data_5min_0405_green <- data_5min_0405_green %>%
                        filter(stationid %in% stations_I84$stationid)  %>%
                        mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), # 07/08 means DST changes which does not affect analysis
                               date=as.Date(starttime),
                               day_week=weekdays(date),
                               before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                               time_hour=hour(date_time),
                               AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), # the peak period is defined the same as commuting time 
                               AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
                        ) %>%
                        select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
                        arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
                        mutate(ec_group="experimental")

max(data_5min_0405_green$date)



data_5min_0405_green %>%
  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week)  %>%
    filter(date == "2005-09-12", detector_id==1425)

data_5min_0506_green %>%
  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week)  %>%
  filter(date == "2005-09-12", detector_id==1425)


table(data_5min_0607_green$starttime)
table(data_5min_0708_green$starttime)
table(data_5min_0809_green$starttime)
table(data_5min_0910_green$starttime)
table(data_5min_1011_green$starttime)
table(data_5min_1112_green$starttime)
table(data_5min_1213_green$starttime)
table(data_5min_1314_green$starttime)

min(data_5min_0506_green$date)




data_5min_0406_green <- rbind(data_5min_0405_green, data_5min_0506_green)

head(data_5min_0506_green)






table(data_5min_0506_green_1$date)




data_5min_0506_green_1 <- data_5min_0506_green %>%
                          mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), # 07/08 means DST changes which does not affect analysis
                                 date=as.Date(starttime),
                                 day_week=weekdays(date),
                                 before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                                 time_hour=hour(date_time),
                                 AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), # the peak period is defined the same as commuting time 
                                 AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
                          ) %>%
                          select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
                          arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
                          mutate(ec_group="experimental")
        



str(data_5min_0506_green)

test3 <- data_5min_0506_green_1 %>% filter(date=="2006-09-12")
nrow(test3)
test2 <- data_5min_0506_green %>% filter(starttime=="2006-09-12 00:00:00-07")
nrow(test2)

nrow(data_5min_0506_green)
nrow(data_5min_0506_green_1)



data_5min_0405_green %>% filter(starttime=="2005-09-12 00:00:00-07")



data_5min_0405_green <- data_5min_0405_green %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), # 07/08 means DST changes which does not affect analysis
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), # the peak period is defined the same as commuting time 
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
  mutate(ec_group="experimental")

table(data_5min_0405_green$date)


data_5min_0506_green <- data_5min_0506_green %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
  mutate(ec_group="experimental")



data_5min_0607_green <- data_5min_0607_green %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
  mutate(ec_group="experimental")

data_5min_0708_green <- data_5min_0708_green %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"),
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
  mutate(ec_group="experimental")


data_5min_0809_green <- data_5min_0809_green %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
  mutate(ec_group="experimental")

data_5min_0910_green <- data_5min_0910_green %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)),
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
  mutate(ec_group="experimental")


data_5min_1011_green <- data_5min_1011_green %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"),
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
  mutate(ec_group="experimental")

data_5min_1112_green <- data_5min_1112_green %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
  mutate(ec_group="experimental")

data_5min_1213_green <- data_5min_1213_green %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
  mutate(ec_group="experimental")

data_5min_1314_green <- data_5min_1314_green %>%
  mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
         date=as.Date(starttime),
         day_week=weekdays(date),
         before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
         time_hour=hour(date_time),
         AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
         AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM)
  ) %>%
  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
  arrange(highwayid, stationid, lanenumber, detector_id, date_time)  %>%
  mutate(ec_group="experimental")




data_5min_0414_green <- rbind(data_5min_0405_green, data_5min_0506_green, data_5min_0607_green, data_5min_0708_green,
                              data_5min_0809_green, data_5min_0910_green, data_5min_1011_green, data_5min_1112_green,
                              data_5min_1213_green, data_5min_1314_green)


length(unique(data_5min_0405_green$detector_id))
length(unique(data_5min_0506_green$detector_id))
length(unique(data_5min_0607_green$detector_id))
length(unique(data_5min_0708_green$detector_id))
length(unique(data_5min_0809_green$detector_id))
length(unique(data_5min_0910_green$detector_id))
length(unique(data_5min_1011_green$detector_id))
length(unique(data_5min_1112_green$detector_id))
length(unique(data_5min_1213_green$detector_id))
length(unique(data_5min_1314_green$detector_id))


sort(unique(data_5min_0405_green$detector_id))==sort(unique(data_5min_0506_green$detector_id))
sort(unique(data_5min_0506_green$detector_id))==sort(unique(data_5min_0607_green$detector_id))
sort(unique(data_5min_0607_green$detector_id))==sort(unique(data_5min_0708_green$detector_id))
sort(unique(data_5min_0708_green$detector_id))
sort(unique(data_5min_0809_green$detector_id))
sort(unique(data_5min_0910_green$detector_id))
sort(unique(data_5min_1011_green$detector_id))
sort(unique(data_5min_1112_green$detector_id))
sort(unique(data_5min_1213_green$detector_id))
sort(unique(data_5min_1314_green$detector_id))


# Check detector ids in each year
head(data_5min_0405_green %>% arrange(starttime))
tail(data_5min_0405_green)
table(data_5min_0405_green$date) 
table(data_5min_0506_green$date)
table(data_5min_0607_green$date)
table(data_5min_0708_green$date)
table(data_5min_0809_green$date)
table(data_5min_0910_green$date)
table(data_5min_1011_green$date)
table(data_5min_1112_green$date)
table(data_5min_1213_green$date)
table(data_5min_1314_green$date)



data_5min_0405_green %>% 
                filter(date=="2005-09-12") %>% 
                select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date)

data_5min_0506_green %>% 
              filter(starttime=="2005-09-12 00:00:00-07") %>% 
              select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date)


length(unique(data_5min_0405_green$detector_id))
length(unique(data_5min_0506_green$detector_id))


data_5min_0506_green %>% 
              filter(date=="2006-09-12") %>% 
              select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date)

data_5min_0607_green %>% 
              filter(starttime=="2006-09-12 00:00:00-07") %>% 
              select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date)

length(unique(data_5min_0506_green$detector_id))


head(data_5min_0405)


table(data_5min_0405$resolution)



test <- data_5min_0607_green %>%
        filter(detector_id==1949)

head(test)
head(data_5min_0607_green)

str(data_5min_0607_green)




head(detector_meta)



detector_meta %>% filter(detectorid==1949)


head(data_5min_0405_green)

data_5min_0405_green %>% filter(detector_id==1949)



head(data_5min_0405)


data_5min_0405 %>% filter(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955, 
                                             100369, 100370, 100374, 100375, 100376, 100435, 100436, 100437))


head(data_5min_0405)


data_5min_0607 %>% filter(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955, 
                                             100369, 100370, 100374, 100375, 100376, 100435, 100436, 100437))

test <- data_5min_0607_green %>% 
        filter(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955, 
                                  100369, 100370, 100374, 100375, 100376, 
                                  100435, 100436, 100437)) %>% 
        arrange(detector_id, starttime)

head(test)
table(test$starttime) 

head(data_5min_0405_green)


object.size(data_5min_0414_green)


data_5min_0405_green <- data_5min_0405_green%>%
                        mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
                               date=as.Date(starttime),
                               day_week=weekdays(date),
                               before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                               time_hour=hour(date_time),
                               AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
                               AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM),
                               ec_group=ifelse(highwayid %in% c(1,2), "control", "experimental")
                               ) %>%
                        select(highwayid, stationid, ec_group, lanenumber, detector_id, starttime, date_time, 
                               date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
                        arrange(highwayid, stationid, lanenumber, detector_id, date_time)  


data_5min_0405_green %>% 
               group_by(highwayid) %>% 
               summarise(freq=n(), 
                         ec_group=first(ec_group)) 


table(data_5min_0405_green$before_after)


summary(data_5min_0405_green)

head(data_5min_0405_green)

head(data_5min_0405)
summary(data_5min_0405)

table()


test <- data_5min_0405_green %>%
        filter(!is.na(speed))

nrow(test)/nrow(data_5min_0405_green)



# Todo list note
test1 <- table(data_5min_0405_green$detector_id)
test2 <- table(data_5min_0405_green$date)

max(test1); min(test1)
max(test2); min(test2)



test <- data_5min_0405_green %>%
        filter(date <= "2005-01-31")
length(table(test$date))
length(table(test$detector_id))


31

# data are missing during 2005-02-04 ~ 2005-02-09  
  
  
31+28



31*24*60/5





data_5min_0405_green <- data_5min_0405_green%>%
                        mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
                               date=as.Date(starttime),
                               day_week=weekdays(date),
                               before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                               time_hour=hour(date_time),
                               AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
                               AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM),
                               ec_group=ifelse(highwayid %in% c(1,2), "control", "experimental")
                        ) %>%
                        select(highwayid, stationid, ec_group, lanenumber, detector_id, starttime, date_time, 
                               date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
                        arrange(highwayid, stationid, lanenumber, detector_id, date_time)  
head(data_5min_0405_green)

head(data_5min_0405_green)

str(data_5min_0405_green)

sort(unique(data_5min_0405_green$detector_id))

data_5min_0405_green %>% 
           group_by(time_hour)  %>% 
           summarise(freq=n(),
                     AM_PM=first(AM_PM)
                     ) %>% 
           as.data.frame()



data_5min_0405_green %>% 
              select(highwayid, stationid, ec_group, lanenumber, detector_id, starttime, date_time, date) %>%
              filter(detector_id==1026) %>%
              filter(date =="2005-04-03")
                              
data_5min_0506_green <- data_5min_0506_green%>%
                        mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
                               date=as.Date(starttime),
                               day_week=weekdays(date),
                               before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                               time_hour=hour(date_time),
                               AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
                               AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM),
                               ec_group=ifelse(highwayid %in% c(1,2), "control", "experimental")
                        ) %>%
                        select(highwayid, stationid, ec_group, lanenumber, detector_id, starttime, date_time, 
                               date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
                        arrange(highwayid, stationid, lanenumber, detector_id, date_time)  

data_5min_0506_green %>% 
          select(highwayid, stationid, ec_group, lanenumber, detector_id, starttime, date_time, date) %>%
          filter(detector_id==1026) %>%
          filter(date =="2006-04-02")

data_5min_0506_green %>% 
                  select(highwayid, stationid, lanenumber, detector_id, starttime, date_time, date) %>%
                  filter(detector_id==1026) %>%
                  filter(date =="2005-10-30")

# The zone number does not matter 


# Check lane number of new detector 
head(data_5min_0414_green)
object.size(data_5min_0414_green)



test <- data_5min_1314_green %>%
  filter(starttime!="2014-09-12 00:00:00-07") %>% 
  filter(detector_id %in% c(1949, 1950, 1951, 1953, 1954, 1955, 
                              100369, 100370, 100374, 100375, 100376, 
                              100435, 100436, 100437))
head(test)


test <- data_5min_1314_green %>%
        filter(starttime!="2014-09-12 00:00:00-07") %>% 
        filter(highwayid==1, stationid==1007, lanenumber==1)

head(test)
table(test$detector_id)


test1 <- data_5min_1314_green %>%
          filter(starttime!="2014-09-12 00:00:00-07") %>% 
          filter(detector_id %in% c(1073)) %>%
          mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
                 date=as.Date(starttime),
                 day_week=weekdays(date),
                 before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                 time_hour=hour(date_time),
                 AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
                 AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM),
                 ec_group=ifelse(highwayid %in% c(1,2), "control", "experimental")) %>%
          select(highwayid, detector_id, stationid, lanenumber,  starttime, date_time, day_week, date, time_hour,  before_after, 
                 AM_PM, speed)  %>% # volume
          arrange(highwayid, stationid, lanenumber, detector_id, date_time)  

nrow(test1)
head(test1)
table(test1$date)

test2<- data_5min_1314_green %>%
        filter(starttime!="2014-09-12 00:00:00-07") %>% 
        filter(detector_id %in% c(100437)) %>%
        mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
               date=as.Date(starttime),
               day_week=weekdays(date),
               before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
               time_hour=hour(date_time),
               AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
               AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM),
               ec_group=ifelse(highwayid %in% c(1,2), "control", "experimental")) %>%
        select(highwayid, detector_id, stationid, lanenumber,  starttime, date_time, day_week, date, time_hour,  before_after, 
               AM_PM, speed)  %>% # stationid, lanenumber,  date, time_hour, volume
        arrange(highwayid, stationid, lanenumber, detector_id, date_time)  

table(test2$date)


head(test2)


test3 <- test1 %>% filter(date=="2014-07-14")
head(test3)
table(test3$date_time)
test4 <- test2 %>% filter(date=="2014-07-14")
head(test4)
table(test4$date_time)

test3 %>% filter(date_time=="2014-07-14 23:55:00")
test4 %>% filter(date_time=="2014-07-14 23:55:00")


# Two detectors for one lane 
# highwayid detector_id stationid lanenumber              starttime           date_time day_week       date time_hour before_after   AM_PM speed
#         1        1073      1007          1 2014-07-14 23:55:00-07 2014-07-14 23:55:00   Monday 2014-07-14        23            1 NonPeak  61.4
# test4 %>% filter(date_time=="2014-07-14 23:55:00")
# highwayid detector_id stationid lanenumber              starttime           date_time day_week       date time_hour before_after   AM_PM speed
#         1      100437      1007          1 2014-07-14 23:55:00-07 2014-07-14 23:55:00   Monday 2014-07-14        23            1 NonPeak 35.24


data_5min_0405_green <- data_5min_0405_green%>%
                        mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
                               date=as.Date(starttime),
                               day_week=weekdays(date),
                               before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                               time_hour=hour(date_time),
         
                                                     AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
                               AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM),
                               ec_group=ifelse(highwayid %in% c(1,2), "control", "experimental")
                        ) %>%
                        select(highwayid, stationid, ec_group, lanenumber, detector_id, starttime, date_time, 
                               date, day_week, before_after, time_hour, AM_PM, speed, volume)  %>%
                        arrange(highwayid, stationid, lanenumber, detector_id, date_time)  

test <- data_5min_0414_green %>% group_by(highwayid, stationid, lanenumber, detector_id) %>% 
        summarise(freq=n()) %>% 
        as.data.frame() %>% 
        mutate(sid=paste(highwayid, stationid, lanenumber, sep=""))

table(test$sid)

head(data_5min_0414_green)


object.size()




# Convert 5-minute data to 15-minute data 

head(data_5min_0405_green)

data_5min_0405_green 


test <- data_5min_0405_green %>%
        select(id, starttime)

seq(as.Date("2011-12-30"), as.Date("2012-01-04"), by="days")
rep(seq(as.Date("2011-12-30"), as.Date("2012-01-04"), by="days"), each=5)





test_5min <- data_5min_0405_green %>%
        mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
               date=as.Date(starttime),
               day_week=weekdays(date),
               before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
               time_hour=hour(date_time),
               AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
               AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM),
               ec_group=ifelse(highwayid %in% c(1,2), "control", "experimental")) %>%
        select(highwayid, stationid, lanenumber, detector_id, starttime, date, date_time,  time_hour, day_week, before_after, 
                 AM_PM, speed)  %>% #   volume
        arrange(highwayid, stationid, lanenumber, detector_id, date_time)  

head(test)
max(test$date) - min(test$date)

length(sort(unique(test$date)))



rep(seq(as.Date("2011-12-30"), as.Date("2012-01-04"), by="days"), each=5)


seq(as.Date("2004-12-31"), as.Date("2012-01-04"), by="days")




tryit <- data.frame(day_255=seq(as.Date("2014-12-31"), as.Date("2015-09-12"), by="days"))


nrow(tryit)


a <- data.frame(date_256=as.character(seq(as.Date("2004-12-31"), as.Date("2005-02-28"), by="days")))
head(a)
nrow(a)

b <- data.frame(date_256=as.character(sort(unique(test$date))), exisitng=2)

str(a)
str(b)

head(a)
head(b)
c <- a %>%
     left_join(b)
class(a)
class(b)

set.diff(a, b)



head(test_5min)


test_5min_sub <-  test_5min %>%
                  filter(date <= "2005-02-28")



head(test_5min_sub)

test_5min_sub 
str(test_5min_sub)
seq(ISOdatetime(2005,4,3,0,0,0), ISOdatetime(2005,4,5,0,0,0), by=(60*60))
seq(ISOdatetime(2005,10,30,0,0,0), ISOdatetime(2005,11,1,0,0,0), by=(60*60))

test_5min %>% filter(date=="2005-04-03")



24*60/5


test <- data.frame(hour=)

hour=rep(0:23, each=12)
min=rep(seq(0, 55, by=5), )




seq(ISOdatetime(2005,4,3,0,0,0), ISOdatetime(2005,4,5,0,0,0), by=(60*60))





test <- seq(ISOdatetime(2005,4,2,0,0,0), ISOdatetime(2005,4,2,23,55,0), by=(60*5))
length(test)



head(test_5min)




test <- data.frame(date_time=seq(ISOdatetime(2005,4,2,0,0,0), ISOdatetime(2005,4,4,23,55,0), by=(60*5)))
length(test)

24*60/5
288*2
24*24
12*24

test <- data.frame(date_time=seq(ISOdatetime(2005,1,1,0,0,0), ISOdatetime(2005,1,31,23,55,0), by=(60*5)))
nrow(test)
head(test)


test <- data.frame(date_time=seq(ISOdatetime(2005,4,2,0,0,0), ISOdatetime(2005,4,4,23,55,0), by=(60*5)))
str(test)


test_5min <- data_5min_0405_green %>%
              mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
                     date=as.Date(starttime),
                     day_week=weekdays(date),
                     before_after=ifelse(date < ymd("2009-09-12"), 0, 1),
                     time_hour=hour(date_time),
                     AM_PM=ifelse(time_hour %in% c(6, 7, 8, 9), "AM", "NonPeak"), 
                     AM_PM=ifelse(time_hour %in% c(16, 17, 18, 19), "PM", AM_PM),
                     ec_group=ifelse(highwayid %in% c(1,2), "control", "experimental")) %>%
              select(highwayid, stationid, lanenumber, detector_id, starttime, date, date_time,  time_hour, day_week, before_after, 
                     AM_PM, speed)  %>% #   volume
              arrange(highwayid, stationid, lanenumber, detector_id, date_time)  

head(test_5min)


test_5min_sub <- test_5min %>%
                 select(starttime, date_time, date, speed) %>%
                 filter(date < "2005-02-01")

summary(test_5min_sub)
nrow(test_5min_sub)

time_frame <- data.frame(date_time=seq(ISOdatetime(2005,1,1,0,0,0), ISOdatetime(2005,1,31,23,55,0), by=(60*5))) %>% 
              mutate(date=as.Date(date_time),
                     hour=hour(date_time),
                     min5=minute(date_time),
                     min15=ifelse(min5 %in% c(0,  5,  10), 1, NA),
                     min15=ifelse(min5 %in% c(15, 20, 25), 2, min15),
                     min15=ifelse(min5 %in% c(30, 35, 40), 3, min15),
                     min15=ifelse(min5 %in% c(45, 50, 55), 4, min15)
                     ) %>%
              left_join(test_5min_sub)
         
head(time_frame, 100)      
time_frame %>% filter(date=="2005-01-10")


head(data_5min_0414_green)




data_15min_0414_green <-  data_5min_0414_green %>%
                          mutate(hour=hour(date_time),
                                 min5=minute(date_time),
                                 min15=ifelse(min5 %in% c(0,  5,  10), 1, NA),
                                 min15=ifelse(min5 %in% c(15, 20, 25), 2, min15),
                                 min15=ifelse(min5 %in% c(30, 35, 40), 3, min15),
                                 min15=ifelse(min5 %in% c(45, 50, 55), 4, min15)) %>%
                          group_by (highwayid, stationid, lanenumber, detector_id, date, hour, min15)  %>%
                          summarise(speed=mean(speed, na.rm=TRUE),
                                    volume=mean(volume, na.rm=TRUE))

summary(data_15min_0414_green)

head(data_15min_0414_green %>% filter(!is.na(speed)), 20)

data_5min_0414_green %>% filter()

head(data_5min_0414_green)



test <- data_15min_0414_green %>% filter(!is.na(speed))
head(test)


data_5min_0414_green %>% filter(detector_id==1025, date=="2005-03-01")




(59.88+65.00+54.7)/3
(25+44+31)/3


head(data_5min_0405_green)


data_5min_0405_green <-  data_5min_0405_green %>%
                          mutate(date_time=ymd_hms(substr(starttime, 1, nchar(starttime)-3)), 
                                 hour=hour(date_time),
                                 min5=minute(date_time),
                                 min15=ifelse(min5 %in% c(0,  5,  10), 1, NA),
                                 min15=ifelse(min5 %in% c(15, 20, 25), 2, min15),
                                 min15=ifelse(min5 %in% c(30, 35, 40), 3, min15),
                                 min15=ifelse(min5 %in% c(45, 50, 55), 4, min15)) 


head(data_5min_0405_green, 100)
















                          




