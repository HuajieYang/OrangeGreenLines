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


stations_I84 <- stations_meta %>% 
  filter(highwayid %in% c(7, 8), start_date=="2004-01-01 00:00:00-08") 

# East bound 
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

# 44 percent missing value
test4 <- test %>%
  group_by(highwayid, date, time_hour, time_min5) %>%
  summarise(freq=n())

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

# Second version ====
# Test data 
data_5min_0507_green_I84_east <- data_5min_0507_green %>%
  filter(stationid %in% c(1055, 1056, 1057, 1127))  %>%
  filter(lanenumber==1, AM_PM %in% c("AM", "PM")) %>%
  # select(-date_time) %>%
  left_join(stations_I84_7) %>%
  arrange(date, time_hour, time_min5, stationid)%>%
  mutate(id=paste(highwayid, date, time_hour, time_min5, sep="_"),
         stationid=as.factor(stationid))

head(data_5min_0507_green_I84_east)

# 44 percent missing value
test <- data_5min_0507_green_I84_east %>%
  group_by(highwayid, date, time_hour, time_min5) %>%
  summarise(freq=n())
table(test$freq)

# Try MICE package====
# Reference: https://www.youtube.com/watch?v=An7nPLJ0fsg
# Reference: https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

# Using 5 minutes later/earlier (15 min range) or adjacent stations (upstream first then downstream; 15 min range)====

# Add later/earlier observation 
test2 <- test %>% 
  arrange(highwayid, stationid, date, time_hour, time_min5)

head(test2)

max(test$date)
min(test$date)
table(test$time_min5)  

seq(as.Date("2005-09-12"), as.Date("2007-09-11"), by="days")
length(seq(as.Date("2005-09-12"), as.Date("2007-09-11"), by="days"))
c(6:9, 16:19)  
seq(0, 55, 5)  

# 730 is the number of days: length(seq(as.Date("2005-09-12"), as.Date("2007-09-11"), by="days")) 
# 8: 8 hours: c(6:9, 16:19)
# 12: time intervals: seq(0, 55, 5)
# 4 stations: c(1055, 1056, 1057, 1127)
full_df_I84_east <- data.frame(date=rep(seq(as.Date("2005-09-12"), as.Date("2007-09-11"), by="days"), each=8*12*4),
                               time_hour=rep(c(6:9, 16:19), each=12*4, times=730),
                               time_min5=rep(seq(0, 55, 5), each=4, times=730*8),
                               stationid=rep(c(1055, 1056, 1057, 1127), 730*8*12))  %>%
  left_join(data_5min_0507_green_I84_east) %>%
  mutate(time_id=paste(date, time_hour, time_min5, sep="_"),
         AM_PM=ifelse(time_hour %in% c(6:9), "AM", "PM"))


nrow(full_df_I84_east)  /730
rep(c(1:3), each=2)  
rep(c(1:3), 2)  

head(full_df_I84_east, 100)

table(data_5min_0507_green_I84_east$detector_id)

head(full_df_I84_east)
summary(full_df_I84_east)

# Check whether all observations are missing at time points 
speed_NA_df <- full_df_I84_east %>%
  group_by(date, time_hour, time_min5) %>%
  summarise(speed_NA=all(is.na(speed)))  %>%
  filter(speed_NA==TRUE) %>%
  mutate(time_id=paste(date, time_hour, time_min5, sep="_"))

head(speed_NA_df)
summary(test)
sum(c(NA, NA, NA), na.rm = TRUE)

data_5min_0507_green_I84_east %>% filter(date=="2005-09-28", time_hour==9)

# Calculate peak periods average speed of previous/later day 
full_df_I84_east_day_pp <- full_df_I84_east %>%
  select(stationid, date, day_week, time_hour, time_min5, starttime, AM_PM, speed) %>%
  mutate(week_num=week(date),
         day_week=weekdays(date),
         year=year(date))  %>%
  filter(!(day_week %in% c("Saturday", "Sunday")))  %>%
  group_by(stationid, year, week_num, date, AM_PM) %>%
  summarise(speed_avg=mean(speed, na.rm=TRUE),
            speed_median=median(speed, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(stationid, year, week_num, AM_PM) %>%
  mutate(speed_avg_lag1=lag(speed_avg),
         speed_avg_lag2=lag(speed_avg, 2),
         speed_avg_lag3=lag(speed_avg, 3),
         speed_avg_lag4=lag(speed_avg, 4),
         
         speed_avg_lead1=lead(speed_avg),
         speed_avg_lead2=lead(speed_avg, 2),
         speed_avg_lead3=lead(speed_avg, 3),
         speed_avg_lead4=lead(speed_avg, 4)
  ) %>%
  ungroup() %>%
  mutate(day_week=weekdays(date))  %>%
  select(stationid, date, week_num, AM_PM, day_week, year, speed_avg, speed_median,
         speed_avg_lag1, speed_avg_lag2, speed_avg_lag3, speed_avg_lag4, 
         speed_avg_lead1, speed_avg_lead2, speed_avg_lead3, speed_avg_lead4) %>%
  arrange(stationid, year, week_num, AM_PM) %>%
  mutate(stationid=as.factor(stationid)) %>%
  as.data.frame()

# get peak periods average speed the same day of week of last/previous week 
full_df_I84_east_week_pp <- full_df_I84_east_day_pp %>%
  arrange(stationid, AM_PM, date)  %>%
  group_by(stationid, AM_PM)  %>%
  mutate(speed_avg_lag5=lag(speed_avg, 5),
         speed_avg_lag10=lag(speed_avg, 10),
         speed_avg_lead5=lead(speed_avg, 5),
         speed_avg_lead10=lead(speed_avg, 10)) %>%
  select(stationid, date, week_num, AM_PM, day_week, year, speed_avg, 
         speed_avg_lag5, speed_avg_lag10, speed_avg_lead5, speed_avg_lead10) %>%
  as.data.frame()

# Plot data====
# Plot 5 minute interval data by stationid
p_5min_sid <- ggplot(data=data_5min_0507_green_I84_east, aes(x=date_time, y=speed, group=stationid)) +
  geom_point(aes(colour=stationid), size=0.25) + 
  #geom_smooth(aes(linetype=stationid), colour="yellow", size=0.75) + 
  ggtitle("5-min speed by stationid over time") + #+  ylim(0, 50)
  labs( x="Date", y="speed (mph)") +  facet_grid( ~ AM_PM) + 
  theme(plot.title = element_text(hjust = 0.5))

p_5min_sid

# plot daily average speed during AM/PM by stationid 
p_day_sid <- ggplot(data=full_df_I84_east_day_pp, aes(x=date, y=speed_avg, group=stationid)) +
  geom_point(aes(colour=stationid), size=0.5) + 
  #geom_smooth(aes(linetype=stationid), colour="yellow", size=0.75) + 
  ggtitle("Daily AM/PM speed by stationid over time") + #+  ylim(0, 50)
  labs( x="Date", y="speed (mph)") +  facet_grid( ~ AM_PM) + 
  theme(plot.title = element_text(hjust = 0.5))

p_day_sid

# Organize data 
head(full_df_I84_east)
nrow(full_df_I84_east_time)
full_df_I84_east_time <- full_df_I84_east %>%
  select(stationid, date, time_hour, time_min5, starttime, AM_PM, speed) %>%
  arrange(stationid, date, time_hour, time_min5)  %>%
  group_by(stationid, date, AM_PM)  %>%
  mutate(speed_tlag1=lag(speed),
         speed_tlag2=lag(speed, 2),
         speed_tlag3=lag(speed, 3),
         speed_tlag4=lag(speed, 4),
         speed_tlag5=lag(speed, 5),
         speed_tlag6=lag(speed, 6),
         speed_tlag7=lag(speed, 7),
         speed_tlag8=lag(speed, 8),
         speed_tlag9=lag(speed, 9),
         speed_tlag10=lag(speed, 10),
         speed_tlag11=lag(speed, 11),
         speed_tlag12=lag(speed, 12),
         
         speed_tlag13=lag(speed, 13),
         speed_tlag14=lag(speed, 14),
         speed_tlag15=lag(speed, 15),
         speed_tlag16=lag(speed, 16),
         speed_tlag17=lag(speed, 17),
         speed_tlag18=lag(speed, 18),
         speed_tlag19=lag(speed, 19),
         speed_tlag20=lag(speed, 20),
         speed_tlag21=lag(speed, 21),
         speed_tlag22=lag(speed, 22),
         speed_tlag23=lag(speed, 23),
         speed_tlag24=lag(speed, 24),
         
         speed_tlag25=lag(speed, 25),
         speed_tlag26=lag(speed, 26),
         speed_tlag27=lag(speed, 27),
         speed_tlag28=lag(speed, 28),
         speed_tlag29=lag(speed, 29),
         speed_tlag30=lag(speed, 30),
         speed_tlag31=lag(speed, 31),
         speed_tlag32=lag(speed, 32),
         speed_tlag33=lag(speed, 33),
         speed_tlag34=lag(speed, 34),
         speed_tlag35=lag(speed, 35),
         speed_tlag36=lag(speed, 36),
         
         speed_tlag37=lag(speed, 37),
         speed_tlag38=lag(speed, 38),
         speed_tlag39=lag(speed, 39),
         speed_tlag40=lag(speed, 40),
         speed_tlag41=lag(speed, 41),
         speed_tlag42=lag(speed, 42),
         speed_tlag43=lag(speed, 43),
         speed_tlag44=lag(speed, 44),
         speed_tlag45=lag(speed, 45),
         speed_tlag46=lag(speed, 46),
         speed_tlag47=lag(speed, 47),
         
         speed_tlead1=lead(speed),
         speed_tlead2=lead(speed, 2),
         speed_tlead3=lead(speed, 3),
         speed_tlead4=lead(speed, 4),
         speed_tlead5=lead(speed, 5),
         speed_tlead6=lead(speed, 6),
         speed_tlead7=lead(speed, 7),
         speed_tlead8=lead(speed, 8),
         speed_tlead9=lead(speed, 9),
         speed_tlead10=lead(speed, 10),
         speed_tlead11=lead(speed, 11),
         speed_tlead12=lead(speed, 12),
         
         speed_tlead13=lead(speed, 13),
         speed_tlead14=lead(speed, 14),
         speed_tlead15=lead(speed, 15),
         speed_tlead16=lead(speed, 16),
         speed_tlead17=lead(speed, 17),
         speed_tlead18=lead(speed, 18),
         speed_tlead19=lead(speed, 19),
         speed_tlead20=lead(speed, 20),
         speed_tlead21=lead(speed, 21),
         speed_tlead22=lead(speed, 22),
         speed_tlead23=lead(speed, 23),
         speed_tlead24=lead(speed, 24),
         
         speed_tlead25=lead(speed, 25),
         speed_tlead26=lead(speed, 26),
         speed_tlead27=lead(speed, 27),
         speed_tlead28=lead(speed, 28),
         speed_tlead29=lead(speed, 29),
         speed_tlead30=lead(speed, 30),
         speed_tlead31=lead(speed, 31),
         speed_tlead32=lead(speed, 32),
         speed_tlead33=lead(speed, 33),
         speed_tlead34=lead(speed, 34),
         speed_tlead35=lead(speed, 35),
         speed_tlead36=lead(speed, 36),
         
         speed_tlead37=lead(speed, 37),
         speed_tlead38=lead(speed, 38),
         speed_tlead39=lead(speed, 39),
         speed_tlead40=lead(speed, 40),
         speed_tlead41=lead(speed, 41),
         speed_tlead42=lead(speed, 42),
         speed_tlead43=lead(speed, 43),
         speed_tlead44=lead(speed, 44),
         speed_tlead45=lead(speed, 45),
         speed_tlead46=lead(speed, 46),
         speed_tlead47=lead(speed, 47)
  )  %>%
  as.data.frame() %>%
  mutate(time_id=paste(date, time_hour, time_min5, sep="_")) %>%
  left_join(full_df_I84_east_day_pp) %>%
  left_join(full_df_I84_east_week_pp) %>%
  arrange(stationid, date, time_hour, time_min5)


head(full_df_I84_east_time, 50)

# Explore inputing strategy =====
# Check correlation 
cor(full_df_I84_east_time$speed, 
    full_df_I84_east_time[, c("speed_avg", "speed_median",
                              "speed_tlag1", "speed_tlag2", "speed_tlag3", 
                              "speed_tlag4", "speed_tlag5", "speed_tlag6",
                              "speed_tlag7", "speed_tlag8", "speed_tlag9",
                              "speed_tlag10", "speed_tlag11", "speed_tlag12")], 
    use = "complete.obs")

cor(full_df_I84_east_time$speed, 
    full_df_I84_east_time[, c("speed_tlag13", "speed_tlag14", "speed_tlag15", 
                              "speed_tlag16", "speed_tlag17", "speed_tlag18",
                              "speed_tlag19", "speed_tlag20", "speed_tlag21",
                              "speed_tlag22", "speed_tlag23", "speed_tlag24")], 
    use = "complete.obs")

cor(full_df_I84_east_time$speed, 
    full_df_I84_east_time[, c("speed_tlag25", "speed_tlag26", "speed_tlag27", 
                              "speed_tlag28", "speed_tlag29", "speed_tlag30",
                              "speed_tlag31", "speed_tlag32", "speed_tlag33",
                              "speed_tlag34", "speed_tlag35", "speed_tlag36")], 
    use = "complete.obs")


cor(full_df_I84_east_time$speed, 
    full_df_I84_east_time[, c("speed_tlag37", "speed_tlag38", "speed_tlag39", 
                              "speed_tlag40", "speed_tlag41", "speed_tlag42",
                              "speed_tlag43", "speed_tlag44", "speed_tlag45",
                              "speed_tlag46", "speed_tlag47")], 
    use = "complete.obs")

cor(full_df_I84_east_time$speed, 
    full_df_I84_east_time[, c("speed_avg_lag1", "speed_avg_lag2", "speed_avg_lag3", "speed_avg_lag4")], 
    use = "complete.obs")

cor(full_df_I84_east_time$speed, 
    full_df_I84_east_time[, c("speed_avg_lead1", "speed_avg_lead2", "speed_avg_lead3", "speed_avg_lead4")], 
    use = "complete.obs")

cor(full_df_I84_east_time$speed, 
    full_df_I84_east_time[, c("speed_avg_lag5", "speed_avg_lag10", "speed_avg_lead5", "speed_avg_lead10")], 
    use = "complete.obs")


cor(full_df_I84_east_station$speed, 
    full_df_I84_east_station[, c("speed_slead1", "speed_slead2", "speed_slead3")], 
    use = "complete.obs")



# Add later/earlier observation columns ====
# Use earlier/later observation for missing value. 
# Reference: https://www.guru99.com/r-if-else-elif-statement.html
head(test)

# 15 min range 
full_df_I84_east_time_sub <- full_df_I84_east_time %>%
  filter(is.na(speed), !(time_id %in% speed_NA_df$time_id))  %>%
  select(-starttime, -time_id) %>%
  mutate(speed_new=ifelse(!is.na(speed_tlag1), speed_tlag1, speed_tlead1),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag2), speed_tlag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead2), speed_tlead2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag3), speed_tlag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead3), speed_tlead3, speed_new)
  )


# 93.3 percent missing values are not inputed 
nrow(full_df_I84_east_time_sub %>% filter(is.na(speed_new)))/nrow(full_df_I84_east_time_sub)


head(full_df_I84_east_time_sub)
summary(full_df_I84_east_time_sub)

test3 <- full_df_I84_east_time_sub %>%
  filter(is.na(speed_new))


head(test3)



# 30 min range
full_df_I84_east_time_sub_30 <- full_df_I84_east_time %>%
  filter(is.na(speed), !(time_id %in% speed_NA_df$time_id))  %>%
  select(-starttime, -time_id) %>%
  mutate(speed_new=ifelse(!is.na(speed_tlag1), speed_tlag1, NA),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead1), speed_tlead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag2), speed_tlag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead2), speed_tlead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag3), speed_tlag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead3), speed_tlead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag4), speed_tlag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead4), speed_tlead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag5), speed_tlag5, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead5), speed_tlead5, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag6), speed_tlag6, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead6), speed_tlead6, speed_new))

# 90.4 percent missing values are not inputed 
nrow(full_df_I84_east_time_sub_30 %>% filter(is.na(speed_new)))/nrow(full_df_I84_east_time_sub_30)

# An hour range 
full_df_I84_east_time_sub_60 <- full_df_I84_east_time %>%
  filter(is.na(speed), !(time_id %in% speed_NA_df$time_id))  %>%
  select(-starttime, -time_id) %>%
  mutate(speed_new=ifelse(!is.na(speed_tlag1), speed_tlag1, NA),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead1), speed_tlead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag2), speed_tlag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead2), speed_tlead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag3), speed_tlag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead3), speed_tlead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag4), speed_tlag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead4), speed_tlead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag5), speed_tlag5, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead5), speed_tlead5, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag6), speed_tlag6, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead6), speed_tlead6, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag7), speed_tlag7, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead7), speed_tlead7, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag8), speed_tlag8, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead8), speed_tlead8, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag9), speed_tlag9, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead9), speed_tlead9, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag10), speed_tlag10, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead10), speed_tlead10, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag11), speed_tlag11, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead11), speed_tlead11, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag12), speed_tlag12, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead12), speed_tlead12, speed_new)
  )

# 90.4 percent missing values are not inputed 
nrow(full_df_I84_east_time_sub_60 %>% filter(is.na(speed_new)))/nrow(full_df_I84_east_time_sub_60)

# Two hours range 
full_df_I84_east_time_sub_120 <- full_df_I84_east_time %>%
  filter(is.na(speed), !(time_id %in% speed_NA_df$time_id))  %>%
  select(-starttime, -time_id) %>%
  mutate(speed_new=ifelse(!is.na(speed_tlag1), speed_tlag1, NA),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead1), speed_tlead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag2), speed_tlag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead2), speed_tlead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag3), speed_tlag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead3), speed_tlead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag4), speed_tlag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead4), speed_tlead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag5), speed_tlag5, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead5), speed_tlead5, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag6), speed_tlag6, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead6), speed_tlead6, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag7), speed_tlag7, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead7), speed_tlead7, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag8), speed_tlag8, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead8), speed_tlead8, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag9), speed_tlag9, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead9), speed_tlead9, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag10), speed_tlag10, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead10), speed_tlead10, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag11), speed_tlag11, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead11), speed_tlead11, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag12), speed_tlag12, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead12), speed_tlead12, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag13), speed_tlag13, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead13), speed_tlead13, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag14), speed_tlag14, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead14), speed_tlead14, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag15), speed_tlag15, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead15), speed_tlead15, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag16), speed_tlag16, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead16), speed_tlead16, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag17), speed_tlag17, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead17), speed_tlead17, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag18), speed_tlag18, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead18), speed_tlead18, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag19), speed_tlag19, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead19), speed_tlead19, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag20), speed_tlag20, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead20), speed_tlead20, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag21), speed_tlag21, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead21), speed_tlead21, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag22), speed_tlag22, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead22), speed_tlead22, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag23), speed_tlag23, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead23), speed_tlead23, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag24), speed_tlag24, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead24), speed_tlead24, speed_new)
  )

# 85 percent missing values are not inputed 
nrow(full_df_I84_east_time_sub_120 %>% filter(is.na(speed_new)))/nrow(full_df_I84_east_time_sub_120)

# 4 hours range 
full_df_I84_east_time_sub_240 <- full_df_I84_east_time %>%
  filter(is.na(speed), !(time_id %in% speed_NA_df$time_id))  %>%
  select(-starttime, -time_id) %>%
  mutate(speed_new=ifelse(!is.na(speed_tlag1), speed_tlag1, NA),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead1), speed_tlead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag2), speed_tlag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead2), speed_tlead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag3), speed_tlag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead3), speed_tlead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag4), speed_tlag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead4), speed_tlead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag5), speed_tlag5, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead5), speed_tlead5, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag6), speed_tlag6, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead6), speed_tlead6, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag7), speed_tlag7, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead7), speed_tlead7, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag8), speed_tlag8, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead8), speed_tlead8, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag9), speed_tlag9, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead9), speed_tlead9, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag10), speed_tlag10, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead10), speed_tlead10, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag11), speed_tlag11, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead11), speed_tlead11, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag12), speed_tlag12, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead12), speed_tlead12, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag13), speed_tlag13, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead13), speed_tlead13, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag14), speed_tlag14, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead14), speed_tlead14, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag15), speed_tlag15, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead15), speed_tlead15, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag16), speed_tlag16, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead16), speed_tlead16, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag17), speed_tlag17, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead17), speed_tlead17, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag18), speed_tlag18, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead18), speed_tlead18, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag19), speed_tlag19, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead19), speed_tlead19, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag20), speed_tlag20, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead20), speed_tlead20, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag21), speed_tlag21, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead21), speed_tlead21, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag22), speed_tlag22, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead22), speed_tlead22, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag23), speed_tlag23, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead23), speed_tlead23, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag24), speed_tlag24, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead24), speed_tlead24, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag25), speed_tlag25, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead25), speed_tlead25, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag26), speed_tlag26, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead26), speed_tlead26, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag27), speed_tlag27, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead27), speed_tlead27, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag28), speed_tlag28, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead28), speed_tlead28, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag29), speed_tlag29, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead29), speed_tlead29, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag30), speed_tlag30, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead30), speed_tlead30, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag31), speed_tlag31, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead31), speed_tlead31, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag32), speed_tlag32, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead32), speed_tlead32, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag33), speed_tlag33, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead33), speed_tlead33, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag34), speed_tlag34, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead34), speed_tlead34, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag35), speed_tlag35, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead35), speed_tlead35, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag36), speed_tlag36, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead36), speed_tlead36, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag37), speed_tlag37, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead37), speed_tlead37, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag38), speed_tlag38, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead38), speed_tlead38, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag39), speed_tlag39, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead39), speed_tlead39, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag40), speed_tlag40, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead40), speed_tlead40, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag41), speed_tlag41, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead41), speed_tlead41, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag42), speed_tlag42, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead42), speed_tlead42, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag43), speed_tlag43, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead43), speed_tlead43, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag44), speed_tlag44, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead44), speed_tlead44, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag45), speed_tlag45, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead45), speed_tlead45, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag46), speed_tlag46, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead46), speed_tlead46, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag47), speed_tlag47, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead47), speed_tlead47, speed_new),
         
  )

# 80.3 percent missing values are not inputed 
nrow(full_df_I84_east_time_sub_240 %>% filter(is.na(speed_new)))/nrow(full_df_I84_east_time_sub_240)

# Check missing values by stationid 
test <- full_df_I84_east_time_sub_240 %>%
  filter(is.na(speed_new)) %>%
  mutate(date_stationid=paste(date, stationid, sep="_"))
table(test$stationid)/48

test1 <- test %>%
  filter(stationid=="1127")
table(test1$date)

# Use previou or later days to input missing value

# Inputing order: time lag 1-9, speed_avg, spee_avg lag 1:4
full_df_I84_east_time_sub_day_PP_90 <- full_df_I84_east_time %>%
  filter(is.na(speed), !(time_id %in% speed_NA_df$time_id))  %>%
  select(-starttime, -time_id) %>%
  mutate(speed_new=ifelse(!is.na(speed_tlag1), speed_tlag1, NA),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead1), speed_tlead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag2), speed_tlag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead2), speed_tlead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag3), speed_tlag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead3), speed_tlead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag4), speed_tlag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead4), speed_tlead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag5), speed_tlag5, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead5), speed_tlead5, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag6), speed_tlag6, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead6), speed_tlead6, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag7), speed_tlag7, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead7), speed_tlead7, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag8), speed_tlag8, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead8), speed_tlead8, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag9), speed_tlag9, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead9), speed_tlead9, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg), speed_avg, speed_new), # average speed
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag1), speed_avg_lag1, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead1), speed_avg_lead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag2), speed_avg_lag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead2), speed_avg_lead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag3), speed_avg_lag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead3), speed_avg_lead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag4), speed_avg_lag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead4), speed_avg_lead4, speed_new)
  )

# 66.8 percent missing values are not inputed 
nrow(full_df_I84_east_time_sub_day_PP_90 %>% filter(is.na(speed_new)))/nrow(full_df_I84_east_time_sub_day_PP_90)


# Use previou or later days and the same day of week of last/next week to input missing value
# Inputing order: time lag 1-9, speed_avg, spee_avg lag 1:4, the same day of week of last/next week
full_df_I84_east_time_sub_week_day_PP_90 <- full_df_I84_east_time %>%
  filter(is.na(speed), !(time_id %in% speed_NA_df$time_id))  %>%
  select(-starttime, -time_id) %>%
  mutate(speed_new=ifelse(!is.na(speed_tlag1), speed_tlag1, NA),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead1), speed_tlead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag2), speed_tlag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead2), speed_tlead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag3), speed_tlag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead3), speed_tlead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag4), speed_tlag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead4), speed_tlead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag5), speed_tlag5, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead5), speed_tlead5, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag6), speed_tlag6, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead6), speed_tlead6, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag7), speed_tlag7, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead7), speed_tlead7, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag8), speed_tlag8, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead8), speed_tlead8, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag9), speed_tlag9, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead9), speed_tlead9, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg), speed_avg, speed_new), # average speed
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag1), speed_avg_lag1, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead1), speed_avg_lead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag2), speed_avg_lag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead2), speed_avg_lead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag3), speed_avg_lag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead3), speed_avg_lead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag4), speed_avg_lag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead4), speed_avg_lead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag5), speed_avg_lag5, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead5), speed_avg_lead5, speed_new)
  )

# 59.8 percent missing values are not inputed 
nrow(full_df_I84_east_time_sub_week_day_PP_90 %>% filter(is.na(speed_new)))/nrow(full_df_I84_east_time_sub_week_day_PP_90)


# Use previou or later days and the same day of week of last/next 2 week to input missing value
# Inputing order: time lag 1-9, speed_avg, spee_avg lag 1:4, the same day of week of last/next week
full_df_I84_east_time_sub_2week_day_PP_90 <- full_df_I84_east_time %>%
  filter(is.na(speed), !(time_id %in% speed_NA_df$time_id))  %>%
  select(-starttime, -time_id) %>%
  mutate(speed_new=ifelse(!is.na(speed_tlag1), speed_tlag1, NA),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead1), speed_tlead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag2), speed_tlag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead2), speed_tlead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag3), speed_tlag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead3), speed_tlead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag4), speed_tlag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead4), speed_tlead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag5), speed_tlag5, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead5), speed_tlead5, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag6), speed_tlag6, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead6), speed_tlead6, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag7), speed_tlag7, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead7), speed_tlead7, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag8), speed_tlag8, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead8), speed_tlead8, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag9), speed_tlag9, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead9), speed_tlead9, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg), speed_avg, speed_new), # average speed
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag1), speed_avg_lag1, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead1), speed_avg_lead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag2), speed_avg_lag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead2), speed_avg_lead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag3), speed_avg_lag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead3), speed_avg_lead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag4), speed_avg_lag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead4), speed_avg_lead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag10), speed_avg_lag10, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead10), speed_avg_lead10, speed_new)
  )

# 53.9 percent missing values are not inputed 
nrow(full_df_I84_east_time_sub_2week_day_PP_90 %>% filter(is.na(speed_new)))/nrow(full_df_I84_east_time_sub_2week_day_PP_90)




# Add observations at adjacent stations ====
head(full_df_I84_east, 12)

full_df_I84_east_station <- full_df_I84_east %>%
  select(stationid, date, time_hour, time_min5, starttime, AM_PM, speed) %>%
  arrange(date, time_hour, time_min5, stationid)  %>%
  group_by(date, AM_PM)  %>%
  mutate(speed_slag1=lag(speed),
         speed_slag2=lag(speed, 5),
         speed_slag3=lag(speed, 9),
         speed_slead1=lead(speed),
         speed_slead2=lead(speed, 5),
         speed_slead3=lead(speed, 9)) %>%
  mutate(time_id=paste(date, time_hour, time_min5, sep="_"),
         speed_slag1=ifelse(stationid==1055, NA, speed_slag1),
         speed_slag2=ifelse(stationid==1055, NA, speed_slag2),
         speed_slag3=ifelse(stationid==1055, NA, speed_slag3),
         speed_slead1=ifelse(stationid==1127, NA, speed_slead1),
         speed_slead2=ifelse(stationid==1127, NA, speed_slead2),
         speed_slead3=ifelse(stationid==1127, NA, speed_slead3)
  ) %>%
  as.data.frame()

head(full_df_I84_east_station, 10)

full_df_I84_east_station[190:210, ]


full_df_I84_east_station_sub <- full_df_I84_east_station %>%
  filter(is.na(speed), !(time_id %in% speed_NA_df$time_id)) %>%
  mutate(speed_new=ifelse(!is.na(speed_slag1), speed_slag1, speed_slead1),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slag2), speed_slag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slead2), speed_slead2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slag3), speed_slag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slead3), speed_slead3, speed_new))

table(full_df_I84_east_station_sub$stationid)
head(full_df_I84_east_station_sub, 20)



summary(full_df_I84_east_station_sub)



full_df_I84_east_station %>% filter(date=="2005-09-17", time_hour==19, time_min5==15)

# 0.098 percent missing values are not inputed  
nrow(full_df_I84_east_station_sub %>% filter(is.na(speed_new)))/nrow(full_df_I84_east_station_sub)


# Input missing value with earlier/later and adjacent stationid observations ====
full_df_I84_east_time_station <- full_df_I84_east_time %>%
  left_join(full_df_I84_east_station)  %>%
  arrange(date, time_hour, time_min5, stationid) %>%
  filter(is.na(speed), !(time_id %in% speed_NA_df$time_id)) %>%
  mutate(speed_new=ifelse(!is.na(speed_tlag1), speed_tlag1, NA),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead1), speed_tlead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag2), speed_tlag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead2), speed_tlead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag3), speed_tlag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead3), speed_tlead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag4), speed_tlag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead4), speed_tlead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag5), speed_tlag5, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead5), speed_tlead5, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag6), speed_tlag6, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead6), speed_tlead6, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag7), speed_tlag7, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead7), speed_tlead7, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag8), speed_tlag8, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead8), speed_tlead8, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag9), speed_tlag9, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead9), speed_tlead9, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg), speed_avg, speed_new), # average speed
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag1), speed_avg_lag1, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead1), speed_avg_lead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag2), speed_avg_lag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead2), speed_avg_lead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag3), speed_avg_lag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead3), speed_avg_lead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag4), speed_avg_lag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead4), speed_avg_lead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag10), speed_avg_lag10, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead10), speed_avg_lead10, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slag1), speed_slag1, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slead1), speed_slead1, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slag2), speed_slag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slead2), speed_slead2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slag3), speed_slag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slead3), speed_slead3, speed_new)) %>%
  select(date, time_hour, time_min5, stationid, speed, speed_new)      


head(full_df_I84_east_comb, 10)
summary(full_df_I84_east_comb)
head(full_df_I84_east, 50)

# 0.0288 percent missing value are not inputed 
head(full_df_I84_east_time_station)
nrow(full_df_I84_east_time_station %>% filter(is.na(speed_new)))/nrow(full_df_I84_east_time_station)

full_df_I84_east_update <- full_df_I84_east_time %>%
  left_join(full_df_I84_east_station)  %>%
  arrange(date, time_hour, time_min5, stationid) %>%
  mutate(day_week=weekdays(date))  %>%
  filter(!(time_id %in% speed_NA_df$time_id), !(day_week %in% c("Saturday", "Sunday")))  %>%
  mutate(speed_new=ifelse(is.na(speed), speed_tlag1, speed),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag1), speed_tlag1, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead1), speed_tlead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag2), speed_tlag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead2), speed_tlead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag3), speed_tlag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead3), speed_tlead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag4), speed_tlag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead4), speed_tlead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag5), speed_tlag5, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead5), speed_tlead5, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag6), speed_tlag6, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead6), speed_tlead6, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag7), speed_tlag7, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead7), speed_tlead7, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag8), speed_tlag8, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead8), speed_tlead8, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlag9), speed_tlag9, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_tlead9), speed_tlead9, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg), speed_avg, speed_new), # average speed
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag1), speed_avg_lag1, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead1), speed_avg_lead1, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag2), speed_avg_lag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead2), speed_avg_lead2, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag3), speed_avg_lag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead3), speed_avg_lead3, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag4), speed_avg_lag4, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead4), speed_avg_lead4, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lag10), speed_avg_lag10, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_avg_lead10), speed_avg_lead10, speed_new),
         
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slag1), speed_slag1, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slead1), speed_slead1, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slag2), speed_slag2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slead2), speed_slead2, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slag3), speed_slag3, speed_new),
         speed_new=ifelse(is.na(speed_new) & !is.na(speed_slead3), speed_slead3, speed_new)) %>%
  select(date, time_hour, time_min5, stationid, speed, speed_new)    

nrow(full_df_I84_east_update %>% filter(is.na(speed_new)))/nrow(full_df_I84_east_update)

# Convert station-level data to route-level data 
full_df_I84_east_update_route <- full_df_I84_east_update %>%
  na.omit() %>%
  mutate(travel_time=ifelse(stationid==1055, (0.81/2)/speed_new, NA),
         travel_time=ifelse(stationid==1056, (0.81/2 + 1.27/2)/speed_new, travel_time),
         travel_time=ifelse(stationid==1057, (1.27/2 + 1.09/2)/speed_new, travel_time),
         travel_time=ifelse(stationid==1127, (1.09/2)/speed_new, travel_time)) %>%
  group_by(date, time_hour, time_min5) %>%
  summarise(speed_seg=(3.69-0.52)/sum(travel_time))
