# Station analysis ====

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

write.csv(stations_I84, file="output/intermediate/stations_I84.csv",row.names=FALSE)

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

# Check 
# Previous filter conditions may not work well; it'd better use GIS and CSV file
# After checking GIS map, previous station I-205 filter work well.

# The upstream and downstream stations are related 
test <- stations_meta %>%
        filter(stationid %in% c(1143, 1051, 1052, 1053, 1054, 1098), highwayid==4) # , start_date=="2004-01-01 00:00:00-08"

# Test long-term effect 
load("output/intermediate/data_combined_day_wdpeak.RData")
head(data_combined_day_wdpeak)


# Get historical mean speed or speed limit/early morning speed 
# Speed limit may change over time 
summary(data_combined_5)


test <-  data_combined_5 %>%
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
nrow(test)
nrow

table(data_combined_5$Avg.Travel.Time..mins.)

test1 <- test %>%
         filter(is.na(AvgSpeed)) %>%
         mutate(id=paste(X5.Minutes, roadway, sep="_"))

table(test1$Avg.Travel.Time..mins.)

head(test1)
nrow(test1)
head(test2)
table()

test2 <- data_combined_5 %>%
         filter(Avg.Travel.Time..mins.=="Closed")  %>%
         mutate(id=paste(X5.Minutes, roadway, sep="_")) %>%
         filter(!(id %in% test1$id))

nrow(test2)
table()

table(test2$id)


test %>%
  filter(X5.Minutes=="01/02/2019 07:35")

table(data_combined_5$time_hour)

# Calculate early morning (2:00 am - 3:59 am) speed on weekdays 
em <- data_combined_5 %>%
        filter(time_hour %in% c(2, 3), !(day_week %in% c("Saturday", "Sunday")))  %>%
        group_by(roadway, date)  %>%
        summarise(em_TTime=mean(TTime, na.rm=TRUE),
                  em_AvgSpeed=mean(AvgSpeed, na.rm=TRUE)) %>%
        ungroup() 

em_summary <- em_sp %>%
              group_by(roadway)  %>%
              summarise(em_AvgSpeed_min=min(em_AvgSpeed, na.rm = TRUE),
                        em_AvgSpeed_median=median(em_AvgSpeed, na.rm = TRUE),
                        em_AvgSpeed_mean=mean(em_AvgSpeed, na.rm = TRUE),
                        em_AvgSpeed_max=max(em_AvgSpeed, na.rm = TRUE),
                        em_AvgSpeed_sd=sd(em_AvgSpeed, na.rm = TRUE)
                        )

# Calculate historical average speed
his_avg_wdpeak <- data_combined_5 %>%
                  filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday"))) %>%
                  group_by(roadway) %>%
                  summarise(his_avg_wdpeak_sp=mean(AvgSpeed, na.rm=TRUE),
                               his_avg_wdpeak_tt=mean(TTime, na.rm=TRUE))

head(data_combined_day_wdpeak)

str(data_combined_day_wdpeak)


summary(data_combined_day_wdpeak)


# add year or month index 
# How to add year dummy in the DID model:
# Reference: https://stats.stackexchange.com/questions/76058/specifying-a-difference-in-differences-model-with-multiple-time-periods
head(data_combined_day_wdpeak)
str(data_combined_day_wdpeak)

test <- data_combined_day_wdpeak %>% 
        filter(roadway=="Division_E")

test1 <- test %>%
         mutate(month_num=month(date),
                year_num=year(date),
                year_1st=ifelse(date>="2015-09-12"&date<="2016-09-11", 1, 0),
                year_2nd=ifelse(date>="2016-09-12"&date<="2017-09-11", 1, 0),
                year_3rd=ifelse(date>="2017-09-12"&date<="2018-09-11", 1, 0)) %>%
         as.data.frame()  %>%
         select(roadway, date, AM_PM, year_num, month_num, year_1st, year_2nd, year_3rd)
test1 %>% filter(year_num==2016)
test1[1:500,]
head(test1)
tail(test1, 10)
nrow(test1)
head(test1)
max(data_combined_day_wdpeak$date)
min(data_combined_day_wdpeak$date)

test <- data_combined_day_wdpeak %>%
        filter(DDV_em_avg>0) %>%
        select(roadway, date, AM_PM, AvgSpeed, em_AvgSpeed_mean, his_avg_wdpeak_sp, DDV_his_avg, DDV_em_avg)


# Add year dummy variables in DID models 
sp_amo_day_ma <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group,
                    data=data_combined_day_wdpeak %>% 
                      filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="AM"))

summary(sp_amo_day_ma)

# Add year dummy variable 
str(data_combined_day_wdpeak)

data_combined_day_wdpeak <- data_combined_day_wdpeak %>%
                            mutate(ec_group_num=ifelse(ec_group=="experimental", 1, 0)
                                   )




ddv_ami_day_ma <- lm(DDV_his_em_avg ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% filter(roadway %in% c("McLoughlin_N", "Powell_W"), AM_PM=="AM"))

summary(ddv_ami_day_ma)


test <- data_combined_day_wdpeak %>% 
        filter(roadway %in% c("McLoughlin_N", "Powell_W"), AM_PM=="AM")

str(test)
summary(test)


sp_amo_day_ma <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group,
                    data=data_combined_day_wdpeak %>% filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="AM"))

sp_amo_day_ma_yd <- lm(AvgSpeed ~  before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group,
                       data=data_combined_day_wdpeak %>% filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="AM"))

sp_amo_day_ma_yd_int <- lm(AvgSpeed ~  before_after + ec_group + year_2nd + year_3rd + before_after*ec_group + ec_group*year_2nd + ec_group*year_3rd, 
                           data=data_combined_day_wdpeak %>% filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="AM"))


stargazer(sp_amo_day_ma, sp_amo_day_ma_yd, sp_amo_day_ma_yd_int,
          column.labels = c("Base model", "year", "year&interaction term"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "2nd year post-treatment", "3rd year post-treatment", "Time Period × Group",
                             "Group × 2nd year", "Group × 3rd year"),
          type="text", title="AM outbound from CBD speed models")



sp_amo_day_lr <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="AM"))

sp_amo_day_lr_yd <- lm(AvgSpeed ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group, 
                    data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="AM"))

sp_amo_day_lr_yd_int <- lm(AvgSpeed ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group + ec_group*year_2nd + ec_group*year_3rd, 
                          data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="AM"))

stargazer(sp_amo_day_lr, sp_amo_day_lr_yd, sp_amo_day_lr_yd_int,
          column.labels = c("Base model", "year", "year&interaction term"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "2nd year post-treatment", "3rd year post-treatment", "Time Period × Group",
                             "Group × 2nd year", "Group × 3rd year"),
          type="text", title="AM outbound from CBD day-to-day variation models")


sp_ami_day_atr <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="AM"))

sp_ami_day_atr_yd <- lm(AvgSpeed ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group, 
                        data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="AM"))

sp_ami_day_atr_yd_int <- lm(AvgSpeed ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group + ec_group*year_2nd + ec_group*year_3rd, 
                            data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="AM"))

stargazer(sp_ami_day_atr, sp_ami_day_atr_yd, sp_ami_day_atr_yd_int,
          column.labels = c("Base model", "year", "year&interaction term"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "2nd year post-treatment", "3rd year post-treatment", "Time Period × Group",
                             "Group × 2nd year", "Group × 3rd year"),
          type="text", title="AM inbound CBD CBD day-to-day variation models")


sp_pmo_day_lr <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="PM"))

sp_pmo_day_lr_yd <- lm(AvgSpeed ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="PM"))

sp_pmo_day_lr_yd_int <- lm(AvgSpeed ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group + ec_group*year_2nd + ec_group*year_3rd, 
                     data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="PM"))

stargazer(sp_pmo_day_lr, sp_pmo_day_lr_yd, sp_pmo_day_lr_yd_int,
          column.labels = c("Base model", "year", "year&interaction term"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "2nd year post-treatment", "3rd year post-treatment", "Time Period × Group",
                             "Group × 2nd year", "Group × 3rd year"),
          type="text", title="PM outbound from CBD day-to-day variation models")

sp_pmi_day_lr <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="PM"))

sp_pmi_day_lr_yd <- lm(AvgSpeed ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group, 
                    data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="PM"))

sp_pmi_day_lr_yd_int <- lm(AvgSpeed ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group + ec_group*year_2nd + ec_group*year_3rd, 
                          data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="PM"))

stargazer(sp_pmi_day_lr, sp_pmi_day_lr_yd, sp_pmi_day_lr_yd_int,
          column.labels = c("Base model", "year", "year&interaction term"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "2nd year post-treatment", "3rd year post-treatment", "Time Period × Group",
                             "Group × 2nd year", "Group × 3rd year"),
          type="text", title="PM inbound to CBD day-to-day variation models")


ddv_amo_day_lr <- lm(DDV_his_em_avg ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="AM"))

ddv_amo_day_lr_yd <- lm(DDV_his_em_avg ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="AM"))

ddv_amo_day_lr_yd_int <- lm(DDV_his_em_avg ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group + ec_group*year_2nd + ec_group*year_3rd, 
                            data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="AM"))

stargazer(ddv_amo_day_lr, ddv_amo_day_lr_yd, ddv_amo_day_lr_yd_int,
          column.labels = c("Base model", "year", "year&interaction term"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "2nd year post-treatment", "3rd year post-treatment", "Time Period × Group",
                             "Group × 2nd year", "Group × 3rd year"),
          type="text", title="AM outbound from CBD day-to-day variation models")



ddv_ami_day_lr <- lm(DDV_his_em_avg ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="AM"))

ddv_ami_day_lr_yd <- lm(DDV_his_em_avg ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group, 
                      data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="AM"))

ddv_ami_day_lr_yd_int <- lm(DDV_his_em_avg ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group + ec_group*year_2nd + ec_group*year_3rd,
                            data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="AM"))

stargazer(ddv_ami_day_lr, ddv_ami_day_lr_yd, ddv_ami_day_lr_yd_int,
          column.labels = c("Base model", "year", "year&interaction term"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "2nd year post-treatment", "3rd year post-treatment", "Time Period × Group",
                             "Group × 2nd year", "Group × 3rd year"),
          type="text", title="AM inbound to CBD day-to-day variation models")


ddv_pmo_day_lr <- lm(DDV_his_em_avg ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="PM"))

ddv_pmo_day_lr_yd <- lm(DDV_his_em_avg ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group, 
                    data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="PM"))

ddv_pmo_day_lr_yd_int <- lm(DDV_his_em_avg ~ before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group + ec_group*year_2nd + ec_group*year_3rd,
                    data=data_combined_day_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="PM"))

stargazer(ddv_pmo_day_lr, ddv_pmo_day_lr_yd, ddv_pmo_day_lr_yd_int,
          column.labels = c("Base model", "year", "year&interaction term"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "2nd year post-treatment", "3rd year post-treatment", "Time Period × Group",
                             "Group × 2nd year", "Group × 3rd year"),
          type="text", title="PM outbound to CBD day-to-day variation models")



sp_amo_day_ma <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group,
                    data=data_combined_day_wdpeak %>% filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="AM"))

sp_amo_day_ma_2yd <- lm(AvgSpeed ~  before_after + ec_group +  year_2nd + year_3rd + before_after*ec_group,
                       data=data_combined_day_wdpeak %>% filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="AM"))

sp_amo_day_ma_2yd_int <- lm(AvgSpeed ~  before_after + ec_group + year_2nd + year_3rd + before_after*ec_group + ec_group*year_2nd + ec_group*year_3rd, 
                           data=data_combined_day_wdpeak %>% filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="AM"))

# Reference: https://stats.stackexchange.com/questions/76058/specifying-a-difference-in-differences-model-with-multiple-time-periods
sp_amo_day_ma_3yd <- lm(AvgSpeed ~  ec_group + year_1st + year_2nd + year_3rd + before_after*ec_group,
                        data=data_combined_day_wdpeak %>% filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="AM"))

# Reference: https://stats.stackexchange.com/questions/225145/difference-in-difference-with-multiple-period-pre-during-and-post-treatment
sp_amo_day_ma_3yd_int <- lm(AvgSpeed ~  ec_group + year_1st + year_2nd + year_3rd + ec_group*year_1st + ec_group*year_2nd + ec_group*year_3rd, 
                            data=data_combined_day_wdpeak %>% filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="AM"))



stargazer(sp_amo_day_ma, sp_amo_day_ma_2yd, sp_amo_day_ma_2yd_int, sp_amo_day_ma_3yd, sp_amo_day_ma_3yd_int,
          dep.var.labels=c("Travel Speed"),
          type="text", title="AM outbound from CBD speed models", keep.stat=c("n", "adj.rsq", "rsq", "ser"))


# Plot of regression results 

# Plot base did model 
# major artrial speed base models: sp_amo_day_ma, sp_ami_day_ma, sp_pmo_day_ma, sp_pmi_day_ma
stargazer(sp_amo_day_ma, sp_ami_day_ma, sp_pmo_day_ma, sp_pmi_day_ma, 
          column.labels = c("AM Outbound", "AM Inbound", "PM Outbound",  "PM Inbound"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", "Time Period × Group"),
          type="text", title="Speed base models")  


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


# Major arterial day-to-day variation base model 
stargazer(ddv_amo_day_ma, ddv_ami_day_ma, ddv_pmo_day_ma, ddv_pmi_day_ma, 
          column.labels = c("AM Outbound", "AM Inbound", "PM Outbound",  "PM Inbound"),
          dep.var.labels=c("Day-to-day variation"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", "Time Period × Group"),
          type="text", title="Day-to-day variation in weekday peak periods speed base models")  











# Plot multiple years 
test <- summary(sp_amo_day_ma_3yd_int)$coefficient[, 1]
class(test)

coefficient.mx <- as.matrix(summary(sp_amo_day_ma_3yd_int)$coefficient[, 1])

year.mx <- matrix(1:24, nrow=3)
year.mx %*% coefficient.mx




# Plot issue ====
plot(1, type="n", xlab="before/after period", ylab="fte", xaxt="n",
     xlim=c(-0.01, 1.01), ylim=c(18, 24))

df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("D0.5", "D1", "D2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))

# Change line types by groups (supp)
ggplot(df2, aes(x=dose, y=len, group=supp)) +
        geom_line(aes(linetype=supp))+
        geom_point()


























