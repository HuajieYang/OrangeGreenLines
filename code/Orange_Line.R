# This R script analyzes data of the Orange Line
# Settings
if (!require("pacman")) {install.packages("pacman"); library(pacman)}
p_load(dplyr, lubridate, ggplot2, stargazer, stringr)

# Load data ====
# PeMS data
Powell_E <- read.delim("data/Travel time and speed/SE Powell Blvd (E).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE)
Powell_W <- read.delim("data/Travel time and speed/SE Powell Blvd (W).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 

Holgate_E <- read.delim("data/Travel time and speed/SE Holgate Blvd (E).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 
Holgate_W <- read.delim("data/Travel time and speed/SE Holgate Blvd (W).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 

McLoughlin_N <- read.delim("data/Travel time and speed/SE McLoughlin Blvd (N).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 
McLoughlin_S <- read.delim("data/Travel time and speed/SE McLoughlin Blvd (S).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 

Milwaukie_17th_N <- read.delim("data/Travel time and speed/SE Milwaukie AveSE 17th (N).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 
Milwaukie_17th_S <- read.delim("data/Travel time and speed/SE Milwaukie AveSE 17th (S).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 

Division_E <- read.delim("data/Travel time and speed/SE Division St (E).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE) 
Division_W <- read.delim("data/Travel time and speed/SE Division St (W).txt", header = TRUE, sep="\t", stringsAsFactors=FALSE)

# Weather data 
# 1803651 location: PORTLAND INTERNATIONAL AIRPORT, OR US （45.5958	-122.6093）
wd <- read.csv("data/weather/1803651.csv", stringsAsFactors = F) %>% 
      select(date=DATE, PRCP, TMAX, TMIN, TAVG)  %>% 
      mutate(date=as.Date(date),
      TMAX90=ifelse(TMAX>90, 1,0))

# Reorganze data ====
Powell_E <- Powell_E %>% mutate(roadway="Powell_E")
Powell_W <- Powell_W %>% mutate(roadway="Powell_W")

Holgate_E <- Holgate_E %>% mutate(roadway="Holgate_E")
Holgate_W <- Holgate_W %>% mutate(roadway="Holgate_W")

McLoughlin_N <- McLoughlin_N %>% mutate(roadway="McLoughlin_N")
McLoughlin_S <- McLoughlin_S %>% mutate(roadway="McLoughlin_S")

Milwaukie_17th_N <- Milwaukie_17th_N %>% mutate(roadway="Milwaukie_17th_N")
Milwaukie_17th_S <- Milwaukie_17th_S %>% mutate(roadway="Milwaukie_17th_S")

Division_E <- Division_E %>% mutate(roadway="Division_E")
Division_W <- Division_W %>% mutate(roadway="Division_W")

data_combined_5 <- rbind(Powell_E, Powell_W, Holgate_E, Holgate_W, McLoughlin_N, McLoughlin_S, Milwaukie_17th_N, Milwaukie_17th_S, Division_E, Division_W)

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
                                        "control", "experimental")) 

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
save(data_combined_15_wdpeak, file="output/intermediate/data_combined_15_wdpeak.RData")

data_combined_day_wdpeak <- data_combined_day %>%
                           filter(AM_PM != "NonPeak", !(day_week %in% c("Saturday", "Sunday")))  
save(data_combined_day_wdpeak, file="output/intermediate/data_combined_day_wdpeak.RData")

# 15-minute interval #####
# T-test 
# Freeway 
# AM Peak period
# AM east CBD: control
t.test(data_combined_15_wdpeak %>% filter(roadway=="Powell_E", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway=="Powell_E", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# AM south CBD: experiment 
t.test(data_combined_15_wdpeak %>% filter(roadway=="McLoughlin_S", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway=="McLoughlin_S", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# AM west CBD: control
t.test(data_combined_15_wdpeak %>% filter(roadway=="Powell_W", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway=="Powell_W", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# AM north CBD: experiment 
t.test(data_combined_15_wdpeak %>% filter(roadway=="McLoughlin_N", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway=="McLoughlin_N", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# PM Peak period
# PM east CBD: control
t.test(data_combined_15_wdpeak %>% filter(roadway=="Powell_E", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway=="Powell_E", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# PM south CBD: experiment 
t.test(data_combined_15_wdpeak %>% filter(roadway=="McLoughlin_S", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway=="McLoughlin_S", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# PM west CBD: control
t.test(data_combined_15_wdpeak %>% filter(roadway=="Powell_W", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway=="Powell_W", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# PM north CBD: experiment 
t.test(data_combined_15_wdpeak %>% filter(roadway=="McLoughlin_N", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway=="McLoughlin_N", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())


# Arterials 
# AM Peak period
# AM outward CBD: experimental
t.test(data_combined_15_wdpeak %>% filter(roadway=="Milwaukie_17th_S", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway=="Milwaukie_17th_S", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# AM outward CBD: control 
t.test(data_combined_15_wdpeak %>% filter(roadway %in% c("Division_E","Holgate_E"), AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway %in% c("Division_E","Holgate_E"), AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())


# AM inward CBD: experimental
t.test(data_combined_15_wdpeak %>% filter(roadway=="Milwaukie_17th_N", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway=="Milwaukie_17th_N", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# AM inward CBD: control 
t.test(data_combined_15_wdpeak %>% filter(roadway %in% c("Division_W","Holgate_W"), AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway %in% c("Division_W","Holgate_W"), AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())


# PM Peak period
# PM outward CBD: experimental
t.test(data_combined_15_wdpeak %>% filter(roadway=="Milwaukie_17th_S", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway=="Milwaukie_17th_S", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# PM outward CBD: control 
t.test(data_combined_15_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E"), AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway %in% c("Division_E", "Holgate_E"), AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# PM inward CBD: experimental
t.test(data_combined_15_wdpeak %>% filter(roadway=="Milwaukie_17th_N", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway=="Milwaukie_17th_N", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())


# PM inward CBD: control 
t.test(data_combined_15_wdpeak %>% filter(roadway %in% c("Division_W", "Holgate_W"), AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_15_wdpeak %>% filter(roadway %in% c("Division_W", "Holgate_W"), AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())


# DID models 
# AM Peak outward CBD
# # 15-minute interval 
# m_amo_15 <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
#                 data=data_combined_15_wdpeak %>% 
#                      filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="AM"))
# 
# # AM Peak inward CBD
# m_ami_15 <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
#                 data=data_combined_15_wdpeak %>% 
#                   filter(roadway %in% c("McLoughlin_N", "Powell_W"), AM_PM=="AM"))
# 
# 
# # PM Peak outward CBD
# m_pmo_15 <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
#                 data=data_combined_15_wdpeak %>% 
#                   filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="PM"))
# 
# # PM Peak inward CBD
# m_pmi_15 <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
#                 data=data_combined_15_wdpeak %>% 
#                   filter(roadway %in% c("McLoughlin_N", "Powell_W"), AM_PM=="PM"))
# 
# stargazer(m_amo_15, m_ami_15, m_pmo_15, m_pmi_15, 
#           column.labels = c("AM Outward", "AM Inward", "PM Outward",  "PM Inward"),
#           dep.var.labels=c("Travel Speed"),
#           covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
#                              "Time Period × Group"),
#           type="text", out="output/intermediate/ol_fwy_15_summary.htm")




# daily summary analysis #### 
# T-test 
# Freeway 
# AM Peak period
# AM outward CBD: control
t.test(data_combined_day_wdpeak %>% filter(roadway=="Powell_E", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Powell_E", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# AM outward CBD: experiment 
t.test(data_combined_day_wdpeak %>% filter(roadway=="McLoughlin_S", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="McLoughlin_S", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# AM inward CBD: control
t.test(data_combined_day_wdpeak %>% filter(roadway=="Powell_W", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Powell_W", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# AM inward CBD: experiment 
t.test(data_combined_day_wdpeak %>% filter(roadway=="McLoughlin_N", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="McLoughlin_N", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# PM Peak period
# PM outward CBD: control
t.test(data_combined_day_wdpeak %>% filter(roadway=="Powell_E", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Powell_E", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# PM outward CBD: experiment 
t.test(data_combined_day_wdpeak %>% filter(roadway=="McLoughlin_S", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="McLoughlin_S", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# PM inward CBD: control
t.test(data_combined_day_wdpeak %>% filter(roadway=="Powell_W", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Powell_W", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# PM inward CBD: experiment 
t.test(data_combined_day_wdpeak %>% filter(roadway=="McLoughlin_N", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="McLoughlin_N", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# Arterials 
# AM Peak period
# AM outward CBD: experimental
t.test(data_combined_day_wdpeak %>% filter(roadway=="Milwaukie_17th_S", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Milwaukie_17th_S", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# AM outward CBD: control 
t.test(data_combined_day_wdpeak %>% filter(roadway=="Division_E", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Division_E", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

t.test(data_combined_day_wdpeak %>% filter(roadway=="Holgate_E", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Holgate_E", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())


# AM inward CBD: experimental
t.test(data_combined_day_wdpeak %>% filter(roadway=="Milwaukie_17th_N", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Milwaukie_17th_N", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())


# AM inward CBD: control 
t.test(data_combined_day_wdpeak %>% filter(roadway=="Division_W", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Division_W", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

t.test(data_combined_day_wdpeak %>% filter(roadway=="Holgate_W", AM_PM=="AM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Holgate_W", AM_PM=="AM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())


# PM Peak period
# PM outward CBD: experimental
t.test(data_combined_day_wdpeak %>% filter(roadway=="Milwaukie_17th_S", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Milwaukie_17th_S", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# PM outward CBD: control 
t.test(data_combined_day_wdpeak %>% filter(roadway=="Division_E", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Division_E", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

t.test(data_combined_day_wdpeak %>% filter(roadway=="Holgate_E", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Holgate_E", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())


# PM inward CBD: experimental
t.test(data_combined_day_wdpeak %>% filter(roadway=="Milwaukie_17th_N", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Milwaukie_17th_N", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())


# PM inward CBD: control 
t.test(data_combined_day_wdpeak %>% filter(roadway=="Division_W", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Division_W", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

t.test(data_combined_day_wdpeak %>% filter(roadway=="Holgate_W", AM_PM=="PM", before_after==0) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector(),
       data_combined_day_wdpeak %>% filter(roadway=="Holgate_W", AM_PM=="PM", before_after==1) %>% select(AvgSpeed) %>% as.data.frame() %>% as.vector())

# Daily summary roadway
# freeway 
# AM outward CBD
sp_amo_day_fwy <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group,
                     data=data_combined_day_wdpeak %>% 
                          filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="AM"))

sd_amo_day_fwy <- lm(SDSpeed ~ before_after + ec_group + before_after*ec_group,
                     data=data_combined_day_wdpeak %>% 
                       filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="AM"))

# AM Peak inward CBD
sp_ami_day_fwy <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                     filter(roadway %in% c("McLoughlin_N", "Powell_W"), AM_PM=="AM"))

sd_ami_day_fwy <- lm(SDSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                       filter(roadway %in% c("McLoughlin_N", "Powell_W"), AM_PM=="AM"))

# PM Peak outward CBD
sp_pmo_day_fwy <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                     filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="PM"))

sd_pmo_day_fwy <- lm(SDSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                       filter(roadway %in% c("McLoughlin_S", "Powell_E"), AM_PM=="PM"))

# PM Peak inward CBD
sp_pmi_day_fwy <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
                    data=data_combined_day_wdpeak %>% 
                    filter(roadway %in% c("McLoughlin_N", "Powell_W"), AM_PM=="PM"))

sd_pmi_day_fwy <- lm(SDSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                       filter(roadway %in% c("McLoughlin_N", "Powell_W"), AM_PM=="PM"))

stargazer(sp_amo_day_fwy, sp_ami_day_fwy, sp_pmo_day_fwy, sp_pmi_day_fwy, 
          column.labels = c("AM Outward", "AM Inward", "PM Outward",  "PM Inward"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "Time Period × Group"),
          type="text", out="output/intermediate/ol_fwy_day_sp.htm")

stargazer(sd_amo_day_fwy, sd_ami_day_fwy, sd_pmo_day_fwy, sd_pmi_day_fwy, 
          column.labels = c("AM Outward", "AM Inward", "PM Outward",  "PM Inward"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "Time Period × Group"),
          type="text", out="output/intermediate/ol_fwy_day_sd.htm")

# arterial 
# AM outward CBD
sp_amo_day_atr <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                       filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="AM"))

sd_amo_day_atr <- lm(SDSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                       filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="AM"))

# AM inward CBD
sp_ami_day_atr <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                       filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="AM"))

sd_ami_day_atr <- lm(SDSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                       filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="AM"))

# PM Peak outward CBD
sp_pmo_day_atr <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                       filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="PM"))

sd_pmo_day_atr <- lm(SDSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                       filter(roadway %in% c("Division_E", "Holgate_E", "Milwaukie_17th_S"), AM_PM=="PM"))

# PM Peak inward CBD
sp_pmi_day_atr <- lm(AvgSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                       filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="PM"))

sd_pmi_day_atr <- lm(SDSpeed ~ before_after + ec_group + before_after*ec_group, 
                     data=data_combined_day_wdpeak %>% 
                       filter(roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N"), AM_PM=="PM"))

stargazer(sp_amo_day_atr, sp_ami_day_atr, sp_pmo_day_atr, sp_pmi_day_atr, 
          column.labels = c("AM Outward", "AM Inward", "PM Outward",  "PM Inward"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "Time Period × Group"),
          type="text", out="output/intermediate/ol_art_day_sp.htm")

stargazer(sd_amo_day_atr, sd_ami_day_atr, sd_pmo_day_atr, sd_pmi_day_atr, 
          column.labels = c("AM Outward", "AM Inward", "PM Outward",  "PM Inward"),
          dep.var.labels=c("Travel Speed"),
          covariate.labels=c("Time Period (after = 1)", "Group (experimental group = 1)", 
                             "Time Period × Group"),
          type="text", out="output/intermediate/ol_art_day_sd.htm")


# PLots =====
PAM_I5_I84_I205  <- ggplot(data=data_day_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(1, 3, 8)), 
                           aes(x=date, y=speed, group=Roadways)) +
  geom_point(aes(colour=Roadways)) + geom_smooth(aes(linetype=Roadways), colour="yellow", size=0.75) + 
  geom_vline(xintercept=ymd("2009-09-12"), size=0.5) +  ylim(20, 70) + 
  labs( x="Date", y="Average speed (mph)") +
  theme(plot.title = element_text(hjust = 0.5))

PI5_0414_south <-  ggplot(data=data_day_0414_green_wdpeak %>% filter(highwayid==2), aes(x=date, y=speed, group=AM_PM)) +
  geom_point(aes(colour=AM_PM)) + geom_smooth() + geom_vline(xintercept=ymd("2009-09-12")) +  ylim(20, 70) + 
  labs(title="Southbound average speed of I-5 highway during peak periods", 
       x="Date", y="Average speed (mph)", colour="Peak period") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "output/plots/04-14/PI5_0414_south.png", PI5_0414_south, width = 8, height = 4)

ls()
head(data_combined_day)


ggplot(data=data_day_0414_green_wdpeak %>% filter(AM_PM=="AM", highwayid %in% c(1, 3, 8)), 
       aes(x=date, y=speed, group=Roadways)) +
  geom_point(aes(colour=Roadways)) + geom_smooth(aes(linetype=Roadways), colour="yellow", size=0.75) + 
  geom_vline(xintercept=ymd("2009-09-12"), size=0.5) +  ylim(20, 70) + 
  labs( x="Date", y="Average speed (mph)") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data=data_combined_day_wdpeak %>% filter(AM_PM=="AM", roadway %in% c("Division_W", "Holgate_W", "Milwaukie_17th_N")), 
       aes(x=date, y=AvgSpeed, group=roadway)) +
  geom_point(aes(colour=roadway)) + geom_smooth(aes(linetype=roadway), colour="yellow", size=0.75) + 
  geom_vline(xintercept=ymd("2015-09-12"), size=0.5) +  ylim(20, 25) + 
  labs( x="Date", y="Average speed (mph)") +
  theme(plot.title = element_text(hjust = 0.5))

table(data_combined_day_wdpeak$roadway)
head(data_combined_day_wdpeak)
ls()



