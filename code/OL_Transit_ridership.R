# Load data ====
# Transit ridership data
# Ridership dataset does not provide streetcar ridership 
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
                       rt_dir_stopdesc=paste(ROUTE_NUMBER, DIRECTION, PUBLIC_LOCATION_DESCRIPTION, sep="_"),
                       hm_char=substr(STOP_TIME, 1, 1),
                       AM_PM=substr(STOP_TIME, nchar(STOP_TIME)-1, nchar(STOP_TIME)),
                       AM_Peak=ifelse((hm_char %in% c(6, 7, 8, 9))&AM_PM=="AM", "AM Peak", "Non AM Peak"),
                       PM_Peak=ifelse((hm_char %in% c(4, 5, 6, 7))&AM_PM=="PM", "PM Peak", "Non PM Peak"),
                       AM_PM_Peak=ifelse(AM_Peak=="AM Peak"|PM_Peak=="PM Peak", "Peak Periods", "Non Peak Periods")
                )  %>%
                left_join(month_df, by=c("MONTH"="month_char")) %>%
                mutate(date=ymd(paste(YEAR, month_num, DAY, sep="-")))

save(ridership_w, file="output/intermediate/ridership_w.RData")

# Load  experimental and control stops GIS bus stop data 
control_busstops <-  read.dbf("data/GIS/Control_Corridor_BusStops.dbf", as.is=TRUE) 
experimental_busstops <- read.dbf("data/GIS/Experimental_Corridor_BusStops.dbf", as.is=TRUE) 

# RTE 200 is the MAX Green Line
control_busstops <- control_busstops %>%
                    mutate(rt_dir_stopdesc=paste(RTE, DIR, LOCATION, sep="_"))

# 194, 195, 196 are streetcar; no streetcar ridership in ridership dataset 
# RTE 290 is the MAX Orange Line 
experimental_busstops <- experimental_busstops %>%
                         filter(!(RTE %in% c("194", "195", "196"))) %>% 
                         mutate(rt_dir_stopdesc=paste(RTE, DIR, LOCATION, sep="_"))

# extract ridership of control stops from ridership data 
ridership_w_control <- ridership_w %>%
                          filter(rt_dir_stopdesc %in% control_busstops$rt_dir_stopdesc)

# all rt_dir_stopdesc are available from ridership dataset 
# sort(unique(ridership_w_rt_control$rt_dir_stopdesc))==sort(unique(control_busstops$rt_dir_stopdesc)) 

ridership_w_experimental <- ridership_w %>%
                               filter(rt_dir_stopdesc %in% experimental_busstops$rt_dir_stopdesc)

# sort(unique(ridership_w_rt_experimental$rt_dir_stopdesc))==sort(unique(experimental_busstops$rt_dir_stopdesc))
# There is one stops that do not appear in ridership dataset: SE McLoughlin & Park Ave is not in ridership dataset 
# setdiff(sort(unique(experimental_busstops$rt_dir_stopdesc)), sort(unique(ridership_w_rt_experimental$rt_dir_stopdesc)))

# Analyze the transit ridership/Comparisons 
# average total weekday boardings at all bus stops and rail stations within the experimental and control corridors====

# Route 2  provides ridership from 2018-09-02 to present
# 291-Orange Night Bus provides ridership from 2015-09-13 to present 
# 290 MAX Orange Line provides ridership from 2015-09-13 to present 
ridership_w_experimental_summary1 <- ridership_w_experimental %>%
  filter(date >= "2012-09-02") %>% 
  group_by(ROUTE_NUMBER, date) %>%
  summarise(ONES_SUM=sum(ONS, na.rm = TRUE)) %>%
  ungroup()

ridership_w_experimental_summary2 <- ridership_w_experimental %>%
  filter(!(ROUTE_NUMBER %in% c(2)), date >= "2012-09-02") %>% 
  group_by(date) %>%
  summarise(ONES_SUM=sum(ONS, na.rm = TRUE)) %>%
  ungroup()

ggplot(ridership_w_experimental_summary2, aes(date, ONES_SUM)) + geom_smooth() + geom_point()  + 
  ggtitle("average total weekday boardings at all bus stops and \n rail stations within the experimental corridor")

ridership_w_experimental_summary3 <- ridership_w_experimental %>%
  filter(ROUTE_NUMBER %in% c(290)) %>% 
  group_by(ROUTE_NUMBER, date) %>%
  summarise(ONES_SUM=sum(ONS, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ROUTE_NUMBER=as.factor(ROUTE_NUMBER))

# The ridership of MAX Orange Line is large 
ggplot(ridership_w_experimental_summary3, aes(date, ONES_SUM)) + geom_smooth() + geom_point()  + ylim (4000, 7000) + 
  ggtitle("average total weekday boardings at the MAX Orange Line stations within the experimental corridor")


# Route 2 only provide ridership from 2018-09-02 to present
# Route 73 only provide ridership from 2016-09-04 to present 
# Ridership of RTE 200 MAX Green Line is from 2012-09-02 to present 
ridership_w_control_summary1 <- ridership_w_control %>%
  filter(date >= "2012-09-02") %>%
  group_by(ROUTE_NUMBER, date) %>%
  summarise(ONES_SUM=sum(ONS, na.rm = TRUE)) %>%
  ungroup() 


ridership_w_control_summary2 <- ridership_w_control %>%
  filter(!(ROUTE_NUMBER %in% c(2, 73)), date >= "2012-09-02") %>%
  group_by(date) %>%
  summarise(ONES_SUM=sum(ONS, na.rm = TRUE)) %>%
  ungroup() 

ggplot(ridership_w_control_summary2, aes(date, ONES_SUM)) + geom_point() + geom_smooth() + ylim (11000, 16000) +
  ggtitle("average total weekday boardings at all bus stops and \n rail stations within the control corridor")

# compared changes in average total weekday ridership of east–west transit lines within the study area.
# These are lines that are alternatives to the Expo Line within the experimental corridor
# ridership (i.e., total boardings along entire routes; both directions)
# Routes 
control_RT <- sort(unique(ridership_w_rt_control_summary$ROUTE_NUMBER))
experimental_RT <- sort(unique(ridership_w_rt_experimental_summary$ROUTE_NUMBER))

# Experimental corridor 
# East-west routes: 9
# North-south routes: 30, 32, 33, 34, 70, 75, 99, 291
# Route 17: half north-south and half east-west
# Route 19: 2/3 east-west and 1/3 north-south 
# Routh 29: short length 
# Route 66 west part/short 
# Route 99 short east west; 
# Route 290 MAX Orange Line

# Routes 19, 30, 70, 291 cross the screenline  
ridership_experimental  <- ridership_w %>% 
  filter(ROUTE_NUMBER %in% c(19, 30, 70, 290, 291), date >= "2012-09-02")  %>% 
  group_by(date)  %>% 
  summarise(ONES_SUM=sum(ONS, na.rm = TRUE)) %>%
  ungroup() 

ggplot(ridership_experimental, aes(date, ONES_SUM)) + geom_point() + geom_smooth() +
  ggtitle("average total weekday ridership of east–west \n transit lines within the experimental corridor")


# Control corridor  
# East-west routes: 9, 10, 14, 17, 
# North-south: 71, 72, 75, 
# Route 66 west part/short 
# 200 MAX Green Line 

# East-west route 9, 17 cross the screenliens 
ridership_control  <- ridership_w %>% 
  filter(ROUTE_NUMBER %in% c(9, 17), date >= "2012-09-02")  %>% 
  group_by(date)  %>% 
  summarise(ONES_SUM=sum(ONS, na.rm = TRUE)) %>%
  ungroup() 

ggplot(ridership_control, aes(date, ONES_SUM)) + geom_point() + geom_smooth() + ylim (11000, 20000) +
  ggtitle("average total weekday ridership of east–west \n transit lines within the control corridor")

# Weekday peak-period (AM and PM peaks combined) bidirectional person throughput by east–west Metro bus and rail lines across the same screenlines, before and after.

# Experimental ridership for peak period    
ridership_experimental_peak  <- ridership_w %>% 
  filter(ROUTE_NUMBER %in% c(19, 30, 70, 290, 291), date >= "2012-09-02", AM_PM_Peak=="Peak Periods")  %>% 
  group_by(date)  %>% 
  summarise(ONES_SUM=sum(ONS, na.rm = TRUE)) %>%
  ungroup() 

ggplot(ridership_experimental_peak, aes(date, ONES_SUM)) + geom_point() + geom_smooth() + 
  ggtitle("Weekday peak-period (AM and PM peaks combined) bidirectional person throughput by \n east–west Metro bus and rail lines across the experimental corridor")

# Control ridership for peak period    
ridership_control_peak  <- ridership_w %>% 
  filter(ROUTE_NUMBER %in% c(9, 17), date >= "2012-09-02", AM_PM_Peak=="Peak Periods")  %>% 
  group_by(date)  %>% 
  summarise(ONES_SUM=sum(ONS, na.rm = TRUE)) %>%
  ungroup() 

ggplot(ridership_control_peak, aes(date, ONES_SUM)) + geom_point() + geom_smooth() + 
  ggtitle("Weekday peak-period (AM and PM peaks combined) bidirectional person throughput by \n east–west Metro bus and rail lines across the control corridor")

# Boardings and alightings at specific bus stops within a quarter-mile walking radius of Expo Line stations.
bus_stops_quarter_mile <-  read.dbf("data/GIS/Bus_stops_quarter_mile_withoutMAX_SC.dbf", as.is=TRUE) 

bus_stops_quarter_mile <- bus_stops_quarter_mile %>%
  mutate(rt_dir_stopdesc=paste(RTE, DIR, LOCATION, sep="_"))  %>%
  select(RTE, DIR, LOCATION,  rt_dir_stopdesc, join_STATI)

ridership_bus_stops_quarter_mile <- ridership_w %>%
  filter(rt_dir_stopdesc %in% bus_stops_quarter_mile$rt_dir_stopdesc) %>%
  left_join(bus_stops_quarter_mile)  


ridership_bus_stops_quarter_mile_summary1 <- ridership_bus_stops_quarter_mile %>%
  filter(!(ROUTE_NUMBER %in% c(2)), date >= "2012-09-02") %>%
  group_by(date) %>%
  summarise(ONES_SUM=sum(ONS, na.rm = TRUE),
            OFFS_SUM=sum(OFFS, na.rm = TRUE))

ggplot(ridership_bus_stops_quarter_mile_summary1, aes(date, ONES_SUM)) + geom_point() + geom_smooth() + 
  ggtitle("Boardings at specific bus stops within a quarter-mile walking radius of Expo Line stations")

ggplot(ridership_bus_stops_quarter_mile_summary1, aes(date, OFFS_SUM)) + geom_point() + geom_smooth() + 
  ggtitle("alightings at specific bus stops within a quarter-mile walking radius of Expo Line stations")