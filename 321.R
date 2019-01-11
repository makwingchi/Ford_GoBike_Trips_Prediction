library(tidyverse)
library(sf)
library(FNN)
library(caret)
library(spdep)
library(ggmap)
library(stargazer)
library(ggfortify)
library(knitr)
library(kableExtra)

##---- 
## Preparation
bike <- read.csv('201810-fordgobike-tripdata.csv',
                 as.is = TRUE) %>%
  filter(start_station_id != 'NULL')

sf_unique <- 
  bike %>%
  group_by(start_station_id) %>%
  summarize(count = n(),
            start_station_name = first(start_station_name),
            lat = first(start_station_latitude), 
            lon = first(start_station_longitude))

sf_unique <-
  sf_unique %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
  st_sf() %>%
  st_transform(crs = 102641)

sf_boundary <-
  read_sf('sf_boundary.shp') %>%
  st_transform(crs = 102641) %>%
  mutate(area = st_area(.)) %>%
  mutate(area = as.numeric(area))

sf_bike <-
  st_intersection(sf_boundary, sf_unique) %>%
  select(start_station_id, start_station_name)

nn_function <- function(measureFrom,measureTo,k) {
  
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint)
  
  return(output)  
}

##---- 
## Dependent Variables
bike <- 
  bike %>%
  mutate(month = substr(start_time, 6, 7)) %>%
  mutate(day = substr(start_time, 9, 10)) %>%
  mutate(hour = substr(start_time, 12, 13))

n <- length(unique(sf_bike$start_station_name)) * 1 * 31 * 24

empty <-
  data.frame(station = rep(unique(sf_bike$start_station_name), 
                           times = n/length(unique(sf_bike$start_station_name))),
             month = rep('10', n),
             day = rep(c('01', '02', '03', '04', '05', '06', '07', 
                         '08', '09', '10', '11', '12', '13', '14', 
                         '15', '16', '17', '18', '19', '20', '21', 
                         '22', '23', '24', '25', '26', '27', '28',
                         '29', '30', '31'),
                       each = n/31),
             hour = rep(c('00', '01', '02', '03', '04', '05', '06', '07', 
                          '08', '09', '10', '11', '12', '13', '14', '15',
                          '16', '17', '18', '19', '20', '21', '22', '23'),
                        each = length(unique(sf_bike$start_station_name))))

bike_arrival <-
  bike %>%
  group_by(end_station_id, end_station_name, month, day, hour) %>%
  summarize(arrival = n()) %>%
  left_join(empty, ., by = c('station' = 'end_station_name', 
                             'month' = 'month',
                             'day' = 'day', 
                             'hour' = 'hour')) %>%
  left_join(., sf_unique, by = c('station' = 'start_station_name')) %>%
  mutate(station_id = start_station_id) %>%
  select(-start_station_id, -end_station_id, -count) %>%
  mutate(arrival = replace_na(arrival, 0)) %>%
  arrange(., station_id, day, hour) %>%
  mutate(identifier = rep(c(1:744), length(unique(sf_bike$start_station_name))))

bike_departure <-
  bike %>%
  group_by(start_station_id, start_station_name, month, day, hour) %>%
  summarize(departure = n()) %>%
  left_join(empty, ., by = c('station' = 'start_station_name', 
                             'month' = 'month',
                             'day' = 'day', 
                             'hour' = 'hour')) %>%
  left_join(., sf_unique, by = c('station' = 'start_station_name')) %>%
  mutate(station_id = start_station_id.y) %>%
  select(-start_station_id.x, -start_station_id.y, -count) %>%
  mutate(departure = replace_na(departure, 0))%>%
  arrange(., station_id, day, hour) %>%
  mutate(identifier = rep(c(1:744), length(unique(sf_bike$start_station_name))))

##----
## Loading Independent Variables
bus <- 
  read.socrata('https://data.sfgov.org/resource/ikg8-jrfu.json') %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_sf() %>%
  st_transform(crs = 102641) %>%
  select() %>%
  mutate(Legend = "Bus_Stop")

park <-
  read.socrata('https://data.sfgov.org/Culture-and-Recreation/Recreation-and-Parks-Facilities/xvq2-rjrk') %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_sf() %>%
  st_transform(crs = 102641) %>%
  select() %>%
  mutate(Legend = "Park")

crash <-
  read.csv('Collisions.csv') %>%
  filter(COUNT_PED_INJURED > 0 | COUNT_BICYCLIST_INJURED > 0 |
           COUNT_PED_KILLED > 0 | COUNT_BICYCLIST_KILLED > 0) %>%
  st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 4326, agr = "constant") %>%
  st_sf() %>%
  st_transform(crs = 102641) %>%
  select() %>%
  mutate(Legend = "Crash")

## Distance to bus stops, parks, and traffic accidents
sf_bike.xy <- 
  sf_bike %>%
  cbind(.,st_coordinates(st_centroid(sf_bike)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

bus.xy <- 
  bus %>%
  cbind(.,st_coordinates(st_centroid(bus)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

park.xy <- 
  park %>%
  cbind(.,st_coordinates(st_centroid(park)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

crash.xy <- 
  crash %>%
  cbind(.,st_coordinates(st_centroid(crash)))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

dist_Park1 <- 
  as.data.frame(nn_function(sf_bike.xy, park.xy, 1)) %>%
  mutate(dist_Park1 = log(pointDistance)) %>%
  select(dist_Park1)

dist_Bus1 <- 
  as.data.frame(nn_function(sf_bike.xy, bus.xy, 1)) %>%
  mutate(log_dist_Bus1 = log(pointDistance)) %>%
  select(log_dist_Bus1)

dist_Bus3 <- 
  as.data.frame(nn_function(sf_bike.xy, bus.xy, 3)) %>%
  mutate(log_dist_Bus3 = log(pointDistance)) %>%
  select(log_dist_Bus3)

dist_Crash1 <- 
  as.data.frame(nn_function(sf_bike.xy, crash.xy, 3)) %>%
  mutate(log_dist_Crash1 = log(pointDistance)) %>%
  select(log_dist_Crash1)

dist_variables <-
  cbind(dist_Park1, dist_Bus1, dist_Bus3, dist_Crash1, unique(sf_bike_WGS$start_station_id))

#----
## Neighborhood
neighborhood <-
  read_sf('neighborhood.shp') %>%
  st_transform(crs = 102641)

sf_bike <- 
  st_intersection(neighborhood, sf_bike)

#----
## Block Group
sf_bike <- 
  st_intersection(sf_boundary %>%
                    select(GEOID, area), sf_bike) %>%
  mutate(block_area = area) %>%
  select(-area)

#----
## Census & Jobs
job <-
  read.csv('jobs.csv') %>%
  select(w_geocode, S000) %>%
  mutate(w_geocode = as.numeric(substr(w_geocode, 1, 11))) %>%
  group_by(w_geocode) %>%
  summarize(jobs = sum(S000))

census <- 
  read.csv('census.csv', 
           as.is = TRUE)

census <- 
  left_join(census, job, by = c('GeoID' = 'w_geocode')) %>%
  mutate(jobs = replace_na(jobs, 0))

sf_bike <-
  sf_bike %>%
  mutate(GEOID = as.numeric(GEOID))

#----
## Time
bike_departure <-
  bike_departure %>%
  mutate(whichday = ifelse(day == '01'|day == '08'|
                             day == '15'|day == '22'|
                             day == '29', 'Mon',
                           ifelse(day == '02'|day == '09'|
                                    day == '16'|day == '23'|
                                    day == '30', 'Tue',
                                  ifelse(day == '03'|day == '10'|
                                           day == '17'|day == '24'|
                                           day == '31', 'Wed',
                                         ifelse(day == '04'|day == '11'|
                                                  day == '18'|day == '25', 'Thur',
                                                ifelse(day == '05'|day == '12'|
                                                         day == '19'|day == '26', 'Fri',
                                                       ifelse(day == '06'|day == '13'|
                                                                day == '20'|day == '27', 'Sat', 'Sun')))))))


#----
## Distance to high-volume stations
high_station <- 
  sf_bike %>%
  filter(start_station_id == '67' | start_station_id == '58' | start_station_id == '15')

dist_highvolume <- 
  st_distance(sf_bike, high_station) %>%
  as.data.frame() %>%
  cbind(., sf_bike$start_station_id) %>%
  mutate(dist_ferryBuilding = as.numeric(V1),
         dist_Market = as.numeric(V2),
         dist_Townsend = as.numeric(V3),
         station_id = sf_bike$start_station_id) %>%
  select(station_id, dist_Townsend, dist_ferryBuilding)

#----
## Land Use
landUse <- 
  read_sf('landuse.shp') %>%
  st_transform(crs = 102641) %>%
  select(landuse)

landuse_stat <-
  st_intersection(landUse, bike_buffer_hfmile) %>%
  mutate(area = as.numeric(st_area(.))) %>%
  filter(landuse != 'MISSING DATA') %>%
  filter(landuse != 'Right of Way') %>%
  mutate(landuse = ifelse(landuse == 'RETAIL/ENT', 'RETAIL', landuse)) %>%
  group_by(start_station_id, landuse) %>%
  summarize(sum_area = sum(area)) %>%
  st_set_geometry(NULL) %>%
  spread(landuse, sum_area, fill = 1) %>%
  mutate(area = CIE + MED + MIPS + MIXED + MIXRES + OpenSpace + PDR + RESIDENT + RETAIL + VACANT + VISITOR) %>%
  mutate(pct_CIE = CIE/area,
         pct_MED = MED/area,
         pct_MIPS = MIPS/area,
         pct_MIXED = MIXED/area,
         pct_MIXRES = MIXRES/area,
         pct_OpenSpace = OpenSpace/area,
         pct_PDR = PDR/area,
         pct_RESIDENT = RESIDENT/area,
         pct_RETAIL = RETAIL/area,
         pct_VACANT = VACANT/area,
         pct_VISITOR = VISITOR/area) %>%
  mutate(entropy = pct_CIE*(-log(pct_CIE)/log(11))+
           pct_MED*(-log(pct_MED)/log(11))+
           pct_MIPS*(-log(pct_MIPS)/log(11))+
           pct_MIXED*(-log(pct_MIXED)/log(11))+
           pct_MIXRES*(-log(pct_MIXRES)/log(11))+
           pct_OpenSpace*(-log(pct_OpenSpace)/log(11))+
           pct_PDR*(-log(pct_PDR)/log(11))+
           pct_RESIDENT*(-log(pct_RESIDENT)/log(11))+
           pct_RETAIL*(-log(pct_RETAIL)/log(11))+
           pct_VACANT*(-log(pct_VACANT)/log(11))+
           pct_VISITOR*(-log(pct_VISITOR)/log(11)))

landUse_summary <-
  read.csv('landUse_summary.txt',
           as.is = TRUE) %>%
  select(STRT_STTN_:SUM_AREA) %>%
  rename(station_id = STRT_STTN_,
         landuse = LANDUSE,
         sum_area = SUM_AREA) %>%
  filter(landuse != 'MISSING DATA') %>%
  filter(landuse != 'Right of Way') %>%
  mutate(landuse = ifelse(landuse == 'RETAIL/ENT', 'RETAIL', landuse),
         station_id = as.character(station_id)) %>%
  spread(landuse, sum_area, fill = 1) %>%
  mutate(area = CIE + MED + MIPS + MIXED + MIXRES + OpenSpace + PDR + RESIDENT + RETAIL + VACANT + VISITOR) %>%
  mutate(pct_CIE = CIE/area,
         pct_MED = MED/area,
         pct_MIPS = MIPS/area,
         pct_MIXED = MIXED/area,
         pct_MIXRES = MIXRES/area,
         pct_OpenSpace = OpenSpace/area,
         pct_PDR = PDR/area,
         pct_RESIDENT = RESIDENT/area,
         pct_RETAIL = RETAIL/area,
         pct_VACANT = VACANT/area,
         pct_VISITOR = VISITOR/area) %>%
  mutate(entropy = pct_CIE*(-log(pct_CIE)/log(11))+
           pct_MED*(-log(pct_MED)/log(11))+
           pct_MIPS*(-log(pct_MIPS)/log(11))+
           pct_MIXED*(-log(pct_MIXED)/log(11))+
           pct_MIXRES*(-log(pct_MIXRES)/log(11))+
           pct_OpenSpace*(-log(pct_OpenSpace)/log(11))+
           pct_PDR*(-log(pct_PDR)/log(11))+
           pct_RESIDENT*(-log(pct_RESIDENT)/log(11))+
           pct_RETAIL*(-log(pct_RETAIL)/log(11))+
           pct_VACANT*(-log(pct_VACANT)/log(11))+
           pct_VISITOR*(-log(pct_VISITOR)/log(11)))

#----
## Past Departure/Arrival
bike_departure_timeLag <-
  bike_departure %>%
  mutate(identifier_1h = identifier + 1,
         identifier_2h = identifier + 2,
         identifier_3h = identifier + 3,
         identifier_24h = identifier + 24,
         identifier_1w = identifier + 168) %>%
  select(station_id, departure, identifier, identifier_1h, 
         identifier_2h, identifier_3h, identifier_24h, identifier_1w)

bike_arrival_timelag <-
  bike_arrival %>%
  mutate(identifier_1h = identifier + 1,
         identifier_2h = identifier + 2,
         identifier_3h = identifier + 3,
         identifier_24h = identifier + 24,
         identifier_1w = identifier + 168,
         identifier_1w1h = identifier + 169) %>%
  select(station_id, arrival, identifier, identifier_1h, identifier_2h,
         identifier_3h, identifier_24h, identifier_1w, identifier_1w1h)

bike_departure <-
  left_join(bike_departure, bike_departure_timeLag, 
            by = c('identifier' = 'identifier_1h',
                   'station_id' = 'station_id')) %>%
  select(station_id, station, month, day, hour, departure.x, 
         weekday, peak, whichday, departure.y, identifier) %>%
  rename(departure = departure.x, 
         departure_1h = departure.y) %>%
  left_join(., bike_departure_timeLag, 
            by = c('identifier' = 'identifier_2h',
                   'station_id' = 'station_id')) %>%
  select(station_id, station, month, day, hour, departure.x, 
         weekday, peak, whichday, departure_1h, departure.y, identifier) %>%
  rename(departure = departure.x, 
         departure_2h = departure.y) %>%
  left_join(., bike_departure_timeLag, 
            by = c('identifier' = 'identifier_3h',
                   'station_id' = 'station_id')) %>%
  select(station_id, station, month, day, hour, departure.x, weekday, 
         peak, whichday, departure_1h, departure_2h, departure.y, identifier) %>%
  rename(departure = departure.x, 
         departure_3h = departure.y) %>%
  left_join(., bike_departure_timeLag, 
            by = c('identifier' = 'identifier_24h',
                   'station_id' = 'station_id')) %>%
  select(station_id, station, month, day, hour, departure.x, weekday, peak, whichday, 
         departure_1h, departure_2h, departure_3h, departure.y, identifier) %>%
  rename(departure = departure.x, 
         departure_24h = departure.y) %>%
  left_join(., bike_departure_timeLag, 
            by = c('identifier' = 'identifier_1w',
                   'station_id' = 'station_id')) %>%
  select(station_id, station, month, day, hour, departure.x, weekday, peak, whichday, 
         departure_1h, departure_2h, departure_3h, departure_24h, departure.y, identifier) %>%
  rename(departure = departure.x, 
         departure_1w = departure.y) %>%
  left_join(., bike_arrival_timelag, 
            by = c('identifier' = 'identifier',
                   'station_id' = 'station_id')) %>%
  select(station_id, station, month, day, hour, weekday, peak, whichday, departure, departure_1h, 
         departure_2h, departure_3h, departure_24h, departure_1w, arrival, identifier) %>%
  left_join(., bike_arrival_timelag, 
            by = c('identifier' = 'identifier_1h',
                   'station_id' = 'station_id')) %>%
  select(station_id, station, month, day, hour, weekday, peak, whichday, departure, departure_1h, 
         departure_2h, departure_3h, departure_24h, departure_1w, arrival.x, arrival.y, identifier) %>%
  rename(arrival = arrival.x,
         arrival_1h = arrival.y) %>%
  left_join(., bike_arrival_timelag, 
            by = c('identifier' = 'identifier_2h',
                   'station_id' = 'station_id')) %>%
  select(station_id, station, month, day, hour, weekday, peak, whichday, departure, departure_1h, 
         departure_2h, departure_3h, departure_24h, departure_1w, arrival.x, arrival_1h, arrival.y, identifier) %>%
  rename(arrival = arrival.x,
         arrival_2h = arrival.y) %>%
  left_join(., bike_arrival_timelag, 
            by = c('identifier' = 'identifier_3h',
                   'station_id' = 'station_id')) %>%
  select(station_id, station, month, day, hour, weekday, peak, whichday, departure, departure_1h, 
         departure_2h, departure_3h, departure_24h, departure_1w, arrival.x, arrival_1h, arrival_2h, arrival.y, identifier) %>%
  rename(arrival = arrival.x,
         arrival_3h = arrival.y) %>%
  left_join(., bike_arrival_timelag, 
            by = c('identifier' = 'identifier_24h',
                   'station_id' = 'station_id')) %>%
  select(station_id, station, month, day, hour, weekday, peak, whichday, departure, departure_1h, 
         departure_2h, departure_3h, departure_24h, departure_1w, arrival.x, arrival_1h, arrival_2h, 
         arrival_3h, arrival.y, identifier) %>%
  rename(arrival = arrival.x,
         arrival_24h = arrival.y) %>%
  left_join(., bike_arrival_timelag, 
            by = c('identifier' = 'identifier_1w',
                   'station_id' = 'station_id')) %>%
  select(station_id, station, month, day, hour, weekday, peak, whichday, departure, departure_1h, 
         departure_2h, departure_3h, departure_24h, departure_1w, arrival.x, arrival_1h, arrival_2h, 
         arrival_3h, arrival_24h, arrival.y, identifier) %>%
  rename(arrival = arrival.x,
         arrival_1w = arrival.y) %>%
  left_join(., bike_arrival_timelag, 
            by = c('identifier' = 'identifier_1w1h',
                   'station_id' = 'station_id')) %>%
  select(station_id, station, month, day, hour, weekday, peak, whichday, departure, departure_1h, 
         departure_2h, departure_3h, departure_24h, departure_1w, arrival.x, arrival_1h, arrival_2h, 
         arrival_3h, arrival_24h, arrival_1w, arrival.y, identifier) %>%
  rename(arrival = arrival.x,
         arrival_1w1h = arrival.y)

#----
## Nearby Departure/Arrival
sf_bike <-
  sf_bike %>%
  mutate(fakeID = as.numeric(rownames(.)))

nearest_table <-
  get.knnx(sf_bike.xy, sf_bike.xy, 6)$`nn.index` %>%
  as.data.frame() %>%
  rename(fakeID = V1,
         nearest1 = V2,
         nearest2 = V3,
         nearest3 = V4,
         nearest4 = V5,
         nearest5 = V6) %>%
  left_join(., 
            sf_bike %>%
              st_set_geometry(NULL) %>%
              select(fakeID, start_station_id),
            by = c('nearest1' = 'fakeID')) %>%
  rename(nearest_1 = start_station_id) %>%
  left_join(., 
            sf_bike %>%
              st_set_geometry(NULL) %>%
              select(fakeID, start_station_id),
            by = c('nearest2' = 'fakeID')) %>%
  rename(nearest_2 = start_station_id) %>%
  left_join(., 
            sf_bike %>%
              st_set_geometry(NULL) %>%
              select(fakeID, start_station_id),
            by = c('nearest3' = 'fakeID')) %>%
  rename(nearest_3 = start_station_id) %>%
  left_join(., 
            sf_bike %>%
              st_set_geometry(NULL) %>%
              select(fakeID, start_station_id),
            by = c('nearest4' = 'fakeID')) %>%
  rename(nearest_4 = start_station_id) %>%
  left_join(., 
            sf_bike %>%
              st_set_geometry(NULL) %>%
              select(fakeID, start_station_id),
            by = c('nearest5' = 'fakeID')) %>%
  rename(nearest_5 = start_station_id) %>%
  cbind(sf_bike$start_station_id, .) %>%
  mutate(station_id = sf_bike$start_station_id) %>%
  select(nearest_1:station_id)


bike_departure_full <-
  left_join(bike_departure, nearest_table, by = c('station_id')) %>%
  left_join(., 
            bike_departure %>%
              select(station_id, day, hour, departure, departure_1h, departure_2h, 
                     departure_3h, departure_24h, departure_1w, arrival, arrival_1h,
                     arrival_2h, arrival_3h, arrival_24h, arrival_1w, arrival_1w1h),
            by = c('nearest_1' = 'station_id',
                   'day' = 'day',
                   'hour' = 'hour')) %>%
  left_join(., 
            bike_departure %>%
              select(station_id, day, hour, departure, departure_1h, departure_2h, 
                     departure_3h, departure_24h, departure_1w, arrival, arrival_1h,
                     arrival_2h, arrival_3h, arrival_24h, arrival_1w, arrival_1w1h),
            by = c('nearest_2' = 'station_id',
                   'day' = 'day',
                   'hour' = 'hour')) %>%
  left_join(., 
            bike_departure %>%
              select(station_id, day, hour, departure, departure_1h, departure_2h, 
                     departure_3h, departure_24h, departure_1w, arrival, arrival_1h,
                     arrival_2h, arrival_3h, arrival_24h, arrival_1w, arrival_1w1h),
            by = c('nearest_3' = 'station_id',
                   'day' = 'day',
                   'hour' = 'hour')) %>%
  left_join(., 
            bike_departure %>%
              select(station_id, day, hour, departure, departure_1h, departure_2h, 
                     departure_3h, departure_24h, departure_1w, arrival, arrival_1h,
                     arrival_2h, arrival_3h, arrival_24h, arrival_1w, arrival_1w1h),
            by = c('nearest_4' = 'station_id',
                   'day' = 'day',
                   'hour' = 'hour')) %>%
  left_join(., 
            bike_departure %>%
              select(station_id, day, hour, departure, departure_1h, departure_2h, 
                     departure_3h, departure_24h, departure_1w, arrival, arrival_1h,
                     arrival_2h, arrival_3h, arrival_24h, arrival_1w, arrival_1w1h),
            by = c('nearest_5' = 'station_id',
                   'day' = 'day',
                   'hour' = 'hour'))

names(bike_departure_full) <-
  gsub('.x.x.x', '_n4', names(bike_departure_full), fixed = TRUE)

names(bike_departure_full) <-
  gsub('.y.y.y', '_n5', names(bike_departure_full), fixed = TRUE)

names(bike_departure_full) <-
  gsub('.x.x', '_n2', names(bike_departure_full), fixed = TRUE)

names(bike_departure_full) <-
  gsub('.y.y', '_n3', names(bike_departure_full), fixed = TRUE)

names(bike_departure_full) <-
  gsub('.y', '_n1', names(bike_departure_full), fixed = TRUE)

names(bike_departure_full) <-
  gsub('.x', '', names(bike_departure_full), fixed = TRUE)

bike_departure_full <-
  bike_departure_full %>%
  mutate(nearest3_departure = (departure_n1 + departure_n2 + departure_n3) / 3,
         nearest3_departure_1h = (departure_1h_n1 + departure_1h_n2 + departure_1h_n3) / 3,
         nearest3_departure_2h = (departure_2h_n1 + departure_2h_n2 + departure_2h_n3) / 3,
         nearest3_departure_3h = (departure_3h_n1 + departure_3h_n2 + departure_3h_n3) / 3,
         nearest3_departure_24h = (departure_24h_n1 + departure_24h_n2 + departure_24h_n3) / 3,
         nearest3_departure_1w = (departure_1w_n1 + departure_1w_n2 + departure_1w_n3) / 3,
         nearest3_arrival = (arrival_n1 + arrival_n2 + arrival_n3) / 3,
         nearest3_arrival_1h = (arrival_1h_n1 + arrival_1h_n2 + arrival_1h_n3) / 3,
         nearest3_arrival_2h = (arrival_2h_n1 + arrival_2h_n2 + arrival_2h_n3) / 3,
         nearest3_arrival_3h = (arrival_3h_n1 + arrival_3h_n2 + arrival_3h_n3) / 3,
         nearest3_arrival_24h = (arrival_24h_n1 + arrival_24h_n2 + arrival_24h_n3) / 3,
         nearest3_arrival_1w = (arrival_1w_n1 + arrival_1w_n2 + arrival_1w_n3) / 3,
         nearest3_arrival_1w1h = (arrival_1w1h_n1 + arrival_1w1h_n2 + arrival_1w1h_n3) / 3,
         nearest5_departure = (departure_n1 + departure_n2 + departure_n3 + departure_n4 + departure_n5) / 5,
         nearest5_departure_1h = (departure_1h_n1 + departure_1h_n2 + departure_1h_n3 + departure_1h_n4 + departure_1h_n5) / 5,
         nearest5_departure_2h = (departure_2h_n1 + departure_2h_n2 + departure_2h_n3 + departure_2h_n4 + departure_2h_n5) / 5,
         nearest5_departure_3h = (departure_3h_n1 + departure_3h_n2 + departure_3h_n3 + departure_3h_n4 + departure_3h_n5) / 5,
         nearest5_departure_24h = (departure_24h_n1 + departure_24h_n2 + departure_24h_n3 + departure_24h_n4 + departure_24h_n5) / 5,
         nearest5_departure_1w = (departure_1w_n1 + departure_1w_n2 + departure_1w_n3 + departure_1w_n4 + departure_1w_n5) / 5,
         nearest5_arrival = (arrival_n1 + arrival_n2 + arrival_n3 + arrival_n4 + arrival_n5) / 5,
         nearest5_arrival_1h = (arrival_1h_n1 + arrival_1h_n2 + arrival_1h_n3 + arrival_1h_n4 + arrival_1h_n5) / 5,
         nearest5_arrival_2h = (arrival_2h_n1 + arrival_2h_n2 + arrival_2h_n3 + arrival_2h_n4 + arrival_2h_n5) / 5,
         nearest5_arrival_3h = (arrival_3h_n1 + arrival_3h_n2 + arrival_3h_n3 + arrival_3h_n4 + arrival_3h_n5) / 5,
         nearest5_arrival_24h = (arrival_24h_n1 + arrival_24h_n2 + arrival_24h_n3 + arrival_24h_n4 + arrival_24h_n5) / 5,
         nearest5_arrival_1w = (arrival_1w_n1 + arrival_1w_n2 + arrival_1w_n3 + arrival_1w_n4 + arrival_1w_n5) / 5,
         nearest5_arrival_1w1h = (arrival_1w1h_n1 + arrival_1w1h_n2 + arrival_1w1h_n3 + arrival_1w1h_n4 + arrival_1w1h_n5) / 5) %>%
  select(station_id:nearest_5, nearest3_departure:nearest5_arrival_1w1h)

#----
## Weather
weather <-
  read.csv('SF_climate_hourly.csv',
           as.is = TRUE)

#----
## Integrating into one dataset
vars <-
  left_join(dist_variables, dist_landmark, by = c('start_station_id' = 'start_station_id')) %>%
  left_join(., landUse_summary, by = c('start_station_id' = 'station_id')) %>%
  left_join(., dist_highvolume, by = c('start_station_id' = 'station_id')) %>%
  left_join(sf_bike %>%
              dplyr::select(start_station_id, geoid10, GEOID, nhood, block_area, taz),
            .,
            by = c('start_station_id' = 'start_station_id')) %>%
  left_join(., census, by = c('GEOID.x' = 'GeoID')) %>%
  left_join(bike_departure_full, ., by = c('station_id' = 'start_station_id')) %>%
  left_join(., weather, by = c('day' = 'Date',
                               'hour' = "HOUR")) %>%
  mutate(Wind.Speed = as.numeric(substr(Wind.Speed, 1, 1))) %>%
  mutate(eastMost = ifelse(nhood == 'Financial District/South Beach' | nhood == 'Mission Bay' | nhood == 'South of Market', 'in', 'out')) %>%
  mutate(weekday_hot = ifelse(station == 'San Francisco Caltrain Station 2  (Townsend St at 4th St)' | 
                                station == 'San Francisco Ferry Building (Harry Bridges Plaza)' |
                                station == 'San Francisco Caltrain (Townsend St at 4th St)' |
                                station == 'Montgomery St BART Station (Market St at 2nd St)' |
                                station == 'Berry St at 4th St' |
                                station == 'Powell St BART Station (Market St at 4th St)' |
                                station == 'Steuart St at Market St' |
                                station == 'The Embarcadero at Sansome St' |
                                station == 'Howard St at Beale St', 1, 0)) %>%
  mutate(weekend_hot = ifelse(weekday == 'weekday', 0, weekend_hot)) %>%
  filter(hour != '0' & hour != '1' & hour != '2' & hour != '3' & hour != '4' & hour != '5')

#----
## Training and Test Set
vars_train <-
  vars %>%
  filter(day >= 14 & day <= 20,
         station_id != '80' & station_id != '344')

vars_test1 <-
  vars %>%
  filter(day == 22,
         station_id != '80' & station_id != '344')

vars_test2 <-
  vars %>%
  filter(day == 21,
         station_id != '80' & station_id != '344')
