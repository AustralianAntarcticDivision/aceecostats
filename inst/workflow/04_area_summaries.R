load("/home/acebulk/data/aes_area_summary_tables.Rdata")
library(sp)
library(rgeos)
library(dplyr)
#### - Chlorophyll-a 
#### 
## 40 is about the max, include 0 and max in the intervals
chl_intervals <- c(0, 0.012, 0.2, 0.35, 0.5, 40)

#### - SST
#### 
## 30 is about the max, include 0 and max in the intervals
sst_min_intervals <- c(-2, 0, 2, 5, 30)
sst_max_intervals <- c(-2, 0, 2, 5, 30)

## - ICE make sure we are including  0 and 365
ice_day_intervals <- c(-0.5, 90, 180, 270, 365.5)


## this table is the result
chl_areas<- chl_tab %>% 
  mutate(chla_lev = cut(chla_johnson, chl_intervals)) %>% 
  group_by(decade, SectorName, Zone, season, chla_lev) %>% 
  summarize(area = n() * (4600 * 4600) / 1e6) %>% 
  ungroup()

## compare that the summary of the observable area is approximately the same as that observed in summer
#sum(aes_region_simple$total_area_km2 )
#chl_areas %>% dplyr::filter(season == "Summer") %>% group_by(decade) %>% summarize(area_km2 = sum(area))


## this table is the result
sst_min_areas <- sst_tab %>% 
  mutate(sst_min_lev = cut(min, sst_min_intervals)) %>% 
  group_by(decade, SectorName, Zone, season, sst_min_lev) %>% 
  summarize(area = sum(area)) %>% 
  ungroup()

## this table is the result
sst_max_areas <- sst_tab %>% 
  mutate(sst_max_lev = cut(max, sst_max_intervals)) %>% 
  group_by(decade, SectorName, Zone, season, sst_max_lev) %>% 
  summarize(area = sum(area)) %>% 
  ungroup()

## compare that the summary of the observable area is approximately the same as that observed in summer
#sum(aes_region_simple$total_area_km2 )
#sst_min_areas %>% dplyr::filter(season == "Summer") %>% group_by(decade) %>% summarize(area_km2 = sum(area))
#sst_max_areas %>% dplyr::filter(season == "Summer") %>% group_by(decade) %>% summarize(area_km2 = sum(area))


## this table is the result
ice_areas <- ice_tab %>% 
  mutate(day_lev = cut(days, ice_day_intervals)) %>% 
  group_by(SectorName, Zone,  day_lev, decade) %>% 
  summarize(area = sum(area)/ 10) %>% 
  ungroup()

## compare that the summary of the observable area 
#sum(aes_region_simple$total_area_km2 )
#ice_areas  %>% group_by(decade) %>% summarize(area_km2 = sum(area))

