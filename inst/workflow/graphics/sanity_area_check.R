## check area details
library(dplyr)
dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))

library(aceecostats)
aes_region_simple$total_area_km2 <- rgeos::gArea(aes_region_simple, byid = TRUE)

tbl(db, "chl_johnson_tab") %>% mutate(area = (4600 * 4600) / 1e6) %>% 
  group_by(season, SectorName, decade, zone) %>% 
  summarize(total_area_km2 = sum(area), n = n()) %>% collect() %>% DT::datatable(nrow = Inf)

tbl(db, "sst_density_tab") %>% 
  group_by(season, SectorName, decade, zone) %>% summarize(total_area_km2 = sum(area), n = n()) %>% 
  collect() %>% 
  inner_join(aes_region_simple@data, c("SectorName" = "SectorName", "Zone" = "Zone"))
%>% 
             DT::datatable()


tbl(db, "ice_days_density_tab") %>% group_by(season, SectorName, decade, zone) %>% summarize(total_area_km2 = sum(area), n = n()) %>% collect() %>% DT::datatable()


# 
# ## a sample of the options
# input <- list(region = "Atlantic", zone = "High-Latitude", season = "Summer")
# 
# ## chl_johnson_tab, sst_density_tab, or ice_days_density_tab
# tabname <- "chl_johnson_tab"
# tab <- tbl(db, tabname)  %>%   
#   filter(season == input$season, 
#          SectorName == input$region) %>% 
#   collect(n = Inf)
# library(aceecostats)
# 
# ## plots are grouped by decade, so 
# 
# ## for chla they are all equal and I failed to put in db
# if (tabname == "chl_johnson_tab") tab$area <- (4600 * 4600)/1e6
# tab %>% group_by(decade) %>% summarize(total_area_km2 = sum(area), n = n())
# sum(rgeos::gArea(subset(aes_region_simple, SectorName == input$region, Zone == input$zone), byid = TRUE) / 1e6)
