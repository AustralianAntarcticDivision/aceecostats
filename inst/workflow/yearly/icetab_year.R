
library(raadtools)
library(tabularaster)
library(dplyr)
ice <- readice(seq(as.Date("1979-02-15"), as.Date("2017-01-15"), by = "1 month"), time.resolution = "monthly")
muice <- calc(ice, mean, na.rm = TRUE)
#anom <- stackApply(ice - muice, as.integer(format(getZ(ice), "%m")), fun = mean, na.rm = TRUE)
anom <- stackApply(ice - muice, as.integer(factor(aes_season(getZ(ice)))), fun = mean, na.rm = TRUE)

icetab <- as_tibble(ice, cell = FALSE, dim = TRUE) %>% 
  mutate(year = as.integer(format(dimindex, "%Y"))) %>% 
  ## 25km pixels for NSIDC, 12 months in a year
  group_by(year) %>% summarize(ice_prop_area = sum((cellvalue/100) * 25000 * 25000, na.rm = TRUE)/12, 
                               ncell = sum(cellvalue > 0 & !is.na(cellvalue)))
plot(icetab$year, icetab$ice_prop_area, ylim = c(0, max(icetab$ice_prop_area)), type = "l" )
par(new = TRUE)
plot(icetab$year, icetab$ncell, axes = FALSE, ylab = "")
axis(4)
write.csv(icetab, file = "icetab_1979_2016.csv", row.names = FALSE)


#Column +1: average area of total extent of sea ice over July-August each year.  
# This would require on each meridian, taking the northern most pixel which has greater 
# than or equal to 15% concentration and then including all area to the coastline.  This 
# will mean all polynyas will be included in the calculation.

##Column +2: average area of sea ice in the July-August period each year as per calculation on 
## Tuesday but excluding cells with less than 15% concentration.



# 
# 
# plot(anom, 
#      col = colorRampPalette(c("firebrick", "white", "dodgerblue"))(11), 
#      breaks = seq(-80, 80, length = 12))
# 
# 
# 
# ice <- readice(seq(as.Date("1979-01-15"), as.Date("2016-12-15"), by = "1 month"), time.resolution = "monthly")
# icetab <- as_tibble(ice, cell = FALSE, dim = TRUE) %>% 
#   mutate(year = as.integer(format(dimindex, "%Y"))) 
