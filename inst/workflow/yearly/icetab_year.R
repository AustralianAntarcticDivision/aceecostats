
library(raadtools)
library(tabularaster)
library(dplyr)
ice <- readice(seq(as.Date("1979-01-15"), as.Date("2016-12-15"), by = "1 month"), time.resolution = "monthly")
icetab <- as_tibble(ice, cell = FALSE, dim = TRUE) %>% 
  mutate(year = as.integer(format(dimindex, "%Y"))) %>% 
  ## 25km pixels for NSIDC, 365/12 days per month
  group_by(year) %>% summarize(ice_prop_area = sum((cellvalue/100) * 25000 * 25000, na.rm = TRUE)/30.4, 
                               ncell = sum(cellvalue > 0 & !is.na(cellvalue)))
plot(icetab$year, icetab$ice_prop_area, ylim = c(0, max(icetab$ice_prop_area)), type = "l" )
par(new = TRUE)
plot(icetab$year, icetab$ncell, axes = FALSE, ylab = "")
axis(4)
write.csv(icetab, file = "icetab_1979_2016.csv", row.names = FALSE)

