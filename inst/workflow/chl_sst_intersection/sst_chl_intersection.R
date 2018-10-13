default_grid <- function() {
  prjj <-         "+proj=laea +lat_0=-90 +datum=WGS84"
  raster(spex::buffer_extent(projectExtent(raster(extent(-180, 180, -90, -30), 
                                                  crs = "+init=epsg:4326"), 
                                           prjj), 25000), 
         res = 25000, crs = prjj)
  
}

cttab <- readRDS("/home/acebulk/data/chl_sst-min-med-max_chl_25k.rds")
library(dplyr)
cttab<- cttab %>% mutate(
  season=factor(season, levels=c("Summer","Autumn","Winter","Spring")),
  SectorName=factor(SectorName, levels=c("Atlantic", "Indian","WestPacific","EastPacific"))
)

cttab <- cttab %>% dplyr::select(chla_johnson, sst_med, cell, Zone, SectorName, decade, season, area) %>% 
  group_by(cell, season, decade, SectorName, Zone) %>% 
  summarise(mean_chla=mean(chla_johnson, na.rm=T), sst_med=median(sst_med)) %>% 
  ungroup() %>% mutate(area=625)

gfc_sst <- cttab %>% filter(sst_med <= 2)
gfc_chl <- cttab %>% filter(mean_chla >= 0.5)
gfc_combined <-  cttab %>% 
  filter(sst_med <= 2) %>% filter(mean_chla >= 0.5)

# use this to plot 6 map panels:
# columns: decades (2)
# row 1 = good chl area
# row 2 = good sst area
# row 3 = good for both
# all panels also show sector boundaries
library(sf)
grid <- spex::polygonize(default_grid())
grid$cell <- 1:nrow(grid)


#  plot(grid$geometry, col = "grey", border = NA)

dec1 <- "1998-2007"
dec2 <- "2007-2017"
gridNA <- setValues(default_grid(), NA_integer_)
chl_1  <- gridNA
chl_1[gfc_chl$cell[gfc_chl$decade == dec1]] <- 1L
sst_1 <- gridNA
sst_1[gfc_sst$cell[gfc_sst$decade == dec1]] <- 1L
chl_2  <- gridNA
chl_2[gfc_chl$cell[gfc_chl$decade == dec2]] <- 1L
sst_2 <- gridNA
sst_2[gfc_sst$cell[gfc_sst$decade == dec2]] <- 1L

c_s_1 <- gridNA
c_s_1[gfc_combined$cell[gfc_combined$decade == dec1]] <- 1L
c_s_2 <- gridNA
c_s_2[gfc_combined$cell[gfc_combined$decade == dec2]] <- 1L

layout(cbind(1:3, 4:6))
image(chl_1, main = dec1, ylab = "", xlab = "", col = "grey", axes = FALSE, box = FALSE, asp = 1)
mtext("chl", line = -2, side = 2, las = 1)
plot(aes_zone, add = TRUE)

image(sst_1, ylab = "", xlab = "", col = "grey", axes = FALSE, box = FALSE, asp = 1)
mtext("sst", line = -2, side = 2, las = 1)
plot(aes_zone, add = TRUE)

image(c_s_1,  ylab = "", xlab = "", col = "grey", axes = FALSE, box = FALSE, asp = 1)
mtext("chl+sst", line = -2, side = 2, las = 1)
plot(aes_zone, add = TRUE)

image(chl_2, main = dec2, ylab = "", xlab = "", col = "grey", axes = FALSE, box = FALSE, asp = 1) 
plot(aes_zone, add = TRUE)

image(sst_2, ylab = "", xlab = "", col = "grey", axes = FALSE, box = FALSE, asp = 1)
plot(aes_zone, add = TRUE)

image(c_s_2, ylab = "", xlab = "", col = "grey", axes = FALSE, box = FALSE, asp = 1)
plot(aes_zone, add = TRUE)
