library(raadtools)
library(roc)
library(aceecostats)
files <- chla_johnsonfiles(product = "MODISA")
files$season_segs <- as.integer(factor(cumsum(c(0, abs(diff(unclass(factor(aes_season(files$date)))))))))


## season_year needs a formalization above (using date)
## collection list to store summary tables per season-i
## trim this to only summer and winter
time_tab <- vector("list", length(unique(files$season_segs)))

## this loop 10 summers in a decade
alldays <- tibble(date = files$date, decade = aceecostats:::decade_maker(date), season = aes_season(date), 
                  season_year = files$season_segs)


big_tab <- vector("list")
icount <- 0
idecade <- 1
iseason <- 1

library(future)
plan(multiprocess)



udecades <- unique(levels(alldays$decade)[alldays$decade])
useasons <- c("Spring", "Summer")
for (idecade in seq_along(udecades)) {
  for (iseason in seq_along(useasons)) {
    ## identify every day uniquely badged by season_year labels
    this_decade_days <- alldays %>% filter(decade == udecades[idecade], season == useasons[iseason])
    #decade_years <- this_decade_days %>% split(.$season_year)
    #list_of_seasons <- vector("list", length(decade_years))
    #a_obj <- readAll(subset(obj, which(segs %in% this_decade_days$season_year)))
    a_dat <- bind_rows(future_lapply(files$fullname[which(files$season_segs %in% this_decade_days$season_year)], readRDS)) %>% 
      group_by(bin_num)  %>% 
      summarize(chla_johnson = mean(chla_johnson), chla_nasa = mean(chla_nasa)) %>% 
      mutate(date = this_decade_days$date[1]) %>% 
      mutate(decade = aceecostats:::decade_maker(date), season = useasons[iseason])
    
    icount <- icount + 1
    big_tab[[icount]] <- tab
    print(icount)
    rm(a_dat)
    gc()
  }
}

big_tab <- bind_rows(big_tab)
big_tab$area <- 4600

init <- initbin(NUMROWS = 4320)
## counts up from the south
maxbin <- init$totbin/2
## unique grid map cell number
#ucell <- tibble(cell_ = seq_len(ncell(ras)), area = values(gridarea))

ucell <- tibble(cell_ = seq_len(maxbin), area = 4)
## classify cell index by polygon
# ucell$ID <- over(spTransform(xyFromCell(ras, ucell$cell_, spatial=TRUE), projection(aes_zone)), 
#                  aes_zone)$ID
xy <- sp::SpatialPoints(do.call(cbind, bin2lonlat(ucell$cell_, 4320)), proj4string = CRS("+init=epsg:4326"))
ucell$ID <- over(spTransform(xy, projection(aes_zone)), 
                 aes_zone)$ID

ucell <- ucell %>% filter(!is.na(ID))



big_tab <- big_tab %>% left_join(ucell %>% select(-area), "cell_") %>% inner_join(aes_zone_data) %>% select(-ID)

#db$con %>% db_drop_table(table='chl_density_tab')

copy_to(db, big_tab, "chl_density_tab", temporary = FALSE)

















