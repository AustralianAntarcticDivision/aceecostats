library(raadtools)
library(roc)
library(aceecostats)


dp <- "/home/acebulk/data"
db <- dplyr::src_sqlite(file.path(dp, "habitat_assessment.sqlite3"))

files <- chla_johnsonfiles(product = "MODISA")

files$season_segs <- as.integer(factor(cumsum(c(0, abs(diff(unclass(factor(aes_season(files$date)))))))))

## season_year needs a formalization above (using date)
## collection list to store summary tables per season-i
## trim this to only summer and winter
time_tab <- vector("list", length(unique(files$season_segs)))

## this loop 10 summers in a decade
alldays <- tibble(date = files$date, decade = aceecostats:::decade_maker(date), season = aes_season(date), 
                  season_year = files$season_segs)

icount <- 0
#db$con %>% db_drop_table(table='chl_raw_tab')
udecades <- unique(levels(alldays$decade)[alldays$decade])
useasons <- c("Spring", "Summer", "Autumn", "Winter")

for (idecade in seq_along(udecades)) {
  for (iseason in seq_along(useasons)) {
    ## identify every day uniquely badged by season_year labels
    this_decade_days <- alldays %>% filter(decade == udecades[idecade], season == useasons[iseason])
   a_dat <- bind_rows(lapply(files$fullname[which(files$season_segs %in% this_decade_days$season_year)], readRDS)) %>% 
      group_by(bin_num)  %>% 
      summarize(chla_johnson = mean(chla_johnson), chla_nasa = mean(chla_nasa)) %>% 
      mutate(date = this_decade_days$date[1]) %>% 
      mutate(decade = aceecostats:::decade_maker(date), season = useasons[iseason])
    
    icount <- icount + 1
    if (icount == 1) {
      copy_to(db, a_dat, "chl_raw_tab", temporary = FALSE, indexes = list("bin_num", "decade", "season"))
    } else {
      db_insert_into( con = db$con, table = "chl_raw_tab", values = a_dat)
    }
    print(icount)
    rm(a_dat)
    gc()
  }
}


## now "chl_raw_tab" is the main table (was "chl_density_tab") and
## "chl_johnson_tab" is the main worker for shiny

#db$con %>% db_drop_table(table='chl_johnson_tab')

## modify the raw tab and write a copy for the density work
a <- tbl(db, "chl_raw_tab")   %>%  #  filter(season == input$season) %>%  
    select(-date, -chla_nasa) %>% 
    left_join(tbl(db, "modis_bins") %>% #filter(ROWID %% 10 == 0) %>% 
                #filter(SectorName == input$region, Zone == input$zone ) %>% 
                select(-area, -ID),  c("bin_num" = "cell_")) %>% 
    filter(!is.na(Zone), !is.na(SectorName))

## is this enough?   YES IT IS
b <- copy_to(db, collect(a, n = Inf), name = "chl_johnson_tab", 
             indexes = list("bin_num", c("Zone", "SectorName", "season")), 
             temporary = FALSE)
