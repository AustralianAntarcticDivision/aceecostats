library(raadtools)
library(roc)
library(aceecostats)
files <- chla_johnsonfiles(product = "MODISA")
files$season_segs <- as.integer(factor(cumsum(c(0, abs(diff(unclass(factor(aes_season(files$date)))))))))

outf <- "/mnt/acebulk"
db <- dplyr::src_sqlite("/mnt/acebulk/habitat_assessment_output.sqlite3")

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

# library(future)
# plan(multiprocess)



udecades <- unique(levels(alldays$decade)[alldays$decade])
useasons <- c("Spring", "Summer")
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
    big_tab[[icount]] <- a_dat
    print(icount)
    rm(a_dat)
    gc()
  }
}

big_tab <- bind_rows(big_tab)

ucell <- tbl(db, "modis_bins") %>% collect(n = Inf)

big_tab <- big_tab %>% left_join(ucell %>% select(-area), "cell_") %>% inner_join(aes_zone_data) %>% select(-ID)

#db$con %>% db_drop_table(table='chl_density_tab')

copy_to(db, big_tab, "chl_density_tab", temporary = FALSE)

















