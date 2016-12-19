## Gantt charts

library(aceecostats)
library(dplyr)
library(ggplot2)
dtime <-  bind_rows(list(sst = aes_sstfiles, ice = aes_icefiles, chl = aes_chlfiles), .id = "var")

gdtime <- dtime %>% group_by(var) %>% summarize(mdate = mean(date), min = min(date), max = max(date))
ggplot(gdtime) + aes(var, mdate, ymin = min, ymax = max) + geom_linerange() + coord_flip()


ggplot(dtime) + aes(x = date, y = var, group = var, col = var) + 
  geom_point(shape=1,      # Use hollow circles
             position=position_jitter(height=.01))
  
