library(raadtools)

icf <- tail(icefiles(), 365)
ice <- readice(icf$date)
ice[!ice > 0] <- NA
mn <- calc(ice, mean, na.rm = TRUE)
vr <- calc(ice, var, na.rm = TRUE)
nn <- calc(ice, function(x) sum(!is.na(x)))

library(tibble)
library(raadtools)
dates <- tibble(start = seq(as.Date("1979-02-15"), by = "1 year", length = 38))
read_year <- function(start, inputfiles = NULL) {
  dates <- seq(start, by = "2 days", length = 182)
  readice(dates, inputfiles = NULL)
}

icf <- icefiles()
ex <- extent(readice())
dates$vr <- dates$nn <- dates$mn <- vector("list", nrow(dates))
par(mar = rep(0, 4), mfrow = grDevices::n2mfrow(nrow(dates)))
for (i in seq_len(nrow(dates))) {
  ice <- readAll(read_year(dates$start[i], inputfiles = icf))
  dates$mn[[i]] <- calc(ice, mean, na.rm = TRUE)
  dates$vr[[i]] <- calc(ice, var, na.rm = TRUE)
  dates$nn[[i]] <- calc(ice,function(x) sum(!is.na(x)))
  print(i)
  plot(ex - 1.5e6, axes = FALSE, xlab = "", ylab = "", asp = 1, type = "n")
  plot(dates$vr[[i]], col = viridis::viridis(100), axes = FALSE, legend = FALSE, zlim = c(0, 2100), add = TRUE)
  text(0, 0, format(dates$start[i], "%Y"))
  
}



par(mar = rep(0, 4), mfrow = grDevices::n2mfrow(nrow(dates)))
for (i in seq_len(nrow(dates))) {
  
  plot(ex - 1.5e6, axes = FALSE, xlab = "", ylab = "", asp = 1, type = "n")
  plot(dates$mn[[i]], col = viridis::viridis(100), axes = FALSE, legend = FALSE, zlim = c(0, 100), add = TRUE)
  text(0, 0, format(dates$start[i], "%Y"))
  
}