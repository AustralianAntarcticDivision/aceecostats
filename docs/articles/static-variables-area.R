## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(aceecostats)
library(sp)
data(aes_region)


## ------------------------------------------------------------------------
knitr::kable(as.data.frame(aes_region))

## ------------------------------------------------------------------------
plot(aes_region, col = aes_region$colour)
text(coordinates(aes_region), lab = aes_region$ID)

