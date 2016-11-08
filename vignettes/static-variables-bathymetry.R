## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(aceecostats)
library(sp)
data(aes_region_ll)


## ----eval=FALSE----------------------------------------------------------
#  library(raadtools)
#  topo <- readtopo("gebco_14")
#  library(tabularaster)
#  cn <- cellnumbers(topo, aes_region_ll)
#  saveRDS(cn, file = "cn_GEBCO14_cellnumbers_aes_region_ll.rds")
#  #xy <- xyFromCell(topo, cn$cell_)

## ------------------------------------------------------------------------
knitr::kable(as.data.frame(aes_region))

## ----eval=FALSE----------------------------------------------------------
#  library(raadtools)
#  topo <- readtopo("gebco_14")
#  library(dplyr)
#  cn <- readRDS("cn_GEBCO14_cellnumbers_aes_region_ll.rds")
#  cn <- cn %>% mutate(topo = extract(topo, cn$cell_))
#  saveRDS(cn, file = "cn_GEBCO14_values_aes_region_ll.rds")
#  

