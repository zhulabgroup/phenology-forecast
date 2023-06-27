library(tidyverse)
library(terra)
library(foreach)
library(doSNOW)

.path <- list( # hidden variable won't be removed
  neon_domain = "data/NEON_domain/",
  rs_modis = "data/raw/MODIS/",
  rs_evi = "data/processed/EVI/",
  dat_proc = "data/processed/",
  fig = "figures"
)

area_full <- "Appalachians / Cumberland Plateau"
area <- "Appalachians"
# area_full<-"Pacific Southwest"
