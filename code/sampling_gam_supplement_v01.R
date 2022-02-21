library(here)
library(tidyverse)
library(mgcv)

setwd(here::here("data"))

load("boreal_microclimate_data_v01.RData")

head(d)

tmin_samp_cat <- gam(
  tmin ~
    s(date_id, k = 40, m = 2) + 
    s(date_id, category, k = 40, bs = "fs", m = 2) + 
    s(btnHt, k = 5) + 
    s(btnAspect, bs = "cc", k = 10) +
    s(site, bs = "re") + 
    ti(date_id, btnHt) + 
    ti(date_id, btnAspect),
  data = d, 
  method = "REML"
)

tmean_samp_cat <- gam(
  tmean ~
    s(date_id, k = 40, m = 2) + 
    s(date_id, category, k = 40, bs = "fs", m = 2) + 
    s(btnHt, k = 5) + 
    s(btnAspect, bs = "cc", k = 10) +
    s(site, bs = "re") + 
    ti(date_id, btnHt) + 
    ti(date_id, btnAspect),
  data = d, 
  method = "REML"
)

tmax_samp_cat <- gam(
  tmax ~
    s(date_id, k = 40, m = 2) + 
    s(date_id, category, k = 40, bs = "fs", m = 2) + 
    s(btnHt, k = 5) + 
    s(btnAspect, bs = "cc", k = 10) +
    s(site, bs = "re") + 
    ti(date_id, btnHt) + 
    ti(date_id, btnAspect),
  data = d, 
  method = "REML"
)

trange_samp_cat <- gam(
  trange ~
    s(date_id, k = 40, m = 2) + 
    s(date_id, category, k = 40, bs = "fs", m = 2) + 
    s(btnHt, k = 5) + 
    s(btnAspect, bs = "cc", k = 10) +
    s(site, bs = "re") + 
    ti(date_id, btnHt) + 
    ti(date_id, btnAspect),
  data = d, 
  method = "REML"
)

# setwd(here::here("results"))
# save(tmin_samp_cat, tmean_samp_cat, tmax_samp_cat, trange_samp_cat,
     # file = "sampling_category_gam_v01.RData")