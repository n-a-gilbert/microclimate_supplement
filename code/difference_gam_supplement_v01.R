library(here)
library(tidyverse)
library(mgcv)

setwd(here::here("data"))

load("microclimate_macroclimate_comparison_v01.RData")

head(d)

tmin <- gamm(
  formula = tmin ~
    s(date_scale, k = 10) +
    s(scope, k = 5) +
    s(relev, k = 5) +
    s(pDeciduous, k = 5) +
    s(site, bs = "re") +
    ti(date_scale, scope) +
    ti(date_scale, relev) + 
    ti(date_scale, pDeciduous),
  data = d, 
  family = gaussian,
  correlation = corARMA(form = ~ date_id | btn, p = 1),
  method = "REML"
)

tmean <- gamm(
  formula = tmean ~
    s(date_scale, k = 10) +
    s(scope, k = 5) +
    s(relev, k = 5) +
    s(pDeciduous, k = 5) +
    s(site, bs = "re") +
    ti(date_scale, scope) +
    ti(date_scale, relev) + 
    ti(date_scale, pDeciduous),
  data = d, 
  family = gaussian,
  correlation = corARMA(form = ~ date_id | btn, p = 1),
  method = "REML"
)

tmax <- gamm(
  formula = tmax ~
    s(date_scale, k = 10) +
    s(scope, k = 5) +
    s(relev, k = 5) +
    s(pDeciduous, k = 5) +
    s(site, bs = "re") +
    ti(date_scale, scope) +
    ti(date_scale, relev) + 
    ti(date_scale, pDeciduous),
  data = d, 
  family = gaussian,
  correlation = corARMA(form = ~ date_id | btn, p = 1),
  method = "REML"
)

trange <- gamm(
  formula = trange ~
    s(date_scale, k = 10) +
    s(scope, k = 5) +
    s(relev, k = 5) +
    s(pDeciduous, k = 5) +
    s(site, bs = "re") +
    ti(date_scale, scope) +
    ti(date_scale, relev) + 
    ti(date_scale, pDeciduous),
  data = d, 
  family = gaussian,
  correlation = corARMA(form = ~ date_id | btn, p = 1),
  method = "REML"
)
# setwd(here::here("results"))
# save(tmin, tmean, tmax, trange, file = "difference_gam_v02.RData")
