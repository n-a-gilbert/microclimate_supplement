library(here)
library(tidyverse)
library(mgcv)

setwd(here::here("data"))

load("weather_station_comparison_data.RData")

d <- station_comparison_data

# data for iButton-weather station comparison
# site = bog
# date = date
# date_id = date code, integer, starting at first date of sampling
# scope = canopy cover
# pDeciduous = proportion deciduous
# relev = relative elevation
# btnHt = iButton height
# btnAspect = iButton aspect
# tmin = iButton - weather station minimum temp
# tmax = iButton- weather station max temp
# trange = iButton - weather statation temp range
# btn = ID of each iButton
# predictor variables are already scaled
glimpse(d)

# these models take awhile (maybe an hourish) to run
tmin <- gamm(
  formula = tmin ~
    s(date_id, k = 10) +
    s(scope, k = 5) +
    s(relev, k = 5) +
    s(pDeciduous, k = 5) +
    s(site, bs = "re") +
    s(btnHt, k = 5) + 
    s(btnAspect, bs = "cc", k = 10) +
    ti(date_id, scope) +
    ti(date_id, relev) + 
    ti(date_id, pDeciduous) + 
    ti(date_id, btnHt) + 
    ti(date_id, btnAspect),
  data = d, 
  family = gaussian,
  correlation = corARMA(form = ~ date_id | btn, p = 1),
  method = "REML")


tmax <- gamm(
  formula = tmax ~
    s(date_id, k = 10) +
    s(scope, k = 5) +
    s(relev, k = 5) +
    s(pDeciduous, k = 5) +
    s(site, bs = "re") +
    s(btnHt, k = 5) + 
    s(btnAspect, bs = "cc", k = 10) +
    ti(date_id, scope) +
    ti(date_id, relev) + 
    ti(date_id, pDeciduous) + 
    ti(date_id, btnHt) + 
    ti(date_id, btnAspect),
  data = d, 
  family = gaussian,
  correlation = corARMA(form = ~ date_id | btn, p = 1),
  method = "REML"
)

trange <- gamm(
  formula = trange ~
    s(date_id, k = 10) +
    s(scope, k = 5) +
    s(relev, k = 5) +
    s(pDeciduous, k = 5) +
    s(site, bs = "re") +
    s(btnHt, k = 5) + 
    s(btnAspect, bs = "cc", k = 10) +
    ti(date_id, scope) +
    ti(date_id, relev) + 
    ti(date_id, pDeciduous) + 
    ti(date_id, btnHt) + 
    ti(date_id, btnAspect),
  data = d, 
  family = gaussian,
  correlation = corARMA(form = ~ date_id | btn, p = 1),
  method = "REML"
)

# inspect results
summary(tmin$gam)
summary(tmax$gam)
summary(trange$gam)