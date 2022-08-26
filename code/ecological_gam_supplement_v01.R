library(here)
library(mgcv)
library(tidyverse)

setwd(here::here("data"))

load("microclimate_data_v01.RData")

d <- microclimate_data

# site - bog name
# btn - iButton ID
# date
# date_id = 1 = first date of sampling
# xcoord = longitude of ibutton
# ycoord = latitude of ibutton
# tmin = ibutton daily minimum temp
# tmax = ibutton daily max temp
# tmean = ibutton daily mean temp
# trange = ibutton daily temp range
# scope = canopy cover
# relev = relative elevation
# pdecidious = proportion decidiuos trees
# btnht = ibutton height
#btnaspect = ibutton aspect
# predictor variables are already scaled
glimpse(d)

# these models take a pretty long time to run, be aware
tminAR <- gamm(
  formula = tmin ~
    s(date_id, k = 20) +
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

tmeanAR <- gamm(
  formula = tmean ~
    s(date_id, k = 20) +
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

tmaxAR <- gamm(
  formula = tmax ~
    s(date_id, k = 20) +
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

trangeAR <- gamm(
  formula = trange ~
    s(date_id, k = 20) +
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
