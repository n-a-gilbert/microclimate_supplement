library(here)
library(tidyverse)
library(mgcv)

setwd(here::here("data"))

load("microclimate_macroclimate_comparison_v01.RData")

# site - bog name
# btn - iButton ID
# date
# date_id = 1 = first date of sampling
# scope = canopy cover
# relev = relative elevation
# pdecidious = proportion decidiuos trees
# btnht = ibutton height
# btnaspect = ibutton aspect
# tmin = ibutton - Daymet daily min temp
# tmax = ibutton - Daymet daily max temp
# tmean = ibutton - Daymet daily mean temp
# trange = ibutton -Daymet daily temp range

# predictor variables are already scaled
glimpse(d)

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
  method = "REML"
)

tmean <- gamm(
  formula = tmean ~
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
