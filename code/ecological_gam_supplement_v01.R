library(here)
library(mgcv)
library(tidyverse)

setwd(here::here("data"))

load("boreal_microclimate_data_v01.RData")

head(d)

start <- Sys.time()

tminAR20 <- gamm(
  formula = tmin ~
    s(date_scale, k = 20) +
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


tmeanAR20 <- gamm(
  formula = tmean ~
    s(date_scale, k = 20) +
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

tmaxAR20 <- gamm(
  formula = tmax ~
    s(date_scale, k = 20) +
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

trangeAR20 <- gamm(
  formula = trange ~
    s(date_scale, k = 20) +
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

setwd(here::here("results"))
# save(tminAR20, tmeanAR20, tmaxAR20, trangeAR20,
     # file = "ecological_gam_v05.RData")

end <- Sys.time()
end - start
