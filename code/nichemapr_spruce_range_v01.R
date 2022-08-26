# devtools::install_github('mrke/NicheMapR')
library(NicheMapR)
library(here)
library(tidyverse)
library(sf)
library(here)

# only have to do this once; download data and tell what folder to put it in
# get.global.climate(folder = "Y:/ngilbert/bogz/data/global_climate")

setwd(here::here("data/spruce"))

spruce <- st_read("picemari.shp") %>% 
  st_set_crs(4267) %>% 
  st_transform(., crs = 4326)

# systematic sample of points within the range of black spruce
pts <- st_sample(spruce, 
                 size = 200,
                 type = "regular") %>% 
  st_set_crs(4326) %>% 
  st_as_sf() %>% 
  mutate(xcoord = st_coordinates(.)[,1],
         ycoord = st_coordinates(.)[,2])

dstart <- "27/09/2013"
dfinish <- "09/07/2014"

# predict microclimate at 200 systematically placed points
# within black spruce range
res <- list(list())
for(i in 1:nrow(pts)){
  
  lon <- pts[[i, "xcoord"]]
  lat <- pts[[i, "ycoord"]]
  
  loc <- c(lon, lat)
  
  micro <- micro_ncep(loc = loc,
                      dstart = dstart,
                      dfinish = dfinish,
                      Usrhyt = 1.8,
                      minshade = 0, 
                      maxshade = 75,
                      runshade = 1)
  
  open <-
    as_tibble(micro$metout) %>% 
    group_by(DOY) %>% 
    summarise(tmax = max(TALOC), 
              tmin = min(TALOC), 
              tmean = mean(TALOC), 
              trange = max(TALOC) - min(TALOC)) %>% 
    add_column(cover = "open", 
               xcoord = lon, 
               ycoord = lat) %>% 
    pivot_longer(tmax:trange,
                 names_to = "variable",
                 values_to = "temp")
  
  point_micro <-
    as_tibble(micro$shadmet) %>%
    group_by(DOY) %>% 
    summarise(tmax = max(TALOC), 
              tmin = min(TALOC), 
              tmean = mean(TALOC), 
              trange = max(TALOC) - min(TALOC)) %>% 
    add_column(cover = "shade", 
               xcoord = lon, 
               ycoord = lat) %>% 
    pivot_longer(tmax:trange,
                 names_to = "variable",
                 values_to = "temp") %>% 
    full_join(open)
  
  res[[i]] <- point_micro
  
}

str(res)

all_results <- do.call(rbind, res)

# setwd(here::here("results"))
# write_csv(all_results, "nichemapr_black_spruce_microclimate_v02.csv")
