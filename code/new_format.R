library(tidyverse)
library(maptools)
library(sp)
library(sf)

# read data ---------------------------------------------------------------

tract_shapes <-
  getKMLcoordinates(kmlfile = "https://opendata.arcgis.com/datasets/b78d164649ad487db0f2b20a9261fb8c_17.kml",
                    ignoreAltitude = TRUE) %>% 
  map(Polygon)

tract_info <- 
  read_csv(file = "https://opendata.arcgis.com/datasets/b78d164649ad487db0f2b20a9261fb8c_17.csv",
           col_types = cols(TRACTNO = col_character())) %>% 
  rename_all(tolower) %>%
  select(-area) %>% 
  rename(tract = tractno,
         area = areasqmi,
         pop = total, 
         child = age0_17,
         adult  = age18plus,
         houses = occupiedho,
         total = fagi_total_2005, 
         median = fagi_median_2005) %>% 
  select(tract, area,  total, median, pop, 
         child, adult, white, black,  hispanic) %>% 
  # normalize for population
  mutate(child = child / pop, 
         adult = adult / pop, 
         white = white / pop, 
         black = black / pop,  
         hispanic = hispanic / pop) %>%
  # compute population density
  mutate(density = pop / area)

bmp <- 
  read_csv(file = "https://opendata.arcgis.com/datasets/a973c2c7b7c14a918859f3e38bdffdd2_42.csv") %>% 
  rename_all(tolower)

return_tract <- function(row) {
  t <- rep(NA, 188)
  for (i in 1:188) {
    t[i] <- point.in.polygon(
      point.x = bmp$longitude[row],
      point.y = bmp$latitude[row],
      pol.x   = tract_shapes[[i]]@coords[, 1],
      pol.y   = tract_shapes[[i]]@coords[, 2]
    )
  }
  return(tract_info$tract[t == 1])
}

return_tract(2)
