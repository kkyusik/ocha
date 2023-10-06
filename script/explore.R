# explore

library(tidyverse)
library(tmap)
library(sf)


demand <- read_sf(file.path("data/tidy", "demand_grid.gpkg"))
demand.pt <- st_centroid(demand)
fire <- read_sf("data/tidy/poi_fire.gpkg")
ems <- st_read("data/tidy/poi_ems_beds.gpkg")
road <- read_sf("data/tidy/road_network.gpkg")
area <- read_sf("data/tidy/study_area_buffer_sgg.gpkg")
studyarea <- read_sf("./data/tidy/study_area_sgg.gpkg")

m1 <- tm_shape(area) + tm_borders(col = "red") +
        tm_shape(demand) + tm_fill(col = "demand") +
        tm_credits("Data: total demand", position = c("left", "bottom"))
# tmap_save(m, "output/explore/grid_total_demand.png", dpi=300)

m2 <- tm_shape(area) + tm_borders(col = "red") +
        tm_shape(demand) + tm_fill(col = "demand_ohca") +
        tm_credits("Data: ohca demand", position = c("left", "bottom"))
# tmap_save(m, "output/explore/grid_ohca_demand.png", dpi=300)

m <- tmap_arrange(m1, m2, ncol=2)
tmap_save(m, "output/explore/grid_demand.png", dpi=300, height=5, width=9)


qtm(fire)

m <- tm_shape(area) + tm_borders(col = "red") +
        tm_shape(studyarea) + tm_borders(col="gray30")+
        tm_shape(fire) + tm_dots() +
        tm_credits("Data: firestations", position = c("left", "bottom"))
m
tmap_save(m,"output/explore/firestation.png", dpi=300)

m <- tm_shape(area) + tm_borders(col = "red") +
        tm_shape(studyarea) + tm_borders(col="gray30")+
        tm_shape(ems) + tm_bubbles(size="beds") +
        tm_credits("Data: ems beds", position = c("left", "bottom"))
m
tmap_save(m,"output/explore/ems.png", dpi=300)
hist(ems$beds)
