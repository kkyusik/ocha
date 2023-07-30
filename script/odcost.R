#' @title calculate od cost
#' @initialized 7-28-23
#' @description
#'      calculate accessibility
#' @update
#' 


library(tidyverse)
library(sf)
library(tmap)
library(sfnetworks)
library(tidygraph)
library(data.table)
library(rgdal)
library(rgeos)

demand <- read_sf(file.path("data/tidy", "demand_grid.gpkg"))
demand.sp <- as(demand, "Spatial")
demand.pt <- gCentroid(demand.sp, byid = TRUE)

fire <- read_sf("data/tidy/poi_fire.gpkg")
ems <- read_sf("data/tidy/poi_ems_beds.gpkg")
road <- read_sf("data/tidy/road_network.gpkg")

area <- read_sf("data/tidy/study_area_buffer_sgg.gpkg")

# check crs
st_crs(demand) == st_crs(fire)
st_crs(demand) == st_crs(ems)
st_crs(demand) == st_crs(road)

route <- st_cast(road, "LINESTRING")

# make network
net = as_sfnetwork(route, directed = FALSE) %>%
        # st_transform(3035) %>%
        activate("edges") %>%
        mutate(weight = travel_time)


# demand to fire

cost_mat <- st_network_cost(net, from = demand, to = fire, weights = "weight", direction = "out")

rownames(cost_mat) <- demand$gid
colnames(cost_mat) <- fire$id

cost.df <- reshape2::melt(cost_mat, c("origin", "destin"), value.name = "time")

fwrite(cost.df, "data/tidy/odcost_grid_fire.csv")



# demand to ems
cost_mat <- st_network_cost(net, from = demand, to = ems, weights = "weight", direction = "out")

rownames(cost_mat) <- demand$gid
colnames(cost_mat) <- ems$id

cost.df <- reshape2::melt(cost_mat, c("origin", "destin"), value.name = "time")
fwrite(cost.df, "data/tidy/odcost_grid_ems.csv")
