#' @title validation
#' @initialized 7-29-23
#' @description
#'      valid accessibility result
#' @update
#' 

library(tidyverse)
library(sf)
library(tmap)
library(data.table)

access <- st_read("output/access_result.gpkg")
fire <- st_read("data/tidy/poi_fire.gpkg")
ems <- st_read("data/tidy/poi_ems_beds.gpkg")
study_area <- st_read("data/tidy/study_area_sgg.gpkg")
fire_cost <- fread("data/tidy/odcost_grid_fire.csv")
ems_cost <- fread("data/tidy/odcost_grid_ems.csv")

grid <- access %>% select(gid)

fire_cost <- fire_cost %>%
        filter(time <= 15) %>%
        group_by(origin) %>%
        summarise(time_avg_fire = mean(time))

ems_cost <- ems_cost %>%
        filter(time <= 30) %>%
        group_by(origin) %>%
        summarise(time_avg_ems = mean(time))

grid <- grid %>% left_join(fire_cost, by=c("gid"="origin")) %>% left_join(ems_cost, by=c("gid"="origin"))

tm_shape(grid, bbox = st_bbox(study_area))+
        tm_fill(col="time_avg_fire", breaks = seq(0, 16, 4)) +
        tm_shape(study_area) + tm_borders(col="gray40")

tm_shape(grid, bbox = st_bbox(study_area))+
        tm_fill(col="time_avg_ems", breaks = seq(0, 30, 5)) +
        tm_shape(study_area) + tm_borders(col="gray40")


# histogram of average travel time ----------
png("output/fig/avg_traveltime.png", width=9, height=4.5, units='in', res=300)
par(mfrow=c(1, 2), mar=c(5,5,4,2))
hist(grid$time_avg_fire, 
     main = "Average travel time to fire stations", 
     xlab = "Average travel time (min)", 
     ylab = "Frequency", ylim = c(0, 300))
hist(grid$time_avg_ems, 
     main = "Average travel time to EMS hospitals", 
     xlab = "Average travel time (min)", 
     ylab = "Frequency", ylim = c(0, 250))
dev.off()


demand <- st_read("data/tidy/demand_grid.gpkg")

demand <- demand %>% filter(gid %in% unique(access$gid))

tm_shape(demand, bbox = st_bbox(study_area))+
        tm_fill(col="demand") +
        tm_shape(study_area) + tm_borders(col="gray40")

tm_shape(demand, bbox = st_bbox(study_area))+
        tm_fill(col="demand_ohca") +
        tm_shape(study_area) + tm_borders(col="gray40")
