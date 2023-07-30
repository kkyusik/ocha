#' @title make road network data
#' @initialized 7-28-23
#' @description
#'      make road network with level 6 information
#' @update
#' 


library(tidyverse)
library(sf)
library(tmap)
library(data.table)
library(here)

level6_speed <- fread("data/raw/roadnetwork/2021년_충북,충남,세종,대전_평균속도_상세도로망Level6네트워크기준.csv")
head(level6_speed)

unique(level6_speed$timeslot)

speed <- level6_speed %>%
        filter(timeslot == "all") %>%
        select(-week_type, -timeslot)

# load all roads and make it one file
file_names <- c("chungbuk_level6_2021/chungbuk_level6_link_2021.shp",
                "chungnam_level6_2021/chungnam_level6_link_2021.shp",
                "sejong_level6_2021/sejong_level6_link_2021.shp",
                "daejeon_level6_2021/daejeon_level6_link_2021.shp")

road <- data.frame()
for (file_name in file_names){
        temp <- read_sf(here("data/raw/roadnetwork", file_name))
        road <- rbind.data.frame(road, temp)
}

tmap_save(tm = qtm(road), "output/explore/road_level6.png", dpi=300)


# link_id matching
# shapefile: 9 digits
# up_v_link: upward virtual link; dw_v_link: downward virtual link
# speed: 11 digits
speed_up <- speed %>% 
        mutate(up_v_link = as.double(lev6_link_id), velocity_up = velocity_AVRG) %>%
        select(up_v_link, velocity_up)
speed_dw <- speed %>% 
        mutate(dw_v_link = as.double(lev6_link_id), velocity_dw = velocity_AVRG) %>%
        select(dw_v_link, velocity_dw)

temp <- road %>% left_join(speed_up, by="up_v_link")
temp <- temp %>% left_join(speed_dw, by="dw_v_link")

temp <- temp %>%
        mutate(velocity = case_when(
                !is.na(velocity_up) & !is.na(velocity_dw) ~ (velocity_up+velocity_dw)/2,
                !is.na(velocity_up) & is.na(velocity_dw) ~ velocity_up,
                is.na(velocity_up) & !is.na(velocity_dw) ~ velocity_dw,
                TRUE ~ 0
        ))

# impute velocity based on road_rank and Ahn et al (2019).
# road_rank 101, 102: 100km
# road rank 103-106: 80 km
# road_rank 107: 60 km
# road_rank 108: 40 km

temp <- temp %>%
        mutate(speed_imputed = case_when(
                max_speed > 0 & velocity == 0 ~ max_speed,
                max_speed == 0 & velocity == 0 & road_rank %in% c(101, 102) ~ 100,
                max_speed == 0 & velocity == 0 & road_rank %in% 103:106 ~ 80,
                max_speed == 0 & velocity == 0 & road_rank == 107 ~ 60,
                max_speed == 0 & velocity == 0 & road_rank == 108 ~ 40,
                TRUE ~ 0
        ))

temp %>% filter(velocity == 0)

temp %>% select(velocity, speed_imputed)

qtm(temp %>% filter(velocity > 0), lines.col = "velocity")

# calculate time using velocity and speed_imputed
# length : km
# here, time will be minutes
# length * 60 / speed * weight
# here, if velocity exists, weight is 1.5, otherwise, 0.9

temp <- temp %>%
        mutate(travel_time = case_when(
                velocity > 0 ~ (length*60) / (velocity*1.5),
                speed_imputed > 0 ~ (length*60) / (speed_imputed*0.9)
        ))

# qtm(temp, lines.col = "travel_time")

road <- temp

# intersect with study area
area <- read_sf("data/tidy/study_area_buffer_sgg.gpkg")
road <- st_transform(road, st_crs(area))
road_area <- st_intersection(road, area)

m <- tm_shape(road_area) +
        tm_lines(col = "travel_time", breaks = c(0, 1, 4, 6)) +
        tm_layout(title = "Travel time (min)")

tmap_save(m, filename = "output/explore/travel_time_level6.png")


# export road network in study area
result <- road_area %>%
        select(link_id, road_rank, travel_time)

st_write(result, "data/tidy/road_network.gpkg", delete_layer = TRUE)
