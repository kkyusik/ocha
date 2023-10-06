#' @title produce
#' @initialized 7-30-23
#' @description
#'      produce figures and tables
#' @update
#' 

library(tidyverse)
library(sf)
library(tmap)
library(data.table)
library(openxlsx)

# OHCA likelihood -----------------------------

ohca <- fread("output/tab/ohca_likelihood.csv")
ohca <- ohca %>%
        filter(grepl("청주", name))
ohca <- ohca %>%
        mutate(ohca_10k = ohca_ratio * 10000)

temp <- ohca %>%
        select(name, sex, ohca_10k)
temp <- temp %>%
        spread(name, ohca_10k)
temp %>% separate(sex, into = c("연령", "성별"), sep=" ")

man <- temp %>% filter(grepl("남자", sex)) %>% separate(sex, into = c("연령", "성별"), sep=" ")
man <- man %>% rename(남자=청주시) %>% select(-성별)

woman <- temp %>% filter(grepl("여자", sex)) %>% separate(sex, into = c("연령", "성별"), sep=" ")
woman <- woman %>% rename(여자=청주시) %>% select(여자)

result <- cbind.data.frame(man, woman)

round_func <- function(x) round(x, 3)
result <- result %>%
        mutate_if(is.numeric, round_func)

# result <- result %>% rename(성별및연령 = sex)

write.xlsx(result, "output/tab/ohca_likelihood.xlsx", overwrite=TRUE)


# average and minimum travel time ---------------------------

od_fire <- fread("data/tidy/odcost_grid_fire.csv")
od_ems <- fread("data/tidy/odcost_grid_ems.csv")

access <- st_read("output/access_result.gpkg")

grid <- access %>% select(gid)

od_fire <- od_fire %>%
        filter(time <= 15)
od_fire <- od_fire %>%
        group_by(origin) %>%
        summarise(fire_avg_time = mean(time),
                  fire_min_time = min(time))

od_ems <- od_ems %>%
        filter(time <= 30) %>%
        group_by(origin) %>%
        summarise(ems_avg_time = mean(time), 
                  ems_min_time = min(time))


grid <- grid %>%
        left_join(od_fire, by=c("gid"="origin")) %>%
        left_join(od_ems, by=c("gid"="origin"))

# histogram of average travel time
png("output/fig/avg_traveltime.png", width=9, height=4.5, units='in', res=300)
par(mfrow=c(1, 2), mar=c(5,5,4,2))
hist(grid$fire_avg_time, 
     main = "Average travel time from EMS", 
     xlab = "Average travel time (min)", 
     ylab = "Frequency")
hist(grid$ems_avg_time, 
     main = "Average travel time to hospitals", 
     xlab = "Average travel time (min)", 
     ylab = "Frequency")
dev.off()

# histogram of min travel time
png("output/fig/min_traveltime.png", width=9, height=4.5, units='in', res=300)
par(mfrow=c(1, 2), mar=c(5,5,4,2))
hist(grid$fire_min_time, 
     main = "Travel time from the nearest EMS", 
     xlab = "Travel time (min)", 
     ylab = "Frequency")
hist(grid$ems_min_time, 
     main = "Travel time to the nearest hospitals", 
     xlab = "Travel time (min)", 
     ylab = "Frequency", breaks=16)
dev.off()

# buffered area information ----------------------------------

# demand grid
study_area <- st_read("data/tidy/study_area_sgg.gpkg")
qtm(study_area)
study_area_union <- st_union(study_area)
qtm(study_area_union)
buffer_sgg <- st_read("data/tidy/study_area_buffer_sgg.gpkg")
qtm(buffer_sgg)

demand <- read_sf(file.path("data/tidy", "demand_grid.gpkg"))
qtm(demand) + qtm(buffer_sgg)

fire <- st_read("data/tidy/poi_fire.gpkg")
hospital <- st_read("data/tidy/poi_ems_beds.gpkg")

qtm(buffer_sgg) + qtm(fire)
qtm(buffer_sgg) + qtm(hospital)

road <- st_read("data/tidy/road_network.gpkg")
# qtm(road)

# time to speed
# distance/time*60
road$length <- as.numeric(units::set_units(st_length(road), "km"))
road <- road %>% mutate(velocity = length/travel_time*60)
# setting
bounding <- st_bbox(buffer_sgg)
background_maps <- tm_shape(buffer_sgg) + tm_borders(col = "gray20", lwd = .5) +
        tm_shape(study_area_union) + tm_borders(col="red2", lwd=1.5)

# emd
dong <- st_read("data/raw/bnd_dong_00_2021_2021/bnd_dong_00_2021_2021_2Q.shp") 
dong.ppp <- st_centroid(dong)
idx <- st_intersects(dong.ppp, buffer_sgg %>% filter(SIGUNGU_NM == "청주시"))
selected <- dong.ppp[which(lengths(idx) > 0),]

dong <- dong %>% filter(ADM_DR_CD %in% selected$ADM_DR_CD)

tm_shape(dong) + tm_polygons() + tm_text(text="ADM_DR_NM", size = .8, remove.overlap = T, shadow = T, fontface = 'bold')

xy <- st_bbox(dong)
xy[1] <- xy$xmin + 12000
xy[3] <- xy$xmax - 18000
xy[2] <- xy$ymin + 18000
xy[4] <- xy$ymax - 10000
        
tm_shape(dong, bbox=xy) + tm_polygons() + tm_text(text="ADM_DR_NM", size=.6)

# population
g1 <- tm_shape(demand %>% filter(demand>0), bbox = bounding) +
        tm_fill(col="demand", 
                style="quantile", n=5,
                # breaks = c(-Inf, 100,  500, 750, 1000, 2000, Inf),
                palette="viridis", title = "General population") +
        tm_shape(demand %>% filter(demand == 0)) + tm_fill(col="gray90") +
        background_maps +
        tm_layout(title = "(a) General population", frame = FALSE, legend.position = c("left", "bottom"))
g2 <- tm_shape(demand %>% filter(demand_ohca>0), bbox = bounding) +
        tm_fill(col="demand_ohca", 
                style="quantile", n=5,
                # breaks = c(-Inf, 100,  500, 750, 1000, 2000, Inf),
                palette="viridis", title = "OHCA population") +
        tm_shape(demand %>% filter(demand == 0)) + tm_fill(col="gray90") +
        background_maps +
        tm_layout(title = "(b) OHCA population", frame = FALSE, legend.position = c("left", "bottom"))
g2
# road
r <- tm_shape(road, bbox=bounding) +
        tm_lines(col = "velocity", breaks=c(-Inf, 40, 80, 100, Inf), palette = "viridis", title.col = "Speed (km/h)") +
        background_maps +
        tm_layout(title = "(c) Road speed (km/h)", frame = FALSE, legend.position = c("left", "bottom"))

# EMS locations
s1 <- tm_shape(fire, bbox=bounding) +
        tm_dots(col = "blue1", size=.1) +
        background_maps +
        tm_layout(title = "(d) EMS locations", frame = FALSE) +
        tm_compass(position = c("left", "bottom")) +
        tm_scale_bar(position = c("left", "bottom"))

# Hospital locations
hospital$Beds <- hospital$beds
s2 <- background_maps +
        tm_shape(hospital, bbox=bounding) +
        tm_bubbles(size="Beds") +
        tm_layout(title = "(e) Hospital locations", frame = FALSE, legend.position = c("left", "bottom"))

name <- tm_shape(dong) + 
        tm_polygons(col="gray90") + 
        tm_text(text="ADM_DR_NM", size = .8, remove.overlap = T, shadow = T, fontface = 'bold') +
        tm_compass(position = c("right", "bottom")) +
        tm_scale_bar(position = c("right", "bottom"), breaks=c(0, 5, 10))+
        tm_layout(title = "(f) Place names", frame = FALSE)

m <- tmap_arrange(g1, g2, r, s1, s2, name, nrow=2)
tmap_save(m, "output/fig/data_used.png", dpi=300, width=15, height=10, units="in")
#


grid_percent_within_4_min_fire <- grid %>% filter(fire_min_time <= 4) %>% nrow() / nrow(grid) * 100
summary(grid$fire_min_time)
grid_percent_within_5_min_ems <- grid %>% filter(ems_min_time <= 5) %>% nrow() / nrow(grid) * 100

library(openxlsx)
xlsx <- read.xlsx(xlsxFile = file.path("data/raw", "cardiac_data", "ohca_21.xlsx"), sheet = 1)
cardiac <- xlsx %>% select(H_ADMINCODE, H_ADD_CITY, H_ADD_DIST, P_ADMINCODE, P_ADD_CITY, P_ADD_DIST, H_SEX, AGE)
cardiac <- cardiac %>% 
        mutate_if(is.character, str_trim)
# 만약 P_AMINDCODE가 NA면, 환자의 거주지 미상이므로, 병원 정보를 대신 입력함. 

cardiac <- cardiac %>%
        mutate(
                P_ADD_CITY = case_when(is.na(P_ADMINCODE) ~ H_ADD_CITY,
                                       TRUE ~ P_ADD_CITY),
                P_ADD_DIST = case_when(is.na(P_ADMINCODE) ~ H_ADD_DIST,
                                       TRUE ~ P_ADD_DIST),
                P_ADMINCODE = case_when(is.na(P_ADMINCODE) ~ H_ADMINCODE,
                                        TRUE ~ P_ADMINCODE))
cardiac <- cardiac %>%
        select(P_ADMINCODE, P_ADD_CITY, P_ADD_DIST, H_SEX, AGE) %>%
        as_tibble

cardiac <- cardiac %>%
        filter(P_ADD_DIST == "청주시 서원구")

cardiac <- cardiac %>%
        mutate(age_group = case_when(AGE %in% 20:29 ~ "20대", 
                                     AGE %in% 30:39 ~ "30대",
                                     AGE %in% 40:49 ~ "40대",
                                     AGE %in% 50:59 ~ "50대",
                                     AGE %in% 60:69 ~ "60대",
                                     AGE %in% 70:79 ~ "70대",
                                     AGE %in% 80:89 ~ "80대",
                                     AGE >= 90 ~ "90대",
                                     # AGE >= 100 ~ "100세 이상",
                                     TRUE ~ "none"))
cardiac %>% filter(age_group == "none")

cardiac <- cardiac %>%
        mutate(sex = case_when(H_SEX == 1 ~ paste(age_group, "남자"),
                               H_SEX == 2 ~ paste(age_group, "여자"),
                               TRUE ~ "error"))
cardiac
cardiac %>% filter(sex == "20대 여자")
