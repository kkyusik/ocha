#' @title accessibility calculation
#' @initialized 7-28-23
#' @description
#'      calculate accessibility
#' @update
#' 

options(scipen = 10)

library(tidyverse)
library(sf)
library(tmap)
library(data.table)

area <- st_read("data/tidy/study_area_sgg.gpkg")
road <- st_read("data/tidy/road_network.gpkg")

area_buffer <- read_sf("data/tidy/study_area_buffer_sgg.gpkg")
area_buffer <- area_buffer %>%
        mutate(sido_cd = substr(SIGUNGU_CD, 1, 2))
area_buffer <- area_buffer %>% filter(!sido_cd %in% c("31", "37"))

fire <- st_read("data/tidy/poi_fire.gpkg")
ems <- st_read("data/tidy/poi_ems_beds.gpkg")
ems_bed <- ems %>% as_tibble %>% select(id, beds)

demand <- read_sf(file.path("data/tidy", "demand_grid.gpkg"))

# select demand within area_buffer
idx <- st_intersects(demand, area_buffer)
demand_intersected <- demand[which(lengths(idx) > 0), ]

demand <- demand_intersected
rm(demand_intersected)


# filtering cheongju
study_area <- st_read("data/tidy/study_area_sgg.gpkg")
idx <- st_intersects(demand, study_area)
demand_intersected <- demand[which(lengths(idx) > 0), ]

grid <- demand_intersected
rm(demand_intersected)


od_fire <- fread("data/tidy/odcost_grid_fire.csv")
od_ems <- fread("data/tidy/odcost_grid_ems.csv")

# average time

cost_fire_avg <- od_fire[, .(time_avg_fire = mean(time)), by=origin]

demand <- demand %>% left_join(cost_fire_avg, by=c("gid"="origin"))

# qtm(demand, fill="time_avg") +
# demand %>% filter(is.infinite(time_avg)) %>%
#         qtm(fill = "black") +
#         tm_shape(area) + tm_borders(col="black") +
#         tm_shape(road) + tm_lines() +
#         tm_shape(area_buffer) + tm_borders(col="black")

# fill missing values
demand <- demand %>%
        mutate(time_avg_fire = case_when(is.infinite(time_avg_fire) ~ NA, TRUE ~ time_avg_fire)) %>%
        fill(time_avg_fire, .direction="updown")

cost_ems_avg <- od_ems[, .(time_avg_ems = mean(time)), by=origin]
demand <- demand %>% left_join(cost_ems_avg, by=c("gid"="origin"))
demand <- demand %>%
        mutate(time_avg_ems = case_when(is.infinite(time_avg_ems) ~ NA, TRUE ~ time_avg_ems)) %>%
        fill(time_avg_ems, .direction="updown")


m1 <- tm_shape(demand) + tm_fill(col="time_avg_fire", palette = "-viridis") +
        tm_shape(area) + tm_borders(col = "gray40") +
        tm_credits("Average travel time to firestation", position = c("left", "bottom"))
m2 <- tm_shape(demand) + tm_fill(col="time_avg_ems", palette = "-viridis") +
        tm_shape(area) + tm_borders(col = "gray40") +
        tm_credits("Average travel time to hospital", position = c("left", "bottom"))
m <- tmap_arrange(m1, m2, ncol=2)
tmap_save(m, "output/explore/average_travel_time.png", dpi=300, width=9, height=5)


# filtering origin based on gid of demand
gid_list <- unique(demand$gid)
od_fire <- od_fire %>% filter(origin %in% gid_list)
od_ems <- od_ems %>% filter(origin %in% gid_list)

# firestation time decay function

fire_decay <- function(d, cutoff=4){
        if (d <= cutoff){
                w <- 1
        } else {
                w <- -0.09 * d + 1.36
        }
        return(w)
}

times_fire <- seq(0, 15, 0.1)
decay_list_fire <- c()
for (d in times_fire) {
        decay_list_fire <- c(decay_list_fire, fire_decay(d, 4))
        
}
plot(x = times_fire, y = decay_list_fire, type = "l")

# ems decay function
ems_decay <- function(d, beta=200){
        w <- exp(-1 * d^2 / beta)
        return(w)
}

times_ems <- seq(0, 30, 0.1)
decay_list_ems <- c()
for (d in times_ems){
        decay_list_ems <- c(decay_list_ems, ems_decay(d, 200))
}

plot(x = times_ems, y = decay_list_ems, type = "l")

# export plot
png("output/fig/decay_functions.png", width=9, height=4.5, units='in', res=300)
par(mfrow=c(1, 2), mar=c(5,5,4,2))
plot(x = times_fire, y = decay_list_fire, type="l", xlab="Travel time (min)", ylab="Weight", main="(a) Fire stations")
# axis(1, at=seq(0, 15, 1))
plot(x = times_ems, y = decay_list_ems, type="l", xlab="Travel time (min)", ylab="Weight", main="(b) EMS Hospitals")

dev.off()

# access to firestation

od_fire_w <- od_fire %>% 
        filter(time <= 15) %>%
        mutate(weight = sapply(time, fire_decay))

temp_demand <- demand %>% as_tibble %>% select(gid, demand, demand_ohca)
## calculate supply ratio
temp <- od_fire_w %>% left_join(temp_demand, by=c("origin"="gid"))

temp <- temp %>%
        mutate(w_demand = demand * weight, 
               w_demand_ohca = demand_ohca * weight) %>%
        group_by(destin) %>%
        summarise(w_demand = sum(w_demand),
                  w_demand_ohca = sum(w_demand_ohca))

temp$supply <- 1

temp <- temp %>%
        mutate(s_demand = supply/w_demand,
               s_demand_ohca = supply/w_demand_ohca)
rj_fire <- temp %>% select(destin, s_demand, s_demand_ohca)

## calulate Ai

temp <- od_fire_w %>%
        left_join(rj_fire, by="destin")
ai_fire <- temp %>%
        mutate(w_demand = weight * s_demand,
               w_demand_ohca = weight * s_demand_ohca) %>%
        group_by(origin) %>%
        summarise(fire_general = sum(w_demand),
                  fire_ohca = sum(w_demand_ohca))

# access to ems
od_ems_w <- od_ems %>% 
        filter(time <= 30) %>%
        mutate(weight = sapply(time, ems_decay))

temp_demand <- demand %>% as_tibble %>% select(gid, demand, demand_ohca)
## calculate supply ratio
temp <- od_ems_w %>% left_join(temp_demand, by=c("origin"="gid"))

temp <- temp %>%
        mutate(w_demand = demand * weight, 
               w_demand_ohca = demand_ohca * weight) %>%
        group_by(destin) %>%
        summarise(w_demand = sum(w_demand),
                  w_demand_ohca = sum(w_demand_ohca))

temp <- temp %>% left_join(ems_bed, by=c("destin"="id"))
temp$supply <- temp$beds
# temp$supply <- 1
temp <- temp %>%
        mutate(s_demand = supply/w_demand,
               s_demand_ohca = supply/w_demand_ohca)
rj_ems <- temp %>% select(destin, s_demand, s_demand_ohca)

## calculate Ai

temp <- od_ems_w %>%
        left_join(rj_ems, by="destin")
ai_ems <- temp %>%
        mutate(w_demand = weight * s_demand,
               w_demand_ohca = weight * s_demand_ohca) %>%
        group_by(origin) %>%
        summarise(ems_general = sum(w_demand),
                  ems_ohca = sum(w_demand_ohca))


# join access to grid
grid <- grid %>%
        left_join(ai_fire, by=c("gid"="origin")) %>%
        left_join(ai_ems, by=c("gid"="origin"))

grid <- grid %>%
        mutate(fire_general = case_when(is.na(fire_general) ~ 0, TRUE ~ fire_general),
               fire_ohca = case_when(is.na(fire_ohca) ~ 0, TRUE ~ fire_ohca))


# map of raw values--------------

palette <- tmaptools::get_brewer_pal("-RdBu", n = 4)
bound <- st_bbox(study_area)
m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "fire_general", title="Access (General)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_shape(fire) + tm_dots()+
        tm_credits("Access to fire stations of general population", position = c("left", "bottom"))
m_general
m_ohca <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "fire_ohca", title="Access (OHCA)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("Access to fire stations of estimated OHCA population", position = c("left", "bottom"))

m <- tmap_arrange(m_general, m_ohca, ncol=2)
m
tmap_save(m, "output/fig/access_fire.png", dpi=300, width=9, height=5)

m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "ems_general", title="Access (General)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_shape(fire) + tm_dots()+
        tm_credits("Access to EMS hospitals of general population", position = c("left", "bottom"))
m_ohca <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "ems_ohca", title="Access (OHCA)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("Access to EMS hospitals of estimated OHCA population", position = c("left", "bottom"))

m <- tmap_arrange(m_general, m_ohca, ncol=2)
m
tmap_save(m, "output/fig/access_ems.png", dpi=300, width=9, height=5)



# compute relative 2SFCA -------------------------
spar <- grid %>%
        mutate(fire_general_spar = fire_general / mean(fire_general, na.rm = T),
               fire_ohca_spar = fire_ohca / mean(fire_ohca, na.rm=T),
               ems_general_spar = ems_general / mean(ems_general, na.rm = T),
               ems_ohca_spar = ems_ohca / mean(ems_ohca, na.rm=T))

# map SPAR of fire

palette <- tmaptools::get_brewer_pal("-RdBu", n = 4)
bound <- st_bbox(study_area)
m_general <- tm_shape(spar, bbox = bound) +
        tm_fill(col = "fire_general_spar", palette=palette, breaks = c(0, 0.5, 1, 1.5, Inf), title="SPAR (General)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_shape(fire) + tm_dots()+
        tm_credits("SPAR to fire stations of general population", position = c("left", "bottom"))

m_ohca <- tm_shape(spar, bbox = bound) +
        tm_fill(col = "fire_ohca_spar", palette=palette, breaks = c(0, 0.5, 1, 1.5, Inf), title="SPAR (OHCA)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("SPAR to fire stations of estimated OHCA population", position = c("left", "bottom"))

m <- tmap_arrange(m_general, m_ohca, ncol=2)
tmap_save(m, "output/fig/spar_fire.png", dpi=300, width=9, height=5)

# map SPAR of ems
m_general <- tm_shape(spar, bbox = bound) +
        tm_fill(col = "ems_general_spar", palette=palette, breaks = c(0, 0.5, 1, 1.5, Inf), title="SPAR (General)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("SPAR to EMS hospitals of general population", position = c("left", "bottom"))

m_ohca <- tm_shape(spar, bbox = bound) +
        tm_fill(col = "ems_ohca_spar", palette=palette, breaks = c(0, 0.5, 1, 1.5, Inf), title="SPAR (OHCA)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("SPAR to EMS hospitals of estimated OHCA population", position = c("left", "bottom"))

m <- tmap_arrange(m_general, m_ohca, ncol=2)
tmap_save(m, "output/fig/spar_ems.png", dpi=300, width=9, height=5)

# export result
st_write(grid, "output/access_result.gpkg", delete_layer=TRUE)


# scaling: z-score

grid <- grid %>%
        mutate(fire_general_z = (fire_general - mean(fire_general)) / sd(fire_general),
               fire_ohca_z = (fire_ohca - mean(fire_ohca)) / sd(fire_ohca),
               ems_general_z = (ems_general - mean(ems_general)) / sd(ems_general),
               ems_ohca_z = (ems_ohca - mean(ems_ohca)) / sd(ems_ohca))


mean_value <- mean(grid$ems_general_z)
one_sd <- sd(grid$ems_general)

m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "ems_general_z", midpoint=NA,
                breaks = c(-Inf, 0-one_sd, 0 - one_sd/4, 0+one_sd/4, 0 + one_sd, Inf), 
                title="SPAR (General)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_shape(fire) + tm_dots()+
        tm_credits("SPAR to fire stations of general population", position = c("left", "bottom"))
m_general
# compute bivariate moran's I


hist(grid$fire_general_z)
hist(grid$ems_general_z)



# scaling: min-max

min_max_func <- function(x){
        res <- (x - min(x)) / (max(x)-min(x))
        return(res)
}

grid <- grid %>% 
        mutate(fire_general_mm = min_max_func(fire_general),
               fire_ohca_mm = min_max_func(fire_ohca), 
               ems_general_mm = min_max_func(ems_general),
               ems_ohca_mm = min_max_func(ems_ohca)) 

hist(grid$fire_general_mm)
hist(grid$ems_general_mm)


summary(grid$fire_general)
summary(grid$ems_general)

# sum between raw values ----------------
weight_fire <- 0.7
weight_ems <- 1-weight_fire

grid <- grid %>%
        mutate(acc_general = (fire_general*weight_fire + ems_general*weight_ems),
               acc_ohca = (fire_ohca*weight_fire + ems_ohca*weight_ems))

m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "acc_general", title="Total access (General)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("Access of general population", position = c("left", "bottom"))
m_general
m_ohca <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "acc_ohca",  title="Total access (OHCA)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("Access of estimated OHCA population", position = c("left", "bottom"))

m <- tmap_arrange(m_general, m_ohca, ncol=2)
m
tmap_save(m, paste0("output/fig/access_total_", weight_fire, "_", weight_ems, ".png"), dpi=300, width=9, height=5)


# gi --------------------

library(spdep)

# set spatial weights
wr <- knearneigh(st_centroid(grid), k=8, longlat = NULL, use_kd_tree=TRUE)
ws <- knn2nb(wr)
ws <- include.self(ws)
listw <- nb2listw(ws, style="W")

variables <- c("fire_general", "fire_ohca", "ems_general", "ems_ohca", "acc_general", "acc_ohca")
for (variable in variables){
        localg <- localG_perm(grid[[variable]], listw=listw, nsim=999)
        gi_variable <- paste0("gi_", variable)
        grid[[gi_variable]] <- localg
        
        grid <- grid %>%
                mutate(!!sym(gi_variable) := case_when(
                        !!sym(gi_variable) > 1.96 ~ 'High',
                        !!sym(gi_variable) < -1.96 ~ 'Low', 
                        TRUE ~ 'insig'))
        grid <- grid %>%
                mutate(!!sym(gi_variable) := factor(!!sym(gi_variable), levels = c("High", "Low", "insig"),
                                                         labels = c("High", "Low", "Not significant")))
}




gi_palette <- c('#b2182b', '#2166ac', '#f7f7f7')

# fire stations
m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "gi_fire_general", title="Gi*", palette=gi_palette) +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("Gi* of access to fire stations of general population", position = c("left", "bottom"))
m_ohca <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "gi_fire_ohca", title="Gi*", palette=gi_palette) +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("Gi* of access to fire stations of estimated OHCA population", position = c("left", "bottom"))
m <- tmap_arrange(m_general, m_ohca, ncol=2)
tmap_save(m, "output/fig/gi_fire_access.png", dpi=300, width=9, height=5)

# ems hospitals
m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "gi_ems_general", title="Gi*", palette=gi_palette) +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("Gi* of access to ems hospitals of general population", position = c("left", "bottom"))
m_ohca <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "gi_ems_ohca", title="Gi*", palette=gi_palette) +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("Gi* of access to ems hospitals of estimated OHCA population", position = c("left", "bottom"))
m <- tmap_arrange(m_general, m_ohca, ncol=2)
m
tmap_save(m, "output/fig/gi_ems_access.png", dpi=300, width=9, height=5)

# total access
m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "gi_acc_general", title="Gi*", palette=gi_palette) +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("Gi* of total access of general population", position = c("left", "bottom"))
m_ohca <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "gi_acc_ohca", title="Gi*", palette=gi_palette) +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_credits("Gi* of total access of estimated OHCA population", position = c("left", "bottom"))
m <- tmap_arrange(m_general, m_ohca, ncol=2)
m
tmap_save(m, paste0("output/fig/gi_access_total_", weight_fire, "_", weight_ems, ".png"), dpi=300, width=9, height=5)






