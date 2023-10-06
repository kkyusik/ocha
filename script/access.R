#' @title accessibility calculation
#' @initialized 7-28-23
#' @description
#'      calculate accessibility
#' @update
#'      07-31-23: fire access * demand = demand for hospitals
#'      08-01-23: add minimized travel time calculation

options(scipen = 10)

library(tidyverse)
library(sf)
library(tmap)
library(data.table)

area <- st_read("data/tidy/study_area_sgg.gpkg")
road <- st_read("data/tidy/road_network.gpkg")

# emd
dong <- st_read("data/raw/bnd_dong_00_2021_2021/bnd_dong_00_2021_2021_2Q.shp") 
dong <- st_centroid(dong)
dong$name <- dong$ADM_DR_NM


area_buffer <- read_sf("data/tidy/study_area_buffer_sgg.gpkg")
# area_buffer <- area_buffer %>%
#         mutate(sido_cd = substr(SIGUNGU_CD, 1, 2))
# area_buffer <- area_buffer %>% filter(!sido_cd %in% c("31", "37"))
# st_write(area_buffer, "data/tidy/study_area_buffer_sgg.gpkg", delete_layer=TRUE)

fire <- st_read("data/tidy/poi_fire.gpkg")
ems <- st_read("data/tidy/poi_ems_beds.gpkg")
ems_bed <- ems %>% as_tibble %>% select(id, beds)

demand <- read_sf(file.path("data/tidy", "demand_grid.gpkg"))

# select demand within area_buffer
idx <- st_intersects(demand, area_buffer)
demand_intersected <- demand[which(lengths(idx) > 0), ]



demand <- demand_intersected
rm(demand_intersected)


# Check 
qtm(area_buffer)
qtm(area_buffer) + qtm(fire) + qtm(ems)
qtm(area)
qtm(demand)

sink("report/data_information.txt")
cat("Number of firestation", nrow(fire), "\n")
cat("Number of hospitals", nrow(ems), "\n")
cat("Number of grids", nrow(demand))
sink()

# adjust demand_ohca according to total population ---------
# needs sgg

# demand.ppp <- st_centroid(demand)
# demand.sgg <- st_join(demand.ppp, area_buffer)
# 
# demand.w <- demand.sgg %>%
#         as_tibble %>%
#         group_by(SIGUNGU_NM) %>%
#         summarise(demand_pop = sum(demand),
#                   demand_ohca_pop = sum(demand_ohca))
# 
# demand.w$weight <- demand.w$demand_pop/demand.w$demand_ohca_pop
# 
# demand.sgg <- demand.sgg %>% left_join(demand.w, by="SIGUNGU_NM")
# demand.sgg <- demand.sgg %>%
#         mutate(demand_ohca = demand_ohca * weight)
# 
# demand.sgg %>%
#         as_tibble %>%
#         group_by(SIGUNGU_NM) %>%
#         summarise(demand = sum(demand),
#                   ohca = sum(demand_ohca))
# 
# demand <- demand %>%
#         select(gid) %>%
#         left_join(demand.sgg %>% as_tibble %>% select(gid, demand, demand_ohca), by="gid")
# 
# st_write(demand, "data/tidy/demand_grid.gpkg", delete_layer = TRUE)

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


# demand mapping----------------

# palette <- tmaptools::get_brewer_pal("-RdBu", n = 4)
bound <- st_bbox(study_area)
m_general <- tm_shape(demand, bbox = bound) +
        tm_fill(col = "demand", title="Access (General)") +
        tm_shape(study_area) + tm_borders(col = "gray40")
m_general
m_ohca <- tm_shape(demand, bbox = bound) +
        tm_fill(col = "demand_ohca", title="Access (OHCA)") +
        tm_shape(study_area) + tm_borders(col = "gray40") 

m <- tmap_arrange(m_general, m_ohca, ncol=2)
m
tmap_save(m, "output/fig/access_fire.png", dpi=300, width=9, height=5)




# filtering origin based on gid of demand
gid_list <- unique(demand$gid)
od_fire <- od_fire %>% filter(origin %in% gid_list)
od_ems <- od_ems %>% filter(origin %in% gid_list)




# minimum travel time from EMS to Place to Hospital ------------------------

min_time_fire <- od_fire %>% group_by(origin) %>% summarise(min_time_fire = min(time))
min_time_hospital <- od_ems %>% group_by(origin) %>% summarise(min_time_hospital = min(time))

grid <- grid %>% 
        left_join(min_time_fire, by=c("gid"="origin")) %>% 
        left_join(min_time_hospital, by=c("gid"="origin")) %>%
        mutate(min_time_total = min_time_fire + min_time_hospital)


m1 <- tm_shape(grid, bbox=bound) + 
        tm_fill(col="min_time_fire", palette = "viridis",
                breaks = c(-Inf, 4, 10, 15, Inf), 
                title="Shortest time (min)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_layout(title="(a) Shortest time from a EMS", frame=F) +
        tm_compass(position = c("left", "bottom")) +
        tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 5, 10), text.size = 0.6)
m1
m2 <- tm_shape(grid, bbox=bound) +
        tm_fill(col="min_time_hospital",palette = "viridis",
                breaks=c(-Inf, 5, 10, 15, Inf), 
                title="Shortest time (min)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_layout(title="(b) Shortest time to a hospital", frame=F)
m2
m3 <- tm_shape(grid, bbox=bound) +
        tm_fill(col="min_time_total",palette = "viridis",
                breaks=c(-Inf, 5, 10, 15, 20, 25, Inf), 
                title="Shortest time (min)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_layout(title="(c) Total time (EMS + hospital)", frame=F)
m3

m <- tmap_arrange(m1, m2, m3, ncol=3)
tmap_save(m, "output/fig/shortest_time.png", width=12, height=5, dpi=300)


# box plots
png("output/fig/boxplot_shortest_time.png", width=9, height=3.5, units='in', res=300)
par(mfrow=c(1, 3), mar=c(5,5,4,2))
boxplot(grid$min_time_fire, 
        xlab="Shortest time from EMS (min)", 
        main=paste0("Mean: ", round(mean(grid$min_time_fire), 2), 
                    "\nSD: ", round(sd(grid$min_time_fire),2)))
boxplot(grid$min_time_hospital, 
        xlab="Shortest time to hospital (min)", 
        main=paste0("Mean: ", round(mean(grid$min_time_hospital), 2), 
                    "\nSD: ", round(sd(grid$min_time_hospital),2)))
boxplot(grid$min_time_total, 
        xlab="Total time (EMS + hospital) (min)", 
        main=paste0("Mean: ", round(mean(grid$min_time_total), 2), 
                    "\nSD: ", round(sd(grid$min_time_total),2)))
dev.off()




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
plot(x = times_fire, y = decay_list_fire, type="l", xlab="Travel time (min)", ylab="Weight", main="(a) EMS")
# axis(1, at=seq(0, 15, 1))
plot(x = times_ems, y = decay_list_ems, type="l", xlab="Travel time (min)", ylab="Weight", main="(b) Hospitals")

dev.off()

# access to firestation ------------------

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

# # demand adjustment
# temp_demand <- temp_demand %>%
#         left_join(ai_fire, by=c("gid"="origin")) %>%
#         mutate(demand = demand * fire_general,
#                demand_ohca = demand_ohca * fire_ohca)

## calculate supply ratio
temp <- od_ems_w %>% left_join(temp_demand, by=c("origin"="gid"))

temp %>% filter(is.na(demand))

temp <- temp %>%
        mutate(w_demand = demand * weight, 
               w_demand_ohca = demand_ohca * weight) %>%
        group_by(destin) %>%
        summarise(w_demand = sum(w_demand, na.rm=T),
                  w_demand_ohca = sum(w_demand_ohca, na.rm=T))

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
        summarise(ems_general = sum(w_demand, na.rm=T),
                  ems_ohca = sum(w_demand_ohca, na.rm=T))

ai_ems %>% filter(is.na(ems_general))
# join access to grid
grid <- grid %>%
        left_join(ai_fire, by=c("gid"="origin")) %>%
        left_join(ai_ems, by=c("gid"="origin"))

grid <- grid %>%
        mutate(ems_general = case_when(is.na(fire_general) ~ NA, TRUE ~ ems_general),
               ems_ohca = case_when(is.na(fire_ohca) ~ NA, TRUE ~ ems_ohca))

# grid <- grid %>%
#         mutate(fire_general = case_when(is.na(fire_general) ~ 0, TRUE ~ fire_general),
#                fire_ohca = case_when(is.na(fire_ohca) ~ 0, TRUE ~ fire_ohca))


# # map ems demand ----------------------
# temp <- grid %>% select(gid) %>% left_join(temp_demand, by="gid")
# bound <- st_bbox(study_area)
# m_general <- tm_shape(temp, bbox = bound) +
#         tm_fill(col = "demand", title="Weighted demand (General)", breaks=c(-Inf, 0.05, 0.1, 0.15, Inf), palette = "viridis") +
#         tm_shape(study_area) + tm_borders(col = "gray40")
# m_general
# m_ohca <- tm_shape(temp, bbox = bound) +
#         tm_fill(col = "demand_ohca", title="Weighted demand (OHCA)", breaks=c(-Inf, 0.05, 0.1, 0.15, Inf), palette = "viridis") +
#         tm_shape(study_area) + tm_borders(col = "gray40")
# 
# m <- tmap_arrange(m_general, m_ohca, ncol=2)
# m
# tmap_save(m, "output/fig/demand_for_hospital_access.png", dpi=300, width=9, height=5)



# map of raw values--------------

palette <- tmaptools::get_brewer_pal("-RdBu", n = 4)
bound <- st_bbox(study_area)
m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "fire_general", title="Access (General)", palette = "viridis",
                textNA = "Inaccessible",
                # breaks=c(-Inf, 0.000025, 0.000100, 0.000200, Inf),
                # labels = c("Less than 0.000025", "0.00025 to 0.0001",
                #            "0.0001 to 0.0002", "0.0002 or more", "Missing"),
                labels = c("1th quintile", "2nd quintile", "3rd quintile",
                           "4th quintile", "5th quintile"),
                style="quantile", n=5) +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        # tm_shape(filter(dong, grepl("오송|미원", name))) + tm_text(text="name", size = .8, shadow = T, fontface = 'bold') +
        tm_layout(title = "(a) Access to EMS (General)", frame=F) +
        tm_compass(position = c("left", "bottom")) +
        tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 5, 10), text.size = 0.6)
m_general
m_ohca <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "fire_ohca", title="Access (OHCA)", palette = "viridis",
                textNA = "Inaccessible",
                # breaks=c(-Inf, 0.000025, 0.0001, 0.0002, Inf), 
                # labels = c("Less than 0.000025", "0.00025 to 0.0001",
                           # "0.0001 to 0.0002", "0.0002 or more", "Missing"),
                labels = c("1th quintile", "2nd quintile", "3rd quintile",
                           "4th quintile", "5th quintile"),
                style="quantile", n=5) +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        # tm_shape(filter(dong, grepl("오송|미원", name))) + tm_text(text="name", size = .8, shadow = T, fontface = 'bold') +
        tm_layout(title = "(a) Access to EMS (OHCA)", frame=F)

m <- tmap_arrange(m_general, m_ohca, ncol=2)
m
tmap_save(m, "output/fig/access_fire.png", dpi=300, width=9, height=5)

m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "ems_general", title="Access (General)", palette = "viridis",
                textNA = "Inaccessible",
                # breaks=c(-Inf, 0.0001, 0.0002, 0.0003, Inf),
                labels = c("1th quintile", "2nd quintile", "3rd quintile",
                           "4th quintile", "5th quintile"),
                style="quantile", n=5) +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        # tm_shape(filter(dong, grepl("오송", name))) + tm_text(text="name", size = .8, shadow = T, fontface = 'bold') +
        tm_layout(title = "(a) Access to hospital (General)", frame=F) +
        tm_compass(position = c("left", "bottom")) +
        tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 5, 10), text.size = 0.6)
m_ohca <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "ems_ohca", title="Access (OHCA)", palette = "viridis",
                textNA = "Inaccessible",
                # breaks=c(-Inf, 0.0001, 0.0002, 0.0003, Inf),
                labels = c("1th quintile", "2nd quintile", "3rd quintile",
                           "4th quintile", "5th quintile"),
                style="quantile", n=5) +
        # tm_shape(filter(dong, grepl("오송", name))) + tm_text(text="name", size = .8, shadow = T, fontface = 'bold') +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_layout(title = "(a) Access to hospital (OHCA)", frame=F)

m <- tmap_arrange(m_general, m_ohca, ncol=2)
m
tmap_save(m, "output/fig/access_ems.png", dpi=300, width=9, height=5)

# # Differences ---------------
# grid <- grid %>% mutate(diff_acc = ems_ohca - ems_general, diff_acc_ratio = (ems_ohca/ems_general*100)-100)
# m_diff <- tm_shape(grid, bbox = bound) +
#         tm_fill(col = "diff_acc", title="Access (OHCA)", midpoint = 0) +
#         tm_shape(study_area) + tm_borders(col = "gray40")
# m_diff
# 
# palette <- tmaptools::get_brewer_pal("-RdBu", n = 6)
# m_diff <- tm_shape(grid, bbox = bound) +
#         tm_fill(col = "diff_acc_ratio", title="Comparison\nOHCA/General (%)", midpoint = 0, 
#                 palette = palette,
#                 breaks=c(-Inf, -0.75, -0.5, 0, 0.5, 0.75, Inf)) +
#         tm_shape(study_area) + tm_borders(col = "gray40") +
#         tm_layout(frame=F) +
#         tm_compass(position = c("left", "bottom")) +
#         tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 5, 10), text.size = 0.6)
# m_diff
# 
# tmap_save(m_diff, "output/fig/access_total_difference.png", dpi=300, width=6, height=6)

# compute relative 2SFCA (SPAR) -------------------------
grid <- grid %>%
        mutate(fire_general_spar = fire_general / mean(fire_general, na.rm = T),
               fire_ohca_spar = fire_ohca / mean(fire_ohca, na.rm=T),
               ems_general_spar = ems_general / mean(ems_general, na.rm = T),
               ems_ohca_spar = ems_ohca / mean(ems_ohca, na.rm=T))

# map SPAR of fire

palette <- tmaptools::get_brewer_pal("-RdBu", n = 6)
bound <- st_bbox(study_area)
m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "fire_general_spar", palette=palette, 
                textNA = "Inaccessible",
                breaks = c(-Inf, 0.5, 0.75, 1, 1.25, 1.5, Inf), title="SPAR (General)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        # tm_shape(filter(dong, grepl("오송|미원|낭성|가덕|문의", name))) + tm_text(text="name", size = .8, shadow = T, fontface = 'bold') +
        tm_layout(title = "(a) SPAR to EMS (General)", frame=F) +
        tm_compass(position = c("left", "bottom")) +
        tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 5, 10), text.size = 0.6)

m_ohca <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "fire_ohca_spar", palette=palette, 
                textNA = "Inaccessible",
                breaks = c(-Inf, 0.5, 0.75, 1, 1.25, 1.5, Inf), title="SPAR (OHCA)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        # tm_shape(filter(dong, grepl("오송|미원|낭성|가덕|문의", name))) + tm_text(text="name", size = .8, shadow = T, fontface = 'bold') +
        tm_layout(title = "(b) SPAR to EMS (OHCA)", frame=F)

m <- tmap_arrange(m_general, m_ohca, ncol=2)
m
tmap_save(m, "output/fig/spar_fire.png", dpi=300, width=9, height=5)

# map SPAR of ems
m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "ems_general_spar", palette=palette, 
                textNA = "Inaccessible",
                breaks = c(-Inf, 0.5, 0.75, 1, 1.25, 1.5, Inf), title="SPAR (General)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        # tm_shape(filter(dong, grepl("미원", name))) + tm_text(text="name", size = .8, shadow = T, fontface = 'bold') +
        tm_layout(title = "(a) SPAR to hospital (General)", frame=F) +
        tm_compass(position = c("left", "bottom")) +
        tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 5, 10), text.size = 0.6)

m_ohca <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "ems_ohca_spar", palette=palette, 
                textNA = "Inaccessible",
                breaks = c(-Inf, 0.5, 0.75, 1, 1.25, 1.5, Inf), title="SPAR (OHCA)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        # tm_shape(filter(dong, grepl("미원", name))) + tm_text(text="name", size = .8, shadow = T, fontface = 'bold') +
        tm_layout(title = "(b) SPAR to hospital (OHCA)", frame=F)

m <- tmap_arrange(m_general, m_ohca, ncol=2)
m
tmap_save(m, "output/fig/spar_ems.png", dpi=300, width=9, height=5)


# scaling: min-max

min_max_func <- function(x){
        res <- (x - min(x, na.rm=T)) / (max(x, na.rm=T)-min(x, na.rm=T))
        return(res)
}

grid <- grid %>% 
        mutate(fire_general_mm = min_max_func(fire_general),
               fire_ohca_mm = min_max_func(fire_ohca), 
               ems_general_mm = min_max_func(ems_general),
               ems_ohca_mm = min_max_func(ems_ohca)) %>%
        mutate(acc_general_mm = fire_general_mm + ems_general_mm,
               acc_ohca_mm = fire_ohca_mm + ems_ohca_mm)

# minmax - raw values
m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "acc_general_mm", title="Total access (General)", palette = "viridis",
                style = "quantile", n=5, textNA = "Inaccessible",
                labels = c(
                        "1st quintile (0.012 to 0.486)",
                        "2nd quintile (0.486 to 0.625)",
                        "3rd quintile (0.625 to 0.783)",
                        "4th quintile (0.783 to 0.914)",
                        "5th quintile (0.914 to 1.267)",
                        "Inaccessible"
                )
                # breaks=c(-Inf, 0.5, 0.75, 1, 1.25, 1.5, Inf),
                # breaks=c(-Inf, 0.25, 0.5, 0.75, 1, 1.25, Inf)
                ) +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        # tm_shape(filter(dong, grepl("미원|오송|오창|낭성|가덕|문의", name))) + tm_text(text="name", size = .8, shadow = T, fontface = 'bold') +
        tm_layout(title = "(a) Total access (General)", frame=F) +
        tm_compass(position = c("left", "bottom")) +
        tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 5, 10), text.size = 0.6)
m_general
m_ohca <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "acc_ohca_mm",  title="Total access (OHCA)", palette = "viridis", 
                style = "quantile", n=5, textNA = "Inaccessible",
                labels = c(
                        "1st quintile (0.051 to 0.581)",
                        "2nd quintile (0.581 to 0.767)",
                        "3rd quintile (0.767 to 0.939)",
                        "4th quintile (0.939 to 1.089)",
                        "5th quintile (1.089 to 1.437)"
                )
                # breaks=c(-Inf, 0.5, 0.75, 1, 1.25, 1.5, Inf),
                # breaks=c(-Inf, 0.25, 0.5, 0.75, 1, 1.25, Inf)
                ) +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        # tm_shape(filter(dong, grepl("미원|오송|오창|낭성|가덕|문의", name))) + tm_text(text="name", size = .8, shadow = T, fontface = 'bold') +
        tm_layout(title = "(b) Total access (OHCA)", frame=F)

m <- tmap_arrange(m_general, m_ohca, ncol=2)
m
tmap_save(m, paste0("output/fig/access_total_mm.png"), dpi=300, width=9, height=5)

# minmax SPAR
grid <- grid %>%
        mutate(acc_general_mm_spar = acc_general_mm / mean(acc_general_mm, na.rm = T),
               acc_ohca_mm_spar = acc_ohca_mm / mean(acc_ohca_mm, na.rm=T))

hist(grid$acc_general_mm_spar)
hist(grid$acc_ohca_mm_spar)

palette <- tmaptools::get_brewer_pal("-RdBu", n = 6)

m_general <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "acc_general_mm_spar", palette=palette, 
                textNA = "Inaccessible",
                breaks = c(-Inf, 0.5, 0.75, 1, 1.25, 1.5, Inf), title="SPAR (General)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        # tm_shape(filter(dong, grepl("미원|오송|오창|낭성|가덕|문의", name))) + tm_text(text="name", size = .8, shadow = T, fontface = 'bold') +
        tm_layout(title = "(a) SPAR of total access (General)", frame=F) +
        tm_compass(position = c("left", "bottom")) +
        tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 5, 10), text.size = 0.6)

m_ohca <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "acc_ohca_mm_spar", palette=palette, 
                textNA = "Inaccessible",
                breaks = c(-Inf, 0.5, 0.75, 1, 1.25, 1.5, Inf), title="SPAR (OHCA)") +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        # tm_shape(filter(dong, grepl("미원|오송|오창|낭성|가덕|문의", name))) + tm_text(text="name", size = .8, shadow = T, fontface = 'bold') +
        tm_layout(title = "(b) SPAR of total access (OHCA)", frame=F)

m <- tmap_arrange(m_general, m_ohca, ncol=2)
m
tmap_save(m, "output/fig/acc_mm_spar.png", dpi=300, width=9, height=5)


# minmax comparison

grid <- grid %>%
        mutate(acc_compare_mm_ratio = (acc_ohca_mm/acc_general_mm))

hist(grid$acc_compare_mm_ratio)

palette <- tmaptools::get_brewer_pal("-RdBu", n = 6)
m_diff <- tm_shape(grid, bbox = bound) +
        tm_fill(col = "acc_compare_mm_ratio", title="Comparison\nOHCA/General", midpoint = 1,
                textNA = "Inaccessible",
                palette = palette,
                # breaks=c(-Inf, -2.5, 0, 2.5, 5, Inf),
                breaks=c(min(grid$acc_compare_mm_ratio, na.rm=T), 0.9, 1, 1.1,  1.3, Inf)
                ) +
        tm_shape(study_area) + tm_borders(col = "gray40") +
        tm_layout(frame=F) +
        tm_compass(position = c("left", "bottom")) +
        tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 5, 10), text.size = 0.6)
m_diff

tmap_save(m_diff, "output/fig/access_total_mm_difference.png", dpi=300, width=6, height=6)



# bilisa of minmax total access -------------------------

# library(spdep)
# library(rgeoda)
# library(rgdal)
# 
# temp <- grid %>% filter(!is.na(acc_general_mm))
# 
# W <- rgeoda::queen_weights(temp)
# 
# W <- rgeoda::distance_weights(temp, dist_thres = 5000)
# 
# qsa <- local_bimoran(W, temp[c("acc_general_mm", "acc_ohca_mm")], significance_cutoff = 0.05, permutations = 999)
# 
# lisa_clusters <- lisa_clusters(qsa)
# lms <- lisa_values(qsa)
# lisa_labels <- lisa_labels(qsa)
# lisa_clusters[lisa_clusters==0] <- lisa_labels[1]
# lisa_clusters[lisa_clusters==1] <- lisa_labels[2]
# lisa_clusters[lisa_clusters==2] <- lisa_labels[3]
# lisa_clusters[lisa_clusters==3] <- lisa_labels[4]
# lisa_clusters[lisa_clusters==4] <- lisa_labels[5]
# lisa_clusters[lisa_clusters==5] <- lisa_labels[6]
# lisa_clusters[lisa_clusters==6] <- lisa_labels[7]
# 
# temp$acc_bilisa <- lisa_clusters
# 
# bilisa_palette <- c('#fb9a99', '#e31a1c', '#1f78b4', '#a6cee3', '#f7f7f7')
# m <- tm_shape(temp, bbox = bound) +
#         tm_fill(col="acc_bilisa", palette=bilisa_palette, title="Bivariate LISA clusters\nGeneral - OHCA") +
#         tm_shape(study_area) + tm_borders(col = "gray40")
# m
# tmap_save(m, paste0("output/fig/bilisa_access_total_mm.png"), dpi=300, width=9, height=8.5)


# bilisa between general access - demand_ohca
# 
# W <- rgeoda::queen_weights(temp)
# 
# # W <- rgeoda::distance_weights(temp, dist_thres = 2000)
# 
# qsa <- local_bimoran(W, temp[c("fire_general", "demand_ohca")], significance_cutoff = 0.05, permutations = 999)
# 
# lisa_clusters <- lisa_clusters(qsa)
# lms <- lisa_values(qsa)
# lisa_labels <- lisa_labels(qsa)
# lisa_clusters[lisa_clusters==0] <- lisa_labels[1]
# lisa_clusters[lisa_clusters==1] <- lisa_labels[2]
# lisa_clusters[lisa_clusters==2] <- lisa_labels[3]
# lisa_clusters[lisa_clusters==3] <- lisa_labels[4]
# lisa_clusters[lisa_clusters==4] <- lisa_labels[5]
# lisa_clusters[lisa_clusters==5] <- lisa_labels[6]
# lisa_clusters[lisa_clusters==6] <- lisa_labels[7]
# 
# temp$acc_bilisa <- factor(lisa_clusters, levels = c("High-High", "High-Low", "Low-High", "Low-Low", "Not significant"))
# 
# 
# 
# bilisa_palette <- c('#fb9a99', '#e31a1c', '#1f78b4', '#a6cee3', '#f7f7f7')
# bilisa_palette <- c('#e31a1c', '#fb9a99', '#a6cee3', '#1f78b4', '#f7f7f7')
# m <- tm_shape(temp, bbox = bound) +
#         tm_fill(col="acc_bilisa", palette=bilisa_palette, 
#                 title="Bivariate LISA clusters\nGeneral 119 access - OHCA demand") +
#         tm_shape(study_area) + tm_borders(col = "gray40") +
#         tm_layout(title = "1st order queen weights")
#         # tm_layout(title = "2000 m distance weights")
# m



# sum between raw values ----------------
# weight_fire <- 0.7
# weight_ems <- 1-weight_fire
# 
# grid <- grid %>%
#         mutate(acc_general = (fire_general*weight_fire + ems_general*weight_ems),
#                acc_ohca = (fire_ohca*weight_fire + ems_ohca*weight_ems))
# 
# m_general <- tm_shape(grid, bbox = bound) +
#         tm_fill(col = "acc_general", title="Total access (General)") +
#         tm_shape(study_area) + tm_borders(col = "gray40") +
#         tm_credits("Access of general population", position = c("left", "bottom"))
# m_general
# m_ohca <- tm_shape(grid, bbox = bound) +
#         tm_fill(col = "acc_ohca",  title="Total access (OHCA)") +
#         tm_shape(study_area) + tm_borders(col = "gray40") +
#         tm_credits("Access of estimated OHCA population", position = c("left", "bottom"))
# 
# m <- tmap_arrange(m_general, m_ohca, ncol=2)
# m
# tmap_save(m, paste0("output/fig/access_total_", weight_fire, "_", weight_ems, ".png"), dpi=300, width=9, height=5)


# gi --------------------
# 
# library(spdep)
# 
# # set spatial weights
# wr <- knearneigh(st_centroid(grid), k=8, longlat = NULL, use_kd_tree=TRUE)
# ws <- knn2nb(wr)
# ws <- include.self(ws)
# listw <- nb2listw(ws, style="W")
# 
# variables <- c("fire_general", "fire_ohca", "ems_general", "ems_ohca", "acc_general_mm", "acc_ohca_mm")
# for (variable in variables){
#         localg <- localG_perm(grid[[variable]], listw=listw, nsim=999)
#         gi_variable <- paste0("gi_", variable)
#         grid[[gi_variable]] <- localg
#         
#         grid <- grid %>%
#                 mutate(!!sym(gi_variable) := case_when(
#                         !!sym(gi_variable) > 1.96 ~ 'High',
#                         !!sym(gi_variable) < -1.96 ~ 'Low', 
#                         TRUE ~ 'insig'))
#         grid <- grid %>%
#                 mutate(!!sym(gi_variable) := factor(!!sym(gi_variable), levels = c("High", "Low", "insig"),
#                                                          labels = c("High", "Low", "Not significant")))
# }
# 
# 
# 
# 
# gi_palette <- c('#b2182b', '#2166ac', 'gray70')
# 
# # fire stations
# m_general <- tm_shape(grid, bbox = bound) +
#         tm_fill(col = "gi_fire_general", title="Gi*", palette=gi_palette) +
#         tm_shape(study_area) + tm_borders(col = "gray40") +
#         tm_credits("Gi* of access to fire stations of general population", position = c("left", "bottom"))
# m_ohca <- tm_shape(grid, bbox = bound) +
#         tm_fill(col = "gi_fire_ohca", title="Gi*", palette=gi_palette) +
#         tm_shape(study_area) + tm_borders(col = "gray40") +
#         tm_credits("Gi* of access to fire stations of estimated OHCA population", position = c("left", "bottom"))
# m <- tmap_arrange(m_general, m_ohca, ncol=2)
# m
# tmap_save(m, "output/fig/gi_fire_access.png", dpi=300, width=9, height=5)
# 
# # ems hospitals
# m_general <- tm_shape(grid, bbox = bound) +
#         tm_fill(col = "gi_ems_general", title="Gi*", palette=gi_palette) +
#         tm_shape(study_area) + tm_borders(col = "gray40") +
#         tm_credits("Gi* of access to ems hospitals of general population", position = c("left", "bottom"))
# m_ohca <- tm_shape(grid, bbox = bound) +
#         tm_fill(col = "gi_ems_ohca", title="Gi*", palette=gi_palette) +
#         tm_shape(study_area) + tm_borders(col = "gray40") +
#         tm_credits("Gi* of access to ems hospitals of estimated OHCA population", position = c("left", "bottom"))
# m <- tmap_arrange(m_general, m_ohca, ncol=2)
# m
# tmap_save(m, "output/fig/gi_ems_access.png", dpi=300, width=9, height=5)
# 
# # total access
# m_general <- tm_shape(grid, bbox = bound) +
#         tm_fill(col = "gi_acc_general_mm", title="Gi*", palette=gi_palette) +
#         tm_shape(study_area) + tm_borders(col = "gray40") +
#         tm_credits("Gi* of total access of general population", position = c("left", "bottom"))
# m_ohca <- tm_shape(grid, bbox = bound) +
#         tm_fill(col = "gi_acc_ohca_mm", title="Gi*", palette=gi_palette) +
#         tm_shape(study_area) + tm_borders(col = "gray40") +
#         tm_credits("Gi* of total access of estimated OHCA population", position = c("left", "bottom"))
# m <- tmap_arrange(m_general, m_ohca, ncol=2)
# m
# tmap_save(m, paste0("output/fig/gi_access_total_mm.png"), dpi=300, width=9, height=5)
# 
# 
# grid <- grid %>%
#         mutate(compare = case_when(gi_acc_general_mm == "High" & gi_acc_ohca_mm == "High" ~ "HH",
#                                    gi_acc_general_mm == "High" & gi_acc_ohca_mm == "Low" ~ "HL",
#                                    gi_acc_general_mm == "Low" & gi_acc_ohca_mm == "High" ~ "LH",
#                                    gi_acc_general_mm == "Low" & gi_acc_ohca_mm == "Low" ~ "LL",
#                                    gi_acc_general_mm == "High" & gi_acc_ohca_mm == "Not significant" ~ "H-",
#                                    gi_acc_general_mm == "Not significant" & gi_acc_ohca_mm == "High" ~ "-H",
#                                    gi_acc_general_mm == "Low" & gi_acc_ohca_mm == "Not significant" ~ "L-",
#                                    gi_acc_general_mm == "Not significant" & gi_acc_ohca_mm == "Low" ~ "-L",
#                                    gi_acc_general_mm == "Not significant" & gi_acc_ohca_mm == "Not significant" ~ "Not significant"))
# 
# m <- tm_shape(grid, bbox = bound) +
#         tm_fill(col = "compare", title="Gi*") +
#         tm_shape(study_area) + tm_borders(col = "gray40") +
#         tm_credits("Gi* of total access of estimated OHCA population", position = c("left", "bottom"))
# 
# tmap_save(m, paste0("output/fig/gi_access_compare.png"), dpi=300, width=9, height=9)
# 
# # BILISA --------------------------------
# ## https://gist.github.com/rafapereirabr/5348193abf779625f5e8c5090776a228
# 
# library(rgeoda)
# 
# W <- knn_weights(st_centroid(grid), k=8)
# 
# qsa <- local_bimoran(W, grid[c("acc_general_mm", "acc_ohca_mm")])
# 
# lisa_clusters <- lisa_clusters(qsa)
# lisa_labels <- lisa_labels(qsa)
# lisa_clusters[lisa_clusters==0] <- lisa_labels[1]
# lisa_clusters[lisa_clusters==1] <- lisa_labels[2]
# lisa_clusters[lisa_clusters==2] <- lisa_labels[3]
# lisa_clusters[lisa_clusters==3] <- lisa_labels[4]
# lisa_clusters[lisa_clusters==4] <- lisa_labels[5]
# lisa_clusters[lisa_clusters==5] <- lisa_labels[6]
# lisa_clusters[lisa_clusters==6] <- lisa_labels[7]
# 
# grid$acc_bilisa <- lisa_clusters
# 
# bilisa_palette <- c('#fb9a99', '#e31a1c', '#1f78b4', '#a6cee3', '#f7f7f7')
# m <- tm_shape(grid, bbox = bound) +
#         tm_fill(col="acc_bilisa", palette=bilisa_palette, title="Bivariate LISA clusters\nGeneral - OHCA") +
#         tm_shape(study_area) + tm_borders(col = "gray40")
# m
# tmap_save(m, paste0("output/fig/bilisa_access_total_mm.png"), dpi=300, width=9, height=8.5)
# 
# 
# 
# # compare -------------------
# library(scales)
# 
# png("output/fig/acc_comparison.png", width=6, height=5, units='in', res=300)
# par(mfrow=c(1, 1), mar=c(4,6,1,4))
# x <- grid$acc_general_mm
# y <- grid$acc_ohca_mm
# plot(x, y, pch=16, cex=.5,   
#      xlab="Accessibility (General population)", 
#      ylab="Accessibility (OHCA population)",
#      xlim=c(0, 1), 
#      ylim=c(0, 1))
# abline(coef = c(0,1), col="gray50")
# dev.off()


# export result
st_write(grid, "output/access_result.gpkg", delete_layer=TRUE)




