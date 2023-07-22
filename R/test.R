#' @title do_demand_estimation
#' @initialized 7-20-23
#' @description
#'      심정지 발생확률을 실제 자료로부터 추정한 뒤, 각 격자별로 연령별|성별 발생확률을 곱하여 수요를 추정함. 
#' @update
#' 
source("load_pkgs.R")

load_pkgs(c("tidyverse", "tmap", "sf", "here", "openxlsx", "data.table"))

x <- 1
y <- 5
x
y
print(x+y)

plot(x, y)

df <- data.frame(x = x, y = y)
write.csv(df, file.path("output", "testrest.csv"))
xlsx <- read.xlsx(xlsxFile = file.path("data/raw", "cardiac_data", "ohca_21.xlsx"), sheet = 1)

census <- read_sf(file.path("data/raw", "bnd_sigungu_00_2021_2021/bnd_sigungu_00_2021_2021_2Q.shp"))
cheongju <- census %>% filter(grepl("청주", SIGUNGU_NM))
cheongju_union <- st_union(cheongju)
cheongju_buffer <- st_buffer(cheongju_union, dist=10000)

write_sf(cheongju, file.path("data/tidy", "study_area_sgg.gpkg"), delete_layer=TRUE)