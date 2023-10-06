#' @title data_ems_bed
#' @initialized 7-21-23
#' @description
#'      데이터는 hira에서 다운받으며, 전국 병의원 및 약국 현황 자료임.' 
#' @update

source("load_pkgs.R")

load_pkgs(c("tidyverse", "tmap", "sf", "here", "openxlsx"))


poi_xy <- read.xlsx(file.path("data/raw/hira", "hospital_service_infomation_2021.12.xlsx")) %>% as_tibble # 1.병원정보서비스
beds <- read.xlsx(file.path("data/raw/hira", "facility_information_2021.12.xlsx")) %>% as_tibble # 3.~시설정보

# head(poi_xy)
# head(beds)

#' What I need?
#' poi_xy: 암호화요양기호, 요양기관명, 종별코드명, 시도코드명, x좌표, y좌표
#' beds: 암호화요양기호, 응급실병상수

poi_xy <- poi_xy %>%
        select(암호화요양기호, 요양기관명, 종별코드명, 시도코드명, x좌표, y좌표)
beds <- beds %>%
        select(암호화요양기호, 응급실병상수)

beds <- beds %>%
        filter(응급실병상수 > 0 )
# nrow(beds)
# Join beds to poi
poi <- left_join(poi_xy, beds, by="암호화요양기호")
# poi

poi <- poi %>% filter(!is.na(응급실병상수))

poi <- poi %>% 
        mutate(x좌표 = as.numeric(x좌표),
               y좌표 = as.numeric(y좌표), 
               beds = as.numeric(응급실병상수))

# 세종, 대전, 충북, 충남
poi <- poi %>%
        filter(시도코드명 %in% c("세종", "대전", "충북", "충남"))
poi <- poi %>%
        rename(id = 암호화요양기호, name = 요양기관명, type = "종별코드명") %>%
        select(id, name, type, beds, x좌표, y좌표)

# XY to point geometry
poi <- st_as_sf(poi, coords = c("x좌표", "y좌표"), crs=4326)
# qtm(poi)

# Get poi within study area
studyarea <- read_sf(file.path("data/tidy", "study_area_sgg.gpkg"))
studyarea_buffer <- read_sf(file.path("data/tidy", "study_area_buffer_sgg.gpkg"))
poi <- st_transform(poi, st_crs(studyarea))

idx <- st_intersects(poi, studyarea_buffer, sparse = TRUE)
poi <- poi[which(lengths(idx) > 0), ]

# qtm(studyarea_buffer) + qtm(studyarea) + qtm(poi)

st_write(poi, dsn = "data/tidy/poi_ems_beds.gpkg", driver = "gpkg", delete_layer = TRUE)


# hist(poi$beds)
