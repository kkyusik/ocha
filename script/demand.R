#' @title do_demand_estimation
#' @initialized 7-20-23
#' @description
#'      심정지 발생확률을 실제 자료로부터 추정한 뒤, 각 격자별로 연령별|성별 발생확률을 곱하여 수요를 추정함. 
#' @update
#'      7-30-23: calculate ohca number for all cheongju city
#'      8-1-23: Include 

source("load_pkgs.R")

load_pkgs(c("tidyverse", "tmap", "sf", "here", "openxlsx", "data.table"))

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

# cheongju
cardiac <- cardiac %>% 
        mutate(P_ADD_DIST = case_when(grepl("청주", P_ADD_DIST) ~ "청주시", TRUE ~ P_ADD_DIST))

# study area (census)
census <- read_sf(file.path("data/raw", "bnd_sigungu_00_2021_2021/bnd_sigungu_00_2021_2021_2Q.shp"))
cheongju <- census %>% filter(grepl("청주", SIGUNGU_NM))
cheongju_union <- st_union(cheongju)
cheongju_buffer <- st_buffer(cheongju_union, dist=20000)

write_sf(cheongju, file.path("data/tidy", "study_area_sgg.gpkg"))
write_sf(cheongju_buffer, file.path("data/tidy", "study_area_buffer.gpkg"))

qtm(cheongju_buffer) + qtm(cheongju_union)
idx <- st_intersects(census, cheongju_buffer)
census_intersected <- census[which(lengths(idx) > 0), ]
# qtm(census_intersected)

# remove <- c("상주시", "공주시", "서구", "음성군", "옥천군")
# census <- census_intersected %>%
#         filter(!SIGUNGU_NM %in% remove)
# qtm(census, text = "SIGUNGU_NM")



census_intersected <- census_intersected %>%
        mutate(sido_cd = substr(SIGUNGU_CD, 1, 2))
census_intersected <- census_intersected %>% filter(!sido_cd %in% c("31", "37"))

census <- census_intersected


# cheongju
census <- census %>%
        mutate(SIGUNGU_NM = case_when(grepl("청주", SIGUNGU_NM) ~ "청주시", TRUE ~ SIGUNGU_NM))

qtm(census)

census <- census %>% group_by(SIGUNGU_NM) %>% summarise(n = n())

st_write(census, "data/tidy/study_area_buffer_sgg.gpkg", delete_layer = TRUE)
# cardiac %>% filter(P_ADMINCODE %in% sgg_cd)

selection <- census$SIGUNGU_NM
cardiac <- cardiac %>% 
        mutate(P_ADD_DIST = case_when(P_ADD_CITY == "세종" ~ "세종시", TRUE ~ P_ADD_DIST)) %>%
        filter(P_ADD_DIST %in% selection) %>%
        filter(P_ADD_CITY %in% c("대전", "세종", "충북", "충남"))

# cardiac %>% group_by(P_ADD_DIST) %>%
#         summarise(n =n())


# 연령 및 성별 합산
# 성별 - 1:남자, 2:여자

cardiac <- cardiac %>% filter(AGE >= 20)

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

ohca_num <- cardiac %>%
        group_by(P_ADD_DIST, sex) %>%
        summarise(ohca = n())

ohca_num %>% filter(grepl("청주", P_ADD_DIST))

grid <- read_sf(file.path("data/raw", "grd500m_ngii.gpkg"))
grid_unique <- grid %>% filter(!duplicated(gid))
grid_unique <- st_transform(grid_unique, st_crs(census))
st_write(grid_unique, file.path("data/raw", "grid500m_ngii_unique.gpkg"), delete_layer=TRUE)
grid <- grid_unique
# rm(grid_unique)

grid_age <- fread(file.path("data/raw", "grd500m_ngii_age_202210.txt"))
# head(grid_age)
# unique(grid_age$sex)

grid_age <- grid_age %>% mutate(val = case_when(is.na(val) ~ 0, TRUE ~ val))

# check population
temp <- grid_age %>% mutate(sex_temp = case_when(grepl("전체", sex) ~ "전체",
                                                 grepl("남자", sex) ~ "남자", 
                                                 grepl("여자", sex) ~ "여자"))
what <- temp %>%
        group_by(sex_temp) %>%
        summarise(val = sum(val))


# 100세를 90대로 합산
grid_age <- grid_age %>%
        mutate(sex = case_when(sex == "100세 이상 전체" ~ "90대 전체",
                               sex == "100세 이상 남자" ~ "90대 남자",
                               sex == "100세 이상 여자" ~ "90대 여자",
                               TRUE ~ sex))

# 인구가 있는 gid 수집
grid_totpop <- grid_age %>% 
        filter(grepl("전체", sex)) %>%
        group_by(gid) %>%
        summarise(totpop = sum(val)) %>%
        filter(totpop > 0)
# pop_gid <- grid_totpop$gid              # grid id with population

grid_pt <- st_centroid(grid)
grid_pt <- st_transform(grid_pt, st_crs(census))
idx <- st_intersects(grid_pt, census)
grid_pt_intersected <- grid_pt[which(lengths(idx) > 0), ]

gid_list <- grid_pt_intersected$gid
temp <- grid_age %>% filter(gid %in% gid_list)
temp <- temp %>% as_tibble
temp <- temp %>% mutate(val = case_when(is.na(val) ~ 0, TRUE ~ val))
temp <- temp %>% filter(!grepl("전체", sex))

grid_pt_census <- st_join(grid_pt_intersected, census)
head(grid_pt_census)
# grid_pt_census <- grid_pt_census %>% as_tibble %>% select(gid, SIGUNGU_CD, SIGUNGU_NM)
grid_pt_census <- grid_pt_census %>% as_tibble %>% select(gid, SIGUNGU_NM)

temp <- temp %>% left_join(grid_pt_census, by="gid")


# 각 시군구의 연령, 성별 전체 인구 계산
sgg_pop <- temp %>% 
        group_by(SIGUNGU_NM, sex) %>%
        summarise(pop = sum(val))

names(sgg_pop) <- c("name", "sex", "pop")
names(ohca_num) <- c("name", "sex", "ohca")

sgg_ohca <- left_join(sgg_pop, ohca_num, by=c("name", "sex"))
sgg_ohca <- sgg_ohca %>%
        mutate(ohca = case_when(is.na(ohca) ~ 0, TRUE ~ ohca))
sgg_ohca <- sgg_ohca %>%
        mutate(ohca_ratio = ohca/pop)
sgg_ohca <- sgg_ohca %>%
        mutate(ohca_ratio_100k = ohca_ratio * 100000)

# 시군구별, 연령별, 성별, 심정시 발생 확률 - Export output
write.csv(sgg_ohca, file.path("output/tab", "ohca_likelihood.csv"))

# 이제 중요한 것은 ohca_ratio
# ohca_ratio를 격자별 인구에 곱함.
# temp <- temp %>% filter(gid %in% pop_gid)
temp <- temp %>% mutate(name = SIGUNGU_NM) %>% left_join(sgg_ohca, by=c("name", "sex"))
temp <- temp %>%
        mutate(val_ohca = val * ohca_ratio)

temp %>% filter(is.na(val_ohca)) # there are NA of val_ohca
temp <- temp %>%
        mutate(val_ohca = case_when(is.na(val_ohca) ~ 0, TRUE ~ val_ohca))

# Calculate demand and demand_ohca
demand <- temp %>%
        group_by(gid) %>%
        summarise(demand = sum(val), 
                  demand_ohca = sum(val_ohca))

# demand <- demand %>% filter(demand > 0)

grid_area <- grid %>% filter(gid %in% unique(demand$gid))
demand_grid <- grid_area %>% left_join(demand, by="gid")


demand_map <- tm_shape(demand_grid) + tm_fill(col="demand") + tm_shape(census) + tm_borders()

ohca_map <- tm_shape(demand_grid) + tm_fill(col="demand_ohca") + tm_shape(census) + tm_borders()


demand_compare <- tmap_arrange(demand_map, ohca_map, ncol=2)

# grid within cheongju
idx <- st_intersects(demand_grid, cheongju)
demand_grid_chengju <- demand_grid[which(lengths(idx) > 0), ]

demand_map <- tm_shape(demand_grid_chengju) + tm_fill(col="demand") + tm_shape(census) + tm_borders()
ohca_map <- tm_shape(demand_grid_chengju) + tm_fill(col="demand_ohca") + tm_shape(census) + tm_borders()
demand_compare <- tmap_arrange(demand_map, ohca_map, ncol=2)

# hist(demand_grid_chengju$demand_ohca)

# Export demand grid
write_sf(demand_grid, file.path("data/tidy", "demand_grid.gpkg"))
