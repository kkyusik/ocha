# Check trends of ohca


library(tidyverse)
library(readxl)


folder <- "data/raw/cardiac_data"

ohca <- read_xlsx(file.path(folder, "ohca_21.xlsx"))
ohca

ohca %>% filter(grepl("청주", H_ADD_DIST)) %>% nrow()

ohca <- read_xls(file.path(folder, "ohca_15.XLS"))
ohca


ohca15 <- read_xls(file.path(folder, "ohca_15.XLS"))
ohca16 <- read_xls(file.path(folder, "ohca_16.XLS"))
ohca17 <- read_xls(file.path(folder, "ohca_17.XLS"))
ohca18 <- read_xls(file.path(folder, "ohca_18.XLS"))
ohca19 <- read_xlsx(file.path(folder, "ohca_19.xlsx"))
ohca20 <- read_xlsx(file.path(folder, "ohca_20.xlsx"))
ohca21 <- read_xlsx(file.path(folder, "ohca_21.xlsx"))

filter_data <- function(x){
        x <- x %>% filter(grepl("청주", H_ADD_DIST))
        x <- x %>% dplyr::select(YEAR, H_ADD_DIST)
        return(x)
}

ohca15 <- filter_data(ohca15)
ohca16 <- filter_data(ohca16)
ohca17 <- filter_data(ohca17)
ohca18 <- filter_data(ohca18)
ohca19 <- filter_data(ohca19)
ohca20 <- filter_data(ohca20)
ohca21 <- filter_data(ohca21)

data <- rbind.data.frame(ohca15, ohca16, ohca17, ohca18, ohca19, ohca20, ohca21)

ohca_num <- data %>%
        group_by(YEAR) %>%
        summarise(ohca_num = n())


# 주민등록인구
pop <- data.frame(YEAR = c(2015, 2016, 2017, 2018, 2019, 2020, 2021),
           pop = c(831912, 835197, 835590, 837749, 839566, 844993, 848482))
pop$YEAR <- as.character(pop$YEAR)
data <- ohca_num %>% left_join(pop, by="YEAR")

data <- data %>% mutate(ohca_ratio = ohca_num/pop,
                        ohca_100k = ohca_ratio*100000)
data <- data[2:nrow(data),]

attach(data)

# Export plot
png("output/fig/ohca_ratio.png", width=9, height=4, units="in", res=300)
par(mfrow=c(1, 1), mar=c(5,5,1,2))
plot(x=YEAR, y=ohca_100k, type="b",
     xlab="Year", ylab="OHCA ratio (per 100k persons)")
dev.off()






