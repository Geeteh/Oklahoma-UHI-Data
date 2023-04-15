library(tidyverse)
library(lubridate)
library(grDevices)
library(extrafont)

setwd("C:/Users/drake/Downloads")
font_import("C:/Users/drake/Downloads/Century Gothic/")
for (year in 2018:2022) {
  
  bixby <- read.csv("Bixby_mesonet.csv")
  
  bixby <- filter(bixby, !is.na(TMAX), TMAX >= 0)
  bixby$TMAX <- (bixby$TMAX - 32) * 5 / 9
  bixby$TIME <- ymd(substring(bixby$TIME, 1, 10))
  
  this_year <- filter(bixby, year(TIME) == year, month(TIME) %in% c(6, 7, 8))
  
  this_year <- this_year %>%
    group_by(day = as.Date(cut(TIME, breaks = "day"))) %>%
    summarize(maxTMAX = max(TMAX, na.rm = TRUE))
  
  assign(paste0("bixby_", year), this_year)
}

for (year in 2018:2022) {
  
  bixby <- read.csv("Bixby_mesonet.csv")
  
  bixby <- filter(bixby, !is.na(TMIN), TMIN >= 0)
  bixby$TMIN <- (bixby$TMIN - 32) * 5 / 9
  bixby$TIME <- ymd(substring(bixby$TIME, 1, 10))
  
  this_year <- filter(bixby, year(TIME) == year,
                      month(TIME) %in% c(6, 7, 8))
  
  this_year <- this_year %>%
    group_by(day = as.Date(cut(TIME, breaks = "day"))) %>%
    summarize(maxTMAX = mean(TMIN, na.rm = TRUE))
  
  assign(paste0("min_bixby_", year), this_year)
}

bixby <- rbind(mutate(bixby_2018, year = "2018"),
               mutate(bixby_2019, year = "2019"),
               mutate(bixby_2020, year = "2020"),
               mutate(bixby_2021, year = "2021"),
               mutate(bixby_2022, year = "2022"))


bixby$day <- factor(format(bixby$day, "%b"))

# Plot the data with facet_wrap
b_2018 <- ggplot() +
  geom_line(data = bixby_2018, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = bixby_2018, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_bixby_2018, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_bixby_2018, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2018") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
  # scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
  # theme(legend.text = element_text(family= "Century Gothic")) +
  # theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "bixby_2018_temp.png", width=2000,height=400, units="px")
b_2018
dev.off()

b_2019 <- ggplot() +
  geom_line(data = bixby_2019, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = bixby_2019, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_bixby_2019, mapping=aes(x=day, y=maxTMAX),color="blue",size=1) +
  geom_point(data = min_bixby_2019, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2019") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "bixby_2019_temp.png", width=2000,height=400, units="px")
b_2019
dev.off()

b_2020 <- ggplot() +
  geom_line(data = bixby_2020, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = bixby_2020, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_bixby_2020, mapping=aes(x=day, y=maxTMAX),color="blue",size=1) +
  geom_point(data = min_bixby_2020, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2020") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "bixby_2020_temp.png", width=2000,height=400, units="px")
b_2020
dev.off()

b_2021 <- ggplot() +
  geom_line(data = bixby_2021, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = bixby_2021, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_bixby_2021, mapping=aes(x=day, y=maxTMAX),color="blue",size = 1) +
  geom_point(data = min_bixby_2021, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2021") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "bixby_2021_temp.png", width=2000,height=400, units="px")
b_2021
dev.off()

b_2022 <- ggplot() +
  geom_line(data = bixby_2022, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = bixby_2022, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_bixby_2022, mapping=aes(x=day, y=maxTMAX),color="blue", size =1) +
  geom_point(data = min_bixby_2022, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2022") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic",face="bold" ,size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "bixby_2022_temp.png", width=2000,height=400, units="px")
b_2022
dev.off()



for (year in 2018:2022) {
  
  skiatook <- read.csv("Skiatook_mesonet.csv")
  
  skiatook <- filter(skiatook, !is.na(TMAX), TMAX >= 0)
  skiatook$TMAX <- (skiatook$TMAX - 32) * 5 / 9
  skiatook$TIME <- ymd(substring(skiatook$TIME, 1, 10))
  
  this_year <- filter(skiatook, year(TIME) == year, month(TIME) %in% c(6, 7, 8))
  
  this_year <- this_year %>%
    group_by(day = as.Date(cut(TIME, breaks = "day"))) %>%
    summarize(maxTMAX = max(TMAX, na.rm = TRUE))
  
  assign(paste0("skiatook_", year), this_year)
}

for (year in 2018:2022) {
  
  skiatook <- read.csv("Skiatook_mesonet.csv")
  
  skiatook <- filter(skiatook, !is.na(TMIN), TMIN >= 0)
  skiatook$TMIN <- (skiatook$TMIN - 32) * 5 / 9
  skiatook$TIME <- ymd(substring(skiatook$TIME, 1, 10))
  
  this_year <- filter(skiatook, year(TIME) == year,
                      month(TIME) %in% c(6, 7, 8))
  
  this_year <- this_year %>%
    group_by(day = as.Date(cut(TIME, breaks = "day"))) %>%
    summarize(maxTMAX = mean(TMIN, na.rm = TRUE))
  
  assign(paste0("min_skiatook_", year), this_year)
}

s_2018 <- ggplot() +
  geom_line(data = skiatook_2018, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = skiatook_2018, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_skiatook_2018, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_skiatook_2018, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2018") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "skiatook_2018_temp.png", width=2000,height=400, units="px")
s_2018
dev.off()

s_2019 <- ggplot() +
  geom_line(data = skiatook_2019, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = skiatook_2019, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_skiatook_2019, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_skiatook_2019, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2019") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "skiatook_2019_temp.png", width=2000,height=400, units="px")
s_2019
dev.off()

s_2020 <- ggplot() +
  geom_line(data = skiatook_2020, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = skiatook_2020, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_skiatook_2020, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_skiatook_2020, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2020") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "skiatook_2020_temp.png", width=2000,height=400, units="px")
s_2020
dev.off()

s_2021 <- ggplot() +
  geom_line(data = skiatook_2021, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = skiatook_2021, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_skiatook_2021, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_skiatook_2021, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2021") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "skiatook_2021_temp.png", width=2000,height=400, units="px")
s_2021
dev.off()

s_2022 <- ggplot() +
  geom_line(data = skiatook_2022, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = skiatook_2022, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_skiatook_2022, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_skiatook_2022, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2022") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "skiatook_2022_temp.png", width=2000,height=400, units="px")
s_2022
dev.off()


for (year in 2018:2022) {
  
  trja <- read.csv("USW00053908.csv")
  
  trja <- filter(trja, !is.na(TMAX), TMAX >= 0)
  
  for (i in 1:length(trja$TMAX)) {
    trja$TMAX[i] <- as.numeric(trja$TMAX[i]) / 10
  }
  
  trja$DATE <- ymd(trja$DATE)
  
  this_year <- filter(trja, year(DATE) == year, month(DATE) %in% c(6, 7, 8))
  
  this_year <- this_year %>%
    group_by(day = as.Date(cut(DATE, breaks = "day"))) %>%
    summarize(maxTMAX = max(TMAX, na.rm = TRUE))
  
  assign(paste0("trja_", year), this_year)
}

for (year in 2018:2022) {
  
  trja <- read.csv("USW00053908.csv")
  
  trja <- filter(trja, !is.na(TMIN), TMIN >= 0)
  
  for (i in 1:length(trja$TMIN)) {
    trja$TMIN[i] <- as.numeric(trja$TMIN[i]) / 10
  }
  
  trja$DATE <- ymd(trja$DATE)
  
  this_year <- filter(trja, year(DATE) == year, month(DATE) %in% c(6, 7, 8))
  
  this_year <- this_year %>%
    group_by(day = as.Date(cut(DATE, breaks = "day"))) %>%
    summarize(maxTMAX = max(TMIN, na.rm = TRUE))
  
  assign(paste0("min_trja_", year), this_year)
}

tr_2018 <- ggplot() +
  geom_line(data = trja_2018, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = trja_2018, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_trja_2018, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_trja_2018, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2018") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15)) +
  #scale_y_continuous(c(15,20,25,30,35,40,45))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "trja_2018_temp.png", width=2000,height=400, units="px")
tr_2018
dev.off()

tr_2019 <- ggplot() +
  geom_line(data = trja_2019, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = trja_2019, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_trja_2019, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_trja_2019, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2019") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "trja_2019_temp.png", width=2000,height=400, units="px")
tr_2019
dev.off()

tr_2020 <- ggplot() +
  geom_line(data = trja_2020, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = trja_2020, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_trja_2020, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_trja_2020, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2020") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "trja_2020_temp.png", width=2000,height=400, units="px")
tr_2020
dev.off()

tr_2021 <- ggplot() +
  geom_line(data = trja_2021, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = trja_2021, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_trja_2021, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_trja_2021, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2021") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "trja_2021_temp.png", width=2000,height=400, units="px")
tr_2021
dev.off()

tr_2022 <- ggplot() +
  geom_line(data = trja_2022, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = trja_2022, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_trja_2022, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_trja_2022, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2022") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "trja_2022_temp.png", width=2000,height=400, units="px")
tr_2022
dev.off()

for (year in 2018:2022) {
  
  tia <- read.csv("USW00013968.csv")
  
  tia <- filter(tia, !is.na(TMAX), TMAX >= 0)
  
  for (i in 1:length(tia$TMAX)) {
    tia$TMAX[i] <- as.numeric(tia$TMAX[i]) / 10
  }
  
  tia$DATE <- ymd(tia$DATE)
  
  this_year <- filter(tia, year(DATE) == year, month(DATE) %in% c(6, 7, 8))
  
  this_year <- this_year %>%
    group_by(day = as.Date(cut(DATE, breaks = "day"))) %>%
    summarize(maxTMAX = max(TMAX, na.rm = TRUE))
  
  assign(paste0("tia_", year), this_year)
}

for (year in 2018:2022) {
  
  tia <- read.csv("USW00013968.csv")
  
  tia <- filter(tia, !is.na(TMIN), TMIN >= 0)
  
  for (i in 1:length(tia$TMIN)) {
    tia$TMIN[i] <- as.numeric(tia$TMIN[i]) / 10
  }
  
  tia$DATE <- ymd(tia$DATE)
  
  this_year <- filter(tia, year(DATE) == year, month(DATE) %in% c(6, 7, 8))
  
  this_year <- this_year %>%
    group_by(day = as.Date(cut(DATE, breaks = "day"))) %>%
    summarize(maxTMAX = max(TMIN, na.rm = TRUE))
  
  assign(paste0("min_tia_", year), this_year)
}

t_2018 <- ggplot() +
  geom_line(data = tia_2018, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = tia_2018, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_tia_2018, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_tia_2018, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2018") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "tia_2018_temp.png", width=2000,height=400, units="px")
t_2018
dev.off()

t_2019 <- ggplot() +
  geom_line(data = tia_2019, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = tia_2019, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_tia_2019, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_tia_2019, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2019") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "tia_2019_temp.png", width=2000,height=400, units="px")
t_2019
dev.off()

t_2020 <- ggplot() +
  geom_line(data = tia_2020, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = tia_2020, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_tia_2020, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_tia_2020, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2020") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "tia_2020_temp.png", width=2000,height=400, units="px")
t_2020
dev.off()

t_2021 <- ggplot() +
  geom_line(data = tia_2021, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = tia_2021, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_tia_2021, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_tia_2021, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2021") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "tia_2021_temp.png", width=2000,height=400, units="px")
t_2021
dev.off()

t_2022 <- ggplot() +
  geom_line(data = tia_2022, mapping=aes(x = day, y = maxTMAX),color="red",size=1) +
  geom_point(data = tia_2022, mapping=aes(x=day, y=maxTMAX),color="red", size = 2) +
  geom_line(data = min_tia_2022, mapping=aes(x=day, y=maxTMAX),color="blue", size=1) +
  geom_point(data = min_tia_2022, mapping=aes(x=day, y=maxTMAX),color="blue",size = 2) +
  labs(y = "Daily Temperature (C)", color = "Year") +
  ggtitle("2022") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family = "Century Gothic", face="bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(family = "Century Gothic", face="bold", size=20)) +
  theme(axis.text = element_text(family = "Century Gothic", size = 15))
# scale_color_manual(values= c("red","blue"), labels = c("Max Temperature", "Min Temperature")) +
# theme(legend.text = element_text(family= "Century Gothic")) +
# theme(legend.title = element_text(family= "Century Gothic"))
png(filename = "tia_2022_temp.png", width=2000,height=400, units="px")
t_2022
dev.off()

# bixby_TMIN <- read.csv("Bixby_mesonet.csv")
# bixby_TMIN <- filter(bixby_TMIN, !is.na(TMIN), bixby_TMIN$TMIN >= 0)
# #bixby_TMIN$TMIN <- (bixby_TMIN$TMIN-32)*5/9
# bixby_TMIN$TIME <- substring(bixby_TMIN$TIME, 1, 10)
# bixby_TMIN$TIME <- ymd(bixby_TMIN$TIME)
# class(bixby_TMIN$TIME)
# 
# bixby_TMIN_summer <- bixby_TMIN %>%
#   filter(year(TIME) >= 2018,
#          month(TIME) %in% c(6,7,8))
# bixby_TMIN_summer$day <- as.Date(cut(bixby_TMIN_summer$TIME, breaks='day'))
# bixby_TMIN_summer <- bixby_TMIN_summer %>%
#   group_by(day) %>%
#   summarize(maxTMAX = max(TMIN, na.rm = TRUE))
# 
# bixby_summer <- left_join(bixby_max_TMAX_summer, bixby_TMIN_summer)

  
  
# bixby_max_TMAX <- bixby_max_TMAX %>%
#   group_by(year) %>%
#   summarize(maxTMAX = max(TMAX, na.rm = TRUE))
# View(bixby_max_TMAX)
# summary(bixby_max_TMAX)

#Bixby Max TMAX
# bixby_max_TMAX <- read.csv("USC00340782.csv")
# bixby_max_TMAX <- filter(bixby_max_TMAX, !is.na(TMAX))
# for (i in 1:length(bixby_max_TMAX$TMAX)) {
#   bixby_max_TMAX$TMAX[i] <- as.numeric(bixby_max_TMAX$TMAX[i]) / 10
# }
# bixby_max_TMAX$DATE <- ymd(bixby_max_TMAX$DATE)
# class(bixby_max_TMAX$DATE)
# bixby_max_TMAX$year <- year(bixby_max_TMAX$DATE)
# bixby_max_TMAX <- filter(bixby_max_TMAX, year >=2018)
# View(bixby_max_TMAX)
# summary(bixby_max_TMAX)
# ggplot(bixby_max_TMAX, aes(x = year, y = TMAX)) +
#   geom_line()
# 
# #Bixby Min TMAX
# bixby_min_TMAX <- read.csv("USC00340782.csv")
# bixby_min_TMAX <- filter(bixby_min_TMAX, !is.na(TMAX))
# for (i in 1:length(bixby_min_TMAX$TMAX)) {
#   bixby_min_TMAX$TMAX[i] <- as.numeric(bixby_min_TMAX$TMAX[i]) / 10
# }
# bixby_min_TMAX$DATE <- ymd(bixby_min_TMAX$DATE)
# class(bixby_min_TMAX$DATE)
# bixby_min_TMAX$year <- as.Date(cut(bixby_min_TMAX$DATE, breaks='year'))
# bixby_min_TMAX <- bixby_min_TMAX %>%
#   group_by(year) %>%
#   summarize(minTMAX = min(TMAX, na.rm = TRUE))
# #View(bixby_min_TMAX)
# summary(bixby_min_TMAX)

#sapulpa <- read.csv("USC00347921.csv")


#TIA Max TMAX
tia_TMAX <- read.csv("USW00013968.csv")
tia_TMAX <- filter(tia_TMAX, !is.na(TMAX))
for (i in 1:length(tia_TMAX$TMAX)) {
  tia_TMAX$TMAX[i] <- as.numeric(tia_TMAX$TMAX[i]) / 10
}
tia_TMAX$DATE <- ymd(tia_TMAX$DATE)
class(tia_TMAX$DATE)

tia_TMAX_summer <- tia_TMAX %>%
    filter(year(DATE) >= 2018,
         month(DATE) %in% c(6,7,8))
#View(tia_TMAX)
summary(tia_TMAX)

#TIA Min TMAX
tia_min_TMAX <- read.csv("USW00013968.csv")
tia_min_TMAX <- filter(tia_min_TMAX, !is.na(TMAX))
for (i in 1:length(tia_min_TMAX$TMAX)) {
  tia_min_TMAX$TMAX[i] <- as.numeric(tia_min_TMAX$TMAX[i]) / 10
}
tia_min_TMAX$DATE <- ymd(tia_min_TMAX$DATE)
class(tia_min_TMAX$DATE)
tia_min_TMAX <- tia_min_TMAX %>%
  filter(year(DATE) >= 2018,
         month(DATE) %in% c(6,7,8))
#View(tia_min_TMAX)
summary(tia_min_TMAX)



#TRJA Max TMAX
trja_max_TMAX <- read.csv("USW00053908.csv")
trja_max_TMAX <- filter(trja_max_TMAX, !is.na(TMAX))
for (i in 1:length(trja_max_TMAX$TMAX)) {
  trja_max_TMAX$TMAX[i] <- as.numeric(trja_max_TMAX$TMAX[i]) / 10
}
trja_max_TMAX$DATE <- ymd(trja_max_TMAX$DATE)
class(trja_max_TMAX$DATE)
trja_max_TMAX$year <- as.Date(cut(trja_max_TMAX$DATE, breaks='year'))
trja_max_TMAX <- trja_max_TMAX %>%
  group_by(year) %>%
  summarize(maxTMAX = max(TMAX, na.rm = TRUE))
#View(trja_max_TMAX)
summary(trja_max_TMAX)

#TRJA Min TMAX
trja_min_TMAX <- read.csv("USW00053908.csv")
trja_min_TMAX <- filter(trja_min_TMAX, !is.na(TMAX))
for (i in 1:length(trja_min_TMAX$TMAX)) {
  trja_min_TMAX$TMAX[i] <- as.numeric(trja_min_TMAX$TMAX[i]) / 10
}
trja_min_TMAX$DATE <- ymd(trja_min_TMAX$DATE)
class(trja_min_TMAX$DATE)
trja_min_TMAX$year <- as.Date(cut(trja_min_TMAX$DATE, breaks='year'))
trja_min_TMAX <- trja_min_TMAX %>%
  group_by(year) %>%
  summarize(minTMAX = min(TMAX, na.rm = TRUE))
#View(trja_min_TMAX)
summary(trja_min_TMAX)

#OKC Max TMAX
okc_max_TMAX <- read.csv("OKC_area_mesonet.csv")
okc_max_TMAX <- filter(okc_max_TMAX, !is.na(TMAX), okc_max_TMAX$TMAX >= 0)
okc_max_TMAX$TMAX <- (okc_max_TMAX$TMAX-32)*5/9
okc_max_TMAX$TIME <- substring(okc_max_TMAX$TIME, 1, 10)
okc_max_TMAX$TIME <- ymd(okc_max_TMAX$TIME)
class(okc_max_TMAX$TIME)
okc_max_TMAX$year <- as.Date(cut(okc_max_TMAX$TIME, breaks='year'))
okc_max_TMAX <- okc_max_TMAX %>%
  group_by(year) %>%
  summarize(maxTMAX = max(TMAX, na.rm = TRUE))
#View(okc_max_TMAX)
summary(okc_max_TMAX)

#OKC Min TMAX
okc_min_TMAX <- read.csv("OKC_area_mesonet.csv")
okc_min_TMAX <- filter(okc_min_TMAX, !is.na(TMAX), okc_min_TMAX$TMAX >= 0)
okc_min_TMAX$TMAX <- (okc_min_TMAX$TMAX-32)*5/9
okc_min_TMAX$TIME <- substring(okc_min_TMAX$TIME, 1, 10)
okc_min_TMAX$TIME <- ymd(okc_min_TMAX$TIME)
class(okc_min_TMAX$TIME)
okc_min_TMAX$year <- as.Date(cut(okc_min_TMAX$TIME, breaks='year'))
okc_min_TMAX <- okc_min_TMAX %>%
  group_by(year) %>%
  summarize(minTMAX = min(TMAX, na.rm = TRUE))
#View(okc_min_TMAX)
summary(okc_min_TMAX)



p <- ggplot()+
  geom_line(bixby, mapping=aes(year, meanTMAX, col="bixby", linetype="Mean")) +
  geom_point(bixby, mapping=aes(year, meanTMAX, col="bixby")) +
  geom_line(bixby_max_TMAX, mapping=aes(year, maxTMAX, col="bixby_max_TMAX", linetype="Max")) +
  geom_point(bixby_max_TMAX, mapping=aes(year, maxTMAX, col="bixby_max_TMAX")) +
  geom_line(bixby_min_TMAX, mapping=aes(year, minTMAX, col="bixby_min_TMAX", linetype="Min")) +
  geom_point(bixby_min_TMAX, mapping=aes(year,minTMAX, col="bixby_min_TMAX")) +
  geom_line(tia, mapping=aes(year, meanTMAX, col="tia", linetype="Mean")) +
  geom_point(tia, mapping=aes(year, meanTMAX, col="tia")) +
  geom_line(tia_max_TMAX, mapping=aes(year, maxTMAX, col="tia_max_TMAX", linetype="Max")) +
  geom_point(tia_max_TMAX, mapping=aes(year, maxTMAX, col="tia_max_TMAX")) +
  geom_line(tia_min_TMAX, mapping=aes(year, minTMAX, col="tia_min_TMAX", linetype="Min")) +
  geom_point(tia_min_TMAX, mapping=aes(year, minTMAX, col="tia_min_TMAX")) +
  geom_line(trja, mapping=aes(year, meanTMAX, col="trja", linetype="Mean")) +
  geom_point(trja, mapping=aes(year, meanTMAX, col="trja")) +
  geom_line(trja_max_TMAX, mapping=aes(year, maxTMAX, col="trja_max_TMAX", linetype="Max")) +
  geom_point(trja_max_TMAX, mapping=aes(year, maxTMAX, col="trja_max_TMAX")) +
  geom_line(trja_min_TMAX, mapping=aes(year, minTMAX, col="trja_min_TMAX", linetype="Min")) +
  geom_point(trja_min_TMAX, mapping=aes(year, minTMAX, col="trja_min_TMAX")) +
  geom_line(okc, mapping=aes(year, meanTMAX, col="okc", linetype="Mean")) +
  geom_point(okc, mapping=aes(year, meanTMAX, col="okc")) +
  geom_line(okc_max_TMAX, mapping=aes(year, maxTMAX, col="okc_max_TMAX", linetype="Max")) +
  geom_point(okc_max_TMAX, mapping=aes(year, maxTMAX, col="okc_max_TMAX")) +
  geom_line(okc_min_TMAX, mapping=aes(year, minTMAX, col="okc_min_TMAX", linetype="Min")) +
  geom_point(okc_min_TMAX, mapping=aes(year, minTMAX, col="okc_min_TMAX")) +
  
  scale_x_date(date_breaks="1 year",
               date_labels="%y") +
  labs(title="Max Temp / Year",
       x = "Year",
       y = "Max Temp",
       color="Dataset",
       linetype="Extrema") +
  scale_color_manual(values = c("red","red","red",
                                     "darkgreen","darkgreen","darkgreen",
                                     "blue","blue","blue",
                                     "black","black","black"),
                                     
                     labels = c("Bixby Mean","Bixby Max","Bixby Min",
                                "Tulsa Richard L Jones Jr Airport Mean","Tulsa Richard L Jones Jr Airport Max","Tulsa Richard L Jones Jr Airport Min",
                                "Tulsa International Airport Mean","Tulsa International Airport Max","Tulsa International Airport Min",
                                "Oklahoma City Mean","Oklahoma City Max","Oklahoma City Min")) +
  # 
  scale_linetype_manual(values = c("dashed","solid","dotted"),
                        
                        labels = c("Annual Maximum","Annual Mean","Annual Minimum"))




png(filename = "tmax.png", width=1600,height=800, units="px")
p
dev.off()

summary(bixby)
summary(trja)
summary(tia)
summary(okc)

# x.xx
# for (i in 1:length(data1$TMAX)) {
#  if (data1$TMAX[i] >= 100)
#    data1$TMAX[i] <- as.numeric(data1$TMAX[i]) / 100
#  else
#    data1$TMAX[i] <- as.numeric(data1$TMAX[i]) / 10
# }
# for (j in 1:length(data3$TMAX)) {
#  if (data3$TMAX[j] >= 100)
#    data3$TMAX[j] <- as.numeric(data3$TMAX[j]) / 100
#  else
#    data3$TMAX[j] <- as.numeric(data3$TMAX[j]) / 10
# }
# for (k in 1:length(data4$TMAX)) {
#  if (data4$TMAX[k] >= 100)
#    data4$TMAX[k] <- as.numeric(data4$TMAX[k]) / 100
#  else
#    data4$TMAX[k] <- as.numeric(data4$TMAX[k]) / 10
# }

# bixby_2018 <- read.csv("Bixby_mesonet.csv")
# bixby_2018 <- filter(bixby_2018, !is.na(TMAX), bixby_2018$TMAX >= 0)
# bixby_2018$TMAX <- (bixby_2018$TMAX-32)*5/9
# bixby_2018$TIME <- substring(bixby_2018$TIME, 1, 10)
# bixby_2018$TIME <- ymd(bixby_2018$TIME)
# class(bixby_2018$TIME)
# bixby_2018 <- bixby_2018 %>%
#   filter(year(TIME) == 2018,
#          month(TIME) %in% c(6,7,8))
# bixby_2018$day <- as.Date(cut(bixby_2018$TIME, breaks='day'))
# bixby_2018 <- bixby_2018 %>%
#   group_by(day) %>%
#   summarize(maxTMAX = max(TMAX, na.rm = TRUE))
# 
# bixby_2019 <- read.csv("Bixby_mesonet.csv")
# bixby_2019 <- filter(bixby_2019, !is.na(TMAX), bixby_2019$TMAX >= 0)
# bixby_2019$TMAX <- (bixby_2019$TMAX-32)*5/9
# bixby_2019$TIME <- substring(bixby_2019$TIME, 1, 10)
# bixby_2019$TIME <- ymd(bixby_2019$TIME)
# class(bixby_2019$TIME)
# bixby_2019 <- bixby_2019 %>%
#   filter(year(TIME) == 2019,
#          month(TIME) %in% c(6,7,8))
# bixby_2019$day <- as.Date(cut(bixby_2019$TIME, breaks='day'))
# bixby_2019 <- bixby_2019 %>%
#   group_by(day) %>%
#   summarize(maxTMAX = max(TMAX, na.rm = TRUE))
# 
# bixby_2020 <- read.csv("Bixby_mesonet.csv")
# bixby_2020 <- filter(bixby_2020, !is.na(TMAX), bixby_2020$TMAX >= 0)
# bixby_2020$TMAX <- (bixby_2020$TMAX-32)*5/9
# bixby_2020$TIME <- substring(bixby_2020$TIME, 1, 10)
# bixby_2020$TIME <- ymd(bixby_2020$TIME)
# class(bixby_2020$TIME)
# bixby_2020 <- bixby_2020 %>%
#   filter(year(TIME) == 2019,
#          month(TIME) %in% c(6,7,8))
# bixby_2020$day <- as.Date(cut(bixby_2020$TIME, breaks='day'))
# bixby_2020 <- bixby_2020 %>%
#   group_by(day) %>%
#   summarize(maxTMAX = max(TMAX, na.rm = TRUE))
# datasets <- list()
# datasets[[1]] <- datasets[[2]] <- datasets[[3]] <- datasets[[4]] <- datasets[[5]] <- data.frame()
# for (i in 2018:2022) {
# 
#   bixby_max_TMAX <- read.csv("Bixby_mesonet.csv")
#   
# 
#   bixby_max_TMAX <- filter(bixby_max_TMAX, !is.na(TMAX), TMAX >= 0)
#   bixby_max_TMAX$TMAX <- (bixby_max_TMAX$TMAX - 32) * 5 / 9
#   bixby_max_TMAX$TIME <- ymd(substring(bixby_max_TMAX$TIME, 1, 10))
#   
# 
#   this_dataset <- filter(bixby_max_TMAX, year(TIME) == i, month(TIME) %in% c(6, 7, 8))
#   
# 
#   this_dataset <- this_dataset %>%
#     group_by(day = as.Date(cut(TIME, breaks = "day"))) %>%
#     summarize(maxTMAX = max(TMAX, na.rm = TRUE))
# 
#   datasets[[i - 2017]] <- this_dataset
# }
# #combined_data <- bind_rows(datasets, .id = "year")
# #df <- do.call(rbind, dfs)
# 
# 
# ggplot(combined_data, aes(x = day, y = maxTMAX, color = year)) +
#   geom_line() +
#   ggtitle("Bixby Mesonet Summer Max TMAX, 2018-2022")