library(tidyverse)
library(lubridate)
library(grDevices)

setwd("C:/Users/drake/Downloads")

#Bixby Max TMAX
bixby_max_TMAX <- read.csv("USC00340782.csv")
bixby_max_TMAX <- filter(bixby_max_TMAX, !is.na(TMAX))
for (i in 1:length(bixby_max_TMAX$TMAX)) {
  bixby_max_TMAX$TMAX[i] <- as.numeric(bixby_max_TMAX$TMAX[i]) / 10
}
bixby_max_TMAX$DATE <- ymd(bixby_max_TMAX$DATE)
class(bixby_max_TMAX$DATE)
bixby_max_TMAX$year <- as.Date(cut(bixby_max_TMAX$DATE, breaks='year'))
bixby_max_TMAX <- bixby_max_TMAX %>%
  group_by(year) %>%
  summarize(maxTMAX = max(TMAX, na.rm = TRUE))
#View(bixby_max_TMAX)
summary(bixby_max_TMAX)

#Bixby Min TMAX
bixby_min_TMAX <- read.csv("USC00340782.csv")
bixby_min_TMAX <- filter(bixby_min_TMAX, !is.na(TMAX))
for (i in 1:length(bixby_min_TMAX$TMAX)) {
  bixby_min_TMAX$TMAX[i] <- as.numeric(bixby_min_TMAX$TMAX[i]) / 10
}
bixby_min_TMAX$DATE <- ymd(bixby_min_TMAX$DATE)
class(bixby_min_TMAX$DATE)
bixby_min_TMAX$year <- as.Date(cut(bixby_min_TMAX$DATE, breaks='year'))
bixby_min_TMAX <- bixby_min_TMAX %>%
  group_by(year) %>%
  summarize(minTMAX = min(TMAX, na.rm = TRUE))
#View(bixby_min_TMAX)
summary(bixby_min_TMAX)

#Bixby Mean TMAX
bixby <- read.csv("USC00340782.csv")
bixby <- filter(bixby, !is.na(TMAX))
for (i in 1:length(bixby$TMAX)) {
  bixby$TMAX[i] <- as.numeric(bixby$TMAX[i]) / 10
}
bixby$DATE <- ymd(bixby$DATE)
class(bixby$DATE)
bixby$year <- as.Date(cut(bixby$DATE, breaks='year'))
bixby <- bixby %>%
  group_by(year) %>%
  summarize(meanTMAX = mean(TMAX, na.rm = TRUE))
#View(bixby)
summary(bixby)


#TIA Max TMAX
tia_max_TMAX <- read.csv("USW00013968.csv")
tia_max_TMAX <- filter(tia_max_TMAX, !is.na(TMAX))
for (i in 1:length(tia_max_TMAX$TMAX)) {
  tia_max_TMAX$TMAX[i] <- as.numeric(tia_max_TMAX$TMAX[i]) / 10
}
tia_max_TMAX$DATE <- ymd(tia_max_TMAX$DATE)
class(tia_max_TMAX$DATE)
tia_max_TMAX$year <- as.Date(cut(tia_max_TMAX$DATE, breaks='year'))
tia_max_TMAX <- tia_max_TMAX %>%
  group_by(year) %>%
  summarize(maxTMAX = max(TMAX, na.rm = TRUE))
#View(tia_max_TMAX)
summary(tia_max_TMAX)

#TIA Min TMAX
tia_min_TMAX <- read.csv("USW00013968.csv")
tia_min_TMAX <- filter(tia_min_TMAX, !is.na(TMAX))
for (i in 1:length(tia_min_TMAX$TMAX)) {
  tia_min_TMAX$TMAX[i] <- as.numeric(tia_min_TMAX$TMAX[i]) / 10
}
tia_min_TMAX$DATE <- ymd(tia_min_TMAX$DATE)
class(tia_min_TMAX$DATE)
tia_min_TMAX$year <- as.Date(cut(tia_min_TMAX$DATE, breaks='year'))
tia_min_TMAX <- tia_min_TMAX %>%
  group_by(year) %>%
  summarize(minTMAX = min(TMAX, na.rm = TRUE))
#View(tia_min_TMAX)
summary(tia_min_TMAX)

#TIA Mean TMAX
tia <- read.csv("USW00013968.csv")
tia <- filter(tia, !is.na(TMAX))
for (i in 1:length(tia$TMAX)) {
  tia$TMAX[i] <- as.numeric(tia$TMAX[i]) / 10
}
tia$DATE <- ymd(tia$DATE)
class(tia$DATE)
tia$year <- as.Date(cut(tia$DATE, breaks='year'))
tia <- tia %>%
  group_by(year) %>%
  summarize(meanTMAX = mean(TMAX, na.rm = TRUE))
#View(tia)
summary(tia)

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

#TRJA Mean TMAX
trja <- read.csv("USW00053908.csv")
trja <- filter(trja, !is.na(TMAX))
for (i in 1:length(trja$TMAX)) {
  trja$TMAX[i] <- as.numeric(trja$TMAX[i]) / 10
}
trja$DATE <- ymd(trja$DATE)
class(trja$DATE)
trja$year <- as.Date(cut(trja$DATE, breaks='year'))
trja <- trja %>%
  group_by(year) %>%
  summarize(meanTMAX = mean(TMAX, na.rm = TRUE))
#View(trja)
summary(trja)


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

#OKC Mean TMAX
okc <- read.csv("OKC_area_mesonet.csv")
okc <- filter(okc, !is.na(TMAX), okc$TMAX >= 0)
okc$TMAX <- (okc$TMAX-32)*5/9
okc$TIME <- substring(okc$TIME, 1, 10)
okc$TIME <- ymd(okc$TIME)
class(okc$TIME)
okc$year <- as.Date(cut(okc$TIME, breaks='year'))
okc <- okc %>%
  group_by(year) %>%
  summarize(meanTMAX = mean(TMAX, na.rm = TRUE))
#View(okc)
summary(okc)

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
