library(tidyverse)
library(ggplot2)
library(maps)
library(grDevices)
library(ggmap)
library(rgdal)
library(raster)
library(magrittr)
library(stars)
library(rasterVis)
library(sp)
library(dplyr)
library(rgeos)
library(sf)

install.packages("sf")

packageVersion("sf")

setwd("C:/Users/drake/Downloads/")

land <- raster("nlcd_2019_land_cover_l48_20210604.img")
# plot(land)

subsample <- c(
  xmin=-100000, xmax=100000,
  ymin=1000000, ymax=2000000
)

template_raster <- raster(extent(-96.61787, -95.76989, 35.63225, 36.44585),
                          resolution = c(0.0001, 0.0001),
                          crs = "+proj=longlat +datum=WGS84")

land_cropped@crs

land_cropped <- crop(land, subsample)
land_cropped <- projectRaster(land_cropped, crs = CRS("+proj=longlat +datum=WGS84"))
land_cropped <- resample(land_cropped, template_raster)


values <- getValues(land_cropped)
coords <- coordinates(land_cropped)

land_df <- as.data.frame(land_cropped)
land_df <- data.frame(long = coords[,1], lat = coords[,2], value = values)
# getValues(land_cropped)

ok_map <- map_data("county","oklahoma")
tulsa_county_map <- filter(ok_map, subregion=="tulsa" | subregion=="creek")

land_categories <- c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95)
land_df$value <- land_categories[findInterval(land_df$value, land_categories, all.inside = TRUE)]
landcover_levels <- land_categories
landcover_labels <- c("Open water", "Perennial ice/snow", "Developed, open space", "Developed, low intensity",
                      "Developed, medium intensity", "Developed high intensity", "Barren land (rock/sand/clay)",
                      "Deciduous forest", "Evergreen forest", "Mixed forest", "Shrub/scrub",
                      "Grassland/herbaceous", "Pasture/hay", "Cultivated crops", "Woody wetlands", "Emergent herbaceous wetlands")


land_df$landcover <- factor(land_df$value, levels = landcover_levels, labels = landcover_labels)

land_df %>%
  #activate(edges) %>%
  #as_tibble() %>%
  st_as_sf(coords = c("long","lat")) %>% # this is the important part!
  st_write("nlcd.shp")

point <- data.frame(long = -95.88333, lat = 35.98333)
point_sf <- st_as_sf(point, coords = c("long", "lat"), crs = 4326)
buffer_sf <- st_buffer(point_sf, dist = 1609.34)
buffer <- st_as_sf(buffer_sf)

point_2 <- data.frame(long = -96.13333, lat = 36)
point2_sf <- st_as_sf(point_2, coords = c("long","lat"), crs=4326)
buffer2_sf <- st_buffer(point2_sf, dist=1609.34)
buffer2 <- st_as_sf(buffer2_sf)

point3 <- data.frame(long = -95.87825, lat = 36.19854)
point3_sf <- st_as_sf(point3, coords = c("long","lat"), crs=4326)
buffer3_sf <- st_buffer(point3_sf, dist=1609.34)
buffer3 <- st_as_sf(buffer3_sf)

point4 <- data.frame(long = -95.99016, lat = 36.04243)
point4_sf <- st_as_sf(point4, coords = c("long","lat"), crs=4326)
buffer4_sf <- st_buffer(point4_sf, dist=1609.34)
buffer4 <- st_as_sf(buffer4_sf)




wx_station_map <- ggplot() +
  geom_raster(data=land_df, aes(long, lat, fill=landcover)) +
  scale_fill_manual(values = c("#336699", "#ecf2f9", "#ffcccc", "#ff9999", "#ff6666", "#ff3333", "#8c8c8c", "#1a9641", 
                               "#006d2c", "#a6d96a", "#dfc27d", "#ffe6b3", "#cccc00", "#b37700", "#99ccff", "#6699cc")) +
  xlab("Longitude") +
  ylab("Latitude") +
  geom_polygon(data=tulsa_county_map, aes(long, lat, group = group), color = "black", linewidth=1, fill = NA) +
  geom_sf(data = buffer, fill = NA, color = "black", size = 1.5) +
  geom_point(data=data.frame(long=-95.88333, lat=35.98333), aes(x=long, y=lat), color="black", size=3) +
  geom_text(data=data.frame(long=-95.88333, lat=35.98333), aes(x=long, y=lat, label="BIXBY, OK"), color="black", vjust=2) +
  geom_sf(data=buffer2, fill = NA, color= "black", size = 1.5) +
  geom_point(data=data.frame(long=-96.13333, lat=36), aes(x=long, y=lat), color="black", size=3) +
  geom_text(data=data.frame(long=-96.13333, lat=36), aes(x=long, y=lat, label="SAPULPA 1 W, OK"), color="black", hjust=1.1, vjust=2) +
  geom_sf(data=buffer3, fill=NA, color="black",size=1.5) +
  geom_point(data=data.frame(long=-95.87825, lat=36.19854), aes(x=long, y=lat), color="black", size=3) +
  geom_text(data=data.frame(long=-95.87825, lat=36.19854), aes(x=long, y=lat, label="TULSA INTERNATIONAL AIRPORT, OK"), color="black", hjust=1.05, vjust=-.5) +
  geom_sf(data = buffer4, fill=NA, color= "black", size = 1.5) +
  geom_point(data=data.frame(long=-95.99016, lat=36.04243), aes(x=long, y=lat), color="black", size=3) +
  geom_text(data=data.frame(long=-95.99016, lat=36.04243), aes(x=long, y=lat, label="TULSA RICHARD L JONES JR AIRPORT, OK"), color="black", hjust=.6, vjust=-.8) +
  labs(c("Bixby", "Sapulpa", "Tulsa International", "Tulsa Richard Jones Jr"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15)) +
  theme(axis.text.y = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(plot.title = element_text(size = 30, hjust = 0.5), plot.title.position = "plot")

png(filename = "Weather_station_map.png", width = 1400, height = 1200 , units = "px")
wx_station_map
dev.off()
