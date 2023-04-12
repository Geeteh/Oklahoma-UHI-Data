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
library(plyr)
library(rgeos)
library(extrafont)
library(scales)

setwd("C:/Users/drake/Downloads/")
font_import("C:/Users/drake/Downloads/Century Gothic/")

land <- raster("nlcd_2019_land_cover_l48_20210604.img")
# plot(land)

loadfonts(device = "win")

subsample <- c(
  xmin=-100000, xmax=100000,
  ymin=1000000, ymax=2000000
)

template_raster <- raster(extent(-96.61787, -95.76989, 35.63225, 36.44585),
                          resolution = c(0.0001, 0.0001),
                          crs = "+proj=longlat +datum=WGS84")

land_cropped <- crop(land, subsample)
land_cropped <- projectRaster(land_cropped, crs = CRS("+proj=longlat +datum=WGS84"))
land_cropped <- resample(land_cropped, template_raster)


values <- getValues(land_cropped)
coords <- coordinates(land_cropped)

land_df <- as.data.frame(land_cropped)
land_df <- data.frame(long = coords[,1], lat = coords[,2], value = values)

ok_map <- map_data("county","oklahoma")
tulsa_county_map <- filter(ok_map, subregion=="tulsa" | subregion=="creek")


# Tulsa international
point <- data.frame(long = -95.87825, lat = 36.19854)
point_sf <- st_as_sf(point, coords = c("long", "lat"), crs = 4326)
buffer_sf <- st_buffer(point_sf, dist = 1609.34)
buffer <- st_as_sf(buffer_sf)
nlcd_point1 <- crop(land_cropped, buffer)
coords <- coordinates(nlcd_point1)
values <- getValues(nlcd_point1)
nlcd_pt1_df <- as.data.frame(nlcd_point1)
nlcd_pt1_df <- data.frame(long = coords[,1], lat = coords[,2], value = values)
land_categories <- c(11, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95)
nlcd_pt1_df$value <- land_categories[findInterval(nlcd_pt1_df$value, land_categories, all.inside = TRUE)]
landcover_levels <- land_categories
landcover_labels <- c("Open water", "Developed, open space", "Developed, low intensity",
                      "Developed, medium intensity", "Developed high intensity", "Barren land (rock/sand/clay)",
                      "Deciduous forest", "Evergreen forest", "Mixed forest", "Shrub/scrub",
                      "Grassland/herbaceous", "Pasture/hay", "Cultivated crops", "Woody wetlands", "Emergent herbaceous wetlands")
landcover_colors <- c("#336699", "#ffcccc", "#ff9999", "#ff6666", "#ff3333", "#8c8c8c", "#1a9641", 
                      "#006d2c", "#a6d96a", "#dfc27d", "#ffe6b3", "#cccc00", "#b37700", "#99ccff", "#6699cc")

freq_table <- as.data.frame(table(nlcd_pt1_df$value))
freq_table$Percent <- prop.table(freq_table$Freq)
for (i in 1:nrow(freq_table)) {
  if (freq_table$Percent[i] < 0.02 && freq_table$Percent[i] >= 0.01) {
    freq_table$Percent_string[i] <- paste0(round(freq_table$Percent[i] * 100, 1), "%")
    
  } else if (freq_table$Percent[i] < 0.01) {
    freq_table$Percent_string[i] <- ""
    #freq_table$Percent_string[i] <- paste0(gsub("^0", "",round(freq_table$Percent[i] * 100, 1)), "%")
    #freq_table$Percent_string[i] <- gsub("^0", "", freq_table$Percent_string)
    
  } else {
    freq_table$Percent_string[i] <- paste0(round(freq_table$Percent[i] * 100, 1), "%")
  }
}

buffer1 <- ggplot(data = freq_table, aes(x = "", y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = 1.56, label = Percent_string), position = position_stack(vjust = 0.5), size = ifelse(freq_table$Percent < 0.02, 7.5, 10),
            family = "Century Gothic") + 
  coord_polar("y", start=0) +
  labs(fill = "Land Cover Categories") +
  ggtitle("Land Cover Categories in Tulsa International Airport Buffer of 1 Mile") +
  scale_fill_manual(values = landcover_colors, labels = landcover_labels) +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family="Century Gothic", face="bold"), plot.title.position = "plot") +
  theme(legend.text = element_text(size = 17.5, family="Century Gothic"), legend.title = element_text(size = 20, family="Century Gothic")) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  

png(filename = "tia_buffer.png", width = 1400, height = 1200 , units = "px")
buffer1
dev.off()


#Tulsa Richard L Jones
point <- data.frame(long = -95.99016, lat = 36.04243)
point_sf <- st_as_sf(point, coords = c("long", "lat"), crs = 4326)
buffer_sf <- st_buffer(point_sf, dist = 1609.34)
buffer <- st_as_sf(buffer_sf)
nlcd_point1 <- crop(land_cropped, buffer)
coords <- coordinates(nlcd_point1)
values <- getValues(nlcd_point1)
nlcd_pt1_df <- as.data.frame(nlcd_point1)
nlcd_pt1_df <- data.frame(long = coords[,1], lat = coords[,2], value = values)
land_categories <- c(11, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95)
nlcd_pt1_df$value <- land_categories[findInterval(nlcd_pt1_df$value, land_categories, all.inside = TRUE)]
landcover_levels <- land_categories
landcover_labels <- c("Open water", "Developed, open space", "Developed, low intensity",
                      "Developed, medium intensity", "Developed high intensity", "Barren land (rock/sand/clay)",
                      "Deciduous forest", "Evergreen forest", "Mixed forest", "Shrub/scrub",
                      "Grassland/herbaceous", "Pasture/hay", "Cultivated crops", "Woody wetlands", "Emergent herbaceous wetlands")
landcover_colors <- c("#336699", "#ffcccc", "#ff9999", "#ff6666", "#ff3333", "#8c8c8c", "#1a9641", 
                               "#006d2c", "#a6d96a", "#dfc27d", "#ffe6b3", "#cccc00", "#b37700", "#99ccff", "#6699cc")
                               
freq_table <- as.data.frame(table(nlcd_pt1_df$value))
freq_table$Percent <- prop.table(freq_table$Freq)
for (i in 1:nrow(freq_table)) {
  if (freq_table$Percent[i] < 0.02 && freq_table$Percent[i] >= 0.01) {
    freq_table$Percent_string[i] <- paste0(round(freq_table$Percent[i] * 100, 1), "%")
    
  } else if (freq_table$Percent[i] < 0.01) {
    freq_table$Percent_string[i] <- ""
    #freq_table$Percent_string[i] <- paste0(gsub("^0", "",round(freq_table$Percent[i] * 100, 1)), "%")
    #freq_table$Percent_string[i] <- gsub("^0", "", freq_table$Percent_string)
    
  } else {
    freq_table$Percent_string[i] <- paste0(round(freq_table$Percent[i] * 100, 1), "%")
  }
}

buffer1 <- ggplot(data = freq_table, aes(x = "", y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = 1.56, label = Percent_string), position = position_stack(vjust = 0.5), size = ifelse(freq_table$Percent < 0.02, 7.5, 10),
            family = "Century Gothic") + 
  coord_polar("y", start=0) +
  labs(fill = "Land Cover Categories") +
  ggtitle("Land Cover Categories in Tulsa Richard L Jones Jr Airport Buffer of 1 Mile") +
  scale_fill_manual(values = landcover_colors, labels = landcover_labels) +
  theme(plot.title = element_text(size = 30, hjust = 0.5, family="Century Gothic", face="bold"), plot.title.position = "plot") +
  theme(legend.text = element_text(size = 17.5, family="Century Gothic"), legend.title = element_text(size = 20, family="Century Gothic")) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())


png(filename = "trja_buffer.png", width = 1400, height = 1200 , units = "px")
buffer1
dev.off()














# 
# 
# # Sapulpa
# point2 <- data.frame(long = -95.99016, lat = 36.04243)
# point_sf2 <- st_as_sf(point2, coords = c("long", "lat"), crs = 4326)
# buffer_sf2 <- st_buffer(point_sf2, dist = 1609.34)
# buffer2 <- st_as_sf(buffer_sf2)
# nlcd_point2 <- crop(land_cropped, buffer2)
# coords <- coordinates(nlcd_point2)
# values <- getValues(nlcd_point2)
# nlcd_pt2_df <- as.data.frame(nlcd_point2)
# nlcd_pt2_df <- data.frame(long = coords[,1], lat = coords[,2], value = values)
# land_categories <- c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95)
# nlcd_pt2_df$value <- land_categories[findInterval(nlcd_pt2_df$value, land_categories, all.inside = TRUE)]
# landcover_levels <- land_categories
# landcover_labels <- c("Open water", "Perennial ice/snow", "Developed, open space", "Developed, low intensity",
#                       "Developed, medium intensity", "Developed high intensity", "Barren land (rock/sand/clay)",
#                       "Deciduous forest", "Evergreen forest", "Mixed forest", "Shrub/scrub",
#                       "Grassland/herbaceous", "Pasture/hay", "Cultivated crops", "Woody wetlands", "Emergent herbaceous wetlands")
# landcover_colors <- c("#336699", "#ecf2f9", "#ffcccc", "#ff9999", "#ff6666", "#ff3333", "#8c8c8c", "#1a9641", 
#                                "#006d2c", "#a6d96a", "#dfc27d", "#ffe6b3", "#cccc00", "#b37700", "#99ccff", "#6699cc")
#                                
# freq_table2 <- as.data.frame(table(nlcd_pt2_df$value))
# View(freq_table2)
# 
# buffer2 <- ggplot(data = freq_table2, aes(x = Var1, y = Freq, fill = Var1)) + 
#   geom_pie(stat = "identity") +
#   labs(x = "Land Cover Categories", y = "Frequency") +
#   ggtitle("Frequency of Land Cover Categories in Tulsa Richard L Jones Jr Airport Buffer of 1 Mile") +
#   scale_x_discrete(labels = landcover_labels) +
#   scale_fill_manual(values = landcover_colors) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15)) +
#   theme(axis.text.y = element_text(size = 15)) +
#   theme(axis.title.x = element_text(size = 20)) +
#   theme(axis.title.y = element_text(size = 20)) +
#   theme(plot.title = element_text(size = 30, hjust = 0.5), plot.title.position = "plot") +
#   guides(fill = FALSE)
# 
# 
# png(filename = "trja_buffer.png", width = 1400, height = 1200 , units = "px")
# buffer2
# dev.off()

