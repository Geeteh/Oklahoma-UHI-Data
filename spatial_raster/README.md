# Spatial Raster Data

***spatial.R*** - Program that handles and parses the ***NLCD (National Land Cover Dataset)***, mapping it to a coordinate reference system so that its longitudes and latitudes can be manipulated and cleaned in the form of a data frame. Program will also output a bounding box to Tulsa County and Creek County with simple feature 1-mile buffers around the mesonet sites and a spatial polygon of the Tulsa/Creek County borders (long/lat for border retrieved from ***ggplot2 map_data*** built-in dataset).

***Weather_station_map.png*** - Figure of the data compiled by ***spatial.R*** used for geographical analyzation of land cover categories in Tulsa/Creek counties and the 1-mile buffers of the given mesonet sites
