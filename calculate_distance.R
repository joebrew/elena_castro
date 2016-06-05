library(raster)
library(sp)
library(readr)
library(dplyr)
library(rgdal)
library(ggplot2)
library(rgeos)
library(maptools)
library(ggthemes)

# Read in the spreadsheet with coordinates
coordenadas <- read_csv('CoordenadasDistancia.csv')

# Deal with encoding issues
coordenadas$US_latude <- iconv(enc2utf8(coordenadas$US_latude),sub="byte")
coordenadas$US_longude <- iconv(enc2utf8(coordenadas$US_longude),sub="byte")

# Clean up coordinates
coordenadas <- coordenadas %>%
  rename(lat = LatUTM,
         lng = LongUTM,
         lat_us = US_latude,
         lng_us = US_longude) %>%
  mutate(x = lng,
         y = lat,
         x_us = lng_us,
         y_us = lat_us)

# Convert UTM coordinates to lat long
coordenadas_spatial <- data.frame(coordenadas)
coordinates(coordenadas_spatial) <- ~x+y
# Assign projection
proj4string(coordenadas_spatial) <- CRS("+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# Convert to lat/lng
coordenadas_spatial <- 
  spTransform(coordenadas_spatial,CRS("+proj=longlat"))
# Extract the coordinates
ll <- coordinates(coordenadas_spatial)
# Add back into the original dataframe
coordenadas$x <- coordenadas$lng <- ll[,1]
coordenadas$y <- coordenadas$lat <- ll[,2]

# Clean up the unidad sanitaria coordinates

# First, need to define function
# (not currently vectorized, need to run loop)
convert_ll<-function(x){
  z <- as.numeric(gsub('\"', '', unlist(strsplit(x, '<b0>|\''))))
  z[1] + (z[2]/60) + (z[3]/3600)
} 
xs <- ys <- rep(NA, nrow(coordenadas))
for (i in 1:nrow(coordenadas)){
  message(i)
  xs[i] <- convert_ll(coordenadas$x_us[i])
  ys[i] <- convert_ll(coordenadas$y_us[i])
}

coordenadas$x_us <- coordenadas$lng_us <- xs
coordenadas$y_us <- coordenadas$lat_us <- ys*-1

# Now everything is in the right format: lat/lng

# MANUALLY enter in the true locations of the 
# unidades_sanitarias
unidades_sanitarias <- 
  data.frame(name = c('Xinavane',
                      '3 de fevereiro',
                      'Iha Josina'),
             lat = c("25'2'815",
                     "25'9'43",
                     "25'5'617"),
             lng = c("32'48'308",
                     "32'48'033",
                     "32'55'035"),
             stringsAsFactors = FALSE)
# convert
xs <- ys <- rep(NA, nrow(unidades_sanitarias))
for (i in 1:nrow(unidades_sanitarias)){
  message(i)
  xs[i] <- convert_ll(unidades_sanitarias$lng[i])
  ys[i] <- convert_ll(unidades_sanitarias$lat[i]) * -1
}
unidades_sanitarias$lng <- 
  unidades_sanitarias$x <- 
  xs
unidades_sanitarias$lat <- 
  unidades_sanitarias$y <- 
  ys

# convert unidades sanitarias to spatial
unidades_sanitarias_spatial <- data.frame(unidades_sanitarias)
coordinates(unidades_sanitarias_spatial) <- ~x + y
proj4string(unidades_sanitarias_spatial) <- CRS("+proj=longlat")

# Convert people locations to spatial
coordenadas_spatial <- data.frame(coordenadas)
coordinates(coordenadas_spatial) <- ~x+y
proj4string(coordenadas_spatial) <- CRS("+proj=longlat")

# # Also get place US locations
# us_spatial <- data.frame(coordenadas)
# coordinates(us_spatial) <- ~x_us + y_us
# proj4string(us_spatial) <- CRS("+proj=longlat")

# Now calculate distance
distances <- 
  spDists(x = coordenadas_spatial, 
        y = unidades_sanitarias_spatial,
        longlat = TRUE,
        diagonal = FALSE)

# Add distances to coordenadas dataframe
coordenadas$distance <- apply(distances, 1, function(x){x[which.min(x)]})

# Get nearest center
coordenadas$center <- 
  apply(distances,
        1, function(x){unidades_sanitarias$name[which.min(x)]})

# Make a map of Mozambique
moz <- getData(name = 'GADM', country = 'MOZ', level = 3)
# Subset to only include maputo province
map <-moz[moz@data$NAME_1 == 'Maputo',]
# Projection
# proj4string(map) <- proj4string(coordenadas_spatial)
coordenadas_spatial <- spTransform(coordenadas_spatial, proj4string(map))
# Keep only those polygons which have points
polygons <- as.numeric(unique(sort(over(coordenadas_spatial, polygons(map)))))
map <- map[polygons,]

# Plot map with all the residence points
# par(mar = c(1,1,1,1))
# par(oma = c(0,0,0,0))
plot(map,
     col = adjustcolor('black', alpha.f = 0.6),
     border = 'white')
points(x = coordenadas$lng,
       y = coordenadas$lat,
       col = adjustcolor('darkred', alpha.f = 0.6),
       pch = 20,
       cex = 0.1)
# Add the unique unidades sanitarias
# us <- coordenadas[!duplicated(coordenadas$Unidade_Sanitaria),]
us <- unidades_sanitarias_spatial
points(unidades_sanitarias_spatial,
       col = adjustcolor('darkgreen', alpha.f = 0.6),
       pch = 17)
legend(x = 'bottomleft',
       pch = c(20, 17),
       col = adjustcolor(c('darkred', 'darkgreen'), alpha.f = 0.6),
       legend = c('People', 'Unidade sanitaria'),
       pt.cex = c(0.1, 1))


# Plot map in which they are colored by distance
map_gg <- fortify(map, region = 'OBJECTID')

ggplot() +
  geom_polygon(data = map_gg,
               aes(x = long, 
                   y = lat, 
                   group = group),
               alpha = 0.4,
               fill = 'darkgreen') +
  coord_map() +
  geom_point(data = coordenadas,
              aes(x = x,
                  y = y,
                  color = distance),
             alpha = 0.5,
             size = 0.5) +
  geom_point(data = data.frame(us),
             aes(x = x, 
                 y = y),
             color = 'red',
             pch = 17) +
  theme_fivethirtyeight()

# Export the data
write_csv(coordenadas,
          '~/Desktop/coordinates_distances_and_centers.csv')


# # Define function for adding zero
# add_zero <- function (x, n) {
#   x <- as.character(x)
#   adders <- n - nchar(x)
#   adders <- ifelse(adders < 0, 0, adders)
#   for (i in 1:length(x)) {
#     if (!is.na(x[i])) {
#       x[i] <- paste0(paste0(rep("0", adders[i]), collapse = ""), 
#                      x[i], collapse = "")
#     }
#   }
#   return(x)
# }
# 
# # Loop through each point showing the distance
# setwd('~/Desktop')
# dir.create('elena')
# setwd('elena')
# plot(map)
# points(x = us$x_us,
#        y = us$y_us,
#        col = adjustcolor('red', alpha.f = 0.8),
#        pch = 17)
# 
# # Reorder coordenadas
# coordenadas <- coordenadas[sample(1:nrow(coordenadas), nrow(coordenadas)),]
# for (i in 1:nrow(coordenadas)){
#   # get file number
#   file_number <- add_zero(i, 5)
#   png(filename = paste0(file_number, '.png'))
#   plot(map)
#   points(x = us$x_us,
#          y = us$y_us,
#          col = adjustcolor('red', alpha.f = 0.8),
#          pch = 17)
#   
#   sub_data <- coordenadas[i,]  
#   
#   # Add sub data point
#   points(x = sub_data$lng,
#          y = sub_data$lat,
#          col = 'darkgreen',
#          pch = 1)
#   lines(x = c(sub_data$lng, sub_data$lng_us),
#          y = c(sub_data$lat, sub_data$lat_us),
#         col = adjustcolor('darkgreen', alpha.f = 0.8))
#   
#   # Add sub data place in green
#   points(sub_data$lng_us,
#          sub_data$lat_us,
#          col = 'darkgreen',
#          pch = 17)
#   title(main = paste0(sub_data$name, '\n',
#                       'U.S.: ', sub_data$Unidade_Sanitaria))  
#   dev.off()
# }
