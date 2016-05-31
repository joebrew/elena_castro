library(raster)
library(sp)
library(readr)
library(dplyr)
library(rgdal)


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

# Convert people locations to spatial
coordenadas_spatial <- data.frame(coordenadas)
coordinates(coordenadas_spatial) <- ~x+y
proj4string(coordenadas_spatial) <- CRS("+proj=longlat")

# Also get place US locations
us_spatial <- data.frame(coordenadas)
coordinates(us_spatial) <- ~x_us + y_us
proj4string(us_spatial) <- CRS("+proj=longlat")

# Now calculate distance
distances <- 
  spDists(x = coordenadas_spatial, 
        y = us_spatial,
        longlat = TRUE,
        diagonal = TRUE)

# Add distances to coordenadas dataframe
coordenadas$distance <- distances

# Make a map of Mozambique
moz <- getData(name = 'GADM', country = 'MOZ', level = 3)
# Subset to only include maputo province
map <-moz[moz@data$NAME_1 == 'Maputo',]
# Projection
proj4string(map) <- proj4string(coordenadas_spatial)
# Keep only those polygons which have points
polygons <- as.numeric(unique(sort(over(coordenadas_spatial, polygons(map)))))
map <- map[polygons,]

# Plot map with all the residence points
par(mar = c(1,1,1,1))
par(oma = c(0,0,0,0))
plot(map,
     col = adjustcolor('black', alpha.f = 0.6),
     border = 'white')
points(x = coordenadas$lng,
       y = coordenadas$lat,
       col = adjustcolor('darkred', alpha.f = 0.6),
       pch = 20,
       cex = 0.1)
# Add the unique unidades sanitarias
us <- coordenadas[!duplicated(coordenadas$Unidade_Sanitaria),]
points(x = us$x_us,
       y = us$y_us,
       col = adjustcolor('darkgreen', alpha.f = 0.6),
       pch = 17)
legend(x = 'bottomleft',
       pch = c(20, 17),
       col = adjustcolor(c('darkred', 'darkgreen'), alpha.f = 0.6),
       legend = c('People', 'Unidade sanitaria'),
       pt.cex = c(0.1, 1))

# Loop through each point showing the distance

plot(map)
for (i in 1:nrow(coordenadas)){
  sub_data <- coordenadas[i,]  
  lines(x = c(sub_data$lng, sub_data$lng_us),
         y = c(sub_data$lat, sub_data$lat_us),
        col = adjustcolor('darkred', alpha.f = 0.6))
}
