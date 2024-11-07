# define raster stack for Nigeria spatial price predictions

#### set up #### 
setwd("C:/DATA/Nigeria/prices")

library(leaflet)
library(geodata)
library(terra)

list.files()

#### vector data #### 
# Nigeria code NGA
adm0 <- gadm(country="NGA", level=0, path=getwd())
adm1 <- gadm(country="NGA", level=1, path=getwd())
adm2 <- gadm(country="NGA", level=2, path=getwd())
#adm3 <- gadm(country="NGA", level=3, path=getwd())


geodata_path("C:/DATA/geodata")

# population
pop <- geodata::population(2020, res=0.5)
  writeRaster(pop, "C:/DATA/geodata/pop/gpw_v4_population_density_rev11_2020_30s.tif", overwrite=FALSE)
pop <- crop(pop, adm0, snap="near", mask=TRUE)
  writeRaster(pop, "C:/DATA/Nigeria/geodata/pop/gpw_v4_population_density_rev11_2020_30s_Nigeria.tif", overwrite=FALSE)

# access
#tt_cities_u3 <- travel_time(to="city", size=3, up=TRUE, path, ...) 
tt_cities_u3 <- rast("C:/DATA/geodata/travel/travel_time_to_cities_u3.tif")
tt_cities_u3 <- crop(tt_cities_u3, adm0, snap="near", mask=TRUE)
  writeRaster(tt_cities_u3, "C:/DATA/Nigeria/geodata/travel/travel_time_to_cities_u3_Nigeria.tif", overwrite=FALSE)

#tt_ports_u2 <- travel_time(to="city", size=3, up=TRUE, path, ...) 
tt_ports_u2 <- rast("C:/DATA/geodata/travel/travel_time_to_ports_u2.tif")
tt_ports_u2 <- crop(tt_ports_u2, adm0, snap="near", mask=TRUE)
  writeRaster(tt_ports_u2, "C:/DATA/Nigeria/geodata/travel/travel_time_to_ports_u2_Nigeria.tif", overwrite=FALSE)


#### merge all raster layers ####
stack <- c(pop, tt_cities_u3, tt_ports_u2)
names(stack)

#### bring in prices

# Nitrogen prices 
nprices <- read.csv("C:/DATA/prices/fertilizer/Nigeria_urea_pkg_2024.csv")
nprices <- vect(nprices, geom=c("Longitude", "Latitude"), crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", keepgeom=TRUE)

plot(adm0, main = "Nitrogen prices (from urea)")
plot(nprices, pch = 19, cex = prices$N_pkg_USD / max(prices$N_pkg_USD) * 1, col = "blue", add=TRUE)


# Define a color palette
color_palette <- colorRampPalette(c("yellow", "red"))(length(unique(prices$N_pkg_USD)))

# Map values to colors
point_colors <- color_palette[as.numeric(factor(nprices$N_pkg_USD))]

# Plot with point color based on attribute "color_value"
plot(adm0, main = "Nitrogen prices (from urea)")
plot(nprices, pch = 19, col = point_colors, cex = 1.5, add=TRUE)

# maize prices from WPF 
mprices <- read.csv("C:/DATA/prices/wfp/wft_maize_wholesale_2023_nga.csv")
mprices <- vect(mprices, geom=c("longitude", "latitude"), crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", keepgeom=TRUE)

# Map values to colors
point_colors <- color_palette[as.numeric(factor(mprices$mai_pkg_usd))]

# Plot with point color based on attribute "color_value"
plot(adm0, main = "Maize wholesale prices (Jan 2023)")
plot(mprices, pch = 19, col = point_colors, cex = 1.5, add=TRUE)



# Generate Latitude and Longitude grid
latgrd <- longrd <- stack[[1]]
latgrd[] <- yFromCell(latgrd, 1:ncell(latgrd))
longrd[] <- xFromCell(longrd, 1:ncell(longrd))
latgrd <- mask(latgrd,stack[[1]])
longrd <- mask(longrd,stack[[1]])
names(latgrd) <- c("latitude")
names(longrd) <- c("longitude")

plot(c(stack,latgrd,longrd))
stack <- c(stack, longrd, latgrd)


#Extract to the point datasets
extr1 <- terra::extract(stack, nprices, method = "bilinear")
nprices <- cbind(nprices, extr1)

extr2 <- terra::extract(stack, mprices, method = "bilinear")
mprices <- cbind(mprices, extr2)



##### predict prices

rf1 <- randomForest(N_pkg_USD ~ population_density + 
                      travel_time_to_cities_1 + travel_time_to_ports_1 +
                      longitude + latitude,
                    data = nprices,
                    importance=TRUE, na.rm=TRUE)
rf1
varImpPlot(rf1)

npkg_pr1 <- predict(stack, rf1)
plot(npkg_pr1, main="Predicted N price (USD/kg)")

writeRaster(npkg_pr1, paste("C:/Github/chamb244/EiA2030-ex-ante/Nigeria_response_uncertainty", "npkg.tif", sep="/"), overwrite=TRUE)



rf2 <- randomForest(mai_pkg_usd ~ population_density + 
                    travel_time_to_cities_1 + travel_time_to_ports_1 +
                    longitude + latitude,
                    data = mprices,
                    importance=TRUE, na.rm=TRUE)
rf2
varImpPlot(rf2)

mpkg_pr2 <- predict(stack, rf2)
plot(mpkg_pr2, main="Predicted maize price (USD/kg)")

writeRaster(mpkg_pr2, paste("C:/Github/chamb244/EiA2030-ex-ante/Nigeria_response_uncertainty", "mpkg.tif", sep="/"), overwrite=TRUE)
