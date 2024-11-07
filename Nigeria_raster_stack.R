# define raster stack for Nigeria spatial predictions


this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/maize_variability"
} else if (this == "DESKTOP-JORDAN") {
  wd <- "C:/DATA/Nigeria/EiA"
} else {
  wd <- "."
}

setwd(wd)
library(geodata)
#### soils data ####

vars <- c("Al", "C.tot", "N.tot", "OC", "P", "K", "ph.h2o", "sand", "silt", "clay")
soil.30s <- soil_af_isda(vars, path="data/raw")

template <- rast(ext=c(-20, 55, -40, 40), res=0.05) 
names(soil.30as) <- gsub("ph.h2o", "pH", names(soil_30s))
# aggregate soils to make compatible with CHIRPS data (3 minutes)
rsoil <- resample(soil.30s, template, filename=file.path("data/intermediate/soil", "soil_af_isda_3m.tif"), overwrite=TRUE)

