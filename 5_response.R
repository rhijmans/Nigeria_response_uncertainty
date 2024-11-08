
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/maize_variability"
} else if (this == "DESKTOP-JORDAN") {
  wd <- "C:/DATA/Nigeria/EiA"
} else {
  wd <- "."
}

setwd(wd)
dir.create("data/results/model/", FALSE, TRUE)

# Nigeria code NGA
adm0 <- geodata::gadm(country="NGA", level=0, path="data/raw")
adm1 <- geodata::gadm(country="NGA", level=1, path="data/raw")
adm2 <- geodata::gadm(country="NGA", level=2, path="data/raw")

# price rasters
npkg <- terra::rast("data/intermediate/prices/npkg.tif")
mpkg <- terra::rast("data/intermediate/prices/mpkg.tif")

library(terra)
# maize obs
d <- read.csv("data/intermediate/observations/NGA_Jordan.csv")
s <- vect(d, geom=c("longitude", "latitude"))

# map point locations; looks like pretty good coverage
# plot(adm1); points(s, pch=20, col="Red")

# bring in raster layers for prediction
rainsum <- list.files("data/intermediate/chirps/stats", pattern="_sum", full.names=TRUE) |> terra::rast()
raincv <- list.files("data/intermediate/chirps/stats", pattern="_cv", full.names=TRUE) |> terra::rast()
soil <- rast("data/intermediate/soil/soil_af_isda_3m.tif")

# combine rasters
# for now ignore empty years
s <- s[!is.na(s$pyear), ]
rsum <- extract(rainsum, s, layer=match(s$pyear, names(rainsum)))
rcv <- extract(raincv, s, layer=match(s$pyear, names(raincv)))
rsoil <- extract(soil, s, ID=FALSE)

s <- cbind(s, data.frame(rain=rsum$value, raincv=rcv$value, rsoil))

#### fit model
model <- yield ~ N_fertilizer + P_fertilizer + K_fertilizer + oc + pH + sand + clay + rain + raincv
library(randomForest)
predictors <- c("N_fertilizer", "P_fertilizer", "K_fertilizer", "oc", "pH", "sand", "clay", "rain", "raincv")
d <- data.frame(s)[, c("yield", predictors)] |> na.omit()
rfm <- randomForest(x=d[, -1], y=d[,1], ntree=50)


#### find optimal NPK doses 
#nga_ext <- floor(ext(adm0))
nga_ext <- c(2, 15, 4, 14)

#ftreats <- expand.grid(N_fertlilizer=seq(0, 300, 25), P_fertilzer=seq(0, 150, 25), K_fertilizer=seq(0, 250, 25))
#meanrain <- mean(rainsum, wopt=list(names="rain"))
#meanraincv <- mean(raincv, wopt=list(names="raincv"))
#optpreds <- c(soil, meanrain, meanraincv)
#optpreds <- crop(optpreds, nga_ext)

optNPK <- rast(ext=nga_ext, res=0.05, nlyr=3, names=c("N_fertilizer", "P_fertilizer", "K_fertilizer"))
values(optNPK) <- cbind(100, 50, 15)

#### prepare dataset for model estimation ####

#### predict yields ####

setwd("data/results/model")
preds <- c(soil, optNPK)
cost <- optNPK * npkg
years <- gsub("rain_", "", names(rainsum))

for (y in 1:length(years)) {
	print(y)
	preds$rain <- rainsum[[paste0("rain_", y)]]
	preds$raincv <- raincv[[paste0("raincv_", y)]]
	outf <- paste0("data/results/model/yield_", y, ".tif")
	yield <- predict(preds, rfm, ext=nga_ext, filename=outf)
	profit <- yield * mpkg - cost
	outf2 <- paste0("data/results/model/profit_", y, ".tif")
	writeRaster(profit, filename=outf2)
}


netrev <- (pr.avg * mpkg) - (100 * npkg)


summary(pr3)
# calculate average and variance/standard deviation of predictions
pr3.avg = app(pr3, fun=mean) 
pr3.var = app(pr3, fun=var)
names(pr3.var) <- "var"
pr3.sd = app(pr3, fun=sd)
pr3.cv = pr3.sd/pr3.avg
names(pr3.cv) <- "CV"

plot(c(pr3.avg, pr3.cv))


tmp1 <- as.numeric(global(pr3.avg, "mean", na.rm=TRUE)[1])
tmp2 <- as.numeric(global(pr3.cv, "mean", na.rm=TRUE)[1])

# Classify each cell as above or below the mean for each layer
pr3.avg.rc <- pr3.avg > tmp1
pr3.cv.rc <- pr3.cv > tmp2
plot(c(pr3.avg.rc, pr3.cv.rc))

# Combine classifications into a single raster with four classes
# Class definition:
# 1 = below mean in both layers
# 2 = above mean in r1, below mean in r2
# 3 = below mean in r1, above mean in r2
# 4 = above mean in both layers
classified_raster <- pr3.avg.rc + 2 * pr3.cv.rc + 1  # Encoding the classes

mylabels <- data.frame(values=1:4, label = c("Low returns, low variance", 
                                             "High returns, low variance",
                                             "Low returns, high variance", 
                                             "High returns, high variance"))
levels(classified_raster) <- mylabels

# Plot the classified raster
plot(classified_raster, main="Intervention areas, based on expected returns",
     col=c("blue", "green", "yellow", "red"),
     legend = TRUE, 
     plg = list(x = "bottomright"))

