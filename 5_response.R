
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

adm1 <- geodata::gadm(country="NGA", level=1, path="data/raw")
#nga_ext <- floor(ext(adm0))
nga_ext <- c(2, 15, 4, 14)

# price rasters
npkg <- terra::rast("data/intermediate/prices/npkg.tif")
mpkg <- terra::rast("data/intermediate/prices/mpkg.tif")

library(terra)
# maize obs
d <- read.csv("data/intermediate/observations/NGA_Jordan.csv")
s <- vect(d, geom=c("longitude", "latitude"))
# plot(adm1); points(s, pch=20, col="red")

# bring in raster layers for prediction
rain_sum <- list.files("data/intermediate/chirps/stats", pattern="_sum", full.names=TRUE) |> terra::rast()
rain_cv <- list.files("data/intermediate/chirps/stats", pattern="_cv", full.names=TRUE) |> terra::rast()
soil <- terra::rast("data/intermediate/soil/soil_af_isda_3m.tif")

# extract values from rasters; by year for rain; for now ignore empty years
## TODO: 
## write a generalized function that does this for growing seasons
## this is not as important in Nigeria, but essential for seasons that span two calendar years
s <- s[!is.na(s$pyear), ]
esum <- extract(rain_sum, s, layer=match(s$pyear, gsub("rain_", "", names(rain_sum))))
ecv <- extract(rain_cv, s, layer=match(s$pyear, gsub("raincv_", "", names(rain_cv))))
esoil <- extract(soil, s, ID=FALSE)

s <- cbind(s, data.frame(rain=esum$value, raincv=ecv$value, esoil))

#### fit model
#model <- yield ~ N_fertilizer + P_fertilizer + K_fertilizer + oc + pH + sand + clay + rain + raincv
predictors <- c("N_fertilizer", "P_fertilizer", "K_fertilizer", "oc", "pH", "sand", "clay", "rain", "raincv")
d <- data.frame(s)[, c("yield", predictors)] |> na.omit()
rfm <- randomForest::randomForest(x=d[, -1], y=d[,1], ntree=50)


#### find optimal NPK doses 
optimalNPK <- function() {

## TODO write function for this 

	out <- terra::rast(ext=nga_ext, res=0.05, nlyr=3, names=c("N_fertilizer", "P_fertilizer", "K_fertilizer"))
	terra::values(out) <- cbind(100, 50, 15)
	out
#ftreats <- expand.grid(N_fertlilizer=seq(0, 300, 25), P_fertilzer=seq(0, 150, 25), K_fertilizer=seq(0, 250, 25))
#meanrain <- mean(rainsum, wopt=list(names="rain"))
#meanraincv <- mean(raincv, wopt=list(names="raincv"))
#optpreds <- c(soil, meanrain, meanraincv)
#optpreds <- crop(optpreds, nga_ext)
}


profitability <- function(yield, yield0, value, cost, filename="") {
	profit <- (yield - yield0) * value - cost
	if (filename != "") {
		profit <- writeRaster(profit, filename=filename, overwrite=TRUE)
	}
	profit
}


optNPK <- optimalNPK()

#### predict yields ####
soil <- crop(soil, nga_ext)
rainsum <- crop(rain_sum, nga_ext)
raincv <- crop(rain_cv, nga_ext)
meanrainsum <- mean(rainsum)
meanraincv <- mean(raincv)

npkg <- resample(npkg, soil)
mpkg <- resample(mpkg, soil)
cost <- sum(optNPK * npkg)

preds <- c(soil, optNPK)
preds0 <- preds
preds0$N_fertilizer <- preds0$P_fertilizer <- preds0$K_fertilizer <- 0

### average year profit 
fprofit <- paste0("data/results/model/profit_meanrain.tif")
if (!file.exists(fprofit)) {
	preds0$rain <- preds$rain <- meanrainsum
	preds0$raincv <- preds$raincv <- meanraincv
	outf0 <- paste0("data/results/model/yield0_meanrain.tif")
	outf <- gsub("yield0", "yield", outf)
	yield0 <- predict(preds0, rfm, ext=nga_ext, filename=outf0, na.rm=TRUE, overwrite=TRUE)
	yield <- predict(preds, rfm, ext=nga_ext, filename=outf, na.rm=TRUE, overwrite=TRUE)
	profit <- profitability(yield, yield0, mpkg, cost, filename=fprofit)
}


years <- gsub("rain_", "", names(rainsum))
for (i in 1:length(years)) {
	y <- years[i]
	print(y); flush.console()
	fprofit <- paste0("data/results/model/profit_", y, ".tif")
	if (file.exists(fprofit)) next
	preds0$rain <- preds$rain <- rainsum[[paste0("rain_", y)]]
	preds0$raincv <- preds$raincv <- raincv[[paste0("raincv_", y)]]
	outf0 <- paste0("data/results/model/yield0_", y, ".tif")
	outf <- gsub("yield0", "yield", outf)
	yield0 <- predict(preds0, rfm, ext=nga_ext, filename=outf0, na.rm=TRUE, overwrite=TRUE)
	yield <- predict(preds, rfm, ext=nga_ext, filename=outf, na.rm=TRUE, overwrite=TRUE)
	profit <- profitability(yield, yield0, mpkg, cost, filename=fprofit)
}


