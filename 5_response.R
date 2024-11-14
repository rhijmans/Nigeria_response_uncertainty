
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
## TODO update for SSA
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
set.seed(20241113)
rfm <- randomForest::randomForest(x=d[, -1], y=d[,1], ntree=50)

profitability <- function(yield, yield0, value, cost, filename="") {
	profit <- (yield - yield0) * value - cost
	if (filename != "") {
		profit <- writeRaster(profit, filename=filename, overwrite=TRUE)
	}
	profit
}

#### predict yields ####
soil <- crop(soil, nga_ext)
rainsum <- crop(rain_sum, nga_ext)
raincv <- crop(rain_cv, nga_ext)
meanrainsum <- mean(rainsum)
meanraincv <- mean(raincv)

npkg <- resample(npkg, soil)
mpkg <- resample(mpkg, soil)

### optimal NPK

preds <- soil
preds$rain <- meanrainsum
preds$raincv <- meanraincv
foutnpk <- paste0("data/intermediate/NPK/yield_N%sP%sK%s.tif")
dir.create("data/intermediate/NPK/", FALSE, TRUE)

treats <- expand.grid(N_fertilizer=seq(0, 300, 15), P_fertilizer=seq(0, 200, 15), K_fertilizer=seq(0, 250, 15))
rownames(treats) <- NULL
fynpk <- sprintf(foutnpk, treats$N_fertilizer, treats$P_fertilizer, treats$K_fertilizer)
fprof <- gsub("yield_", "profit_", fynpk)
fcost <- gsub("yield_", "cost_", fynpk)

if (!file.exists(fynpk[1])) {
	yield0 <- terra::predict(preds, rfm, ext=nga_ext, const=data.frame(treats[1,]), filename=fynpk[1], na.rm=TRUE, 
		overwrite=TRUE, wopt=list(names=gsub(".tif", "", basename(fynpk[1])))))
} else {
	yield0 <- terra::rast(fynpk[1])
}

profs <- list()
for (i in 2:nrow(treats)) { # skipping 0, 0, 0
	if (!file.exists(fprof[i])) {
		print(basename(fprof[i])); flush.console()
		yield <- predict(preds, rfm, ext=nga_ext, const=data.frame(treats[i,]), filename=fynpk[i], na.rm=TRUE, overwrite=TRUE, 
			wopt=list(names=gsub(".tif", "", basename(fynpk[i]))))
		## using same cost per kg for N, P, K. Need to improve that.
		cost <- sum(treats[i,]) * npkg
		cost <- writeRaster(cost, fcost[i], overwrite=TRUE, names=gsub(".tif", "", basename(fcost[i])))
		profit <- profitability(yield, yield0, mpkg, cost, filename=fprof[i], 
			wopt=list(names=gsub(".tif", "", basename(fprof[i]))))
	}
}

fopt <- paste0("data/final/optNPK.tif")
dir.create("data/final", FALSE, FALSE)
if (file.exists(fopt)) {
	optNPK <- rast(fopt) 
} else {
	p <- terra::rast(fprof[-1])
	pmx <- max(p)
	wmx <- which.max(p)
	npk <- do.call(rbind, strsplit(gsub(".tif", "", (gsub("profit_", "", basename(sources(p))))), "N|P|K"))[,-1]
	npk <- matrix(as.numeric(npk), ncol=3)
	optNPK <- app(wmx, \(i) {out <- cbind(i,i,i); out[!is.na(i), ] <- npk[na.omit(i),]; out}, 
		wopt=list(names=(paste0(c("N","P","K"), "_fertilizer"))))
	optNPK <- mask(optNPK, pmx<0, maskvalues=TRUE, updatevalue=0, filename=fopt, names="optNPK")
}

cost <- sum(optNPK * npkg)
preds <- c(soil, optNPK)

### average year profit 
favgyld <- paste0("data/final/yield_meanrain.tif")
favgprof <- paste0("data/final/profit_meanrain.tif")
if (!file.exists(favgprof)) {
	preds$rain <- meanrainsum
	preds$raincv <- meanraincv
	yield <- predict(preds, rfm, ext=nga_ext, filename=favgyld, na.rm=TRUE, overwrite=TRUE, wopt=list(names="yield"))
	profit <- profitability(yield, yield0, mpkg, cost, filename=favgprof)
}

preds0 <- preds
preds0$N_fertilizer <- preds0$P_fertilizer <- preds0$K_fertilizer
years <- gsub("rain_", "", names(rainsum))
for (i in 1:length(years)) {
	y <- years[i]
	print(y); flush.console()
	fprofit <- paste0("data/final/model/profit_", y, ".tif")
#	if (file.exists(fprofit)) next
	preds0$rain <- preds$rain <- rainsum[[paste0("rain_", y)]]
	preds0$raincv <- preds$raincv <- raincv[[paste0("raincv_", y)]]
	outf0 <- paste0("data/final/model/yield0_", y, ".tif")
	outf <- gsub("yield0", "yield", outf0)
	yield0 <- predict(preds0, rfm, ext=nga_ext, filename=outf0, na.rm=TRUE, overwrite=TRUE)
	yield <- predict(preds, rfm, ext=nga_ext, filename=outf, na.rm=TRUE, overwrite=TRUE)
	profit <- profitability(yield, yield0, mpkg, cost, filename=fprofit)
}


