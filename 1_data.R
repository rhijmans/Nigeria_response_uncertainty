
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/maize_variability"
} else if (this == "DESKTOP-JORDAN") {
  wd <- "C:/DATA/Nigeria/EiA"
} else {
  wd <- "."
}


### weather 

rpath <- "data/raw/chirps/"
ipath <- "data/intermediate/chirps/"
dir.create(rpath, FALSE, FALSE)
dir.create(ipath, FALSE, FALSE)
setwd(rpath)

url <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_dekad/tifs/"
p <- readLines(url)
p <- grep("a href=", p, value=TRUE)
p <- grep("chirps-v2.0", p, value=TRUE)
k <- sapply(strsplit(p, "a href=\""), \(i)i[2]) 
k <- sapply(strsplit(k, "\""), \(i)i[1]) 
urls <- paste0(url, k)

for (u in rev(urls)) {
	fn <- basename(u)
	if (!file.exists(fn)) {
		try(download.file(u, fn, mode="wb"))
	}
}


for (u in rev(urls)) {
	fn <- basename(u)
	ftif <- gsub(".gz$", "", fn)
	if (!file.exists(ftif)) {
		R.utils::gunzip(fn, remove=FALSE)
	}
}


ff <- list.files(pattern="tif$")
for (y in 1981:2023) {
	outf <- paste0("../../intermediate/chirps/chirps_dekades_africa_", y, ".tif") 
	if (!file.exists(outf)) {
		fs <- grep(y, ff, value=TRUE)
		x <- terra::rast(fs)
		terra::NAflag(x) <- -9999
		print(outf); flush.console()
		terra::writeRaster(x, outf) 
	}
}


ff <- list.files("../../intermediate/chirps", pattern="^chirps_dekades_africa_....\\.tif$", full=TRUE)

outf <- gsub("dekades", "dekades_sum", basename(ff))
outsum <- paste0("../../intermediate/chirps/stats/", outf)
outcv <- gsub("_sum", "_cv", outsum)

dir.create(dirname(outsum[1]), FALSE, FALSE)

for (i in 1:length(ff)) {
	if (!file.exists(outcv[i])) {
		print(basename(ff[i])); flush.console()
		r <- terra::rast(ff[i])
		year <- gsub("chirps_dekades_africa_|.tif", "", basename(ff[i]))
		rsum <- round(sum(r))
		terra::writeRaster(rsum, filename=outsum[i], overwrite=TRUE, wopt=list(names=paste0("rain_", year)))
		terra::writeRaster(terra::stdev(r) / terra::mean(r), filename=outcv[i], names=paste0("raincv_", year), overwrite=TRUE)
	}
}




### soil

setwd(wd)

vars <- c("Al", "C.tot", "N.tot", "OC", "P", "K", "ph.h2o", "sand", "silt", "clay")
soil.30s <- geodata::soil_af_isda(vars, path="data/raw")

template <- terra::rast(ext=c(-20, 55, -40, 40), res=0.05) 
names(soil.30s) <- gsub("ph.h2o", "pH", names(soil.30s))
names(soil.30s) <- gsub(".0-20cm", "", names(soil.30s))
names(soil.30s) <- gsub(".tot....", "", names(soil.30s))

# aggregate soils to make compatible with CHIRPS data (3 minutes)
dir.create("data/intermediate/soil")
rsoil <- terra::resample(soil.30s, template, filename=file.path("data/intermediate/soil", "soil_af_isda_3m.tif"))

### carob
 
ds <- file.path(wd, "data/raw/carob_survey-cc.csv")
### carobda <- file.path(wd, "data/raw/carob_agronomy-cc.csv")
if (!file.exists(draw)) {
	dir.create(dirname(draw), FALSE, TRUE)
	url <- "https://geodata.ucdavis.edu/carob/carob_agronomy-cc.zip"
	fzip <- file.path(wd, "data", "raw", basename(url))
	download.file(url, fzip, mode="wb")
	unzip(fzip, exdir=dirname(fzip))
}

x = read.csv(draw)

#https://geodata.ucdavis.edu/carob/carob_agronomy-cc.zip