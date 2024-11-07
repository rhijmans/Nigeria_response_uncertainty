
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/maize_variability"
} else if (this == "DESKTOP-JORDAN") {
  wd <- "C:/DATA/Nigeria/EiA"
} else {
  wd <- "."
}

setwd(wd)

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
		x <- rast(fs)
		NAflag(x) <- -9999
		print(outf); flush.console()
		writeRaster(x, outf) 
	}
}


ff <- list.files("../../intermediate/chirps", pattern="^chirps_dekades_africa_....\\.tif$", full=TRUE)
outf <- gsub("dekades", "sum", basename(ff))
outf <- paste0("../../intermediate/chirps/sum/", outf)
dir.create(dirname(outf[1]), FALSE, FALSE)

for (i in 1:length(ff)) mean(rast(ff[i]), filename=outf[i])
 