
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	wd <- "G:/.shortcut-targets-by-id/1mfeEftF_LgRcxOT98CBIaBbYN4ZHkBr_/share/maize_variability"
} else if (this == "DESKTOP-JORDAN") {
  wd <- "C:/DATA/Nigeria/EiA"
} else {
  wd <- "."
}

setwd(wd)

# Nigeria code NGA
adm0 <- geodata::gadm(country="NGA", level=0, path="data/raw")
adm1 <- geodata::gadm(country="NGA", level=1, path="data/raw")
adm2 <- geodata::gadm(country="NGA", level=2, path="data/raw")

# price rasters
npkg <- terra::rast("data/intermediate/prices/npkg.tif")
mpkg <- terra::rast("data/intermediate/prices/mpkg.tif")

library(terra)
d <- read.csv("data/intermediate/observations/NGA_Jordan.csv")
s <- vect(d, geom=c("longitude", "latitude"))

# map point locations; looks like pretty good coverage
# plot(adm1); points(s, pch=20, col="Red")

# bring in raster layers for prediction
rain <- rast(file.path("rain", "Nigeria_rain_summaries.tif"))
soil <- rast(paste("soil_af_isda", "Nigeria_soil_layers.tif", sep="/"))

# combine rasters
rdata <- c(soil, rain)
names(rdata)

#### extract raster values to point locations ####
# # view points on raster data
# plot(stack["rain.sum.2017"])
# plot(s, add=TRUE, pch=20, col="Red")

i <- match(s$year, gsub("rain.sum.", "", names(rain)))
rsum <- extract(rain, s, layer=i)
i <- match(s$year, gsub("rain.cv.", "", names(rain)))
rcv <- extract(rain, s, layer=i)
#i <- match(s$year, gsub("rain.avg.", "", names(rain)))
#r2 <- extract(rain, s, layer=i)
#i <- match(s$year, gsub("rain.std.", "", names(rain)))
#r3 <- extract(rain, s, layer=i)

s <- cbind(s, data.frame(rsum=rsum, rcv=rcv))


#### prepare dataset for model estimation ####

# keep only years from 2001-2020 (last year is 2017)
sort(unique(s$year))
s.recent <- s[s$year > 2000]
dim(s.recent) # we still have 5187 obs, so not bad



#### predict yields ####

# first simple model
maizeyield.lm <- lm(yield ~ N_fertilizer + P_fertilizer + K_fertilizer + OC + pH + sand + clay + log(rain.sum) + rain.cv, data=s.recent)
summary(maizeyield.lm)
  # not terrible; 

# random forest model 
library(randomForest)

mypredictors <- c("N_fertilizer", "P_fertilizer", "K_fertilizer", "OC", "pH", "sand", "clay", "rain.sum", "rain.cv")
mydepvar <- c("yield") 
myvars <- append(mydepvar, mypredictors)
names(s.recent)

df <- as.data.frame(s.recent)
df <- df[complete.cases(df[, myvars]), myvars]

# tune the forest
trf <- tuneRF(x=df[, -1], y=df[,1])
trf
mt <- trf[which.min(trf[,2]),1]
mt

# fit the model
crf <- randomForest(x=df[, -1], y=df[,1],
                    mtry=mt,importance = TRUE)
crf
plot(crf)
importance(crf)
varImpPlot(crf)

# predict using raster stack
# note: we must set the rainfall variables equal to a given year
rain.sum <- stack["rain.sum.2020"]
names(rain.sum) <- c("rain.sum")
rain.cv  <- stack["rain.cv.2020"]
names(rain.cv) <- c("rain.cv")

newstack <- c(stack, rain.sum, rain.cv)


plot(pr)
#plot(lakes, col="Turquoise", border = NA, add=TRUE)
plot(adm0, border="Grey", add=TRUE)
plot(s, pch=20, col="Red", add=TRUE)

# this is the predicted value 
writeRaster(pr, "maizeyld_rf_pred.tif", overwrite=TRUE)


optfun <- function(d, pr0, preds, model) {
  if (any(d < 0)) return(-Inf)
  if (any(d > 400)) return(-Inf)
  d <- round(d, -1)
  pr <- predict(model, preds, const=data.frame(N_fertilizer=d[1], P_fertilizer=d[2], K_fertilizer=d[3]), na.rm=TRUE)
  (pr - pr0) * preds$Cp - (d[1] * preds$Np) - (d[2] * preds$Pp) - (d[3] * preds$Kp)
}

########### estimate optimal fert
gridfun <- function(preds, model) {
  if (any(is.na(preds))) return(rep(NA, 3))
  pr0 <- predict(model, cbind(preds, N_fertilizer=0, P_fertilizer=0, K_fertilizer=0))
  opt <- optim(c(100, 50, 50), optfun, preds=preds, model=model, control=list(fnscale=-1))
  opt$par
}

optimNPK <- terra::app(x, gridfun, model=crf)




ftreats <- expand.grid(N_fertlilizer=seq(0, 300, 25), P_fertilzer=seq(0, 150, 25), K_fertilizer=seq(0, 250, 25))



#1 get average wheather from the 20 years 
meanrain = mean(rain)
# replace newstack for each iteration 
newstack <- c(stack, meanrain)
# predict and send each prediction as new layer of output stack
add(pr2) <- predict(newstack, crf, const=data.frame(N_fertilizer=100, P_fertilizer=50, K_fertilizer=15), na.rm=TRUE)
}
############
#### estimate variability in predicted value coming from rainfall uncertainty ####

set.seed(1492)

# list of rainfall seasonal totals 
tlist <- c("rain.sum.2001", "rain.sum.2002", "rain.sum.2003", "rain.sum.2004", "rain.sum.2005", "rain.sum.2006", "rain.sum.2007", "rain.sum.2008", "rain.sum.2009", "rain.sum.2010", "rain.sum.2011", "rain.sum.2012", "rain.sum.2013", "rain.sum.2014", "rain.sum.2015", "rain.sum.2016", "rain.sum.2017", "rain.sum.2018", "rain.sum.2019", "rain.sum.2020")
# list of rainfall seasonal dekadal CVs 
clist <- c("rain.cv.2001", "rain.cv.2002", "rain.cv.2003", "rain.cv.2004", "rain.cv.2005", "rain.cv.2006", "rain.cv.2007", "rain.cv.2008", "rain.cv.2009", "rain.cv.2010", "rain.cv.2011", "rain.cv.2012", "rain.cv.2013", "rain.cv.2014", "rain.cv.2015", "rain.cv.2016", "rain.cv.2017", "rain.cv.2018", "rain.cv.2019", "rain.cv.2020")
# probabilities for each year
# defined such that oldest year is half as likely to be chosen as most recent year, and where all probabilities sum to 1
# see C:\DATA\Nigeria\EiA\rainfall_year_probabillities.xlsx
plist <- c(0.033, 0.035, 0.037, 0.039, 0.040, 0.042, 0.044, 0.046, 0.047, 0.049, 0.051, 0.053, 0.054, 0.056, 0.058, 0.060, 0.061, 0.063, 0.065, 0.067)

# test this 
sample(tlist, 100, replace=TRUE, prob=plist)

rm(pr2)
pr2 <- pr
for (i in 2:300) {
  print(paste("Iteration:", i))  
  # predict using raster stack
  # note: we set the rainfall variables with random draws from last 20 years, as defined in the vectors above
  
  tchoice <-  sample(tlist, 1, replace=TRUE, prob=plist)
  cchoice <-  clist[which(tchoice==tlist)]
  
  print(tchoice)
  print(cchoice)
  
  rain.sum <- stack[tchoice]
  names(rain.sum) <- c("rain.sum")
  rain.cv  <- stack[cchoice]
  names(rain.cv) <- c("rain.cv")
  
  # replace newstack for each iteration 
  newstack <- c(stack, rain.sum, rain.cv)
  # predict and send each prediction as new layer of output stack
  add(pr2) <- predict(newstack, crf, const=data.frame(N_fertilizer=100, P_fertilizer=50, K_fertilizer=15), na.rm=TRUE)
}

summary(pr2)

# calculate average and variance/standard deviation of predictions
pr.avg = app(pr2, fun=mean) 
pr.var = app(pr2, fun=var)
  names(pr.var) <- "var"
pr.sd = app(pr2, fun=sd)
pr.cv = pr.sd/pr.avg
  names(pr.cv) <- "CV"

plot(c(pr.avg, pr.var))
plot(c(pr.avg, pr.cv))

writeRaster(pr2, "tmp_pr2_first10_100N_50P_15K_seed1492.tif")



# resample to lower res
npkg <- resample(npkg, pr.avg)
mpkg <- resample(mpkg, pr.avg)

netrev <- (pr.avg * mpkg) - (100 * npkg)

# predict stochastic net revenue
# set up stack to hold predictions
pr3 <- netrev
for (i in 2:300) {
  print(paste("Iteration:", i))  
  # predict using raster stack
  # note: we set the rainfall variables with random draws from last 20 years, as defined in the vectors above
  
  tchoice <-  sample(tlist, 1, replace=TRUE, prob=plist)
  cchoice <-  clist[which(tchoice==tlist)]
  
  print(tchoice)
  print(cchoice)
  
  rain.sum <- stack[tchoice]
  names(rain.sum) <- c("rain.sum")
  rain.cv  <- stack[cchoice]
  names(rain.cv) <- c("rain.cv")
  
  # replace newstack for each iteration 
  newstack <- c(stack, rain.sum, rain.cv)
  # predict and send each prediction as new layer of output stack
  tmp <- predict(newstack, crf, const=data.frame(N_fertilizer=100, P_fertilizer=50, K_fertilizer=15), na.rm=TRUE)
  add(pr3) <- ((tmp * mpkg) - (100 * npkg))
}

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

