
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

