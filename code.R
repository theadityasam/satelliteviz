# Images are acquired from Indian Remote Sensing Satellite Resoursesat -1,2 with
# LISS-IV sensor. It provides multi-spectral data with three bands with spatial
# resolution of 5m at nadir. The given imageries are band stacked FCC (False Colour Composite) as 
# (Band 1- Near Infrared, Band 2- Red and Band 3- Green).
# It has 10 bits radiometric resolution with the swath of 70 km and the revisit time
# of 5 days. 

path <- file.choose()
GDALinfo(path)
grey <- raster(path)
grey
rgb <- brick(path)
plot(grey)
plot(rgb)

b1 <- raster(path, band=1) # Near Infrared
plot(b1)
b2 <- raster(path, band=2) # Red
plot(b2)
b3 <- raster(path, band=3) # Green
plot(b3)
compareRaster(b1,b3)


# this code specifies how we want to save the plot
png('RGB.png', width = 5, height = 4, units = "in", res = 300)
tayRGB <- stack(list(b3, b2))              # creates raster stack
plotRGB(tayRGB, axes = TRUE, stretch = "lin", main = "Resourcesat NirRG colour composite")
dev.off()


# NDVI
# Created a VI function (vegetation index)
VI <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}
# For Resourcesat, the relevant bands to use are:
# NIR = 1, red = 2
ndvi <- VI(rgb, 1, 2)
# 1 and 2 refer to the bands we'll use
png('ndviplot.png', width = 4, height = 4, units = "in", res = 300)
plot(ndvi, col = rev(terrain.colors(10)), main = 'Resourcesat 1,2, Bareilly-NDVI')
dev.off()

# Create histogram of NDVI data. Histogram skewed towards right means highly vegetated area
png('ndvihist.png', width = 4, height = 4, units = "in", res = 300)
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "aquamarine3",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side = 1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))
dev.off()


# Mask cells that have NDVI of less than 0.4 (less likely to be vegetation)
png('ndvimask.png', width = 4, height = 4, units = "in", res = 300)
veg <- reclassify(ndvi, cbind(-Inf, 0.4, NA))
# We are reclassifying our object and making all values between
# negative infinity and 0.4 be NAs
plot(veg, main = 'Veg cover')
dev.off()


# convert the raster to vector/matrix ('getValues' converts the RasterLAyer to array) )
nr <-getValues(ndvi)
str(nr)
# important to set the seed generator because `kmeans` initiates the centres in random locations
# the seed generator just generates random numbers
set.seed(99)
# create 10 clusters, allow 500 iterations, start with 5 random sets using 'Lloyd' method
kmncluster <- kmeans(na.omit(nr), centers = 10, iter.max = 500,
                     nstart = 5, algorithm = "Lloyd")
# kmeans returns an object of class 'kmeans'
str(kmncluster)


# Recreating matrix to raster for visualisation
# First create a copy of the ndvi layer
knr <- ndvi
# Now replace raster cell values with kmncluster$cluster
# array
knr[] <- kmncluster$cluster
# Alternative way to achieve the same result
# values(knr) <- kmncluster$cluster
# knr

# Plotting kmeans map next to NDVI map
par(mfrow = c(1, 2))
plot(ndvi, col = rev(terrain.colors(10)), main = "NDVI")
plot(knr, main = "Kmeans", col = viridis_pal(option = "D")(10))

# Plotting next to rgnir map and saving
png('rgb_kmeans.png', width = 10, height = 8, units = "in", res = 300)
par(mar = c(10.8, 5, 10.8, 2), mfrow = c(1, 2))
plotRGB(tayRGB, axes = TRUE, stretch = "lin", main = "RGB")
plot(knr, main = "Kmeans", yaxt = 'n', col = viridis_pal(option = "D")(10))
dev.off()