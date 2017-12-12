#####################################################
# Evaluating the impact of solar power generation   #
# developments on deforestation on Kauai Hawaii     #
# C7 project                                        #
# Developed by Vladimir Metelitsa                   #
# Last updated: 30/04/2017                          #
#####################################################

# Project background: Like many remote islands, the island of Kauai, Hawaii used to depend on barrels of oil imported by sea
# for all the majority of their energy needs. In early 2017, the island saw the completion of a major project aiming to end it's reliance of fossil fuel imports,
# instead fully powering the island with renewable solar energy. 
# The project was composed of 55 thousand solar panels (generating 13MW) and large batteries for storage (52MWh capacity).
# However, there are claims that the land use change (Kauai has a very high forrest cover) for the instalation of the solar farm, as well as the grid changes
# likely resulted in more negative environmental impacts than years of importing and using fossil fuel.
# The company in charge of the instalation, however, claims that no major impacts exist.
# This study attempts to verify the veracity of the claim.
# Aims: Compare the vegetation level before and after the project, to verify if there are any major impacts.
# Methods: NDVI, supervised classification, moving window

# Dependencies
install.packages(c('maps', 'mapdata', 'raster', 'rgdal'))

library(maps)
library(mapdata)
library(raster)
library(rgdal)

# Setting the working directory for the project
setwd("~/RScripts/C7project")

## Getting the boundaries of Kauai
# National administrative boundaries downloaded from the GADM project
# The border data was obtained from http://www.gadm.org/
# Selecting "United States" as the country, "R (SpatialPolygonsDataFrame)" as the format
# Level 2 to have access to boundaries of separate states/islands
boundaries <- readRDS("ctr_boundaries/USA_adm2.rds")

# Separating out only the boundaries of Hawaii
# unique(boundaries$NAME_1)
hawaii <-subset(boundaries, NAME_1 == "Hawaii")

# Keeping only the boundaries of Kauai
# unique(boundaries$NAME_2)
kauai_boundaries <- subset(hawaii, NAME_2 == "Kauai")

plot(kauai_boundaries)

## Importing before/after landsat cover images
# Landsat images were downloaded from https://earthexplorer.usgs.gov/
# The before image is from 2015
# The after image is a recent image from 2017
# Unfortunatly they both contain some cloud cover
# Tiff image preparation before use in R:
# 1. The images were converted from the Web Map Service (WMS) format to GeoTiff using the r.in.wms GRASS GIS function
# 2a. Since the images were taken after the failure of the Scan Line Corrector module of the Landsat satelite, they contained gaps in data
# 2b. These gaps were filled using the GDAL fill nodata functionality in QGIS
# 3. Finally the images were exported to tiff for use in R
before <- brick("raster/before.tif")
after <- brick("raster/after.tif")

# Plotting real color images
plotRGB(before, 3, 2, 1, stretch = "lin")
plot(kauai_boundaries, add = T)
plotRGB(after, 3, 2, 1, stretch = "lin")
plot(kauai_boundaries, add = T)

## Simple vegetation cover comparison with NDVI

# Defining the NDVI function
ndvi <- function(nir, red) {
  (nir - red)/(nir + red)
}

ndvi.before <- ndvi(before[4], before[3])
ndvi.after <- ndvi(after[4], after[3])

par(mfrow=c(1, 2))
plot(ndvi.before, main = "NDVI before the project (2015)")
plot(ndvi.after, main = "NDVI after the project (2017)")
par(mfrow=c(1, 1))

# Calculating the difference in NDVI
ndvi.diff <- ndvi.after - ndvi.before
plot(ndvi.diff, main = "Difference in NDVI")

## Change Vector Analysis (CVA)
raster.cva <- rasterCVA(before[[3:4]], after[[3:4]])
plot(raster.cva)

## Supervised classification
classes.before <- readOGR(dsn = "~/RScripts/project/vector", layer = "beforeclass")
classes.after <- readOGR(dsn = "~/RScripts/project/vector", layer = "afterclass")

before.classification <- superClass(before, trainData = classes.before, responseCol = "type")
after.classification <- superClass(after, trainData = classes.after, responseCol = "type")

par(mfrow=c(1, 2))
plot(before.classification$map, main = "Land types before")
plot(after.classification$map, main = "Land types after")
par(mfrow=c(1, 1))

## Post classification: Substraction
difference.class <- (before.classification$map * 10) - after.classification$map
plot(difference.class)

## Moving window
# Standard Deviation
window.sd3x3 <- focal(difference.class, w = matrix(1/9, ncol = 3, nrow = 3), fun = sd)
window.sd5x5 <- focal(difference.class, w = matrix(1/9, ncol = 5, nrow = 5), fun = sd)
window.sd7x7 <- focal(difference.class, w = matrix(1/9, ncol = 7, nrow = 7), fun = sd)
window.sd15x15 <- focal(difference.class, w = matrix(1/9, ncol = 15, nrow = 15), fun = sd)

# Plotting the 4 graphs side-by-side
par(mfrow=c(2, 2))
plot(window.sd3x3)
plot(window.sd5x5)
plot(window.sd7x7)
plot(window.sd15x15)
par(mfrow=c(1, 1))

# Minimum amount of cover
window.min3x3 <- focal(difference.class, w = matrix(1/9, ncol = 3, nrow = 3), fun = min)
window.min5x5 <- focal(difference.class, w = matrix(1/9, ncol = 5, nrow = 5), fun = min)
window.min7x7 <- focal(difference.class, w = matrix(1/9, ncol = 7, nrow = 7), fun = min)
window.min15x15 <- focal(difference.class, w = matrix(1/9, ncol = 15, nrow = 15), fun = min)

# Plotting the 4 graphs side-by-side
par(mfrow=c(2, 2))
plot(window.min3x3)
plot(window.min5x5)
plot(window.min7x7)
plot(window.min15x15)
par(mfrow=c(1, 1))

# Maximum amount of cover
window.max3x3 <- focal(difference.class, w = matrix(1/9, ncol = 3, nrow = 3), fun = max)
window.max5x5 <- focal(difference.class, w = matrix(1/9, ncol = 5, nrow = 5), fun = max)
window.max7x7 <- focal(difference.class, w = matrix(1/9, ncol = 7, nrow = 7), fun = max)
window.max15x15 <- focal(difference.class, w = matrix(1/9, ncol = 15, nrow = 15), fun = max)

# Plotting the 4 graphs side-by-side
par(mfrow=c(2, 2))
plot(window.max3x3)
plot(window.max5x5)
plot(window.max7x7)
plot(window.max15x15)
par(mfrow=c(1, 1))

# Conclusion
# No notable difference could be seen in the level of vegetation between 2015 and 2017.
# Although this could be seen as a confirmation of the claims that there was minimal to no impact from the development project, it could also be due to
# the many sources of uncertainty in this study.
# Notably, it was impossible to find Landsat imagery in which the island was completely devoid of clouds, 
# therefore imagery where the least clouds were visible was selected.
# Additionally the filling of the gaps due to the failure of the Scan Line Corrector module of Landsat, might have introduced further errors.
# Notwithstanding the large sources of uncertainty in this study, it demonstrates the possible use of remote sensing and various analysis methods
# for the study of deforestation.