# Read and plot SIMPLE-G results from text format
# By: Iman Haqiqi
# Aug 2019

# edited by Nick Manning -- May 2023

# note from Iman: use the following command to define the files in your TABLO code, 
# File (New,text,SSE)
# I used GP 12.0.001

# 0: Load required libraries ---- 
rm(list = ls())

library(raster) # use for initial raster stack and basic plotting
library(terra) # use to wrangle geospatial data and plot
library(stringr) # use to manipulate result .txt file
library(RColorBrewer) # use for adding colorblind-friendly color palettes 
library(dplyr) # use for piping & filtering
library(geobr) # use to load BR & Cerrado extent shapefiles


#1: import output from SIMPLE-G run ----------
getwd()

# NOTE: will need to local location
datafile   <- "../Results/US-BR_SIMPLEG-238/US_40pct-out.txt"

# NOTE: change this when you change the result file 
# e.g. for the 10% shock result you should input "10p"
pct_test <- "40p" # change when you change 'datafile'


old.lines  <- readLines(datafile)
new.lines <- 
  old.lines[which(str_sub(old.lines, 1,1) != " " & 
                    str_sub(old.lines, 1,1) != "!" &
                    str_sub(old.lines, 1,1) != ""  )]

# write temporary file 
# NOTE: Will need to change to local location
newfile = "../Results/US-BR_SIMPLEG-238/temp.txt"
writeLines(new.lines, newfile, sep="\n")

# read in new data table -- takes a bit
dat <- read.table(newfile, sep=",", header=T)


# # # #  # KEY FOR RESULTS # # # # # # # # # # 

# GRIDOUT.GRID.VAR == The Grid Cell ID

# LON == Longitude in XXX

# LAT == Latitude in XXX

# pct_QLAND == % change in cropland area

# new_QLAND ==  the post-simulation area of cropland in 1000 (?) ha

# pct_QCROP ==    % change in the gridded crop production index

# new_QCROP ==    the post-simulation quantity index for crop 
# production in 1000-ton (corn-equivalent)




# 2: Create Raster Stack & Save Rasters ----------

# create X and Y coordinates -- takes a bit
dat$x <- as.numeric(round(dat$LON, digits = 1))
dat$y <- as.numeric(round(dat$LAT, digits = 1))

coordinates(dat) = ~x+y

# set as gridded data
gridded(dat) = T 

# create basic raster stack for saving rasters
prct_ras = stack(dat) 

# add rasters to file - uncomment and change 'ras_file' to local location
ras_file <- "../Results/US-BR_SIMPLEG-238/raster/"

# writeRaster(prct_ras$pct_QLAND, paste0(ras_file, "qLand_pct_", pct_test, ".tif"), format="GTiff", overwrite=TRUE)
# writeRaster(prct_ras$new_QLAND, paste0(ras_file, "qLand_new_", pct_test, ".tif"), format="GTiff", overwrite=TRUE)
# writeRaster(prct_ras$pct_QCROP, paste0(ras_file, "qCrop_pct_", pct_test, ".tif"), format="GTiff", overwrite=TRUE)
# writeRaster(prct_ras$new_QCROP, paste0(ras_file, "qCrop_new_", pct_test, ".tif"), format="GTiff", overwrite=TRUE)

# add Longitude and Latitude rasters (only need to do this one time, they aren't different for diff percents)
# writeRaster(prct_ras$LON, "../Results/US-BR_SIMPLEG-238/raster/USBR_SIMPLEG_238_LON.tif", format="GTiff", overwrite=TRUE)
# writeRaster(prct_ras$LAT, "../Results/US-BR_SIMPLEG-238/raster/USBR_SIMPLEG_238_LAT.tif", format="GTiff", overwrite=TRUE)


# 3: Make basic plots using 'raster' -----------

# plot using base R & 'raster'
prct_ras <- subset(prct_ras, c("pct_QLAND", "new_QLAND", "pct_QCROP", "new_QCROP"))
raster::plot(prct_ras)



# 4: Clip to BR & Cerrado Results ------------

# Load Cerrado Shapefile
shp_br_cerr <- read_biomes(
  year = 2019,
  simplified = T,
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

#plot(shp_br_cerr)

# Load BR Shapefile
shp_br <- read_country(year = 2019, 
                       simplified = T, 
                       showProgress = T)
  
#plot(shp_br)


# 5: Plot Results using 'terra' -------------------

# create SpatRaster using terra
r <- terra::rast(dat)
r <- subset(r, c("pct_QLAND", "new_QLAND", "pct_QCROP", "new_QCROP"))

## 5.0: WORLD RESULTS ---------

# Plot World Results
terra::plot(r, axes = F)

# subset to each band - uncomment to do simple Exploratory Data Analysis
r_pct_qland <- subset(r, "pct_QLAND")
# minmax(r_pct_qland)
# hist(r_pct_qland)
# boxplot(r_pct_qland)

r_new_qland <- subset(r, "new_QLAND")
# minmax(r_new_qland)
# hist(r_new_qland)
# boxplot(r_new_qland)

r_pct_qcrop <- subset(r, "pct_QCROP")
# minmax(r_pct_qcrop)
# hist(r_pct_qcrop)
# boxplot(r_pct_qcrop)

r_new_qcrop <- subset(r, "new_QCROP")
# minmax(r_new_qcrop)
# hist(r_new_qcrop)
# boxplot(r_new_qcrop)



## 5.1 BRAZIL RESULTS ------

# get extent as terra object for plotting
ext_br <- vect(ext(shp_br))

# plot basic BR results by cropping and masking to just BR extent
r_br <- terra::crop(r, ext_br, mask = T) 
r_br <- mask(r_br, shp_br)
terra::plot(r_br, axes = F, type = "interval")

# subset to each band 
r_br_pct_qland <- subset(r_br, "pct_QLAND")
# minmax(r_br_pct_qland)
# hist(r_br_pct_qland)

r_br_new_qland <- subset(r_br, "new_QLAND")
# minmax(r_br_new_qland)
# hist(r_br_new_qland)

r_br_pct_qcrop <- subset(r_br, "pct_QCROP")
# minmax(r_br_pct_qcrop)
# hist(r_br_pct_qcrop)

r_br_new_qcrop <- subset(r_br, "new_QCROP")
# minmax(r_br_new_qcrop)
# hist(r_br_new_qcrop)


# set up plotting dimensions - comment to keep one plot at a time
# display.brewer.all(colorblindFriendly = T)
par(mfrow=c(2,2), oma = c(0,0,2,0))

# plot originals individually
terra::plot(r_br_pct_qland, 
            type = "continuous",
            #type="classes", 
            col = brewer.pal(9, "PiYG"), 
            main = "% Change in Cropland Area", plg = list(x="bottomleft"))

terra::plot(r_br_new_qland, 
            #type="interval", 
            col = brewer.pal(5, "Oranges"), 
            main = "Change in Cropland Area (1000 ha)", plg = list(x="bottomleft"))

terra::plot(r_br_pct_qcrop, 
            type="continuous", 
            col = brewer.pal(11, "PiYG"), 
            main = "% Change in Crop Production Index", plg = list(x="bottomleft"))

terra::plot(r_br_new_qcrop, 
            #type="classes", 
            col = brewer.pal(7, "Oranges"), 
            main = "Change in Quantity of Crops (1000-ton Corn Equivalent)", plg = list(x="bottomleft"))


### 5.1.1: Reclassify Results - 40 PCT ONLY ------
### NOTE: THIS SECTION IS NOT REPRODUCIBLE WITH OTHER RESULTS 
r_br_pct_qland_rc40 <- classify(r_br_pct_qland, c(-10, -5, -1, -0.5, 0, 0.5, 1, 5, 10, 40))
r_br_new_qland_rc40 <- classify(r_br_new_qland, c(0, 0.5, 1, 1.5, 2, 2.5, 3, 5))

r_br_pct_qcrop_rc40 <- classify(r_br_pct_qcrop,c(-20, -5, -1, 0, 1, 5, 10, 100))
r_br_new_qcrop_rc40 <- classify(r_br_new_qcrop, c(0, 1, 5, 10, 25, 50, 100, 150))

# restack reclassified results
r_br_rc40_list <- list(r_br_pct_qland_rc40, r_br_new_qland_rc40, r_br_pct_qcrop_rc40, r_br_new_qcrop_rc40)
r_br_rc40 <- rast(r_br_rc40_list)


# plot old results then reclassified results to compare
terra::plot(r_br,  col = rev(brewer.pal(9, "PiYG")))
terra::plot(r_br_rc40, type="classes", col = rev(brewer.pal(9, "PiYG")))


# plot each reclassied raster individually
terra::plot(r_br_pct_qland_rc40, type="classes", col = rev(brewer.pal(9, "PiYG")),
            main = "% Change in Cropland Area", plg = list(x="bottomleft"))

terra::plot(r_br_new_qland_rc40, type="classes", col = brewer.pal(7, "Oranges"),
            main = "Change in Cropland Area (1000 ha)", plg = list(x="bottomleft"))

terra::plot(r_br_pct_qcrop_rc40, type="classes", col = rev(brewer.pal(7, "PiYG")),
            main = "% Change in Crop Production Index", plg = list(x="bottomleft"))

terra::plot(r_br_new_qcrop_rc40, type="classes", col = brewer.pal(7, "Oranges"),
            main = "Change in Crop Production (1000-ton corn-equivalent)", plg = list(x="bottomleft"))





## 5.2: CERRADO RESULTS ------------ 

# get Cerrado extent as terra object 
ext_cerr <- vect(ext(shp_br_cerr))

# crop, mask, and plot 
r_cerr <- terra::crop(r, ext_cerr, mask = T)
r_cerr <- mask(r_cerr, shp_br_cerr)
terra::plot(r_cerr, axes = F)

# subset to each band and do EDA
r_cerr_pct_qland <- subset(r_cerr, "pct_QLAND")
# minmax(r_cerr_pct_qland)
# hist(r_cerr_pct_qland)

r_cerr_new_qland <- subset(r_cerr, "new_QLAND")
# minmax(r_cerr_new_qland)
# hist(r_cerr_new_qland)

r_cerr_pct_qcrop <- subset(r_cerr, "pct_QCROP")
# minmax(r_cerr_pct_qcrop)
# hist(r_cerr_pct_qcrop)

r_cerr_new_qcrop <- subset(r_cerr, "new_QCROP")
# minmax(r_cerr_new_qcrop)
# hist(r_cerr_new_qcrop)


# plot originals individually
terra::plot(r_cerr_pct_qland, 
            #type = "interval",
            #type="classes", 
            col = brewer.pal(11, "PiYG"), 
            main = "% Change in Cropland Area", plg = list(x="topleft"))

terra::plot(r_cerr_new_qland, 
            #type="interval", 
            col = brewer.pal(9, "Oranges"), 
            main = "Change in Cropland Area (1000 ha)", plg = list(x="topleft"))

terra::plot(r_cerr_pct_qcrop, 
            #type="classes", 
            col = brewer.pal(9, "PiYG"), 
            main = "% Change in Crop Production Index", plg = list(x="topleft"))

terra::plot(r_cerr_new_qcrop, 
            #type="classes", 
            col = brewer.pal(7, "Oranges"), 
            main = "Change in Quantity of Crops (1000-ton Corn Equivalent)", plg = list(x="topleft"))



### 5.2.1  Reclassify Results - 40 PCT ONLY ----------------------

# # # # # # 40 PCT ONLY - DOES NOT TRACK FOR OTHER RESULTS # # # # # 

r_cerr_pct_qland_rc40 <- classify(r_cerr_pct_qland, c(-6, -2, -1, -0.5, 0, 0.5, 1, 2, 4, 6))

#r_cerr_new_qland_rc40 <- classify(r_cerr_new_qland, c(0, .25, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5))
r_cerr_new_qland_rc40 <- classify(r_cerr_new_qland, c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5))

r_cerr_pct_qcrop_rc40 <- classify(r_cerr_pct_qcrop,c(-5, -2, -1, -0.5, 0, 0.5, 1, 2, 5, 10))

#r_cerr_new_qcrop_rc40 <- classify(r_cerr_new_qcrop, c(0, 1, 5, 10, 25, 50, 100, 150))
r_cerr_new_qcrop_rc40 <- classify(r_cerr_new_qcrop, c(0, 10, 25, 50, 75,100, 150))

# restack reclassified results
r_cerr_rc40_list <- list(r_cerr_pct_qland_rc40, r_cerr_new_qland_rc40, r_cerr_pct_qcrop_rc40, r_cerr_new_qcrop_rc40)
r_cerr_reclass <- rast(r_cerr_rc40_list)

# plot basic then reclassified raster results
terra::plot(r_cerr, col = rev(brewer.pal(9, "PiYG")))
terra::plot(r_cerr_reclass, type="classes", col = rev(brewer.pal(9, "PiYG")))


# plot each reclassified individually
terra::plot(r_cerr_pct_qland_rc40, type="classes", col = rev(brewer.pal(9, "PiYG")), 
            main = "% Change in Cropland Area", plg = list(x="topleft"))

terra::plot(r_cerr_new_qland_rc40, type="classes", col = brewer.pal(9, "Oranges"), 
            main = "Change in Cropland Area (1000 ha)", plg = list(x="topleft"))

terra::plot(r_cerr_pct_qcrop_rc40, type="classes", col = brewer.pal(9, "PiYG"), 
            main = "% Change in Crop Production Index", plg = list(x="topleft"))

terra::plot(r_cerr_new_qcrop_rc40, type="classes", col = brewer.pal(7, "Oranges"), 
            main = "Change in Quantity of Crops (1000-ton Corn Equivalent)",plg = list(x="topleft"))



