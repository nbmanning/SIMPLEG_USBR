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
library(tigris) # use to load US and US-MW shapefiles


# 1: Prep SIMPLE-G Results --------------------

# 1.1: import and modify the output from the SIMPLE-G model ----------
getwd()

# NOTE: change this when you change the result file 
# e.g. for the 10% shock result you should input "10p"
pct <- "40pct" # change when you change 'datafile'

# NOTE: will need to local location
datafile   <- paste0("../Results/US-BR_SIMPLEG-238/US_", pct, "-out.txt")

# read results and substitute the old row-notation of using "!" on each row
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


## KEY FOR RESULTS ######################## 

# GRIDOUT.GRID.VAR == The Grid Cell ID

# LON == Longitude in XXX

# LAT == Latitude in XXX

# pct_QLAND == % change in cropland area

# new_QLAND ==  the post-simulation (Post-Sim) area of cropland in 1000 (?) ha

# pct_QCROP ==    % change in the gridded crop production index

# new_QCROP ==    the Post-Sim quantity index for crop 
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
 
# writeRaster(prct_ras$pct_QLAND, paste0(ras_file, "qLand_pct_", pct, ".tif"), format="GTiff", overwrite=TRUE)
# writeRaster(prct_ras$new_QLAND, paste0(ras_file, "qLand_new_", pct, ".tif"), format="GTiff", overwrite=TRUE)
# writeRaster(prct_ras$pct_QCROP, paste0(ras_file, "qCrop_pct_", pct, ".tif"), format="GTiff", overwrite=TRUE)
# writeRaster(prct_ras$new_QCROP, paste0(ras_file, "qCrop_new_", pct, ".tif"), format="GTiff", overwrite=TRUE)

# add Longitude and Latitude rasters (only need to do this one time, they aren't different for diff percents)
# writeRaster(prct_ras$LON, "../Results/US-BR_SIMPLEG-238/raster/USBR_SIMPLEG_238_LON.tif", format="GTiff", overwrite=TRUE)
# writeRaster(prct_ras$LAT, "../Results/US-BR_SIMPLEG-238/raster/USBR_SIMPLEG_238_LAT.tif", format="GTiff", overwrite=TRUE)


# 3: Make basic plots using 'raster' -----------

# plot using base R & 'raster'
prct_ras <- subset(prct_ras, c("pct_QLAND", "new_QLAND", "pct_QCROP", "new_QCROP"))
raster::plot(prct_ras)



# 4: Get Shapefiles: US-MW, BR, & Cerrado ------------

# Load US Shapefile
shp_us <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AK", "HI", "PR"))

#plot(shp_us)

# Load US-MW Shapefile
shp_us_mw <- shp_us %>%
  filter(STUSPS %in% c("IA", "IL", "IN", "KS", "MI", "MN",
                       "MO", "ND", "NE", "OH", "SD", "WI"))
plot(shp_us_mw)


# Load Cerrado Shapefile
shp_br_cerr <- read_biomes(
  year = 2019,
  simplified = T,
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

#plot(shp_br_cerr)


# Load BR Shapefile
shp_br <- read_country(
  year = 2019, 
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


## 5.0 US RESULTS ------
# get extent as terra object for plotting
ext_us <- vect(ext(shp_us))

# plot basic us results by cropping and masking to just us extent
r_us <- terra::crop(r, ext_us, mask = T)
r_us <- mask(r_us, shp_us)
terra::plot(r_us, axes = F, type = "continuous")

# subset to each band
r_us_pct_qland <- subset(r_us, "pct_QLAND")
#minmax(r_us_pct_qland)
#terra::hist(r_us_pct_qland)

r_us_new_qland <- subset(r_us, "new_QLAND")
#minmax(r_us_new_qland)
#hist(r_us_new_qland)

r_us_pct_qcrop <- subset(r_us, "pct_QCROP")
# minmax(r_us_pct_qcrop)
#hist(r_us_pct_qcrop)

r_us_new_qcrop <- subset(r_us, "new_QCROP")
# minmax(r_us_new_qcrop)
#hist(r_us_new_qcrop)


# set up plotting dimensions - comment to keep one plot at a time
# display.brewer.all(colorblindFriendly = T)
par(mfrow=c(2,2), oma = c(0,0,2,0))

### 5.0.1: (omit) Plot US original results individually ----------
terra::plot(r_us_pct_qland,
            type = "continuous",
            #type="classes",
            col = brewer.pal(9, "PiYG"),
            main = "% Change in Cropland Area", plg = list(x="bottomleft"))

terra::plot(r_us_new_qland,
            #type="interval",
            col = brewer.pal(5, "Oranges"),
            main = "Change in Cropland Area (1000 ha)", plg = list(x="bottomleft"))

terra::plot(r_us_pct_qcrop,
            type="continuous",
            col = brewer.pal(11, "PiYG"),
            main = "% Change in Crop Production Index", plg = list(x="bottomleft"))

terra::plot(r_us_new_qcrop,
            #type="classes",
            col = brewer.pal(7, "Oranges"),
            main = "Change in Quantity of Crops (1000-ton CE)", plg = list(x="bottomleft"))



## 5.1 BRAZIL RESULTS ------

### 5.1.1: Clip BR -----------
# get extent as terra object for plotting
ext_br <- vect(ext(shp_br))

# plot basic BR results by cropping and masking to just BR extent
r_br <- terra::crop(r, ext_br, mask = T) 
r_br <- mask(r_br, shp_br)
terra::plot(r_br, axes = F, type = "interval")

### 5.1.2 Subset & do EDA ----------------
# subset to each band 
r_br_pct_qland <- subset(r_br, "pct_QLAND")
#minmax(r_br_pct_qland)
#hist(r_br_pct_qland)

r_br_new_qland <- subset(r_br, "new_QLAND")
#minmax(r_br_new_qland)
#hist(r_br_new_qland)

r_br_pct_qcrop <- subset(r_br, "pct_QCROP")
# minmax(r_br_pct_qcrop)
# hist(r_br_pct_qcrop)

r_br_new_qcrop <- subset(r_br, "new_QCROP")
# minmax(r_br_new_qcrop)
# hist(r_br_new_qcrop)


### 5.1.3: Plot Original BR Result Rasters ----------
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
            main = "Post-Sim Cropland Area (1000 ha)", plg = list(x="bottomleft"))

terra::plot(r_br_pct_qcrop, 
            type="continuous", 
            col = brewer.pal(11, "PiYG"), 
            main = "% Change in Crop Production Index", plg = list(x="bottomleft"))

terra::plot(r_br_new_qcrop, 
            #type="classes", 
            col = brewer.pal(7, "Oranges"), 
            main = "Post-Sim Quantity of Crops (1000-ton CE)", plg = list(x="bottomleft"))


### 5.1.4: Reclassify Results - PER PCT RESULT ------
### NOTE: THIS SECTION IS NOT REPRODUCIBLE WITH OTHER RESULTS 

##### 40 PERCENT RECLASSIFICATION BREAKS ##############################################
r_br_pct_qland_rc <- classify(r_br_pct_qland, c(-5, -1, -0.5, -0.1, 0, 0.1, 0.5, 1, 5))
r_br_new_qland_rc <- classify(r_br_new_qland, c(0, 0.5, 1, 1.5, 2, 2.5, 3, 5))

r_br_pct_qcrop_rc <- classify(r_br_pct_qcrop,c(-5, -1, 0.1, 0, 0.1, 1, 5, 10))
r_br_new_qcrop_rc <- classify(r_br_new_qcrop, c(0, 1, 5, 10, 25, 50, 100, 150))



#### TO-DO: 20 PERCENT RECLASSIFICATION BREAKS #########################################
# r_br_pct_qland_rc <- classify(r_br_pct_qland, c(-5, -1, -0.5, -0.1, 0, 0.1, 0.5, 2, 5))
# r_br_new_qland_rc <- classify(r_br_new_qland, c(0, 0.5, 1, 1.5, 2, 2.5, 3, 4))
# 
# r_br_pct_qcrop_rc <- classify(r_br_pct_qcrop,c(-5, -1, 0.1, 0, 0.1, 1, 5, 10))
# r_br_new_qcrop_rc <- classify(r_br_new_qcrop, c(0, 1, 5, 10, 25, 50, 100, 150))


### 5.1.5 Plot Reclassified Results -----------
# restack reclassified results
r_br_rc_list <- list(r_br_pct_qland_rc, r_br_new_qland_rc, r_br_pct_qcrop_rc, r_br_new_qcrop_rc)
r_br_rc <- rast(r_br_rc_list)


# plot old results then reclassified results to compare
terra::plot(r_br,  col = rev(brewer.pal(9, "PiYG")))
terra::plot(r_br_rc, type="classes", col = rev(brewer.pal(9, "PiYG")))


# plot each reclassified raster individually to add titles
terra::plot(r_br_pct_qland_rc, type="classes", col = rev(brewer.pal(9, "PiYG")),
            main = "% Change in Cropland Area", plg = list(x="bottomleft"))

terra::plot(r_br_new_qland_rc, type="classes", col = brewer.pal(7, "Oranges"),
            main = "Post-Sim Cropland Area (1000 ha)", plg = list(x="bottomleft"))

terra::plot(r_br_pct_qcrop_rc, type="classes", col = rev(brewer.pal(7, "PiYG")),
            main = "% Change in Crop Production Index", plg = list(x="bottomleft"))

terra::plot(r_br_new_qcrop_rc, type="classes", col = brewer.pal(7, "Oranges"),
            main = "Post-Sim Crop Production (1000-ton corn-equivalent)", plg = list(x="bottomleft"))





## 5.2: CERRADO RESULTS ------------ 

### 5.2.1: Clip & Subset Cerrado States & MATOPIBA ------ 
# get MATOPIBA outline
shp_br_st <- read_state(
  year = 2019,
  simplified = T,
  #showProgress = T
) 

shp_br_mapitoba <- shp_br_st %>% dplyr::filter(abbrev_state %in% c("MA", "PI", "TO", "BA"))

shp_br_cerr_states <- shp_br_st %>% dplyr::filter(abbrev_state %in% c("TO","MA","PI","BA","MG",
                                        "SP","MS","MT","GO","DF"))
# get Cerrado extent as terra object 
ext_cerr <- vect(ext(shp_br_cerr))

# crop, mask, and plot 
r_cerr <- terra::crop(r, ext_cerr, mask = T)
r_cerr <- mask(r_cerr, shp_br_cerr)
terra::plot(r_cerr, axes = F)

### 5.2.2 Subset & EDA -----------
r_cerr_pct_qland <- subset(r_cerr, "pct_QLAND")
minmax(r_cerr_pct_qland)

r_cerr_new_qland <- subset(r_cerr, "new_QLAND")
minmax(r_cerr_new_qland)

r_cerr_pct_qcrop <- subset(r_cerr, "pct_QCROP")
minmax(r_cerr_pct_qcrop)

r_cerr_new_qcrop <- subset(r_cerr, "new_QCROP")
minmax(r_cerr_new_qcrop)

# hists
terra::hist(r_cerr_pct_qland)
hist(r_cerr_new_qland)
hist(r_cerr_pct_qcrop)
hist(r_cerr_new_qcrop)

#summaries
summary(r_cerr)
summary(r_cerr_pct_qland)
summary(r_cerr_new_qland)
summary(r_cerr_pct_qcrop)
summary(r_cerr_new_qcrop)

#boxplots
#terra::boxplot(r_cerr)
terra::boxplot(r_cerr_pct_qland, main = "% Change in Cropland Area")
terra::boxplot(r_cerr_new_qland, main = "Post-Sim Cropland Area (1000 ha)")
terra::boxplot(r_cerr_pct_qcrop, main = "% Change in Crop Production Index")
terra::boxplot(r_cerr_new_qcrop, main = "Post-Sim Quantity of Crops\n(1000-ton CE)")


###### reset par() options -----
dev.off()

# plot 2x2 grid
par(mfrow=c(2,2), oma = c(0,0,2,0))


### 5.2.3: Plot Originals ----------- 
# plot originals individually
terra::plot(r_cerr_pct_qland, 
            #type = "interval",
            #type="classes", 
            col = brewer.pal(11, "PiYG"), 
            main = "% Change in Cropland Area", plg = list(x="topleft"))

terra::plot(r_cerr_new_qland, 
            #type="interval", 
            col = brewer.pal(9, "Oranges"), 
            main = "Post-Sim Cropland Area (1000 ha)", plg = list(x="topleft"))

terra::plot(r_cerr_pct_qcrop, 
            #type="classes", 
            col = brewer.pal(9, "PiYG"), 
            main = "% Change in Crop Production Index", plg = list(x="topleft"))

terra::plot(r_cerr_new_qcrop, 
            #type="classes", 
            col = brewer.pal(7, "Oranges"), 
            main = "Post-Sim Quantity of Crops (1000-ton CE)", plg = list(x="topleft"))



### 5.2.4  Reclassify Results - PER PCT RESULT ----------------------

##### 40 PERCENT RECLASSIFICATION BREAKS ##################################
# WITH MIN MAX; [1] == MIN, [2] == MAX
# essentially, "Round the min or max value here to 1 decimal point"

#r_cerr_pct_qland_rc <- classify(r_cerr_pct_qland, c(-6, -2, -1, -0.5, 0, 0.5, 1, 2, 4, 6))
r_cerr_pct_qland_rc <- classify(r_cerr_pct_qland, c(round((minmax(r_cerr_pct_qland)[1]), 1),
                                                    -2, -1, -0.5, 0, 0.5, 1, 2, 4, 
                                                    round((minmax(r_cerr_pct_qland)[2]), 1)))


#r_cerr_new_qland_rc <- classify(r_cerr_new_qland, c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5))
# r_cerr_new_qland_rc <- classify(r_cerr_new_qland, c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 
#                                                     round((minmax(r_cerr_new_qland)[2]), 1)))
r_cerr_new_qland_rc <- classify(r_cerr_new_qland, c(0, 0.5, 1, 2, 3, 4, 
                                                    round((minmax(r_cerr_new_qland)[2]), 1)))

#r_cerr_pct_qcrop_rc <- classify(r_cerr_pct_qcrop,c(-2, -1, -0.5, -.1, 0, 0.1, 0.5, 1, 2))
r_cerr_pct_qcrop_rc <- classify(r_cerr_pct_qcrop,c(round((minmax(r_cerr_pct_qcrop)[1])),
                                                         -1, -0.5, -.1, 0, 0.1, 0.5, 1,
                                                   round((minmax(r_cerr_pct_qcrop)[2]))))


#r_cerr_new_qcrop_rc <- classify(r_cerr_new_qcrop, c(0, 10, 25, 50, 75,100, 150))
r_cerr_new_qcrop_rc <- classify(r_cerr_new_qcrop, c(0, 10, 25, 50, 75,100, 
                                                    round((minmax(r_cerr_new_qcrop)[2]))))


# 40 PCT Box Plots --------------

# NEED TO MELT ALL THE DIFF PCT's INTO ONE COLUMN
# Only use 2012 to keep it from getting too crazy
# Follow https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
terra::boxplot(r_cerr)

terra::boxplot(r_cerr_pct_qland, main = "% Change in Cropland Area")
#terra::boxplot(r_cerr_new_qland, main = "Post-Sim Cropland Area (1000 ha)")
terra::boxplot(r_cerr_pct_qcrop, main = "% Change in Crop Production Index")
#terra::boxplot(r_cerr_new_qcrop, main = "Post-Sim Quantity of Crops\n(1000-ton CE)")

######## Next ########### 
### BOXPLOT CODE COPY/PASTED FROM dataprep_US.R ###
### NEEDS REFORMATTING ###
df_diff_melt <- df_diff %>%
  # mutate(year == as.numeric(year(year))) %>% 
  filter(year == 2012) %>% 
  st_drop_geometry()

colnames(df_diff_melt)
str(df_diff_melt)

# melt to change to long
df_diffpct_melt2 <- melt(df_diff_melt, id.vars = c("year", "state", "name"),
                         variable.name = "DiffPctVar", value.name = "DiffPct")

df_diffpct_melt2$crop <- sub("Diff.*","",df_diffpct_melt2$DiffPctVar)

ggplot(data = df_diffpct_melt2, aes(x=crop, y=DiffPct)) + 
  geom_boxplot(aes(fill=DiffPctVar), 
               #outlier.shape = NA
  )+
  geom_jitter(alpha = 0.1, color = 2)

ggplot(data = r_cerr_pct_qland, aes(x=crop, y=DiffPct)) + 
  geom_boxplot(aes(fill=DiffPctVar), 
               #outlier.shape = NA
  )+
  geom_jitter(alpha = 0.1, color = 2)



##### TO-DO: 20 PERCENT RECLASSIFICATION BREAKS ##################################
# r_cerr_pct_qland_rc <- classify(r_cerr_pct_qland, c(-6, -2, -1, -0.5, 0, 0.5, 1, 2, 4, 6))
# 
# #r_cerr_new_qland_rc <- classify(r_cerr_new_qland, c(0, .25, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5))
# r_cerr_new_qland_rc <- classify(r_cerr_new_qland, c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5))
# 
# r_cerr_pct_qcrop_rc <- classify(r_cerr_pct_qcrop,c(-2, -1, -0.5, -.1, 0, 0.1, 0.5, 1, 2))
# 
# #r_cerr_new_qcrop_rc <- classify(r_cerr_new_qcrop, c(0, 1, 5, 10, 25, 50, 100, 150))
# r_cerr_new_qcrop_rc <- classify(r_cerr_new_qcrop, c(0, 10, 25, 50, 75,100, 150))



### 5.2.5: Plot Reclassified Results ---------------

# restack reclassified results
r_cerr_rc_list <- list(r_cerr_pct_qland_rc, r_cerr_new_qland_rc, r_cerr_pct_qcrop_rc, r_cerr_new_qcrop_rc)
r_cerr_reclass <- rast(r_cerr_rc_list)

# plot basic then reclassified raster results
terra::plot(r_cerr, col = rev(brewer.pal(9, "PiYG")))
terra::plot(r_cerr_reclass, type="classes", col = rev(brewer.pal(9, "PiYG")))

# Set a plot for 2x2 grid
dev.off()
# par(mfrow=c(2,2), oma = c(0,0,2,0))

#Plot (NOTE: change brewer.pal(#) to number of reclassified categories)
################### PICK UP HERE RECLASSIFYING 20 PCT RESULTS ########################################
terra::plot(r_cerr_pct_qland_rc, type="classes", col = rev(brewer.pal(9, "PiYG")), 
            main = "% Change in Cropland Area", plg = list(x="topleft"))
#lines(shp_br_matopiba, lwd = 0.8, lty = 3, col = "darkgray")
lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")


terra::plot(r_cerr_new_qland_rc, type="classes", col = brewer.pal(6, "Oranges"), 
            main = "Post-Sim Cropland Area (1000 ha)", plg = list(x="topleft"))
lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")


terra::plot(r_cerr_pct_qcrop_rc, type="classes", col = rev(brewer.pal(9, "PiYG")), 
            main = "% Change in Crop Production Index", plg = list(x="topleft"))
lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")


terra::plot(r_cerr_new_qcrop_rc, type="classes", col = brewer.pal(6, "Oranges"), 
            main = "Post-Sim Quantity of Crops (1000-ton CE)",plg = list(x="topleft"))
lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")


# write rasters for export to QGIS / ArcGis (optional)
# writeRaster(r_cerr_reclass$pct_QLAND, paste0(ras_file, "rc", "qLand_pct_", pct, ".tif"), overwrite=TRUE)
# writeRaster(r_cerr_reclass$new_QLAND, paste0(ras_file, "rc", "qLand_new_", pct, ".tif"), overwrite=TRUE)
# writeRaster(r_cerr_reclass$pct_QCROP, paste0(ras_file, "rc", "qCrop_pct_", pct, ".tif"), overwrite=TRUE)
# writeRaster(r_cerr_reclass$new_QCROP, paste0(ras_file, "rc", "qCrop_new_", pct, ".tif"), overwrite=TRUE)



