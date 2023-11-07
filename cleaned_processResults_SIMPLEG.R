# Title: processResults_SIMPLEG.R
# Purpose: Read and plot SIMPLE-G results from text format across US, World, BR, and Cerrado

# Initial SIMPLE-G script by: Iman Haqiqi
# Initial date: Aug 2019

# Edited by: Nick Manning 
# Initial edit date: May 2023
# Last edited: November 2023

# REQUIRES:
## SIMPLE-G Result files as '.txt' 

# NOTES:
## Create a folder named 'raster' in your local directory before running

# Next Steps:
## Change % Change to Raw Change Maps 
### US
### BR 
### Cerrado

## Save each plotting window after running the 'Best Maps' Section

## Maybe create new script to bring all of these plots in and use 'patchwork' to arrange them

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 0: Load required libraries ---- 
rm(list = ls())

library(tidyverse)
library(raster) # use for initial raster stack and basic plotting
library(terra) # use to wrangle geospatial data and plot
library(stringr) # use to manipulate result .txt file
library(RColorBrewer) # use for adding colorblind-friendly color palettes 
library(geobr) # use to load BR & Cerrado extent shapefiles
library(tigris) # use to load US and US-MW shapefiles
library(rasterVis) # use for easy violin plot 
library(reshape2) # use for melting data to then use ggplot

# 1: Prep SIMPLE-G Results --------------------

## 1.1: import and modify the output from the SIMPLE-G model ----------
getwd()

# NOTE: change this when you change the result file to one of three TXT files
## hi: enter "_hi";
## lo: enter "_lo";
## out: enter "";
pct <- "" # change when you change 'datafile'
pct_title <- "" # for plotting, either " - High" or " - Low"


# NOTE: will need to change to local location
folder <- "../Results/SIMPLEG-2023-10-29/"
der_folder <- "../Data_Derived/"
datafile   <- paste0(folder, "sg1x3x10_v2310", pct, "-out.txt")

# read results and substitute the old row-notation of using "!" on each row
old.lines  <- readLines(datafile)
new.lines <- 
  old.lines[which(str_sub(old.lines, 1,1) != " " & 
                    str_sub(old.lines, 1,1) != "!" &
                    str_sub(old.lines, 1,1) != ""  )]

# write temporary file 
# NOTE: Will need to change to local location
newfile = paste0(folder, "temp.txt")
writeLines(new.lines, newfile, sep=" ")

# read in new data table -- takes a bit
dat <- read.table(newfile, sep=",", header=T)


###### **SIMPLE-G Results Key** ------------

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
ras_file <- paste0(folder, "raster/")
 
# writeRaster(prct_ras$pct_QLAND, paste0(ras_file, "qLand_pct_", pct, ".tif"), format="GTiff", overwrite=TRUE)
# writeRaster(prct_ras$new_QLAND, paste0(ras_file, "qLand_new_", pct, ".tif"), format="GTiff", overwrite=TRUE)
# writeRaster(prct_ras$pct_QCROP, paste0(ras_file, "qCrop_pct_", pct, ".tif"), format="GTiff", overwrite=TRUE)
# writeRaster(prct_ras$new_QCROP, paste0(ras_file, "qCrop_new_", pct, ".tif"), format="GTiff", overwrite=TRUE)

# add Longitude and Latitude rasters (only need to do this one time, they aren't different for diff percents)
# writeRaster(prct_ras$LON, paste0(ras_file, "USBR_SIMPLEG_10292023_LON.tif"), format="GTiff", overwrite=TRUE)
# writeRaster(prct_ras$LAT, paste0(ras_file, "USBR_SIMPLEG_10292023_LAT.tif"), format="GTiff", overwrite=TRUE)

## Save 'terra' object ------ 
# create SpatRaster using terra
r <- terra::rast(dat)
r <- subset(r, c("pct_QLAND", "new_QLAND", "pct_QCROP", "new_QCROP"))

saveRDS(r, file = paste0("../Data_Derived/r", pct, ".rds"))

# 3: Make basic plots using 'raster' -----------

# plot using base R & 'raster'
#prct_ras <- subset(prct_ras, c("pct_QLAND", "new_QLAND", "pct_QCROP", "new_QCROP"))
#raster::plot(prct_ras)


# 4: Get Shapefiles: US-MW, BR, & Cerrado ------------

### Load US Shapefile ###
shp_us <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AK", "HI", "PR"))

#plot(shp_us)

#### Load US-MW Shapefile ###
shp_us_mw <- shp_us %>%
  filter(STUSPS %in% c("IA", "IL", "IN", "KS", "MI", "MN",
                       "MO", "ND", "NE", "OH", "SD", "WI"))
#plot(shp_us_mw)


#### Load Cerrado Shapefile ###
shp_cerr <- read_biomes(
  year = 2019,
  simplified = T,
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

#plot(shp_cerr)


#### Load BR Shapefile ###
shp_br <- read_country(
  year = 2019, 
  simplified = T, 
  showProgress = T)
  
#plot(shp_br)

# get Brazil States outline
shp_cerr_states <- read_state(
  year = 2019,
  simplified = T,
  #showProgress = T
) 

# filter to Cerrado States
shp_cerr_states <- shp_cerr_states %>% 
  dplyr::filter(abbrev_state %in% c("TO","MA","PI","BA","MG",
                                    "SP","MS","MT","GO","DF"))

## Save Shapefiles ------ 
save(shp_br, shp_cerr, shp_cerr_states, 
     shp_us, shp_us_mw,
     file = "../Data_Derived/shp_usbr.RData")


# 5: Edit Stack & Check Values -------------------

## 5.1: Calc & Add Raw Change from % and New -------
# Formula: new - (new / ((pct_change/100)+1))

# rawch = Raw Change

r_rawch_qcrop <- r_new_qcrop - (r_new_qcrop / ((r_pct_qcrop/100)+1))
r_rawch_qland <- r_new_qland - (r_new_qland / ((r_pct_qland/100)+1))

r <- c(r, r_rawch_qcrop, r_rawch_qland)
r

# set names 
names(r)
names(r) <- c("pct_QLAND", "new_QLAND", "pct_QCROP", "new_QCROP", "rawch_QCROP", "rawch_QLAND")


## 5.2: Modify New Land Values ----- 
# get and replace values that were over 50,000 ha per grid cell (impossible)
# link: https://gis.stackexchange.com/questions/421821/how-to-subset-a-spatraster-by-value-in-r

r_new_qland <- subset(r, "new_QLAND")
# set values over 50,000 to 50,000
x <- clamp(r_new_qland, upper=50000)
# set values over 50,000 to NA
x2 <- clamp(r_new_qland, upper=50000, values = F)

# set up a Master Raster (MR); i.e. a raster with valid cells as 1 and invalid as NA for multiplying with other rasters 
masterraster <- ifel(r_new_qland > 50000, NA, 1)

# count cells over 50,000 -- Can't just count NA from MR because others 
y <- ifel(r_new_qland > 50000, 999999, r_new_qland)
ncell(y[y==999999])


## 5.3: WORLD RESULTS ---------

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

r_rawch_qcrop <- subset(r, "rawch_QCROP")
# minmax(r_rawch_qcrop)
# hist(r_rawch_qcrop)
# boxplot(r_rawch_qcrop)

r_rawch_qcrop <- subset(r, "rawch_QCROP")
# minmax(r_rawch_qcrop)
# hist(r_rawch_qcrop)
# boxplot(r_rawch_qcrop)


# 6: US RESULTS ----------------------------

## 6.1 Clip to US Extent ----------------------------
# get extent as terra object for plotting
ext_us <- vect(ext(shp_us))

# plot basic us results by cropping and masking to just us extent
r_us <- crop(r, ext_us, mask = T)
r_us <- mask(r_us, shp_us)

# set Master Raster to US Extent
mr_us <- crop(masterraster, ext_us, mask = T)
mr_us <- mask(mr_us, shp_us)

## 6.2 Subset and do EDA ----------------------------

r_us_new_qland <- r_us %>% subset("new_QLAND")

# Count Invalid Cells & Modify
# count cells over 50,000 -- Can't just count NA from MR because others 
y <- ifel(r_us_new_qland > 50000, 999999, r_us_new_qland)
ncell(y[y==999999])

# set values over 50,000 to 50,000
r_us_new_qland <- clamp(r_us_new_qland, upper=50000)

# FUTURE: Apply Master Raster to set all invalid grid cells to NA
r_us <- r_us * mr_us

# subset to each band
r_us_new_qland <- r_us %>% 
  subset("new_QLAND") %>% 
  # set values over 50,000 to 50,000
  clamp(upper=50000)

r_us_pct_qland <- r_us %>% subset("pct_QLAND")
r_us_rawch_qland <- r_us %>% subset("rawch_QLAND")

# 
r_us_new_qcrop <- r_us %>% 
  subset("new_QCROP") %>% 
  # set values over 50,000 to 50,000
  clamp(upper=50000)

r_us_pct_qcrop <- r_us %>% subset("pct_QCROP")
r_us_rawch_qcrop <- r_us %>% subset("rawch_QCROP")

# re-stack and re-order
r_us <- c(r_us_new_qland, r_us_pct_qland, r_us_rawch_qland,
          r_us_new_qcrop, r_us_pct_qcrop, r_us_rawch_qcrop)

### Summaries ----------------------------
table_us <- summary(r_us, size = 1000000)
table_us
write.csv(t, file = paste0(folder, "table_us_102923", pct, ".csv"))

### EDA Plots ----------------------------
# terra::boxplot(r_us %>% subset(c("pct_QLAND", "pct_QCROP")))
# terra::boxplot(r_us %>% subset(c("new_QLAND", "new_QCROP")))

terra::hist(r_us)
terra::hist(log(r_us))

# Violin Plots 
F_p_violin <- function(df, area){
  
  # subset
  df_pct <- df %>% subset(c("pct_QLAND", "pct_QCROP"))
  df_rawch <- df %>% subset(c("rawch_QLAND", "rawch_QCROP"))
  df_new <- df %>% subset(c("new_QLAND", "new_QCROP"))

  # plot
  p1 <- bwplot(df_pct, 
               main = paste(area, "% Change", pct_title),
               ylab = "% Change")
  p2 <- bwplot(df_rawch, 
               main = paste(area, "Raw Change", pct_title),
               ylab = "Area (ha) or 1000-ton CE")
  p3 <- bwplot(df_new, 
               main = paste(area, "Post-Sim Values", pct_title),
               ylab = "Area (ha) or 1000-ton CE")
  plot(p1)
  plot(p2)
  plot(p3)
}

F_p_violin(r_us, "US")


## 6.3: Plot US Results ----------------------------

## plot all together ##
terra::plot(r_us, axes = F, type = "continuous")

## plot individually ##
par(mfrow=c(2,2), oma = c(0,0,2,0))
# r_us_pct_qland@ptr[["names"]][[1]]

# set new continuous color scheme
col_split <- colorRampPalette(brewer.pal(9, "YlOrRd"))(50)

### Create Functions -------
F_EDA_us_pct_map <- function(layer, title){
  
  par(mfrow=c(2,2), oma = c(0,0,0,0))
  terra::plot(layer,
              type = "continuous",
              col = rev(col_split),
              main = paste(title, "- default", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breaks = c(seq(-25, 5, by = 5)),
              col = rev(brewer.pal(9, "YlOrRd")),
              main = paste(title, "- breaks; seq()", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "cases",
              col = rev(brewer.pal(9, "YlOrRd")),
              main = paste(title, "- breaks; equal quantile", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "eqint",
              col = rev(brewer.pal(9, "YlOrRd")),
              main = paste(title, "- breaks; equal interval", pct_title), 
              plg = list(x="bottomright"))
}



F_EDA_us_new_map <- function(layer, title){
  
  par(mfrow=c(2,2), oma = c(0,0,0,0))
  terra::plot(layer,
              type = "continuous",
              col = col_split,
              main = paste(title, "- default", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breaks = c(seq(0, 250000, by = 50000)),
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; seq()", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "cases",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal quantile", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "eqint",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal interval", pct_title), 
              plg = list(x="bottomright"))
}

### Plot EDA --------
F_EDA_us_pct_map(r_us_pct_qland, "% Change in Cropland Area")
F_EDA_us_pct_map(r_us_pct_qcrop, "% Change in Crop Index")

F_EDA_us_new_map(r_us_new_qland, "Post-Sim Values of Cropland Area")
F_EDA_us_new_map(r_us_new_qcrop, "Post-Sim Values of Crop Index")


### Plot Best Map ---------
par(mfrow=c(3,2), oma = c(0,0,0,0))
par(mfrow=c(1,1), oma = c(0,0,0,0))

### Create mycolors ###
#display.brewer.all(colorblindFriendly = T)
mycolors <- colorRampPalette(brewer.pal(9, "PiYG"))(100) #changed from PiYG bc I needed a monochrome change for 
mycolors2 <- colorRampPalette(brewer.pal(9, "YlGn"))(100)
mycolors3 <- colorRampPalette(brewer.pal(9, "RdPu"))(100)


### OLD % Change in Cropland Area ###
# terra::plot(r_us_pct_qland,
#             type = "interval",
#             #breaks = c(seq(-25, 5, by = 5)),
#             #breaks = c(-25, -22.5, -20, -17.5, -15, -10, -5, 0, 2.5, 5),
#             breaks = c(-25, -22.5, -20, -17.5, -15, -5, 0, 5),
#             col = brewer.pal(7, "RdYlGn"),
#             #col = brewer.pal(9, "YlOrRd"), # try negative to positive 
#             main = paste("US % Change in Cropland Area", pct_title), 
#             plg = list(x="bottomright"))
# lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")
# #north(cbind(-121, 29))

# ### OLD % Change in Crop Index ###
# terra::plot(r_us_pct_qcrop,
#             type = "interval",
#             #breaks = c(seq(-25, 5, by = 5)),
#             breaks = c(-25, -22.5, -20, -17.5, -15, -5, 0, 5),
#             col = brewer.pal(7, "RdYlGn"),
#             #col = brewer.pal(9, "YlOrRd"),
#             main = paste("US % Change in Crop Index", pct_title), 
#             plg = list(x="bottomright"))
# lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")

### % Change in Cropland Area ###

# check ranges for plotting
#global(r_us_pct_qland, quantile, probs=c(0.05, 0.95), na.rm=TRUE)
#summary(r_us_pct_qland, 1000000)

terra::plot(r_us_pct_qland,
            # best yet - only problem is it doesn't show the positives on the 
            type = "continuous",
            breaks = c(-25, 3),
            col = rev(mycolors3), # mycolors3 to highlight the positive changes, or use rev(mycolors3)
            
            # # not bad
            # type = "interval",
            # breaks = c(-24, -22, -20, -15, -5, 0, 2),
            # col = rev(mycolors3),
            
            # eh
            # type = "continuous",
            # breaks = c(-25, 25),
            # col = mycolors,

            main = paste("US % Change in Cropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")
#north(cbind(-121, 29))

### % Change in Crop Index ###
#summary(r_us_pct_qcrop, 1000000)
terra::plot(r_us_pct_qcrop,
            type = "continuous",
            breaks = c(-25, 3),
            col = rev(mycolors3),
            main = paste("US % Change in Crop Production Index", pct_title),
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Cropland Area ###
terra::plot(r_us_new_qland/1000,
            type = "interval",
            #breaks = c(0, 0.1, 1, 5, 10, 20, 25, 30, 35, 50),
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            #col = brewer.pal(9, "YlOrRd"),
            col = brewer.pal(9, "YlGn"),
            #col = brewer.pal(9, "Greens"),
            main = paste("US Post-Simulation Crop Area", pct_title), 
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Crop Index ###
terra::plot(r_us_new_qcrop/1000,
            type = "interval",
            #breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 50),
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            #col = brewer.pal(9, "YlOrRd"),
            col = brewer.pal(9, "YlGn"),
            main = paste("US Post-Simulation Crop Index", pct_title), 
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")


### Actual (Raw) Change in Cropland Area ###

# NOTE: the test here is set to the Crop Production INDEX not area! 
test <- max(abs(minmax(r_us_rawch_qcrop/1000)))
test_breaks <- seq(-test, 1, length.out = 100)

# max is 24.1, set to 25 for simplicity
#test_breaks <- seq(0, 2, length.out = 100)
#test <- max(abs(minmax(r_us_rawch_qland/1000)))
terra::plot(r_us_rawch_qland/1000,
            type = "continuous",
            #breaks = seq(((-test)-(test/5)), test+(test/5), length.out = 100),
            breaks = test_breaks,
            col = rev(mycolors3),
            main = paste("US Raw Change in Cropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")
#north(cbind(-121, 29))


### Actual (Raw) Change in Crop Production Index ###
terra::plot(r_us_rawch_qcrop/1000,
            type = "continuous",
            #breaks = seq(((-test)-(test/5)), test+(test/5), length.out = 100),
            breaks = test_breaks,
            col = rev(mycolors3),
            main = paste("US Raw Change in Crop Production Index", pct_title),
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")
#north(cbind(-121, 29))




# 7: BRAZIL RESULTS ----------------------------

## 7.1 Clip to Brazil Extent ----------------------------

# get extent as terra object for plotting
ext_br <- vect(ext(shp_br))

# plot basic BR results by cropping and masking to just BR extent
r_br <- terra::crop(r, ext_br, mask = T) 
r_br <- mask(r_br, shp_br)

# set Master Raster to BR Extent
mr_br <- crop(masterraster, ext_br, mask = T)
mr_br <- mask(mr_br, shp_br)

## 7.2 Subset and do EDA ----------------------------
r_br_new_qland <- r_br %>% subset("new_QLAND")

# Count Invalid Cells & Modify
# count cells over 50,000 -- Can't just count NA from MR because others 
y <- ifel(r_br_new_qland > 50000, 999999, r_br_new_qland)
ncell(y[y==999999])

# set values over 50,000 to 50,000
r_br_new_qland <- clamp(r_br_new_qland, upper=50000)

# FUTURE: Apply Master Raster to set all invalid grid cells to NA
r_br <- r_br * mr_br

# subset to each band
r_br_new_qland <- r_br %>% 
  subset("new_QLAND") %>% 
  # set values over 50,000 to 50,000
  clamp(upper=50000)

r_br_pct_qland <- r_br %>% subset("pct_QLAND")
r_br_rawch_qland <- r_br %>% subset("rawch_QLAND")

# 
r_br_new_qcrop <- r_br %>% 
  subset("new_QCROP") %>% 
  # set values over 50,000 to 50,000
  clamp(upper=50000)

r_br_pct_qcrop <- r_br %>% subset("pct_QCROP")
r_br_rawch_qcrop <- r_br %>% subset("rawch_QCROP")

# re-stack and re-order
r_br <- c(r_br_new_qland, r_br_pct_qland, r_br_rawch_qland,
          r_br_new_qcrop, r_br_pct_qcrop, r_br_rawch_qcrop)

### Summaries ----------------------------
table_br <- summary(r_br, size = 1000000) # set size to not use a sample
table_br
write.csv(table_br, file = paste0(folder, "table_br_102923", pct, ".csv"))

### Boxplots ----------------------------
F_p_violin(r_br, "Brazil")
hist(log(r_br))
## 7.3: Plot BR Results ----------------------------
terra::plot(r_br, axes = F, type = "interval")

### Create Functions -------
F_EDA_br_pct_map <- function(layer, title){
  
  par(mfrow=c(2,2), oma = c(0,0,0,0))
  terra::plot(layer,
              type = "continuous",
              col = col_split,
              main = paste(title, "- default", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breaks = c(seq(0, 4, by = 0.5)),
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; seq()", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "cases",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal quantile", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "eqint",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal interval", pct_title), 
              plg = list(x="bottomright"))
}



F_EDA_br_new_map <- function(layer, title){
  
  par(mfrow=c(2,2), oma = c(0,0,0,0))
  terra::plot(layer,
              type = "continuous",
              col = col_split,
              main = paste(title, "- default", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breaks = c(seq(0, 50000, by = 5000)),
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; seq()", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "cases",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal quantile", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "eqint",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal interval", pct_title), 
              plg = list(x="bottomright"))
}

### Plot EDA --------
F_EDA_br_pct_map(r_br_pct_qland, "% Change in Cropland Area")
F_EDA_br_pct_map(r_br_pct_qcrop, "% Change in Crop Index")

F_EDA_br_new_map(r_br_new_qland, "Post-Sim Values of Cropland Area")
F_EDA_br_new_map(r_br_new_qcrop, "Post-Sim Values of Crop Index")

### Plot Best Map ---------
par(mfrow=c(3,2), oma = c(0,0,0,0))

# ### OLD % Change in Cropland Area ###
# terra::plot(r_br_pct_qland,
#             type = "interval",
#             breaks = c(seq(0, 4, by = 0.5)),
#             #col = brewer.pal(9, "YlGn"),
#             col = brewer.pal(9, "YlOrRd"),
#             main = paste("Brazil % Change in Cropland Area", pct_title),
#             plg = list(x="bottomright"))
# lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
# #north(cbind(-65, -25))


### OLD % Change in Crop Index ###
# terra::plot(r_br_pct_qcrop,
#             type = "interval",
#             breaks = c(seq(0, 4, by = 0.5)),
#             #col = brewer.pal(9, "YlGn"),
#             col = brewer.pal(9, "YlOrRd"),
#             main = paste("Brazil % Change in Crop Index", pct_title),
#             plg = list(x="bottomright"))
# lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")



### % Change in Cropland Area ###
terra::plot(r_br_pct_qland,
            # type = "interval",
            # breaks = c(seq(0, 4, by = 0.5)),
            # col = brewer.pal(9, "YlOrRd"),
            
            type = "continuous",
            col =  mycolors3,
            breaks = c(seq(0, 4, by = 0.5)),
            
            main = paste("Brazil % Change in Cropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### % Change in Crop Index ###
terra::plot(r_br_pct_qcrop,
            # type = "interval",
            # breaks = c(seq(0, 4, by = 0.5)),
            # col = brewer.pal(9, "YlOrRd"),
            
            type = "continuous",
            col = mycolors3, 
            breaks = c(seq(0, 4, by = 0.5)),
            
            main = paste("Brazil % Change in Crop Index", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Cropland Area ###
terra::plot(r_br_new_qland/1000,
            type = "interval",
            #breaks = c(0, 0.1, 1, 5, 10, 15, 25, 35, 50),
            #breaks = c(0, 0.1, 1, 5, 10, 20, 25, 30, 35, 50),
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            #col = brewer.pal(9, "YlOrRd"),
            col = brewer.pal(9, "YlGn"),
            main = paste("Brazil Post-Simulation Crop Area", pct_title), 
            #plg = list(x="bottomright")
            )
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Crop Index ###
terra::plot(r_br_new_qcrop/1000,
            type = "interval",
            #breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 350, 450),
            #breaks = c(0, 0.1, 1, 5, 10, 20, 25, 30, 35, 50),
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            #col = brewer.pal(9, "YlOrRd"),
            col = brewer.pal(9, "YlGn"),
            main = paste("Brazil Post-Simulation Crop Index", pct_title), 
            #plg = list(x="bottomright")
            )
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")


### Actual (Raw) Change in Cropland Area ###
# NOTE: the test here is set to the Crop Production INDEX not area! 
test <- max(abs(minmax(r_br_rawch_qcrop/1000)))
#test_breaks <- seq(0, test, length.out = 100)

# max is 1.84, set to 2 for simplicity
test_breaks <- seq(0, 2, length.out = 100)

#test <- max(abs(minmax(r_br_rawch_qland/1000)))
terra::plot(r_br_rawch_qland/1000,
            type = "continuous",
            #breaks = seq(((-test)-(test/5)), test+(test/5), length.out = 100),
            #breaks = seq(-test, test, length.out = 100),
            breaks = test_breaks/4,
            col = mycolors3,
            main = paste("Brazil Raw Change in Cropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
#north(cbind(-121, 29))


### Actual (Raw) Change in Crop Production Index ###
# test <- max(abs(minmax(r_br_rawch_qcrop/1000)))
# test_breaks <- seq(0, test, length.out = 100)
terra::plot(r_br_rawch_qcrop/1000,
            type = "continuous",
            #breaks = seq(((-test)-(test/5)), test+(test/5), length.out = 100),
            #breaks = seq(-test, test, length.out = 100),
            breaks = test_breaks,
            col = mycolors3,
            main = paste("Brazil Raw Change in Crop Production Index", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
#north(cbind(-121, 29))





# 8: CERRADO RESULTS ----------------------------

## 8.1 Clip to Cerrado Extent ----------------------------
# get Cerrado extent as terra object 
ext_cerr <- vect(ext(shp_cerr))

# crop and mask
r_cerr <- terra::crop(r, ext_cerr, mask = T)
r_cerr <- mask(r_cerr, shp_cerr)

## 8.2 Subset and do EDA ----------------------------

# get extent as terra object for plotting
ext_cerr <- vect(ext(shp_cerr))

# plot basic BR results by cropping and masking to just BR extent
r_cerr <- terra::crop(r, ext_cerr, mask = T) 
r_cerr <- mask(r_cerr, shp_cerr)

# set Master Raster to BR Extent
mr_cerr <- crop(masterraster, ext_cerr, mask = T)
mr_cerr <- mask(mr_cerr, shp_cerr)

## 8.2 Subset and do EDA ----------------------------
r_cerr_new_qland <- r_cerr %>% subset("new_QLAND")

# Count Invalid Cells & Modify
# count cells over 50,000 -- Can't just count NA from MR because others 
y <- ifel(r_cerr_new_qland > 50000, 999999, r_cerr_new_qland)
ncell(y[y==999999])

# set values over 50,000 to 50,000
r_cerr_new_qland <- clamp(r_cerr_new_qland, upper=50000)

# FUTURE: Apply Master Raster to set all invalid grid cells to NA
r_cerr <- r_cerr * mr_cerr

# subset to each band -- Cropland Area
r_cerr_new_qland <- r_cerr %>% 
  subset("new_QLAND") %>% 
  # set values over 50,000 to 50,000
  clamp(upper=50000)

r_cerr_pct_qland <- r_cerr %>% subset("pct_QLAND")
r_cerr_rawch_qland <- r_cerr %>% subset("rawch_QLAND")

# subset to each band -- Crop Production Index
r_cerr_new_qcrop <- r_cerr %>% 
  subset("new_QCROP") %>% 
  # set values over 50,000 to 50,000
  clamp(upper=50000)

r_cerr_pct_qcrop <- r_cerr %>% subset("pct_QCROP")
r_cerr_rawch_qcrop <- r_cerr %>% subset("rawch_QCROP")

# re-stack and re-order
r_cerr <- c(r_cerr_new_qland, r_cerr_pct_qland, r_cerr_rawch_qland,
          r_cerr_new_qcrop, r_cerr_pct_qcrop, r_cerr_rawch_qcrop)

### Summaries ----------------------------
table_cerr <- summary(r_cerr, size = 1000000) # set size to not use a sample
table_cerr
write.csv(table_cerr, file = paste0(folder, "table_cerr_102923", pct, ".csv"))

### Boxplots ----------------------------
F_p_violin(r_cerr, "Cerrado")
hist(r_cerr)
hist(log(r_cerr))

## 8.3: Plot Cerrado Results ----------------------------
### Create Functions -------
F_EDA_cerr_pct_map <- function(layer, title){
  
  par(mfrow=c(2,2), oma = c(0,0,0,0))
  terra::plot(layer,
              type = "continuous",
              col = col_split,
              main = paste(title, "- default"), 
              plg = list(x="topleft"))
  
  terra::plot(layer,
              type = "interval",
              breaks = c(seq(0, 4, by = 0.5)),
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; seq()"), 
              plg = list(x="topleft"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "cases",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal quantile"), 
              plg = list(x="topleft"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "eqint",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal interval"), 
              plg = list(x="topleft"))
}



F_EDA_cerr_new_map <- function(layer, title){
  
  par(mfrow=c(2,2), oma = c(0,0,0,0))
  terra::plot(layer,
              type = "continuous",
              col = col_split,
              main = paste(title, "- default"), 
              plg = list(x="topleft"))
  
  terra::plot(layer,
              type = "interval",
              breaks = c(seq(0, 450000, by = 100000)),
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; seq()"), 
              plg = list(x="topleft"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "cases",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal quantile"), 
              plg = list(x="topleft"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "eqint",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal interval"), 
              plg = list(x="topleft"))
}

### Plot EDA --------
F_EDA_cerr_pct_map(r_cerr_pct_qland, "% Change in Cropland Area")
F_EDA_cerr_pct_map(r_cerr_pct_qcrop, "% Change in Crop Index")

F_EDA_cerr_new_map(r_cerr_new_qland, "Post-Sim Values of Cropland Area")
F_EDA_cerr_new_map(r_cerr_new_qcrop, "Post-Sim Values of Crop Index")

### Plot Best Map ---------
par(mfrow=c(3,2), oma = c(0,0,0,0))
#par(mfrow=c(1,1), oma = c(0,0,0,0))

### OLD % Change in Cropland Area ###
# terra::plot(r_cerr_pct_qland,
#             type = "interval",
#             breaks = c(seq(0, 4, by = 0.5)),
#             #col = brewer.pal(9, "YlGn"),
#             col = brewer.pal(9, "YlOrRd"),
#             main = paste("Cerrado % Change in Cropland Area", pct_title), 
#             plg = list(x="topleft"))
# lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
# north(cbind(-59, -22))
# sbar(d = 400, type = "bar", xy = "bottomright", divs = 4, below = "km")

### OLD % Change in Crop Index ###
# terra::plot(r_cerr_pct_qcrop,
#             type = "interval",
#             breaks = c(seq(0, 4, by = 0.5)),
#             #col = brewer.pal(9, "YlGn"),
#             col = brewer.pal(9, "YlOrRd"),
#             main = paste("Cerrado % Change in Crop Index", pct_title), 
#             plg = list(x="topleft"))
# lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### % Change in Cropland Area ###
terra::plot(r_cerr_pct_qland,
            type = "continuous",
            col =  mycolors3,
            breaks = c(seq(0, 4, by = 0.5)),
            
            main = paste("Cerrado % Change in Cropland Area", pct_title),
            plg = list(x="topleft"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
#north(cbind(-59, -22))
#sbar(d = 400, type = "bar", xy = "bottomright", divs = 4, below = "km")

### % Change in Crop Index ###
terra::plot(r_cerr_pct_qcrop,
            type = "continuous",
            col =  mycolors3,
            breaks = c(seq(0, 4, by = 0.5)),
            main = paste("Cerrado % Change in Crop Index", pct_title),
            plg = list(x="topleft"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Cropland Area ###
terra::plot(r_cerr_new_qland/1000,
            # type = "continuous",
            # col = mycolors2,
            
            # type = "interval",
            # breaks = c(0, 0.1, 1, 5, 10, 20, 25, 30, 35, 50), # BR cropland legend
            # col = brewer.pal(9, "YlGn"),
            
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            
            #col = brewer.pal(9, "YlOrRd"),
            main = paste("Cerrado Post-Simulation Crop Area", pct_title), 
            #plg = list(x="topleft")
            )
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Crop Index ###
terra::plot(r_cerr_new_qcrop/1000,
            # Same as BR color scheme
            # type = "interval",
            # breaks = c(0, 0.1, 1, 5, 10, 20, 25, 30, 35, 50),
            
            # changed for Cerr; removed 0.1
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            
            # old breaks 
            # breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 350, 450),
            
            #col = brewer.pal(9, "YlOrRd"),
            col = brewer.pal(9, "YlGn"),
            main = paste("Cerrado Post-Simulation Crop Index", pct_title), 
            #plg = list(x="topleft")
            )
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")


### Actual (Raw) Change in Cropland Area ###
test <- max(abs(minmax(r_br_rawch_qcrop/1000)))
#test_breaks <- seq(0, test, length.out = 100)

# max is 1.48, set to 2 for simplicity
test_breaks <- seq(0, 2, length.out = 100)


test <- max(abs(minmax(r_cerr_rawch_qcrop/1000)))
terra::plot(r_cerr_rawch_qland/1000,
            type = "continuous",
            #breaks = seq(((-test)-(test/5)), test+(test/5), length.out = 100),
            #breaks = seq(-test, test, length.out = 100),
            breaks = test_breaks/4,
            col = mycolors3,
            main = paste("Cerrado Raw Change in Cropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
#north(cbind(-121, 29))


### Actual (Raw) Change in Crop Production Index ###
test <- max(abs(minmax(r_cerr_rawch_qcrop/1000)))
terra::plot(r_cerr_rawch_qcrop/1000,
            type = "continuous",
            #breaks = seq(((-test)-(test/5)), test+(test/5), length.out = 100),
            #breaks = seq(-test, test, length.out = 100),
            breaks = test_breaks,
            col = mycolors3,
            main = paste("Cerrado Raw Change in Crop Production Index", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
#north(cbind(-121, 29))





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# FUTURE WORK ----------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Look into removing outliers here:
## https://plantarum.ca/2023/02/13/terra-maps/

# Look into rasterVis for Boxplots: 
## https://oscarperpinan.github.io/rastervis/

## Select within plot to keep clean rather than reclassify ---------------

# plot with breaks defined, not reclassifying the data
terra::plot(r_cerr, "pct_QCROP",
            type="interval", 
            #breakby = "jenks",
            breaks = c(-2, -1, -0.5, -0.1, -0.01, 0.01, 0.1, 0.5, 1, 2), # experimental breaks
            #breaks = c(-2, -1, -0.5, -.1, 0, 0.1, 0.5, 1, 2), # the breaks used in the reclassified
            col = rev(brewer.pal(9, "PiYG")), 
            main = "% Change in Crop Production Index", 
            plg = list(x="topleft"))

# compare with reclassified plot
terra::plot(r_cerr_pct_qcrop_rc, type="classes", col = rev(brewer.pal(9, "PiYG")), 
            main = "% Change in Crop Production Index", plg = list(x="topleft"))

# why does it look so different?? 
## we're reclassifying and using "classes" in the latter, whereas in the former we are including the breaks with the actual data  


## Reclassify with Min/Max --------------------------
# Reclassify with MIN MAX; [1] == MIN, [2] == MAX
# essentially, "Round the min or max value here to 1 decimal point"

r_cerr_pct_qland_rc <- classify(r_cerr_pct_qland, c(round((minmax(r_cerr_pct_qland)[1]), 1),
                                                    -2, -1, -0.5, 0, 0.5, 1, 2, 4, 
                                                    round((minmax(r_cerr_pct_qland)[2]), 1)))

cerr_new_qland_rc <- classify(r_cerr_new_qland, c(0, 0.5, 1, 2, 3, 4, 
                                                    round((minmax(r_cerr_new_qland)[2]), 1)))

# r_cerr_pct_qcrop_rc <- classify(r_cerr_pct_qcrop,c(round((minmax(r_cerr_pct_qcrop)[1])),
#                                                    -1, -0.5, -.1, 0, 0.1, 0.5, 1,
#                                                    round((minmax(r_cerr_pct_qcrop)[2]))))
r_cerr_pct_qcrop_rc <- classify(r_cerr_pct_qcrop,c(round((minmax(r_cerr_pct_qcrop)[1]), digits = 1),
                                                   0.1, 0.5, 1, 2, 2.5, 3,
                                                   round((minmax(r_cerr_pct_qcrop)[2]),digits = 1)))

terra::plot(r_cerr_pct_qcrop_rc, type="classes", col = brewer.pal(9, "Oranges"), 
            main = "% Change in Crop Production Index", plg = list(x="topleft"))

r_
r_cerr_new_qcrop_rc <- classify(r_cerr_new_qcrop, c(0, 10, 25, 50, 75, 100, 
                                                    round((minmax(r_cerr_new_qcrop)[2]))))





## Prettier Box Plots --------------

# NEED TO MELT ALL THE DIFF PCT's INTO ONE COLUMN
# Only use 2012 to keep it from getting too crazy
# Follow https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
terra::boxplot(r_cerr)

terra::boxplot(r_cerr_pct_qland, main = "% Change in Cropland Area")
#terra::boxplot(r_cerr_new_qland, main = "Post-Sim Cropland Area (1000 ha)")
terra::boxplot(r_cerr_pct_qcrop, main = "% Change in Crop Production Index")
#terra::boxplot(r_cerr_new_qcrop, main = "Post-Sim Quantity of Crops (1000-ton CE)")

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



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# GRAVEYARD ------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## 1: 20% Reclassifying Plans -----------------------------

### 20 PERCENT RECLASSIFICATION BREAKS ##################################
# r_cerr_pct_qland_rc <- classify(r_cerr_pct_qland, c(-6, -2, -1, -0.5, 0, 0.5, 1, 2, 4, 6))
# 
# #r_cerr_new_qland_rc <- classify(r_cerr_new_qland, c(0, .25, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5))
# r_cerr_new_qland_rc <- classify(r_cerr_new_qland, c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5))
# 
# r_cerr_pct_qcrop_rc <- classify(r_cerr_pct_qcrop,c(-2, -1, -0.5, -.1, 0, 0.1, 0.5, 1, 2))
# 
# #r_cerr_new_qcrop_rc <- classify(r_cerr_new_qcrop, c(0, 1, 5, 10, 25, 50, 100, 150))
# r_cerr_new_qcrop_rc <- classify(r_cerr_new_qcrop, c(0, 10, 25, 50, 75,100, 150))

### 20 PERCENT RECLASSIFICATION BREAKS #########################################
# r_br_pct_qland_rc <- classify(r_br_pct_qland, c(-5, -1, -0.5, -0.1, 0, 0.1, 0.5, 2, 5))
# r_br_new_qland_rc <- classify(r_br_new_qland, c(0, 0.5, 1, 1.5, 2, 2.5, 3, 4))
# 
# r_br_pct_qcrop_rc <- classify(r_br_pct_qcrop,c(-5, -1, 0.1, 0, 0.1, 1, 5, 10))
# r_br_new_qcrop_rc <- classify(r_br_new_qcrop, c(0, 1, 5, 10, 25, 50, 100, 150))

## 2: Plans for 'tidyterra' package --------
library(tidyterra)
r_df <- r %>% 
  mutate(cut_pctQland = cut(pct_QLAND, breaks = 9))

rr <- raster(r)
r_df <- rr %>% 
  mutate(cut_pctQland = cut(pct_QLAND, breaks = 9))

library(rasterVis)
r_us_pct <- r_us %>% subset(c("pct_QLAND", "pct_QCROP")) 

gplot(r_us_pct)+geom_tile(aes(fill = value))+facet_wrap(~variable)

## 3: Old violin ggplots (part of F_p_viloin, but helpful for melting)--------

# F_p_violin <- function(df){
#   df2 <- as.data.frame(df)
#   
#   df2_new <- as.data.frame(df) %>% 
#     dplyr::select(new_QLAND, new_QCROP) %>% 
#     melt(variable.name = "layer", value.name = "new_value")
#   
#   p_v_1 <- ggplot(df2_new, aes(x = layer, y = new_value))+
#     geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
#     theme_bw()+
#     labs(title = "New Values of Area & Index")
#   
#   df2_pct <- as.data.frame(df) %>% 
#     dplyr::select(pct_QLAND, pct_QCROP) %>% 
#     melt(variable.name = "layer", value.name = "pct_change")
#   
#   p_v_2 <- ggplot(df2_pct, aes(x = layer, y = pct_change))+
#     geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
#     theme_bw()+labs(title = "% Change of Area & Index")
#   
#   plot(p_v_1)
#   plot(p_v_2)
#   
# }

# 4: Old Fxn for Raw Change -------
# write fxn
F_calc_area_change <- function(new, pct_change){
  # might need to add omit.na??
  new - (new / ((pct_change/100)+1))
}