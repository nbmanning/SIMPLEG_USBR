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

## Change Pre-processing into Fxn and remove the EDA plots

## Change % Change to Raw Change Maps -- DONE; could change color schemes
### US
### BR 
### Cerrado

## Save each plotting window after running the 'Best Maps' Section -- DONE

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Load Libraries & Set Constants ---- 
rm(list = ls())

## Libraries ##
library(tidyverse)
library(raster) # use for initial raster stack and basic plotting
library(terra) # use to wrangle geospatial data and plot
library(RColorBrewer) # use for adding colorblind-friendly color palettes 
library(geobr) # use to load BR & Cerrado extent shapefiles
library(tigris) # use to load US and US-MW shapefiles
library(rasterVis) # use for easy violin plot 
library(reshape2) # use for melting data to then use ggplot
library(sf)
library(tidyterra) # plot using ggplot() instead of base R with 'terra'
library(stringr) # use to manipulate result .txt file
library(ggspatial) # N arrow and Scale Bar w tidyterrra
library(rworldmap) # getting simple BR Border

## Constants ##

# NOTE: change this when you change the result file to one of three TXT files
## hi: enter "_hi";
## lo: enter "_lo";
## out / default; enter ""
pct <- "" # change when you change 'datafile'
pct_title <- "" # for plotting, either " - High" or " - Low" or ""


# NOTE: will need to change to local location
folder <- "../Results/SIMPLEG-2023-10-29/"
folder_plot <- "../Figures/102923/"
datafile   <- paste0(folder, "sg1x3x10_v2310", pct, "-out.txt")
datafile <- "../Results/SIMPLEG-2023-10-29/sg1x3x10_v2310-out.txt"


# INITIAL PREP & SAVE -----------------------------------------------------------
# 1: Prep SIMPLE-G Results --------------------

## 1.1: import and modify the output from the SIMPLE-G model ----------
getwd()

# read results and substitute the old row-notation of using "!" on each row
old.lines  <- readLines(datafile)
new.lines <- 
  old.lines[which(str_sub(old.lines, 1,1) != " " & 
                    str_sub(old.lines, 1,1) != "!" &
                    str_sub(old.lines, 1,1) != ""  )]

# write temporary file 
# NOTE: Will need to change to local location
newfile = paste0(folder, "temp.txt")
writeLines(new.lines, newfile, sep="\n")

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


# 3: Get Shapefiles: US-MW, BR, & Cerrado ------------

# ### Load World Shapefile ###
# shp_world <- st_read(system.file("shapes/world.gpkg", package="spData"))
#
# ### Load US Shapefile ###
# shp_us <- states(cb = TRUE, resolution = "20m") %>%
#   filter(!STUSPS %in% c("AK", "HI", "PR"))
# 
# #### Load US-MW Shapefile ###
# shp_us_mw <- shp_us %>%
#   filter(STUSPS %in% c("IA", "IL", "IN", "KS", "MI", "MN",
#                        "MO", "ND", "NE", "OH", "SD", "WI"))
# 
# #### Load Cerrado Shapefile ###
# shp_cerr <- read_biomes(
#   year = 2019,
#   simplified = T,
#   showProgress = T) %>%
#   dplyr::filter(name_biome == "Cerrado")
# 
# 
# #### Load BR Shapefile ###
# shp_br <- read_country(
#   year = 2019,
#   simplified = T,
#   showProgress = T)
# 
# #### Load Simple BR Border Shapefile ###
# data("countriesCoarse")
# shp_br_border <- countriesCoarse %>% subset(SOV_A3 == "BRA")
# shp_br_border <- st_combine(st_as_sf(shp_br_border))
# 
# ## Load Cerrado Outline ##  
# # get Brazil States outline
# shp_cerr_states <- read_state(
#   year = 2019,
#   simplified = T) 
# 
# # filter to Cerrado States
# shp_cerr_states <- shp_cerr_states %>% 
#   dplyr::filter(abbrev_state %in% c("TO","MA","PI","BA","MG",
#                                     "SP","MS","MT","GO","DF"))
# 
## # Save Shapefiles ------
# save(shp_br, shp_br_border,
#      shp_cerr, shp_cerr_states,
#      shp_us, shp_us_mw,
#      file = "../Data_Derived/shp_usbr.RData")

# CREATE FUNCTIONS --------------

# these functions:
## 1) Plot EDA (histograms and violin plots)
## 2) Count the number of Invalid Grid Cells
## 3) Cap / Clamp the value of a single grid cell at 50,000
## 3) Prep the AOI Rasters by using the Count and Clamp Functions
## 4) Save the Summary tables and use the Violin Plot fxn 

# fxn to Create and Save Violin Plots and Basic Histograms 
F_p_violin <- function(df, area){
  
  # histograms
  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_hist", ".png"))
  terra::hist(df, maxcell=100000000000)
  dev.off()
  
  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_hist_log", ".png"))
  terra::hist(log(df), maxcell=10000000000)
  dev.off()
  
  # subset and change names
  df_pct <- df %>% 
    subset(c("pct_QLAND", "pct_QCROP")) 
  names(df_pct) <- c("Cropland Area % Change", "CPI % Change")
  
  df_rawch <- df %>% 
    subset(c("rawch_QLAND", "rawch_QCROP"))
  names(df_rawch) <- c("Cropland Area Raw Change", "CPI Raw Change")
  
  
  df_new <- df %>% 
    subset(c("new_QLAND", "new_QCROP"))
  names(df_new) <- c("Cropland Area New Values", "CPI New Values")
  
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
  
  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_bw", "_pctchange", ".png"))
  plot(p1)
  dev.off()
  
  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_bw", "_rawchange", ".png"))
  plot(p2)
  dev.off()
  
  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_bw", "_newvalues", ".png"))
  plot(p3)
  dev.off()
  
  return(p1)
  return(p2)
  return(p3)
}


# fxn to count the cells over 50,000 in a layer (either new_QLAND or new_QCROP)
F_count_invalid <- function(df, layer_name){
  # get just the layer of interest
  df_new <- df %>% subset(layer_name)
  
  # Count Invalid Cells & Modify
  y <- ifel(df_new > 50000, 999999, df_new)
  y_land <- ncell(y[y==999999])
  
  print(paste(layer_name, "grid cells over 50,000:", y_land))
}


# fxn to max layers out at 50,000
F_clamp <- function(df, layer_name){
  # get just one layer 
  df_new <- df %>% subset(layer_name)
  
  # set values over 50,000 to 50,000
  df_new <- df_new %>% 
    clamp(upper=50000)
}


# Work Flow Functions #

# fxn to incorporate both of these into one function
F_aoi_prep <- function(shp, area_name){
  
  ## Clip to AOI Extent ##
  # get extent as terra object for plotting
  ext_area <- vect(ext(shp))
  
  # crop and masking to just the extent of interest
  r_aoi <- terra::crop(r, ext_area, mask = T) 
  r_aoi <- mask(r_aoi, shp)
  
  ## Call Count and Clamp functions to cap the grid cells at 50,000 ##
  # QLAND #
  F_count_invalid(r_aoi, "new_QLAND")
  r_aoi_new_qland <- F_clamp(r_aoi, "new_QLAND")
  
  # QCROP #
  F_count_invalid(r_aoi, "new_QCROP")
  r_aoi_new_qcrop <- F_clamp(r_aoi, "new_QCROP")
  
  # get other layers
  r_aoi_pct_qland <- r_aoi %>% subset("pct_QLAND")
  r_aoi_rawch_qland <- r_aoi %>% subset("rawch_QLAND")
  r_aoi_pct_qcrop <- r_aoi %>% subset("pct_QCROP")
  r_aoi_rawch_qcrop <- r_aoi %>% subset("rawch_QCROP")
  
  # re-stack and re-order
  r_aoi <- c(r_aoi_new_qland, r_aoi_pct_qland, r_aoi_rawch_qland,
             r_aoi_new_qcrop, r_aoi_pct_qcrop, r_aoi_rawch_qcrop)
  
  return(r_aoi)
}

# fxn to get summary of data, call the violin fxn, and plot a basic map
F_EDA <- function(r_aoi, area_name){  
  # Get and save a summary table
  table_area <- summary(r_aoi, size = 1000000000) # set size to not use a sample
  print(table_area)
  
  write.csv(table_area, file = paste0(folder, "/summary_tables/", "table_", area_name, pct, "_102923", ".csv"))
  print(global(r_aoi$rawch_QLAND, fun = "sum", na.rm = T))
  print(global(r_aoi$rawch_QCROP, fun = "sum", na.rm = T))
  # Call EDA fxn to get and save violin plots 
  F_p_violin(r_aoi, area_name)
  
  # Plot basic initial maps
  terra::plot(r_aoi, axes = F, type = "interval")
}


# PROCESS RESULTS -----------------------------------------------------------

# NOTE: You can re-run the script starting here if you already ran sections 1, 2, and 3

load("../Data_Derived/shp_usbr.RData")
r <- readRDS(file = paste0("../Data_Derived/r", pct, ".rds"))

# 4: Edit Stack & Check Values -------------------

## 4.1: Calc & Add Raw Change from % and New -------
# Formula: new - (new / ((pct_change/100)+1))

# subset 
r_pct_qland <- subset(r, "pct_QLAND")
r_new_qland <- subset(r, "new_QLAND")

r_pct_qcrop <- subset(r, "pct_QCROP")
r_new_qcrop <- subset(r, "new_QCROP")

# NOTE: rawch = Raw Change
r_rawch_qcrop <- r_new_qcrop - (r_new_qcrop / ((r_pct_qcrop/100)+1))
r_rawch_qland <- r_new_qland - (r_new_qland / ((r_pct_qland/100)+1))

r <- c(r, r_rawch_qcrop, r_rawch_qland)
r

# set names 
names(r)
names(r) <- c("pct_QLAND", 
              "new_QLAND", 
              "pct_QCROP", 
              "new_QCROP", 
              "rawch_QCROP", 
              "rawch_QLAND")


## 4.2: Count New Land Values ----- 

# get and replace values that were over 50,000 ha per grid cell (>50,000 ha per grid cell is impossible)
# link: https://gis.stackexchange.com/questions/421821/how-to-subset-a-spatraster-by-value-in-r

# count cells over 50,000 -- Can't just count NA from MR because others 
y <- ifel(r_new_qland > 50000, 999999, r_new_qland)
ncell(y[y==999999])


## 4.3: World Results ---------

# Quick plot of World results
terra::plot(r, axes = F)
terra::plot(log(r), axes = F)

# Call fxn to clip, count, and clamp data 
r_row <- F_aoi_prep(shp = shp_world, area_name = "World")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_row, area_name = "World")



# 5: US Results  ----------------------------

## 5.0 Manual Calculation (Keep for Fxn Debugging) ----------
# 
# ## Clip to US Extent ##
# 
# # get extent as terra object for plotting
# ext_us <- vect(ext(shp_us))
# 
# # plot basic us results by cropping and masking to just us extent
# r_us <- crop(r, ext_us, mask = T)
# r_us <- mask(r_us, shp_us)
# 
# 
# ## Subset and do EDA ##
# r_us_new_qland <- r_us %>% subset("new_QLAND")
# 
# # Count invalid cells
# # count cells over 50,000 -- Can't just count NA from MR because others 
# y <- ifel(r_us_new_qland > 50000, 999999, r_us_new_qland)
# ncell(y[y==999999])
# 
# 
# # subset to each band
# # QLAND - Cropland Area (ha)
# r_us_new_qland <- r_us %>% 
#   subset("new_QLAND") %>% 
#   # set values over 50,000 to 50,000
#   clamp(upper=50000)
# 
# # QCROP - Crop Production Index (1000-ton Corn Equivalent)
# r_us_new_qcrop <- r_us %>% 
#   subset("new_QCROP") %>% 
#   # set values over 50,000 to 50,000
#   clamp(upper=50000)
# 
# # get other layers for re-ordering 
# r_us_pct_qland <- r_us %>% subset("pct_QLAND")
# r_us_rawch_qland <- r_us %>% subset("rawch_QLAND")
# r_us_pct_qcrop <- r_us %>% subset("pct_QCROP")
# r_us_rawch_qcrop <- r_us %>% subset("rawch_QCROP")
# 
# # re-stack and re-order
# r_us <- c(r_us_new_qland, r_us_pct_qland, r_us_rawch_qland,
#           r_us_new_qcrop, r_us_pct_qcrop, r_us_rawch_qcrop)
# 
# ### Summaries ###
# table_us <- summary(r_us, size = 1000000)
# table_us
# write.csv(table_us, file = paste0(folder, "table_us_102923", pct, ".csv"))
# 
# ### EDA Plots ###
# # terra::boxplot(r_us %>% subset(c("pct_QLAND", "pct_QCROP")))
# # terra::boxplot(r_us %>% subset(c("new_QLAND", "new_QCROP")))
# 


## 5.1 Prep Data -------

# Call fxn to clip, count, and clamp data 
r_us <- F_aoi_prep(shp = shp_us, area_name = "US")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_us, area_name = "US")


## 5.2 Plot Best US Map ---------

### Create mycolors ###
# set new continuous color scheme
mycolors3 <- colorRampPalette(brewer.pal(9, "RdPu"))(100)

# Open Save Function
png(filename = paste0(folder_plot, "us", pct, "_maps", ".png"),
    width = 1100, height = 700)
#par(mfrow=c(3,2), oma = c(0,0,0,0))
par(mfrow=c(1,6), oma = c(0,0,0,0))


### % Change in Cropland Area ###
# check ranges for plotting
#global(r_us_pct_qland, quantile, probs=c(0.05, 0.95), na.rm=TRUE)
#summary(r_us_pct_qland, 1000000)
terra::plot(r_us %>% subset("pct_QLAND"),
            type = "continuous",
            breaks = c(-25, 5),
            #col = rev(mycolors3), 
            col = brewer.pal(n = 11, name = "RdBu"), 
            main = paste("US % Change in\nCropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")
north(cbind(-121, 29))



### % Change in Crop Index ###
terra::plot(r_us %>% subset("pct_QCROP"),
            type = "continuous",
            breaks = c(-25, 5),
            #col = rev(mycolors3),
            col = brewer.pal(n = 11, name = "RdBu"), 
            main = paste("US % Change in\nCrop Production Index", pct_title),
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Cropland Area ###
terra::plot(r_us %>% subset("new_QLAND")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            
            col = brewer.pal(9, "YlGn"),
            main = paste("US Post-Simulation\nCrop Area", pct_title), 
            )
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Crop Index ###
terra::plot(r_us %>% subset("new_QCROP")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            main = paste("US Post-Simulation\nCrop Index", pct_title), 
            #plg = list(x="bottomright")
            )
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")


### Actual (Raw) Change in Cropland Area ###

# NOTE: the test here is set to the Crop Production INDEX not area! They need the same color scheme, so I set it to the QCROP! 
test <- max(abs(minmax(r_us %>% subset("rawch_QCROP")/1000)))
test_breaks <- seq(-test, 1, length.out = 100)

# max is 24.1, set to 25 for simplicity
#test_breaks <- seq(0, 2, length.out = 100)
#test <- max(abs(minmax(r_us_rawch_qland/1000)))
terra::plot(r_us %>% subset("rawch_QCROP")/1000,
            type = "continuous",
            breaks = test_breaks,
            #col = rev(mycolors3),
            col = brewer.pal(n = 11, name = "RdBu"), 
            main = paste("US Raw Change in\nCropland Area", pct_title),
            )
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")


### Actual (Raw) Change in Crop Production Index ###
terra::plot(r_us %>% subset("rawch_QCROP")/1000,
            type = "continuous",
            breaks = test_breaks,
            col = rev(mycolors3),
            main = paste("US Raw Change in\nCrop Production Index", pct_title),
            )
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")

dev.off()





# 6: Brazil Results ----------------------------

## 6.1 Prep BR Data -----------

# Call fxn to clip, count, and clamp data 
r_br <- F_aoi_prep(shp = shp_br, area_name = "Brazil")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_br, area_name = "Brazil")

## 6.2 Plot Best BR Map ---------

# Open Save Function
png(filename = paste0(folder_plot, "brazil", pct, "_maps", ".png"),
    width = 1000, height = 950)
#par(mfrow=c(3,2), oma = c(0,0,0,0))
par(mfrow=c(1,6), oma = c(0,0,0,0))


### % Change in Cropland Area ###
terra::plot(r_br %>% subset("pct_QLAND"),
            type = "continuous",
            col =  mycolors3,
            breaks = c(seq(0, 4, by = 0.5)),
            main = paste("Brazil % Change in\nCropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
north(cbind(-69, -29))

### % Change in Crop Index ###
terra::plot(r_br %>% subset("pct_QCROP"),
            type = "continuous",
            col = mycolors3, 
            breaks = c(seq(0, 4, by = 0.5)),
            main = paste("Brazil % Change in\nCrop Index", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Cropland Area ###
terra::plot(r_br %>% subset("new_QLAND")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            main = paste("Brazil Post-Simulation\nCrop Area", pct_title), 
            )
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Crop Index ###
terra::plot(r_br %>% subset("new_QCROP")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            main = paste("Brazil Post-Simulation\nCrop Index", pct_title), 
            )
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")


### Actual (Raw) Change in Cropland Area ###
# NOTE: the test here is set to the Crop Production INDEX not area! 
test <- max(abs(minmax(r_br %>% subset("rawch_QCROP")/1000)))
#test_breaks <- seq(0, test, length.out = 100)

# max is 1.84, set to 2 for simplicity
test_breaks <- seq(0, 2, length.out = 100)

#test <- max(abs(minmax(r_br_rawch_qland/1000)))
terra::plot(r_br %>% subset("rawch_QLAND")/1000,
            type = "continuous",
            breaks = test_breaks/4,
            col = mycolors3,
            main = paste("Brazil Raw Change in\nCropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")


### Actual (Raw) Change in Crop Production Index ###
# test <- max(abs(minmax(r_br_rawch_qcrop/1000)))
# test_breaks <- seq(0, test, length.out = 100)
terra::plot(r_br %>% subset("rawch_QCROP")/1000,
            type = "continuous",
            breaks = test_breaks,
            col = mycolors3,
            main = paste("Brazil Raw Change in\nCrop Production Index", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
dev.off()

## 6.3 GGplot BR with tidyterra ----------

# trying with tidyterra
mycolors3 <- colorRampPalette(brewer.pal(9, "RdPu"))(100)

#"atlas", "high_relief", "arid", "soft", "muted", "purple", "viridi", "gn_yl", "pi_y_g", "bl_yl_rd", "deep"
F_ggplot_BR <- function(df, brks, pal, legend_title, p_title){
 p <- ggplot()+
   geom_spatraster(data = df)+
   scale_fill_whitebox_c(
     #palette = "viridi", direction = 1,
     palette = pal, direction = 1,
     breaks = brks
   )+
   geom_spatvector(data = shp_cerr_states, 
                   col = "darkgray",
                   fill = NA, lwd = 0.8, lty = 3)+
   geom_spatvector(data = shp_br_border, 
                   col = "darkgray",
                   fill = NA, lwd = 0.3, lty = 1)+
   labs(
     fill = legend_title,
     title = p_title
   )+
   theme_minimal() +
   annotation_north_arrow(
     which_north = TRUE,
     location ="bl",
     pad_y = unit(0.07, "npc"),
     style = north_arrow_fancy_orienteering()
   ) +
   annotation_scale(pad_y = unit(0.01, "npc"))+
   theme(
     # set plot size and center it 
     plot.title = element_text(size = 16, hjust = 0.5),
     # put legend in the bottom right 
     legend.position = c(0.8, 0.2),
     legend.title = element_text(size = 14),
     legend.text = element_text(size = 10))
 return(p)
}

F_ggplot_BR(df = r_br %>% subset("pct_QLAND"),
            brks = c(seq(0, 4, by = 0.5)),
            pal = "deep",
            legend_title = "% Change",
            p_title = "Brazil % Change in Cropland Area")

### % Change in Cropland Area ###
ggplot()+
  geom_spatraster(data = r_br %>% subset("pct_QLAND"))+
  scale_fill_whitebox_c(
    #palette = "viridi", direction = 1,
    palette = "deep", direction = 1,
    breaks = c(seq(0, 4, by = 0.5))
  )+
  geom_spatvector(data = shp_cerr_states, 
                  col = "darkgray",
                  fill = NA, lwd = 0.8, lty = 3)+
  geom_spatvector(data = shp_br_border, 
                  col = "darkgray",
                  fill = NA, lwd = 0.3, lty = 1)+
  labs(
    fill = "% Change",
    title = "Brazil % Change in Cropland Area"
    )+
  theme_minimal() +
  annotation_north_arrow(
    which_north = TRUE,
    location ="bl",
    pad_y = unit(0.07, "npc"),
    style = north_arrow_fancy_orienteering()
  ) +
  annotation_scale(pad_y = unit(0.01, "npc"))+
  theme(
    # set plot size and center it 
    plot.title = element_text(size = 16, hjust = 0.5),
    # put legend in the bottom right 
    legend.position = c(0.8, 0.2),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10))
  
### % Change in Crop Index ###
ggplot()+
  geom_spatraster(data = r_br %>% subset("pct_QCROP"))+
  scale_fill_whitebox_c(
    palette = "deep", direction = 1,
    breaks = c(seq(0, 4, by = 0.5))
  )+
  geom_spatvector(data = shp_cerr_states, 
                  col = "darkgray",
                  fill = NA, lwd = 0.8, lty = 3)+
  geom_spatvector(data = shp_br_border, 
                  col = "darkgray",
                  fill = NA, lwd = 0.3, lty = 1)+
  labs(
    fill = "% Change",
    title = paste("Brazil % Change in Crop Index", pct_title)
  )+
  theme_minimal()+
  theme(
    # set plot size and center it 
    plot.title = element_text(size = 16, hjust = 0.5),
    # put legend in the bottom right 
    legend.position = c(0.8, 0.2),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10))

### Post-Sim Cropland Area ###
ggplot()+
  geom_spatraster(data = r_br %>% subset("new_QLAND")/1000)+
  scale_fill_whitebox_c(
    palette = "gn_yl", direction = 1,
    #breaks = c(0, 1, 5, 10, 20, 30, 40, 50)
  )+
  geom_spatvector(data = shp_cerr_states, 
                  col = "darkgray",
                  fill = NA, lwd = 0.8, lty = 3)+
  geom_spatvector(data = shp_br_border, 
                  col = "darkgray",
                  fill = NA, lwd = 0.3, lty = 1)+
  labs(
    fill = "Area (1000 ha)",
    title = paste("Brazil Post-Sim Area", pct_title)
  )+
  theme_minimal()+
  theme(
    # set plot size and center it 
    plot.title = element_text(size = 16, hjust = 0.5),
    # put legend in the bottom right 
    legend.position = c(0.8, 0.2),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10))


### Post-Sim Crop Index ###
ggplot()+
  geom_spatraster(data = r_br %>% subset("new_QCROP")/1000)+
  scale_fill_whitebox_c(
    palette = "gn_yl", direction = 1,
    #breaks = c(0, 1, 5, 10, 20, 30, 40, 50)
  )+
  geom_spatvector(data = shp_cerr_states, 
                  col = "darkgray",
                  fill = NA, lwd = 0.8, lty = 3)+
  geom_spatvector(data = shp_br_border, 
                  col = "darkgray",
                  fill = NA, lwd = 0.3, lty = 1)+
  labs(
    fill = "Production (tons CE)",
    title = paste("Brazil Post-Sim Crop Production Index", pct_title)
  )+
  theme_minimal()+
  theme(
    # set plot size and center it 
    plot.title = element_text(size = 16, hjust = 0.5),
    # put legend in the bottom right 
    legend.position = c(0.8, 0.2),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10))

### Actual (Raw) Change in Cropland Area ###
test_breaks <- seq(0, 2, length.out = 100)

#test <- max(abs(minmax(r_br_rawch_qland/1000)))
terra::plot(r_br %>% subset("rawch_QLAND")/1000,
            type = "continuous",
            breaks = test_breaks/4,
            col = mycolors3,
            main = paste("Brazil Raw Change in\nCropland Area", pct_title),
            plg = list(x="bottomright"))
ggplot()+
  geom_spatraster(data = r_br %>% subset("rawch_QLAND")/1000)+
  scale_fill_whitebox_c(
    palette = "gn_yl", direction = 1,
    #breaks = c(0, 1, 5, 10, 20, 30, 40, 50)
  )+
  geom_spatvector(data = shp_cerr_states, 
                  col = "darkgray",
                  fill = NA, lwd = 0.8, lty = 3)+
  geom_spatvector(data = ext_area <- vect(ext(shp)), 
                  col = "darkgray",
                  fill = NA, lwd = 0.3, lty = 1)+
  labs(
    fill = "Production \n(tons CE)",
    title = paste("Brazil Raw Change in\nCropland Area", pct_title)
  )+
  theme_minimal()+
  theme(
    # set plot size and center it 
    plot.title = element_text(size = 16, hjust = 0.5),
    # put legend in the bottom right 
    legend.position = c(0.8, 0.2),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10))

ext_area <- vect(ext(shp_br))
ext_area

### Actual (Raw) Change in Crop Production Index ###




# 7: Cerrado Results ----------------------------

## 7.1 Prep Data -----------
# Call fxn to clip, count, and clamp data 
r_cerr <- F_aoi_prep(shp = shp_cerr, area_name = "Cerrado")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_cerr, area_name = "Cerrado")


## 7.2 Plot Best Cerrado Map ---------

# Open Save Function
png(filename = paste0(folder_plot, "cerrado", pct, "_maps", ".png"),
    width = 1000, height = 950)
#par(mfrow=c(3,2), oma = c(0,0,0,0))
par(mfrow=c(1,6), oma = c(0,0,0,0))


### % Change in Cropland Area ###
terra::plot(r_cerr %>% subset("pct_QLAND"),
            type = "continuous",
            col =  mycolors3,
            breaks = c(seq(0, 4, by = 0.5)),
            
            main = paste("Cerrado % Change in\nCropland Area", pct_title),
            plg = list(x="topleft"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
north(cbind(-59, -22))
sbar(d = 400, type = "bar", xy = "bottomright", divs = 4, below = "km")

### % Change in Crop Index ###
terra::plot(r_cerr %>% subset("pct_QCROP"),
            type = "continuous",
            col =  mycolors3,
            breaks = c(seq(0, 4, by = 0.5)),
            main = paste("Cerrado % Change in\nCrop Index", pct_title),
            plg = list(x="topleft"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Cropland Area ###
terra::plot(r_cerr %>% subset("new_QLAND")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            main = paste("Cerrado Post-Simulation\nCrop Area", pct_title), 
            )
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Crop Index ###
terra::plot(r_cerr %>% subset("new_QCROP")/1000,
            # changed for Cerr; removed 0.1
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            main = paste("Cerrado Post-Simulation\nCrop Index", pct_title), 
            )
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")


### Actual (Raw) Change in Cropland Area ###
test <- max(abs(minmax(r_cerr %>% subset("rawch_QCROP")/1000)))
#test_breaks <- seq(0, test, length.out = 100)

# max is 1.48, set to 2 for simplicity
test_breaks <- seq(0, 2, length.out = 100)

terra::plot(r_cerr %>% subset("rawch_QLAND")/1000,
            type = "continuous",
            breaks = test_breaks/4,
            col = mycolors3,
            main = paste("Cerrado Raw Change in\nCropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Actual (Raw) Change in Crop Production Index ###
test <- max(abs(minmax(r_cerr %>% subset("rawch_QCROP")/1000)))
terra::plot(r_cerr %>% subset("rawch_QCROP")/1000,
            type = "continuous",
            breaks = test_breaks,
            col = mycolors3,
            main = paste("Cerrado Raw Change in\nCrop Production Index", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# END ----------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# FUTURE WORK ----------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Change maps to ggplots using 'tidyterra' (High Priority) -----
### this will probbaly look SO much better than the base R maps and we don't have to
### mess with the margin, the resolution, or the dpi. This will probably lead to
### better code legibility as well. 

### link: https://github.com/dieghernan/tidyterra

## Get 'F_p_violin' to plot in the plotting window as well as saving (Low Priority) ----

## Look into removing outliers (Medium Priority) ----
### link: https://plantarum.ca/2023/02/13/terra-maps/

## Look into rasterVis for Boxplots (Medium Priority) ---- 
### link: https://oscarperpinan.github.io/rastervis/


## Prettier Box Plots (Medium Priority) --------------

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

# 5: Old EDA Plotting Sections --------------
### Create US Functions -------
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

### Plot US EDA --------
F_EDA_us_pct_map(r_us_pct_qland, "% Change in Cropland Area")
F_EDA_us_pct_map(r_us_pct_qcrop, "% Change in Crop Index")

F_EDA_us_new_map(r_us_new_qland, "Post-Sim Values of Cropland Area")
F_EDA_us_new_map(r_us_new_qcrop, "Post-Sim Values of Crop Index")


### Create BR Functions -------
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

### Plot BR EDA --------
F_EDA_br_pct_map(r_br_pct_qland, "% Change in Cropland Area")
F_EDA_br_pct_map(r_br_pct_qcrop, "% Change in Crop Index")

F_EDA_br_new_map(r_br_new_qland, "Post-Sim Values of Cropland Area")
F_EDA_br_new_map(r_br_new_qcrop, "Post-Sim Values of Crop Index")

### Create Cerrado Functions -------
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

### Plot Cerrado EDA --------
F_EDA_cerr_pct_map(r_cerr_pct_qland, "% Change in Cropland Area")
F_EDA_cerr_pct_map(r_cerr_pct_qcrop, "% Change in Crop Index")

F_EDA_cerr_new_map(r_cerr_new_qland, "Post-Sim Values of Cropland Area")
F_EDA_cerr_new_map(r_cerr_new_qcrop, "Post-Sim Values of Crop Index")

# 6: Useful Snippets ----------
#r_us_pct_qland@ptr[["names"]][[1]]
