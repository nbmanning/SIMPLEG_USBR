# Title: processResults_SIMPLEG.R
# Purpose: Read and plot SIMPLE-G results from text format across US, World, BR, and Cerrado

# Initial SIMPLE-G script by: Iman Haqiqi
# Initial date: Aug 2019

# Edited by: Nick Manning 
# Initial edit date: May 2023
# Last edited: Jan 2024

# REQUIRES:
## SIMPLE-G Result files as '.txt' from 'processResults_SIMPLEG_1.R'

# NOTES:
## Create the following folders in your local 'Results' directory:
### 'raster' which houses the results as rasters to pull into a GIS
### 'summary_tables' which houses a table with the stats for each area (min, mean, median, 1st & 3rd Quartiles, max, and NA's)
### 'stat_summary' which houses the raw values for changes in cropland area and production as an R data file to bring into another script   

# Next Steps:
## implement ggplot code with tidyterra instead of terra for better graphics

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Load Libraries & Set Constants ---- 
rm(list = ls())

## Libraries ##
library(tidyverse)
library(raster) # use for initial raster stack and basic plotting
library(terra) # use to wrangle geospatial data and plot
library(RColorBrewer) # use for adding colorblind-friendly color palettes 
library(rasterVis) # use for easy violin plot 
library(reshape2) # use for melting data to then use ggplot
library(sf)
library(tidyterra) # plot using ggplot() instead of base R with 'terra'
library(ggspatial) # N arrow and Scale Bar w tidyterrra
library(rworldmap) # getting simple BR Border

## Constants ##

# NOTE: change this when you change the result file to one of three TXT files
## hi: enter "_hi" ;
## med: enter "_m" ;
## lo: enter "_lo" ;
## out / default; enter ""

### For 2024-02-12 run ###
pct <- "_m" # change when you change 'datafile'
pct_title <- " - Med" # for plotting, either " - High" or " - Low" or "" or "- Med"

folder <- "../Results/SIMPLEG-2024-02-12/"
folder_plot <- "../Figures/021224/"
datafile   <- paste0(folder, "sg1x3x10_v2401_US_Heat", pct, "-out.txt")
folder_der <- "../Data_Derived/20240212/"
folder_stats <- paste0(folder, "stat_summary/")


# ### For 2024-01-30 run ###
# pct <- "_m" # change when you change 'datafile'
# pct_title <- "- Med" # for plotting, either " - High" or " - Low" or "" or "- Med"
# 
# # NOTE: will need to change to local location
# folder <- "../Results/SIMPLEG-2024-01-30/"
# folder_plot <- "../Figures/013024/new"
# datafile   <- paste0(folder, "US_HEAT", pct, "-out.txt")
# #datafile <- "../Results/SIMPLEG-2023-10-29/sg1x3x10_v2310-out.txt"
# folder_der <- "../Data_Derived/20240130/"
# folder_stats <- "../Results/SIMPLEG-2024-01-30/stat_summary/"


### For 2023-10-29 run ###
# pct <- "" # change when you change 'datafile'
# pct_title <- "" # for plotting, either " - High" or " - Low" or "" or "- Med"
# 
# folder <- "../Results/SIMPLEG-2023-10-29/"
# folder_plot <- "../Figures/102923/new"
# datafile   <- paste0(folder, "sg1x3x10_v2310", pct, "-out.txt")
# #datafile <- "../Results/SIMPLEG-2023-10-29/sg1x3x10_v2310-out.txt"
# folder_der <- "../Data_Derived/20231029/"
# folder_stats <- "../Results/SIMPLEG-2024-10-29/stat_summary/"


# CREATE FUNCTIONS --------------

# these functions:
## 1) F_p_violin: Plot EDA (histograms and violin plots)
## 2) F_count_invalid: Count the number of Invalid Grid Cells
## 3) F_clamp: Cap / Clamp the value of a single grid cell at 50,000
## 3) F_aoi_prep: Prep the AOI Rasters by using the Count and Clamp Functions
## 4) F_EDA: Save the Summary tables and use the Violin Plot fxn to create violin plots

# fxn to Create and Save Violin Plots and Basic Histograms 
F_p_violin <- function(df, area){
  
  # histograms (log and normal) as PNG's for all layers in stack  
  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_hist", ".png"))
  terra::hist(df, maxcell=100000000000)
  dev.off()
  
  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_hist_log", ".png"))
  terra::hist(log(df), maxcell=10000000000)
  dev.off()
  
  ## subset and change names ##
  
  # CPI = Crop Production Index
  
  # separate the PERCENT CHANGES layers into their own df's  
  df_pct <- df %>% 
    subset(c("pct_QLAND", "pct_QCROP")) 
  names(df_pct) <- c("Cropland Area % Change", "CPI % Change")
  
  # separate the RAW CHANGES layers into their own df's  
  df_rawch <- df %>% 
    subset(c("rawch_QLAND", "rawch_QCROP"))
  names(df_rawch) <- c("Cropland Area", "CPI")
  
  # separate the NEW VALUES layers into their own df's
  df_new <- df %>% 
    subset(c("new_QLAND", "new_QCROP"))
  names(df_new) <- c("Cropland Area", "CPI")

  # separate each for Maize and Soy
  df_pct_maizesoy <- df %>% 
    subset(c("pct_LND_MAZ", "pct_LND_SOY")) 
  names(df_pct_maizesoy) <- c("Maize", "Soy")
   
  df_rawch_maizesoy <- df %>% 
    subset(c("rawch_LND_MAZ", "rawch_LND_SOY"))
  names(df_rawch_maizesoy) <- c("Maize", "Soy")
  
  df_new_maizesoy <- df %>% 
    subset(c("new_LND_MAZ", "new_LND_SOY"))
  names(df_new_maizesoy) <- c("Maize", "Soy")
  
  # plot the boxplots next to one another (e.g. all the % change boxplots in one section)
  # F_p_violin_save <- function(){  
  #   png(filename = paste0(folder_plot, str_to_lower(area), pct, "_bw", "_pctchange", "_maizesoy", ".png"))
  #   plot(p1)
  #   dev.off()
  # }
  p1 <- bwplot(df_pct, 
               main = paste(area, "% Change", pct_title),
               ylab = "% Change")
  p2 <- bwplot(df_rawch, 
               main = paste(area, "Raw Change", pct_title),
               ylab = "Area (ha) or kg CE")
  p3 <- bwplot(df_new, 
               main = paste(area, "Post-Sim Values", pct_title),
               ylab = "Area (ha) or kg CE")
  
  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_bw", "_pctchange", ".png"))
  plot(p1)
  dev.off()
  
  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_bw", "_rawchange", ".png"))
  plot(p2)
  dev.off()
  
  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_bw", "_newvalues", ".png"))
  plot(p3)
  dev.off()
  
  # plot the same but for maize and soy now 
  p4 <- bwplot(df_pct_maizesoy, 
               main = paste(area, "% Change Maize & Soy", pct_title),
               ylab = "% Change")
  p5 <- bwplot(df_rawch_maizesoy, 
               main = paste(area, "Raw Change Maize & Soy", pct_title),
               ylab = "Area (ha)")
  p6 <- bwplot(df_new_maizesoy, 
               main = paste(area, "Post-Sim Values Maize & Soy", pct_title),
               ylab = "Area (ha)")
  

  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_bw", "_pctchange", "_maizesoy", ".png"))
  plot(p4)
  dev.off()
  
  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_bw", "_pctchange", "_maizesoy", ".png"))
  plot(p5)
  dev.off()
  
  png(filename = paste0(folder_plot, str_to_lower(area), pct, "_bw", "_rawchange", "_maizesoy", ".png"))
  plot(p6)
  dev.off()
  
  return(p1)
  return(p2)
  return(p3)
  return(p4)
  return(p5)
  return(p6)
  
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
### PICK UP HERE ######################################
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
  
  # # Crop Specific
  # F_count_invalid(r_aoi, "new_LND_MAZ")
  # r_aoi_new_maize <- F_clamp(r_aoi, "new_LND_MAZ")
  # 
  # F_count_invalid(r_aoi, "new_LND_SOY")
  # r_aoi_new_soy <- F_clamp(r_aoi, "new_LND_SOY")
  
  # get other layers
  r_aoi_pct_qland <- r_aoi %>% subset("pct_QLAND")
  r_aoi_rawch_qland <- r_aoi %>% subset("rawch_QLAND")
  
  r_aoi_pct_qcrop <- r_aoi %>% subset("pct_QCROP")
  r_aoi_rawch_qcrop <- r_aoi %>% subset("rawch_QCROP")
  
  # r_aoi_pct_maize <- r_aoi %>% subset("pct_LND_MAZ")
  # r_aoi_rawch_maize <- r_aoi %>% subset("rawch_MAZ")
  # 
  # r_aoi_pct_soy <- r_aoi %>% subset("pct_LND_SOY")
  # r_aoi_rawch_soy <- r_aoi %>% subset("rawch_SOY")
  
  # re-stack and re-order
  r_aoi <- c(
    r_aoi_new_qland, r_aoi_pct_qland, r_aoi_rawch_qland,
    r_aoi_new_qcrop, r_aoi_pct_qcrop, r_aoi_rawch_qcrop
    # r_aoi_new_maize, r_aoi_pct_maize, r_aoi_rawch_soy,
    # r_aoi_new_soy, r_aoi_pct_soy, r_aoi_rawch_soy
  )
  
  # save clipped and clamped raster with new AOI 
  saveRDS(r_aoi, file = paste0(folder_der, "r", pct, "_", area_name, ".rds"))
  
  # return as result
  return(r_aoi)
}

# fxn to get summary of data, call the violin fxn, and plot a basic map
F_EDA <- function(r_aoi, area_name){  
  # Get and save a summary table
  table_area <- summary(r_aoi, size = 1000000000) # set size to not use a sample
  print(table_area)
  
  write.csv(table_area, file = paste0(folder, "summary_tables/", "table_", area_name, pct, "_102923", ".csv"))
  print(global(r_aoi$rawch_QLAND, fun = "sum", na.rm = T))
  print(global(r_aoi$rawch_QCROP, fun = "sum", na.rm = T))
  # print(global(r_aoi$rawch_MAZ, fun = "sum", na.rm = T))
  # print(global(r_aoi$rawch_SOY, fun = "sum", na.rm = T))
  
  # Call EDA fxn to get and save violin plots 
  F_p_violin(r_aoi, area_name)
  
  # Plot basic initial maps
  terra::plot(r_aoi, axes = F, type = "interval")
}


# 1: Load SHP & SIMPLE-G Raster -----------------------------------------------------------

### RUN 'processResults_SIMPLEG_1.R' FIRST TO CREATE RASTER AND SHAPEFILES ###

load("../Data_Derived/shp_usbr.RData")
r <- readRDS(file = paste0(folder_der, "r", pct, ".rds"))
# r <- readRDS(file = paste0(folder_der, "r_maizesoy", pct, ".rds"))

# print cropland area in ha by getting the sum of each grid-cell value
print(global(r$new_QLAND, fun = "sum", na.rm = T))

# 2: Edit Stack & Check Values -------------------

## 2.1: Calc & Add Raw Change from % and New -------
# Formula: new - (new / ((pct_change/100)+1))

# subset 
r_pct_qland <- subset(r, "pct_QLAND")
r_new_qland <- subset(r, "new_QLAND")

r_pct_qcrop <- subset(r, "pct_QCROP")
r_new_qcrop <- subset(r, "new_QCROP")

# r_pct_maize <- subset(r, "pct_LND_MAZ")
# r_new_maize <- subset(r, "new_LND_MAZ")
# 
# r_pct_soy <- subset(r, "pct_LND_SOY")
# r_new_soy <- subset(r, "new_LND_soy")

# NOTE: rawch = Raw Change
r_rawch_qcrop <- r_new_qcrop - (r_new_qcrop / ((r_pct_qcrop/100)+1))
r_rawch_qland <- r_new_qland - (r_new_qland / ((r_pct_qland/100)+1))

# r_rawch_maize <- r_new_maize - (r_new_maize / ((r_pct_maize/100)+1))
# r_rawch_soy <- r_new_soy - (r_new_soy / ((r_pct_soy/100)+1))

r <- c(
  r, 
  r_rawch_qcrop, r_rawch_qland#,
  #r_rawch_maize, r_rawch_soy
  )

r

# set names 
names(r)
names(r) <- c(
  "pct_QLAND", 
  "new_QLAND", 
  "pct_QCROP", 
  "new_QCROP",
  #"pct_LND_MAZ", 
  #"pct_LND_SOY", 
  #"new_LND_MAZ", 
  #"new_LND_SOY"
  
  "rawch_QCROP", 
  "rawch_QLAND"
  # 
  # "rawch_MAZ",
  # "rawch_SOY"
)

## 2.2: Count New Land Values ----- 

# get and replace values that were over 50,000 ha per grid cell (>50,000 ha per grid cell is impossible)
# link: https://gis.stackexchange.com/questions/421821/how-to-subset-a-spatraster-by-value-in-r

# count cells over 50,000 -- Can't just count NA from MR because others 
stat_newland_over50k <- ifel(r_new_qland > 50000, 999999, r_new_qland)
stat_newland_over50k <- ncell(stat_newland_over50k[stat_newland_over50k==999999])
print(stat_newland_over50k)

## 2.3: World Results ---------

# Quick plot of World results
terra::plot(r, axes = F)
terra::plot(log(r), axes = F)

# Call fxn to clip, count, and clamp data 
r_row <- F_aoi_prep(shp = shp_world, area_name = "World")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_row, area_name = "World")


## 2.4 World Results with 'terra' -------

# Set colors 
# mycolors <- colorRampPalette(brewer.pal(9, "RdPu"))(100)

# open saving function

# old; used to use PNG
# png(filename = paste0(folder_plot, "world", pct, "_maps", ".png"),
#     width = 1800, height = 1200)
#par(mfrow=c(3,2), oma = c(0,0,0,0))

# current; use PDF then convert after
pdf(file = paste0(folder_plot, "world", pct, "_maps", ".pdf"),
    width = 18, height = 18
    )
par(mfrow=c(2,2), mar = c(0, 0.1, 0, 0.1))

### Post-Sim Cropland Area ####################
terra::plot(r_row %>% subset("new_QLAND")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            
            col = brewer.pal(9, "YlGn"),
            main = paste("Post-Simulation Crop Area", pct_title),
            plg=list( # parameters for drawing legend
              title = "Area (1000 ha) / Grid Cell",
              #title.cex = 2, # Legend title size
              #cex = 2 # Legend text size
              x = "bottomleft"
            )
)
#lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")



### Actual (Raw) Change in Cropland Area ####################
terra::plot(r_row %>% subset("rawch_QLAND")/1000,
            type = "interval",
            #breaks = c(-50000, -25000, -10000, -1000, -100, 0, 1, 10, 100, 500, 1000),
            breaks = c(-50, -25, -10, -1, -0.1, 0, 0.01, 0.1, 0.25, 0.5, 1),
            #breaks = c(-50, -25, -10, -1, -0.1, 0, 0.01, 0.25, 0.5, 1, 2, 5),
            #col = rev(mycolors),
            col = brewer.pal(n = 11, name = "RdBu"),
            
            main = paste("Raw Change in Cropland Area", pct_title),
            plg=list( # parameters for drawing legend
              title = "Area (1000 ha) / Grid Cell",
              #title.cex = 2, # Legend title size
              #cex = 2 # Legend text size
              x = "bottomleft"
              )
)

#lines(vect(ext(shp_us_mw)), lwd = 0.8, lty = 1, col = "black")
#lines(vect(ext(shp_cerr)), lwd = 0.8, lty = 1, col = "black")

### Post-Sim Crop Index ####################
terra::plot(r_row %>% subset("new_QCROP")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            main = paste("Post-Simulation Crop Index", pct_title), 
            plg=list( # parameters for drawing legend
              title = "Tons CE / Grid Cell",
              x = "bottomleft"
            )
)
lines(shp_world, col = "gray80")
#lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")

### Actual (Raw) Change in Crop Production Index ####################
terra::plot(r_row %>% subset("rawch_QCROP")/1000,
            type = "interval",
            breaks = c(-50, -25, -10, -1, -0.1, 0, 0.01, 0.25, 0.5, 1, 2, 5),
            #col = rev(mycolors),
            col = brewer.pal(n = 11, name = "RdBu"), 
            main = paste("Raw Change in Crop Production Index", pct_title),
            plg=list( # parameters for drawing legend
              title = "Tons CE / Grid Cell",
              #title.cex = 2, # Legend title size
              #cex = 2 # Legend text size
              x = "bottomleft"
            )
)
#lines(vect(ext(shp_us_mw)), lwd = 0.8, lty = 1, col = "black")
#lines(vect(ext(shp_cerr)), lwd = 0.8, lty = 1, col = "black")

dev.off()

# TO-DO #
### WORLD MAIZE MAP ####################
### WORLD SOY MAP ####################

# 3: US Results  ----------------------------
## 3.1 Prep Data -------

# Call fxn to clip, count, and clamp data 
r_us <- F_aoi_prep(shp = shp_us, area_name = "US")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_us, area_name = "US")


## 3.2 Plot Best US Map ---------

### Create mycolors ###
# set new continuous color scheme
mycolors <- colorRampPalette(brewer.pal(9, "RdPu"))(100)


pdf(file = paste0(folder_plot, "us", pct, "_maps", ".pdf"),
    width = 18, height = 18
)
par(mfrow=c(2,2), mar = c(0, 0.1, 0, 0.1))


### Post-Sim Cropland Area ####################
terra::plot(r_us %>% subset("new_QLAND")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            
            col = brewer.pal(9, "YlGn"),
            main = paste("US Post-Simulation\nCrop Area", pct_title),
            plg=list( # parameters for drawing legend
              title = "Area (kha)",
              #title.cex = 2, # Legend title size
              #cex = 2 # Legend text size
              x = "bottomright"
            )
)
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")



### Actual (Raw) Change in Cropland Area ####################

# NOTE: the test here is set to the Crop Production INDEX not area! They need the same color scheme, so I set it to the QCROP! 
test <- max(abs(minmax(r_us %>% subset("rawch_QCROP")/1000)))
test_breaks <- seq(-test, 1, length.out = 100)

# max is 24.1, set to 25 for simplicity
#test_breaks <- seq(0, 2, length.out = 100)
#test <- max(abs(minmax(r_us_rawch_qland/1000)))
terra::plot(r_us %>% subset("rawch_QCROP")/1000,
            type = "continuous",
            breaks = test_breaks,
            col = rev(mycolors),
            #col = brewer.pal(n = 11, name = "RdBu"), 
            main = paste("US Raw Change in\nCropland Area", pct_title),
            plg=list( # parameters for drawing legend
              title = "Area (kha)",
              #title.cex = 2, # Legend title size
              #cex = 2 # Legend text size
              x = "bottomright"
            )
)
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")



### Post-Sim Crop Index ####################
terra::plot(r_us %>% subset("new_QCROP")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            main = paste("US Post-Simulation\nCrop Index", pct_title), 
            #plg = list(x="bottomright")
            plg=list( # parameters for drawing legend
              title = "Tons CE",
              #title.cex = 2, # Legend title size
              #cex = 2 # Legend text size
              x = "bottomright"
            )
)
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")


### Actual (Raw) Change in Crop Production Index ####################
terra::plot(r_us %>% subset("rawch_QCROP")/1000,
            type = "continuous",
            breaks = test_breaks,
            col = rev(mycolors),
            main = paste("US Raw Change in\nCrop Production Index", pct_title),
            plg=list( # parameters for drawing legend
              title = "Tons CE",
              #title.cex = 2, # Legend title size
              #cex = 2 # Legend text size
              x = "bottomright"
            )
)
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")

dev.off()

### Maize Map ####################
### Soy Map ####################



# 4: Brazil Results ----------------------------

## 4.1 Prep BR Data -----------

# Call fxn to clip, count, and clamp data 
r_br <- F_aoi_prep(shp = shp_br, area_name = "Brazil")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_br, area_name = "Brazil")

## 4.2 Plot Best BR Map ---------

pdf(file = paste0(folder_plot, "brazil", pct, "_maps", ".pdf"),
    width = 18, height = 18
)
#par(mfrow=c(3,2), oma = c(0,0,0,0))
par(mfrow=c(2,2), mar = c(0, 0.1, 0, 0.1))

### Post-Sim Cropland Area ####################
terra::plot(r_br %>% subset("new_QLAND")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            main = paste("Brazil Post-Simulation\nCrop Area", pct_title),
            plg=list(
              title = "Area (kha)",
              x = "bottomright"
            ))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Actual (Raw) Change in Cropland Area ####################
# NOTE: the test here is set to the Crop Production INDEX not area! 
test <- max(abs(minmax(r_br %>% subset("rawch_QCROP")/1000)))
#test_breaks <- seq(0, test, length.out = 100)

# max is 1.84, set to 2 for simplicity
test_breaks <- seq(0, 2, length.out = 100)

#test <- max(abs(minmax(r_br_rawch_qland/1000)))
terra::plot(r_br %>% subset("rawch_QLAND")/1000,
            type = "continuous",
            breaks = test_breaks/4,
            col = mycolors,
            main = paste("Brazil Raw Change in\nCropland Area", pct_title),
            plg=list(
              title = "Area (kha)",
              x = "bottomright"
            ))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Crop Index ####################
terra::plot(r_br %>% subset("new_QCROP")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            main = paste("Brazil Post-Simulation\nCrop Index", pct_title), 
            plg=list(
              title = "Tons CE",
              x = "bottomright"
            ))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")



### Actual (Raw) Change in Crop Production Index ####################
# test <- max(abs(minmax(r_br_rawch_qcrop/1000)))
# test_breaks <- seq(0, test, length.out = 100)
terra::plot(r_br %>% subset("rawch_QCROP")/1000,
            type = "continuous",
            breaks = test_breaks,
            col = mycolors,
            main = paste("Brazil Raw Change in\nCrop Production Index", pct_title),
            plg=list(
              title = "Tons CE",
              x = "bottomright"
            ))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
dev.off()


### Maize Map ####################
### Soy Map ####################



# 5: Cerrado Results ----------------------------

## 5.1 Prep Data -----------
# Call fxn to clip, count, and clamp data 
r_cerr <- F_aoi_prep(shp = shp_cerr, area_name = "Cerrado")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_cerr, area_name = "Cerrado")


## 5.2 Plot Best Cerrado Map ---------

pdf(file = paste0(folder_plot, "cerr", pct, "_maps", ".pdf"),
    width = 15, height = 15
)
par(mfrow=c(2,2), mar = c(0.4, 0.8, 0.4, 0.8))

### Post-Sim Cropland Area ####################
terra::plot(r_cerr %>% subset("new_QLAND")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            main = paste("Cerrado Post-Simulation Crop Area", pct_title),
            plg=list(
              title = "Area (kha)",
              x = "bottomright"
            ))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Actual (Raw) Change in Cropland Area ####################
test <- max(abs(minmax(r_cerr %>% subset("rawch_QCROP")/1000)))
#test_breaks <- seq(0, test, length.out = 100)

# max is 1.48, set to 2 for simplicity
test_breaks <- seq(0, 2, length.out = 100)

terra::plot(r_cerr %>% subset("rawch_QLAND")/1000,
            type = "continuous",
            breaks = test_breaks/4,
            col = mycolors,
            main = paste("Cerrado Raw Change in Cropland Area", pct_title),
            plg=list(
              title = "Area (kha)",
              x = "bottomright"
            ))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Crop Index ####################
terra::plot(r_cerr %>% subset("new_QCROP")/1000,
            # changed for Cerr; removed 0.1
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            main = paste("Cerrado Post-Simulation Crop Index", pct_title), 
            plg=list(
              title = "Tons CE",
              x = "bottomright"
            ))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Actual (Raw) Change in Crop Production Index ####################
test <- max(abs(minmax(r_cerr %>% subset("rawch_QCROP")/1000)))
terra::plot(r_cerr %>% subset("rawch_QCROP")/1000,
            type = "continuous",
            breaks = test_breaks,
            col = mycolors,
            main = paste("Cerrado Raw Change in Crop Production Index", pct_title),
            plg=list(
              title = "Tons CE",
              x = "bottomright"
            ))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

dev.off()


### Maize Map ####################
### Soy Map ####################



# 6: STATS --------------------------
# Function to get the total changes
F_sum <- function(df, layer){
  test2 <- df %>% subset(layer)
  global(test2, fun = "sum", na.rm = T)[1,]
}

# Function to create df of changes and calculate "PRE" values
F_area_stats <- function(df, extent_text){
  # create column of labels for easier recall
  labels <- c(
    "new_cropland_area", 
    "raw_change_cropland_area",
    "new_crop_production", 
    "raw_change_crop_production"
    
    # "new_soy_area",
    # "raw_change_soy_area",
    # 
    # "new_maize_area",
    # "raw_change_maize_area"
    )
  
  # get layers from input df
  values <- c(
    F_sum(df, "new_QLAND"),
    F_sum(df, "rawch_QLAND"),
    F_sum(df, "new_QCROP"),
    F_sum(df, "rawch_QCROP")
    
    # F_sum(df, "new_SOY"),
    # F_sum(df, "rawch_SOY")
    # F_sum(df, "new_MAZ"),
    # F_sum(df, "rawch_MAZ")
    
  )
  
  # create data frame from the other layers and their labels 
  df2 <- data.frame(labels, values)
  
  # pivot wide to create 'pre' data, then pivot long to make tidy
  df2 <- df2 %>% 
    pivot_wider(names_from = "labels", values_from = "values") %>% 
    mutate(extent = extent_text,
           pre_cropland_area = new_cropland_area - raw_change_cropland_area,
           pre_crop_production = new_crop_production - raw_change_crop_production) %>% 
    pivot_longer(cols = -extent, names_to = "labels", values_to = "values")
}

## 6.1: Get Cropland Area and Crop Production Stats ------
stat_SG_QLAND_QCROP_US <- F_area_stats(r_us, "US")  
stat_SG_QLAND_QCROP_BR <- F_area_stats(r_br, "Brazil")  
stat_SG_QLAND_QCROP_Cerrado <- F_area_stats(r_cerr, "Cerrado")  

stat_SG_summary <- rbind(stat_SG_QLAND_QCROP_US, stat_SG_QLAND_QCROP_BR, stat_SG_QLAND_QCROP_Cerrado)

## 6.2: Save SIMPLE-G Stats ------- 
save(
  stat_SG_QLAND_QCROP_US, stat_SG_QLAND_QCROP_BR, stat_SG_QLAND_QCROP_Cerrado, stat_SG_summary,
  file = paste0(folder_stats, "sg_QLAND_QCROP_US_BR_Cerr.RData")
)

write.csv(stat_SG_summary, 
          file = paste0(folder_stats, "sg", pct, "_stat_summary_US_BR_Cerr.csv"),
          row.names = F)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# END ----------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# FUTURE WORK ----------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Clamp values to 0 to remove US influence on summary stats ----

# Set any negative US Values to NA
r_row_noUS <- r_row %>%
  terra::clamp(lower = 0, values = F)

# Get Boxplots without US values
F_p_violin(r_row_noUS, "World (Positive)")


# Set negative US Values to 0 for plotting
r_row_US0 <- r_row %>%
  terra::clamp(lower = 0, values = T)

summary(r_row_noUS, size = 1000000000000000000)

table_noUS <- summary(r_row_noUS, size = 1000000000) # set size to not use a sample
print(table_noUS)

write.csv(table_noUS, file = paste0(folder, "/summary_tables/table_World_noUS", pct, "_102923", ".csv"))
print(global(r_row_noUS$rawch_QLAND, fun = "sum", na.rm = T))
print(global(r_row_noUS$rawch_QCROP, fun = "sum", na.rm = T))

### 4.3.3 Plot World w Clamped US --------

pdf(file = paste0(folder_plot, "world_US0", pct, "_maps", ".pdf"),
    width = 18, height = 18
)

par(mfrow=c(2,2), mar = c(0, 0.1, 0, 0.1))

### Post-Sim Cropland Area ###
terra::plot(r_row_US0 %>% subset("new_QLAND")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            
            col = brewer.pal(9, "YlGn"),
            main = paste("Post-Simulation Crop Area", pct_title),
            plg=list( # parameters for drawing legend
              title = "Area (1000 ha) / Grid Cell",
              #title.cex = 2, # Legend title size
              #cex = 2 # Legend text size
              x = "bottomleft"
            )
)
#lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")



### Actual (Raw) Change in Cropland Area ###
terra::plot(r_row_US0 %>% subset("rawch_QLAND")/1000,
            type = "interval",
            #breaks = c(-50000, -25000, -10000, -1000, -100, 0, 1, 10, 100, 500, 1000),
            breaks = c(0, 0.001, 0.01, 0.1, 0.5, 1, 2, 5, 10, 20),
            col = mycolors,
            #breaks = c(-50, -25, -10, -1, -0.1, 0, 0.01, 0.1, 0.25, 0.5, 1),
            #col = brewer.pal(n = 11, name = "RdBu"),
            
            main = paste("Raw Change in Cropland Area", pct_title),
            plg=list( # parameters for drawing legend
              title = "Area (1000 ha) / Grid Cell",
              #title.cex = 2, # Legend title size
              #cex = 2 # Legend text size
              x = "bottomleft"
            )
)

#lines(vect(ext(shp_us_mw)), lwd = 0.8, lty = 1, col = "black")
#lines(vect(ext(shp_cerr)), lwd = 0.8, lty = 1, col = "black")

### Post-Sim Crop Index ###
terra::plot(r_row_US0 %>% subset("new_QCROP")/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 20, 25, 30, 35, 45, 50),
            col = brewer.pal(9, "YlGn"),
            main = paste("Post-Simulation Crop Index", pct_title), 
            plg=list( # parameters for drawing legend
              title = "Tons CE / Grid Cell",
              x = "bottomleft"
            )
)
#lines(shp_world, col = "gray80")

### Actual (Raw) Change in Crop Production Index ###
terra::plot(r_row_US0 %>% subset("rawch_QCROP")/1000,
            type = "interval",
            
            # breaks = c(-50, -25, -10, -1, -0.1, 0, 0.01, 0.25, 0.5, 1, 2, 5),
            # col = brewer.pal(n = 11, name = "RdBu"), 
            
            breaks = c(0, 0.001, 0.01, 0.1, 0.5, 1, 2, 5, 10, 20),
            col = mycolors,
            main = paste("Raw Change in Crop Production Index", pct_title),
            plg=list( # parameters for drawing legend
              title = "Tons CE / Grid Cell",
              #title.cex = 2, # Legend title size
              #cex = 2 # Legend text size
              x = "bottomleft"
            )
)
#lines(vect(ext(shp_us_mw)), lwd = 0.8, lty = 1, col = "black")
#lines(vect(ext(shp_cerr)), lwd = 0.8, lty = 1, col = "black")

dev.off()

# # plot clamped
# terra::plot(
#   #r_row %>% subset("rawch_QCROP")/1000,
#   r_row_rc_QCROP/1000,
#   type = "interval",
#   breaks = c(0, 0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4),
#   #col = rev(mycolors),
#   col = brewer.pal(n = 9, name = "YlOrRd"), 
#   main = paste("Raw Change in Crop Production Index", pct_title),
#   plg=list( # parameters for drawing legend
#     title = "Tons CE / Grid Cell",
#     #title.cex = 2, # Legend title size
#     #cex = 2 # Legend text size
#     x = "bottomleft"
#   )
# )



## Map with GGplot -------------
### 4.3.1 World Results with tidyterra ----------

#"atlas", "high_relief", "arid", "soft", "muted", "purple", "viridi", "gn_yl", "pi_y_g", "bl_yl_rd", "deep"
F_ggplot_world <- function(df, brks, pal, legend_title, p_title){
  
  p <- ggplot()+
    geom_spatraster(data = df, maxcell = Inf)+
    scale_fill_whitebox_c(
      #palette = "viridi", direction = 1,
      palette = pal,
      breaks = brks
    )+
    labs(
      fill = legend_title,
      title = p_title
    )+
    theme_minimal() +
    theme(
      # set plot size and center it 
      plot.title = element_text(size = 16, hjust = 0.5),
      # put legend in the bottom right 
      legend.position = c(0.15, 0.2),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 10))
  return(p)
}




### Post-Sim Cropland Area ### - DONE
# takes a while
F_ggplot_world(df = r_row %>% subset("new_QLAND")/1000,
               brks = waiver(),
               pal = "gn_yl", 
               legend_title = "Area (1000 ha)",
               p_title = paste("Post-Sim Cropland Area", pct_title))

### Post-Sim Crop Index ### - DONE
F_ggplot_world(df = r_row %>% subset("new_QCROP")/1000,
               brks = waiver(),
               pal = "gn_yl", 
               legend_title = "Tons CE",
               p_title = paste("Post-Sim Crop Production Index", pct_title))

### Actual (Raw) Change in Cropland Area ### - NOT DONE!!!!
F_ggplot_world(df = r_row %>% subset("rawch_QLAND")/1000,
               brks = c(-50, -25, -10, -1, -0.1, 0, 0.01, 0.25, 0.5, 1, 2, 5),
               pal = "viridi", 
               legend_title = "Area (1000 ha)",
               p_title = paste("Raw Change in Cropland Area", pct_title))

test <- r_row %>% 
  subset("rawch_QLAND")/1000 %>% 
  raster::clamp(lower = 0, values = T)

test

range(test)


tes[pb1 < 0] <- NA


ggplot()+
  geom_spatraster(data = 
                    r_row %>% 
                    subset("rawch_QLAND")/1000 %>% 
                    clamp(lower = 0, values = F), 
                  maxcell = Inf)+
  scale_fill_whitebox_b(
    #palette = "viridi", direction = 1,
    #"atlas", "high_relief", "arid", "soft", "muted", 
    #"purple", "viridi", "gn_yl", "pi_y_g", "bl_yl_rd", "deep"
    palette = "bl_yl_rd", #direction = 1,
    #breaks = c(-25, -10, -1, 0, 0.1, 0.5, 1)
    #breaks = c(-50, -25, -10, -1, -0.1, 0, 0.01, 0.25, 0.5, 1, 2, 5)
  )+
  labs(
    fill = "Area (1000 ha)",
    title = paste("Raw Change in Cropland Area", pct_title)
  )+
  theme_minimal() +
  theme(
    # set plot size and center it 
    plot.title = element_text(size = 16, hjust = 0.5),
    # put legend in the bottom right 
    legend.position = c(0.15, 0.2),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10))


### Actual (Raw) Change in Crop Production Index ###
F_ggplot_world(df = r_row %>% subset("rawch_QCROP")/1000,
               brks = c(-50, -25, -10, -1, -0.1, 0, 0.01, 0.25, 0.5, 1, 2, 5),
               pal = "gn_yl", 
               legend_title = "Tons CE",
               p_title = paste("Raw Change in Crop Production Index", pct_title))

# NEXT: make it so the values are actually binned 
ggplot()+
  geom_spatraster(data = r_row %>% subset("rawch_QCROP")/1000, maxcell = Inf)+
  scale_fill_whitebox_b(
    #palette = "viridi", direction = 1,
    palette = "gn_yl", direction = 1,
    breaks = c(-50, -25, -10, -1, -0.1, 0, 0.01, 0.25, 0.5, 1, 2, 5)
  )+
  labs(
    fill = legend_title,
    title = p_title
  )+
  theme_minimal() +
  theme(
    # set plot size and center it 
    plot.title = element_text(size = 16, hjust = 0.5),
    # put legend in the bottom right 
    legend.position = c(0.15, 0.2),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10))

## Change maps to ggplots using 'tidyterra' (High Priority) -----
### this will probbaly look SO much better than the base R maps and we don't have to
### mess with the margin, the resolution, or the dpi. This will probably lead to
### better code legibility as well. 

### link: https://github.com/dieghernan/tidyterra

## Change to GGplot BR Maps with tidyterra (High Priority) ----------
# 
# # trying with tidyterra
# mycolors <- colorRampPalette(brewer.pal(9, "RdPu"))(100)
# 
# #"atlas", "high_relief", "arid", "soft", "muted", "purple", "viridi", "gn_yl", "pi_y_g", "bl_yl_rd", "deep"
# F_ggplot_BR <- function(df, brks, pal, legend_title, p_title){
#  p <- ggplot()+
#    geom_spatraster(data = df)+
#    scale_fill_whitebox_c(
#      #palette = "viridi", direction = 1,
#      palette = pal, direction = 1,
#      breaks = brks
#    )+
#    geom_spatvector(data = shp_cerr_states, 
#                    col = "darkgray",
#                    fill = NA, lwd = 0.8, lty = 3)+
#    geom_spatvector(data = shp_br_border, 
#                    col = "darkgray",
#                    fill = NA, lwd = 0.3, lty = 1)+
#    labs(
#      fill = legend_title,
#      title = p_title
#    )+
#    theme_minimal() +
#    theme(
#      # set plot size and center it 
#      plot.title = element_text(size = 16, hjust = 0.5),
#      # put legend in the bottom right 
#      legend.position = c(0.8, 0.2),
#      legend.title = element_text(size = 14),
#      legend.text = element_text(size = 10))
#  return(p)
# }
# 
# 
# 
# ### % Change in Cropland Area ###
# p_BR <- F_ggplot_BR(df = r_br %>% subset("pct_QLAND"),
#                     brks = c(seq(0, 4, by = 0.5)),
#                     pal = "deep",
#                     legend_title = "% Change",
#                     p_title = paste("Brazil % Change in Cropland Area", pct_title))
# # add N arrow and scale bar to this one only
# p_BR +    
#   annotation_north_arrow(
#     which_north = TRUE,
#     location ="bl",
#     pad_y = unit(0.07, "npc"),
#     style = north_arrow_fancy_orienteering()
#   )+
#   annotation_scale(pad_y = unit(0.01, "npc"))  
# 
# ### % Change in Crop Index ###
# F_ggplot_BR(df = r_br %>% subset("pct_QCROP"),
#             brks = c(seq(0, 4, by = 0.5)),
#             pal = "deep",
#             legend_title = "% Change",
#             p_title = paste("Brazil % Change in Crop Index", pct_title))
# 
# 
# ### Post-Sim Cropland Area ###
# F_ggplot_BR(df = r_br %>% subset("new_QLAND")/1000,
#             brks = waiver(),
#             pal = "gn_yl", 
#             legend_title = "Area (1000 ha)",
#             p_title = paste("Brazil Post-Sim Cropland Area", pct_title))
# 
# ### Post-Sim Crop Index ###
# F_ggplot_BR(df = r_br %>% subset("new_QCROP")/1000,
#             brks = waiver(),
#             pal = "gn_yl", 
#             legend_title = "Production (tons CE)",
#             p_title = paste("Brazil Post-Sim Crop Production Index", pct_title))
# 
# ### Actual (Raw) Change in Cropland Area ###
# F_ggplot_BR(df = r_br %>% subset("rawch_QLAND")/1000,
#             brks = round(seq(0, 2, length.out = 11),2),
#             pal = "gn_yl", 
#             legend_title = "Area (1000 ha)",
#             p_title = paste("Brazil Raw Change in\nCropland Area", pct_title))
# 
# ### Actual (Raw) Change in Crop Production Index ###
# F_ggplot_BR(df = r_br %>% subset("rawch_QCROP")/1000,
#             brks = round(seq(0, 2, length.out = 11),2),
#             pal = "gn_yl", 
#             legend_title = "Change in CPI (1000 ha)",
#             p_title = paste("Brazil Raw Change in\nCrop Production Index", pct_title))


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

# 5: Plot as PNG instead of PDF -------------------------
### US OLD (OMITTED) ---------
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
            #col = rev(mycolors), 
            col = brewer.pal(n = 11, name = "RdBu"), 
            main = paste("US % Change in\nCropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")
north(cbind(-121, 29))



### % Change in Crop Index ###
terra::plot(r_us %>% subset("pct_QCROP"),
            type = "continuous",
            breaks = c(-25, 5),
            #col = rev(mycolors),
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
            #col = rev(mycolors),
            col = brewer.pal(n = 11, name = "RdBu"), 
            main = paste("US Raw Change in\nCropland Area", pct_title),
)
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")


### Actual (Raw) Change in Crop Production Index ###
terra::plot(r_us %>% subset("rawch_QCROP")/1000,
            type = "continuous",
            breaks = test_breaks,
            col = rev(mycolors),
            main = paste("US Raw Change in\nCrop Production Index", pct_title),
)
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")

dev.off()

### BR OLD PNG ----------
# Open Save Function
png(filename = paste0(folder_plot, "brazil", pct, "_maps", ".png"),
    width = 1000, height = 950)
#par(mfrow=c(3,2), oma = c(0,0,0,0))
par(mfrow=c(1,6), oma = c(0,0,0,0))


### % Change in Cropland Area ###
terra::plot(r_br %>% subset("pct_QLAND"),
            type = "continuous",
            col =  mycolors,
            breaks = c(seq(0, 4, by = 0.5)),
            main = paste("Brazil % Change in\nCropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
north(cbind(-69, -29))

### % Change in Crop Index ###
terra::plot(r_br %>% subset("pct_QCROP"),
            type = "continuous",
            col = mycolors, 
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
            col = mycolors,
            main = paste("Brazil Raw Change in\nCropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")


### Actual (Raw) Change in Crop Production Index ###
# test <- max(abs(minmax(r_br_rawch_qcrop/1000)))
# test_breaks <- seq(0, test, length.out = 100)
terra::plot(r_br %>% subset("rawch_QCROP")/1000,
            type = "continuous",
            breaks = test_breaks,
            col = mycolors,
            main = paste("Brazil Raw Change in\nCrop Production Index", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
dev.off()


### CERRADO OLD PNG ------------
# Open Save Function
png(filename = paste0(folder_plot, "cerrado", pct, "_maps", ".png"),
    width = 1000, height = 950)
#par(mfrow=c(3,2), oma = c(0,0,0,0))
par(mfrow=c(1,6), oma = c(0,0,0,0))


### % Change in Cropland Area ###
terra::plot(r_cerr %>% subset("pct_QLAND"),
            type = "continuous",
            col =  mycolors,
            breaks = c(seq(0, 4, by = 0.5)),
            
            main = paste("Cerrado % Change in\nCropland Area", pct_title),
            plg = list(x="topleft"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
north(cbind(-59, -22))
sbar(d = 400, type = "bar", xy = "bottomright", divs = 4, below = "km")

### % Change in Crop Index ###
terra::plot(r_cerr %>% subset("pct_QCROP"),
            type = "continuous",
            col =  mycolors,
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
            col = mycolors,
            main = paste("Cerrado Raw Change in\nCropland Area", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Actual (Raw) Change in Crop Production Index ###
test <- max(abs(minmax(r_cerr %>% subset("rawch_QCROP")/1000)))
terra::plot(r_cerr %>% subset("rawch_QCROP")/1000,
            type = "continuous",
            breaks = test_breaks,
            col = mycolors,
            main = paste("Cerrado Raw Change in\nCrop Production Index", pct_title),
            plg = list(x="bottomright"))
lines(shp_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

dev.off()
# 6: Old EDA Plotting Sections --------------
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

# 7: Useful Snippets ----------
#r_us_pct_qland@ptr[["names"]][[1]]
