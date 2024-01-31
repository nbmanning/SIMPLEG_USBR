# Title: processResults_SIMPLEG.R

# Purpose: Run this script FIRST to get SIMPLE-G results from .txt into raster and 
# import all of the necessary source shapefiles

# Initial SIMPLE-G script by: Iman Haqiqi
# Initial date: Aug 2019

# Edited by: Nick Manning 
# Initial edit date: May 2023
# Last edited: Jan 2024

# REQUIRES:
## SIMPLE-G Result files as '.txt' 

# NOTES:
## Create a folder named 'raster' in your local directory before running

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Load Libraries & Set Constants ---- 
rm(list = ls())

## Libraries ##
library(tidyverse)
library(raster) # use for initial raster stack and basic plotting
library(terra) # use to wrangle geospatial data and plot
library(geobr) # use to load BR & Cerrado extent shapefiles
library(tigris) # use to load US and US-MW shapefiles
library(sf)
library(stringr) # use to manipulate result .txt file
library(rworldmap) # getting simple BR Border

## Constants ##

# NOTE: change this when you change the result file to one of three TXT files
## hi: enter "_hi" ;
## med: enter "_m" ;
## lo: enter "_lo" ;
## out / default; enter ""

### For 2024-01-30 run ###
pct <- "_m" # change when you change 'datafile'
pct_title <- "- Med" # for plotting, either " - High" or " - Low" or "" or "- Med"

# NOTE: will need to change to local location
folder <- "../Results/SIMPLEG-2024-01-30/"
folder_plot <- "../Figures/013024/new"
datafile   <- paste0(folder, "US_HEAT", pct, "-out.txt")
#datafile <- "../Results/SIMPLEG-2023-10-29/sg1x3x10_v2310-out.txt"
folder_der <- "../Data_Derived/20240130/"
folder_stats <- "../Results/SIMPLEG-2024-01-30/stat_summary/"

### For 2023-10-29 run ###
# pct <- "" # change when you change 'datafile'
# pct_title <- "" # for plotting, either " - High" or " - Low" or "" or "- Med"

# folder <- "../Results/SIMPLEG-2023-10-29/"
# folder_plot <- "../Figures/102923/new"
# datafile   <- paste0(folder, "sg1x3x10_v2310", pct, "-out.txt")
# #datafile <- "../Results/SIMPLEG-2023-10-29/sg1x3x10_v2310-out.txt"
# folder_der <- "../Data_Derived/20231029/"
# folder_stats <- "../Results/SIMPLEG-2024-10-29/stat_summary/"

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

saveRDS(r, file = paste0(folder_der, "r", pct, ".rds"))


# 3: Get Shapefiles: US-MW, BR, & Cerrado ------------

### Load World Shapefile ###
shp_world <- st_read(system.file("shapes/world.gpkg", package="spData"))

### Load US Shapefile ###
shp_us <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AK", "HI", "PR"))

#### Load US-MW Shapefile ###
shp_us_mw <- shp_us %>%
  filter(STUSPS %in% c("IA", "IL", "IN", "KS", "MI", "MN",
                       "MO", "ND", "NE", "OH", "SD", "WI"))

#### Load Cerrado Shapefile ###
shp_cerr <- read_biomes(
  year = 2019,
  simplified = T,
  showProgress = T) %>%
  dplyr::filter(name_biome == "Cerrado")


#### Load BR Shapefile ###
shp_br <- read_country(
  year = 2019,
  simplified = T,
  showProgress = T)

#### Load Simple BR Border Shapefile ###
data("countriesCoarse")
shp_br_border <- countriesCoarse %>% subset(SOV_A3 == "BRA")
shp_br_border <- st_combine(st_as_sf(shp_br_border))

## Load Cerrado Outline ##
# get Brazil States outline
shp_cerr_states <- read_state(
  year = 2019,
  simplified = T)

# filter to Cerrado States
shp_cerr_states <- shp_cerr_states %>%
  dplyr::filter(abbrev_state %in% c("TO","MA","PI","BA","MG",
                                    "SP","MS","MT","GO","DF"))

# # Save Shapefiles ------
save(shp_br, shp_br_border,
     shp_cerr, shp_cerr_states,
     shp_us, shp_us_mw,
     shp_world,
     file = "../Data_Derived/shp_usbr.RData")
