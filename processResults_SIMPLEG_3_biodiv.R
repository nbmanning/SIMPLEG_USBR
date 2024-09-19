# Title: processResults_SIMPLEG_3_biodiv.R
# Purpose: Get impact sto biodiversity at the WWF ecoregion level

# Initial SIMPLE-G script by: Iman Haqiqi
# Initial date: Aug 2019

# Edited by: Nick Manning 
# Initial edit date: May 2023
# Last edited: April 2024


# REQUIRES:
## SIMPLE-G Result files as 'SpatRasters' from 'processResults_SIMPLEG_2.R'

# To-Do:
## Convert to GGPLOT
## Save new data
## Change colorscheme (right now the 0 isn't centered and the white is unclear if NA or really low value)

# # # # # # # # # # # # # # # # # # # # # # # # 
rm(list = ls())
getwd()

# 0: Load Libraries & Set Constants ----------------------

## Libraries ####
library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
library(scico)

# Calc. Biodiversity Impacts from CFs 
library(rio)

## Constants ####

# set folders 
date_string <- "2024-03-03"

folder_der <- "../Data_Derived/"
folder_der <- paste0(folder_der, date_string, "/")

folder_fig <- "../Figures/"
folder_fig <- paste0(folder_fig, date_string, "/")

# set pct
pct <- "_m" # either "_l" , "_m" , or "_h"
pct_title <- " - Med"
crop <- "soy" # either maize, soy, or sm (Soy+Maize)
layer_choice <- "rawch_SOY"

# 1: Plotting Change -----

## 1.1: Load Shapefiles ------

## WWF Ecoregions ##
raw_eco <- st_read("../Data_Source/wwf_ecoregions/wwf_terr_ecos.shp")

# get names -- future plan is to merge with 'eco' for easy identification
raw_eco_info <- raw_eco %>% 
  st_drop_geometry() %>% 
  select(ECO_NAME, REALM, BIOME, eco_code) %>% 
  distinct()

##
# Annoying error - "Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : Loop 15 is not valid: Edge 0 has duplicate vertex with edge 4
# check the number of problem polygons 
table(sf::st_is_valid(raw_eco))

# see if only removing the problem polygon would work
raw_eco[!sf::st_is_valid(raw_eco),]

# remove polygon by only including valid boundaries 
eco <- sf::st_make_valid(raw_eco)
##

# Do a union to keep all of the ecoregions with the same unique eco ID
eco <- eco %>% 
  group_by(eco_code) %>% 
  summarise() # union

# plot - can't plot with eco_code bc they're all unique. 
ggplot(data = eco) + 
  geom_sf(aes(fill = eco_code), show.legend = F)

# save
st_write(eco, "../Data_Source/wwf_ecoregions/agg_wwf_terr_ecos_fixed_ecocode.shp")

### Load Corrected Ecoregions here -----
eco <- st_read("../Data_Source/wwf_ecoregions/agg_wwf_terr_ecos_fixed_ecocode.shp")


### Load SIMPLE-G -------------
# load Raster - Maize+Soy - World
r_ms_w <- readRDS(file = paste0(folder_der, "r", pct, "_World", ".rds"))

names(r_ms_w)

# plot to test this out 
terra::plot(r_ms_w$new_QLAND, main = "Post-Sim Cropland Area Per Grid Cell")
terra::plot(r_ms_w[[layer_choice]]*1000, 
            main = paste(str_to_title(crop), "Change in Area (ha) Per Grid Cell"))


## 1.2: Zonal Stat Agg to Ecoregion ----------------------

# convert ecoregions to spatial vector
sv_eco <- terra::vect(eco)

# set CRS to the same as the Source SpatVector 
crs(r_ms_w) <- crs(sv_eco)

# set up fxn to calc zonal stats for a given layer based on ecoregions

# NOTE: since we are doing raw change per grid cell, we want the sum per ecoregion
F_zonal <- function(layer){
  layer <- terra::zonal(layer, sv_eco, fun = sum, na.rm = T, as.raster = T)
  layer <- layer * 1000 # get from kha to ha to compare with raw data
  # reclassify NA as 0
  #layer[is.na(layer)] <- 0
  #layer <- terra::subst(layer, from = NA, to = 0)
}

# keep only layers with names that start with rawch to apply our fxn to
r_ms_w_rawch <- r_ms_w %>%
  subset(grepl( "rawch" , names(.)))

# run function over each layer
rawch_eco <- sapp(r_ms_w_rawch, fun = F_zonal)
rawch_eco$rawch_SOY

# do this manually for soy
rawch_s <- r_ms_w %>% subset("rawch_SOY")
rawch_s_eco <- terra::zonal(rawch_s, sv_eco, fun = sum, na.rm = T, as.raster = T)

# multiply by 1000 to go from kha to ha
rawch_s_eco <- rawch_s_eco*1000

boxplot(rawch_s_eco, maxcell = Inf)

rawch_s_eco

plot(rawch_s_eco$rawch_SOY)


## 1.3: Plot ----------------------

## Quick Terra Plots ##

# plot some zonal stats with eco outline  
terra::plot(rawch_eco$rawch_QLAND , main = "Sum of SIMPLE-G Change in Area per Ecoregion")
plot(sv_eco, add = T, lwd = 0.1)

# one layer/crop specifically
terra::plot(rawch_eco[[layer_choice]], 
            main = paste(str_to_title(crop), "Change in Area (ha) Per Ecoregion"))
plot(sv_eco, add = T, lwd = 0.1)

## ggplot ##
F_ggplot_world <- function(df, shp, brks, pal, legend_title, p_title, save_title){
  
  
  
  p <- ggplot()+
    geom_spatraster(data = df, maxcell = Inf)+
    # scale_fill_whitebox_c(
    #   #palette = "viridi", direction = 1,
    #   palette = pal,
    #   breaks = brks
    # )+
    scale_fill_scico(palette = "bam", 
                     direction = 1, 
                     na.value = "white",
                     midpoint = 0)+
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
      legend.text = element_text(size = 10))+
    #geom_spatvector(data = sv_eco, color = "black", fill = "transparent", lwd = 0.5)
    #geom_sf(data = sv_eco, fill = "transparent", color = "gray40", lwd = 0.2)
    geom_sf(data = shp, fill = "transparent", color = "gray40", lwd = 0.2)
  
    #geom_spatvector(data = sv_eco, lwd = 0.2)#+
  
  
  ggsave(plot = p, filename = paste0(folder_fig, "/", save_title),
         width = 14, height = 6, dpi = 300)
  
  return(p)
}

F_ggplot_world(df = rawch_eco %>% subset("rawch_SOY"),
               shp = sv_eco,
               brks = waiver(),
               #pal = "gn_yl", 
               pal = "bam",
               legend_title = "Area (ha)",
               p_title = paste("Post-Sim Soybean Change", pct_title),
               save_title = "test_gg_eco_rawch_soy.png")


## 1.4: Filter to Brazil/Cerrado ------
F_aoi_clip <- function(spat_rast, aoi_shp){
  
  # get extent as terra object for plotting
  ext_area <- vect(ext(aoi_shp))
  
  # crop and masking to just the extent of interest
  r_aoi <- terra::crop(spat_rast, ext_area, mask = T) 
  r_aoi <- mask(r_aoi, aoi_shp)
  
  # return as result
  return(r_aoi)
}

# load shapefiles
load("../Data_Derived/shp_usbr.RData")

# clip to Brazil 
rawch_s_br <- F_aoi_clip(rawch_s, shp_br)

F_ggplot_world(df = rawch_s_br,
               shp = shp_br,
               brks = waiver(),
               #pal = "gn_yl", 
               pal = "bam",
               legend_title = "Area (ha)",
               p_title = paste("Post-Sim Soybean Change", pct_title),
               save_title = "test_gg_eco_rawch_soy_br.png")

# clip to Cerrado 
rawch_s_cerr <- F_aoi_clip(rawch_s, shp_cerr)

F_ggplot_world(df = rawch_s_cerr,
               shp = shp_cerr,
               brks = waiver(),
               #pal = "gn_yl", 
               pal = "bam",
               legend_title = "Area (ha)",
               p_title = paste("Post-Sim Soybean Change", pct_title),
               save_title = "test_gg_eco_rawch_soy_cerr.png")

# now try and get it to ecoregions in Brazil
r_ms_w_rawch <- r_ms_w %>% 
  subset(grepl( "rawch" , names(.)))

rawch_s <- r_ms_w_rawch %>% subset("rawch_SOY")

# clip ecoregions to BR - 
## no point in doing this because the ecoregions overlap political boundaries 
sv_eco_br <- F_aoi_clip(sv_eco, shp_br)

# get extent as terra object for plotting
ext_br <- vect(ext(shp_br))

# crop and masking to just the extent of interest
sv_eco_br <- terra::crop(sv_eco, ext_br) 

plot(shp_br, col = NA, border = "red", lwd = 3)
plot(sv_eco_br, border = "gray20", lwd = 0.8, add = T)

plot(rawch_s_eco$rawch_SOY, 
     main = "Land Converted to Soy per Ecoregion (ha)",
     )

# 2: Calculate Biodiversity from CFs -----

# NOTE: These come from Chaudhary 2015 Supp. Info. 2
# Link: DOI: 10.1021/acs.est.5b02507

# load in regional and global CFs for each ecoregion
cf_regional <- rio::import("../Data_Source/CFs/Chaudhary2015_SI3_CFsEcoregions_AnnualCrops.xlsx",
                           which = "Trans_marginal_regional", 
                           skip = 3)
str(cf_regional)
cf_global <- rio::import("../Data_Source/CFs/Chaudhary2015_SI3_CFsEcoregions_AnnualCrops.xlsx",
                      which = "Trans_marginal_global",
                      skip = 3)

## Data Prep ----
# GOAL: Get Raw Change and Characterization Factors in the same df based on eco_codes

# for future: might be able to use the stacked rasters and then calculate all kinds of stuff. For now, land transformation to soy is fine! 

# make the SpatRaster of change per ecoregion into a SpatVector
sv_rawch_s_eco <- as.polygons(rawch_s_eco, 
                       trunc = F, # if T, changes values to integers
                       dissolve = T, # if F, bad stuff happens
                       values = T, # keep values as an attribute
                       na.rm = F # keep NA values
                       )

# intersect the new SpatVector with the SpatVector containing the eco_codes
sv_rawch_s_ecocodes <- intersect(sv_rawch_s_eco, sv_eco)

# Since we only are interested in how MANY hectares (or m2) converted we have, and we already have the eco_codes, we can turn this into a data frame, which will make the process run a lot smoother
df_rawch_s_eco <- as.data.frame(sv_rawch_s_ecocodes)

# now we can join the df with our df of regional/global characterization factors
df_rawch_s_eco_reg <- left_join(df_rawch_s_eco, cf_regional)
df_rawch_s_eco_global <- left_join(df_rawch_s_eco, cf_global)

## Regional Extinctions -----

# make the names easier to keep up with
df_reg <- df_rawch_s_eco_reg

# remove any faulty eco_codes that don't match the XXYYYY pattern of X = Capital Letter and Y = Digit 
df_reg <- df_reg %>% 
  filter(grepl("^[A-Z]{2}[0-9]{4}", eco_code)) %>% # loses 79 regions - use !grepl to see which ones
  
  # convert from ha to square meters (m2, m^2)
  mutate(rawch_SOY = rawch_SOY*10000) %>%  
  
  # keep only columns ending in _median
  dplyr::select(matches('rawch_SOY|eco_code|Median'))
  
str(df_reg)
str(df_reg_calc)

df_reg_calc <- df_reg %>% 
  # Convert "NaN" strings to NA
  mutate(across(ends_with("_Median"), ~ na_if(., "NaN"))) %>%  
  # Convert CF columns from character to numeric
  mutate(across(ends_with("_Median"), as.numeric)) %>%
  # multiply the characterization factor by the new m2 of soybean (transformation)
  mutate(across(ends_with("_Median"), ~ . * rawch_SOY))

# since we can't have negative extinctions, set everything under 0 to NA
df_reg_calc_na <- df_reg_calc %>% 
  mutate(across(ends_with("_Median"), ~ replace(., .<0, NA)))

# Re-Merge w SV_Eco to plot 

sv_reg_calc <- merge(sv_eco, df_reg_calc_na)

# freezes -- try when I have more time. also, use continuous or interval, since it's biased towards one or two extreme polygons
# ggplot()+
#   geom_spatvector(data = sv_reg_calc, aes(fill = "rawch_SOY"))

# terra::plot(sv_reg_calc, "Mammals_AnnualCrops_Median")



# Graveyard -------

## Plotting with tmap --------

library(tmap)
# tmap link: https://tmieno2.github.io/R-as-GIS-for-Economists/quick-plot-and-interactive-map.html
# View ecoregions in interactive map
tmap_leaflet(x = tm_shape(eco)+ tm_borders())
# Note that we can't use SpatRasters with tmap() - annoying
# tmap_leaflet(                                                      
#   tm_shape(as(rawch_eco1, "Raster")) + # what sf to use for creating a map 
#     tm_raster(), # what type of geometry to put on the map 
#   #mapview.maxpixels = 1000000000
#   midpoint = NA
# ) 

# # # # # # # # # # # # # # # # # # 

