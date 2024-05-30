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

# 1: Load Results & Shapefiles ----------------------

## 1.1: Load WWF EcoRegions ------
raw_eco <- st_read("../Data_Source/wwf_ecoregions/wwf_terr_ecos.shp")

# get names -- future plan is to merge with 'eco' for easy identification
raw_eco_info <- raw_eco %>% 
  st_drop_geometry() %>% 
  select(ECO_NAME, REALM, BIOME, ECO_ID) %>% 
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
  group_by(ECO_ID) %>% 
  summarise() # union

# plot 
ggplot(data = eco) + 
  geom_sf(aes(fill = ECO_ID))

# save
st_write(eco, "../Data_Source/wwf_ecoregions/agg_wwf_terr_ecos.shp")


## 1.2: Load SIMPLE-G -------------
# load Raster - Maize+Soy - World
r_ms_w <- readRDS(file = paste0(folder_der, "r", pct, "_World", ".rds"))

names(r_ms_w)

# plot to test this out 
terra::plot(r_ms_w$new_QLAND, main = "Post-Sim Cropland Area Per Grid Cell")
terra::plot(r_ms_w[[layer_choice]]*1000, 
            main = paste(str_to_title(crop), "Change in Area (ha) Per Grid Cell"))


# 2: Zonal Stat Agg to Ecoregion ----------------------

# convert ecoregions to spatial vector
sv_eco <- terra::vect(eco)

# set CRS to the same as the Source SpatVector 
crs(r_ms_w) <- crs(sv_eco)

# set up fxn to calc zonal stats for a given layer based on ecoregions

# NOTE: since we are doing raw change per grid cell, we want the sum per ecoregion
F_zonal <- function(layer){
  layer <- zonal(layer, sv_eco, fun = sum, na.rm = T, as.raster = T)
  layer <- layer * 1000 # get from kha to ha to compare with raw data
  # reclassify NA as 0
  #layer[is.na(layer)] <- 0
  #layer <- terra::subst(layer, from = NA, to = 0)
}

# get list of only layer names that start with rawch to aply our fxn to
r_ms_w_rawch <- r_ms_w %>% 
  subset(grepl( "rawch" , names(.)))

rawch_eco <- sapp(r_ms_w_rawch, fun = F_zonal)  

# 3: Plot ----------------------

## 3.1: Quick Terra Plots ------

# plot some zonal stats with eco outline  
terra::plot(rawch_eco$rawch_QLAND , main = "Sum of SIMPLE-G Change in Area per Ecoregion")
plot(sv_eco, add = T, lwd = 0.1)

# one layer/crop specifically
terra::plot(rawch_eco[[layer_choice]], 
            main = paste(str_to_title(crop), "Change in Area (ha) Per Ecoregion"))
plot(sv_eco, add = T, lwd = 0.1)

## 3.2: ggplot ------
F_ggplot_world <- function(df, brks, pal, legend_title, p_title, save_title){
  
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
    geom_sf(data = sv_eco, fill = "transparent", color = "gray40", lwd = 0.2)
    #geom_spatvector(data = sv_eco, lwd = 0.2)#+
  
  
  ggsave(plot = p, filename = paste0(folder_fig, "/", save_title),
         width = 14, height = 6, dpi = 300)
  
  return(p)
}

F_ggplot_world(df = rawch_eco %>% subset("rawch_SOY"),
               brks = waiver(),
               #pal = "gn_yl", 
               pal = "bam",
               legend_title = "Area (ha)",
               p_title = paste("Post-Sim Soybean Change", pct_title),
               save_title = "test_gg_eco_rawch_soy.png")


# 4: Save ----------------------


