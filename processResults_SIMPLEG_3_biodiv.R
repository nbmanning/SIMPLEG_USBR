# Title: processResults_SIMPLEG_3_biodiv.R
# Purpose: Get impact sto biodiversity at the WWF ecoregion level

# Initial SIMPLE-G script by: Iman Haqiqi
# Initial date: Aug 2019

# Edited by: Nick Manning 
# Initial edit date: May 2023
# Last edited: April 2024


# REQUIRES:
## SIMPLE-G Result files as 'SpatRasters' from 'processResults_SIMPLEG_2.R'

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
library(treemap)

# Calc. Biodiversity Impacts from CFs 
library(rio)

## Constants ####

# set folders 
date_string <- "2024-09-15"

folder_der <- "../Data_Derived/"
folder_der <- paste0(folder_der, date_string, "/")

folder_fig <- "../Figures/"
folder_fig <- paste0(folder_fig, date_string, "/")

# set pct
pct <- "_m" # either "_l" , "_m" , or "_h"
pct_title <- " - Med"
crop <- "soy" # either maize, soy, or sm (Soy+Maize)
layer_choice <- "rawch_SOY"

# Create Biodiversity plotting folder 
folder_fig_biodiv <- paste0(folder_fig, "/biodiversity/")
files_fig_impexp <- list.dirs(folder_fig_biodiv)

# do the same but specifically for imp/exp folder
if (!(any(grepl(date_string, files_fig_impexp)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_fig_biodiv))
  
  cat("Biodiversity Folder", date_string, "created.\n")
} else {
  cat("A Biodiversity figures folder with the string", date_string, "in its name already exists.\n")
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1: Plotting Change -----

## 1.1: Load Shapefiles ------

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Clean WWF Shapefile - need to run this once to fix broken boundaries ##

# ## WWF Ecoregions ##
raw_eco <- st_read("../Data_Source/wwf_ecoregions/wwf_terr_ecos.shp")
# 
# # get names -- future plan is to merge with 'eco' for easy identification
# raw_eco_info <- raw_eco %>% 
#   st_drop_geometry() %>% 
#   select(ECO_NAME, REALM, BIOME, eco_code) %>% 
#   distinct()
# 
# ##
# # Annoying error - "Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : Loop 15 is not valid: Edge 0 has duplicate vertex with edge 4
# # check the number of problem polygons 
# table(sf::st_is_valid(raw_eco))
# 
# # see if only removing the problem polygon would work
# raw_eco[!sf::st_is_valid(raw_eco),]
# 
# # remove polygon by only including valid boundaries 
# eco <- sf::st_make_valid(raw_eco)
# ##
# 
# # Do a union to keep all of the ecoregions with the same unique eco ID
# eco <- eco %>% 
#   group_by(eco_code) %>% 
#   summarise() # union

# # plot - can't plot with eco_code bc they're all unique. 
# ggplot(data = eco) + 
#   geom_sf(aes(fill = eco_code), show.legend = F)
# 
# # save
# st_write(eco, "../Data_Source/wwf_ecoregions/agg_wwf_terr_ecos_fixed_ecocode.shp")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

### Load Corrected Ecoregions here -----
eco <- st_read("../Data_Source/wwf_ecoregions/agg_wwf_terr_ecos_fixed_ecocode.shp")


### Load SIMPLE-G -------------
# load Raster - Maize+Soy - World
r_ms_w <- readRDS(file = paste0(folder_der, "r", pct, "_World", ".rds"))

# uncomment and plot one layer 
# terra::plot(r_ms_w$new_QLAND, main = "Post-Sim Cropland Area Per Grid Cell")
# terra::plot(r_ms_w[[layer_choice]]*1000, 
#             main = paste(str_to_title(crop), "Change in Area (ha) Per Grid Cell"))


## 1.2: Zonal Stat Agg to Ecoregion ----------------------

# convert ecoregions to spatial vector
sv_eco <- terra::vect(eco)

# set CRS to the same as the Source SpatVector 
crs(r_ms_w) <- crs(sv_eco)

# Calculate Zonal Stats for soy - sum of the raw change (i.e. transformation) to soy per ecoregion
rawch_s <- r_ms_w %>% subset("rawch_SOY")

# Convert to ha from kha by multiplying by 1000 before zonal stats
rawch_s_eco <- rawch_s*1000*10000

# Calculate zonal sum per ecoregion
rawch_s_eco <- terra::zonal(rawch_s_eco, sv_eco, fun = sum, as.raster = T, na.rm = T)
rawch_s_eco_notRaster <- terra::zonal(rawch_s_eco, sv_eco, fun = sum, na.rm = T, wide = T)

## TEST ##

# this works but is sketchy -- it assumes the order is the same for sv and rawch_s_eco
sv <- sv_eco
sv$ID <- 1:nrow(sv) 
sv_df <- as.data.frame(sv)

# returns a df with an ID and sum - this is where the assumption is 
extracted <- terra::extract(rawch_s_eco, sv, na.rm = T, fun = sum)

result_df <- merge(extracted, sv_df[, c("ID", "eco_code")], by = "ID")

## ## ##
## 1.3: Plot ----------------------

## Quick Terra Plots ##

# plot some zonal stats with eco outline  
# terra::plot(rawch_s_eco$rawch_SOY , main = "Sum of SIMPLE-G Soy Change in Area per Ecoregion")
# plot(sv_eco, add = T, lwd = 0.1)

## ggplot ##
# ggplot()+
#   geom_spatraster(data = rawch_s_eco, maxcell = Inf)+
# 
#   # add scico palette to split the colors at 0
#   scale_fill_scico(palette = "bam",
#                    direction = 1,
#                    na.value = "white",
#                    midpoint = 0)+
#   labs(
#     fill = "Area (ha)",
#     title = paste("Post-Sim Soybean Change"
#                   #, pct_title
#                   )
#   )+
#   theme_minimal() +
#   theme(
#     # set plot size and center it
#     plot.title = element_text(size = 16, hjust = 0.5),
#     # put legend in the bottom right
#     legend.position = c(0.15, 0.2),
#     legend.title = element_text(size = 14),
#     legend.text = element_text(size = 10))+
#   # add the ecoregion outlines
#   geom_sf(data = sv_eco, fill = "transparent", color = "gray40", lwd = 0.2)
# 
# # save plot
# ggsave(filename = paste0(folder_fig_biodiv, "/", "gg_eco_rawch_soy.png"),
#        width = 14, height = 6, dpi = 300)

# 2: Calculate Biodiversity from CFs -----

# NOTE: These come from Chaudhary 2015 Supp. Info. 2
# Link: DOI: 10.1021/acs.est.5b02507
# NOTE: I did some manual work in Excel (as follows):
## 1) removed all other sheets except for trans_marginal_regional and trans_marginal_global
## 2) within each taxa, I removed all other land cover classes except "annual crops"
## 3) made each column name tidy by unmerging cells and moving the land cover and stat (median, uppper95, lower95) to the title; e.g. Mammals_AnnualCrops_Median

# load in regional and global CFs for each ecoregion
cf_regional <- rio::import("../Data_Source/CFs/Chaudhary2015_SI3_CFsEcoregions_AnnualCrops.xlsx",
                           which = "Trans_marginal_regional", 
                           skip = 3)

cf_global <- rio::import("../Data_Source/CFs/Chaudhary2015_SI3_CFsEcoregions_AnnualCrops.xlsx",
                         which = "Trans_marginal_global",
                         skip = 3)



# remove "Plants" Column from regional for these reasons:
## match with Global
## Error Bars much too large
## follow Chai et al., 2024
cf_regional <- cf_regional %>% select(-contains("Plants"))


## 2.1 Data Prep ----
# make the SpatRaster of change per ecoregion into a SpatVector
sv_rawch_s_eco <- as.polygons(rawch_s_eco, 
                       trunc = F, # if T, changes values to integers
                       dissolve = T, # if F, bad stuff happens
                       values = T, # keep values as a spatial attribute of each polygon
                       na.rm = F # keep NA values
                       )

# intersect the soy change SpatVector with the SpatVector containing the eco_codes
sv_rawch_s_ecocodes <- intersect(sv_rawch_s_eco, sv_eco)

# Since we only are interested in how MANY hectares (or m2) converted we have, and we already have the eco_codes, we can turn this into a data frame, which will make the process run a lot smoother
df_rawch_s_eco <- as.data.frame(sv_rawch_s_ecocodes)

# now we can join the df with our df of regional/global characterization factors
df_rawch_s_eco_reg <- left_join(df_rawch_s_eco, cf_regional)
df_rawch_s_eco_global <- left_join(df_rawch_s_eco, cf_global)

# new approach using extract()
df_rawch_s_eco_reg <- left_join(cf_regional, result_df)
df_rawch_s_eco_global <- left_join(cf_global, result_df)

## 2.2: Set Functions -----
# first function to clean data
F_clean_calc <- function(df){

  df <- df %>% 
    # remove any faulty eco_codes that don't match the XXYYYY pattern of X = Capital Letter and Y = Digit 
    filter(grepl("^[A-Z]{2}[0-9]{4}", eco_code)) #%>% # loses 79 regions - use !grepl to see which ones
    
    # convert from ha to square meters (m2, m^2)
    # mutate(rawch_SOY = rawch_SOY*10000)
  
  # multiply each of the characterization factors (e.g. regional loss per m2 of transformation) by rawch_SOY (m2 land transformed from some other class to soybean)
  df <- df %>% 
    # Convert "NaN" strings to NA
    mutate(across(contains("AnnualCrops"), ~ na_if(., "NaN"))) %>%  
    # Convert CF columns from character to numeric
    mutate(across(contains("AnnualCrops"), as.numeric)) %>%
    # multiply the characterization factor by the new m2 of soybean (transformation)
    mutate(across(contains("AnnualCrops"), ~ . * rawch_SOY))
  
  # since we can't have negative extinctions, set everything under 0 to NA
  df <- df %>% 
    mutate(across(contains("AnnualCrops"), ~ replace(., .<0, 0)))
}



# set 2nd function to clean and prep for plots 
F_clean_forplots <- function(df){
  
  # take the df from the last cleaning function and...
  df2 <- df %>% 
    
    # remove eco code
    select(-eco_code) %>% 
    
    # remove NA's
    na.omit() %>% 
    
    # summarize total changes by taking their sum
    summarise(across(everything(), list(sum))) %>% 
    
    # remove Rawch_soy
    select(-c(rawch_SOY_1, contains("TaxaAggregated"))) %>%
    
    # make long for plotting
    pivot_longer(
      cols = contains("AnnualCrops"),
      names_to = "Taxa",
      values_to = "Loss"
    ) %>% 
    
    # remove the extra details in the column names and split into error columns 
    mutate(Taxa = gsub("_1", "", Taxa)) %>%
    mutate(Taxa = gsub("_AnnualCrops", "", Taxa)) %>% 
    separate(Taxa, into = c("Taxa", "CI"), sep = "_") %>% 
    pivot_wider(names_from = CI, values_from = Loss) %>% 
    
    # Calculate error (difference between Median and l95/u95)
    mutate(
      error_low = Median - lower95,   # Difference between Median and lower95
      error_high = upper95 - Median   # Difference between upper95 and Median
    )
  
  return(df2)

}

## Plotting Fxn's

F_plot <- function(df, scale){
  
  
  # set conditional variable based on regional or global
  if(scale == "Regional"){
    s = "reg"
  }
  
  else{
    s = "global"
    }
  
  #### Treemap ###
  png(filename = paste0(
    paste0(folder_fig_biodiv, s, "_treemap", ".png")),
    width = 21, height = 12, units = "in", res = 300
    )

  treemap(df,
          index="Taxa",
          vSize="Median",
          type="index"
  )
  
  dev.off()
  
  #### Barplot ###
  ggplot(df, aes(x=Taxa, y=Median)) + 
    geom_bar(stat = "identity") +
    coord_flip()+
    geom_errorbar(aes(
      x=Taxa, 
      ymin = error_low, 
      ymax=error_high
      ), 
      width=0.4, colour="orange", alpha=0.9, linewidth=1.3)
  
  # save
  ggsave(
    filename = paste0(folder_fig_biodiv, 
                      s, "_barplot", ".png"),
    dpi = 300, width = 12, height = 8)
  
  #### Doughnut ###
  # Compute percentages
  df$fraction = df$Median / sum(df$Median)
  
  # Compute the cumulative percentages (top of each rectangle)
  df$ymax = cumsum(df$fraction)
  
  # Compute the bottom of each rectangle
  df$ymin = c(0, head(df$ymax, n=-1))
  
  # Compute label position
  df$labelPosition <- (df$ymax + df$ymin) / 2
  
  # Compute a good label
  df$label <- paste0(
    df$Taxa, 
    #"\n value: ", 
    "\n",
    round(df$Median)
  )
  
  # Make the plot
  ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Taxa)) +
    geom_rect() +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
    #scale_fill_brewer(palette=4) +
    coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
    xlim(c(2, 4))+ # Try to remove that to see how to make a pie chart
    theme_void() #+
    #theme(legend.position = "none") +
    
  
  # save
  ggsave(
    filename = paste0(folder_fig_biodiv, 
                      s, "_doughnut", ".png"),
    dpi = 300, width = 8, height = 8)
}


## Regional Extinctions -----

# clean 
df_reg <- F_clean_calc(df_rawch_s_eco_reg)

# clean for plots 
df_reg_sum <- F_clean_forplots(df_reg)

# Plotting 
F_plot(df_reg_sum, "Regional")


## Global Extinctions ---------

# clean 
df_global <- F_clean_calc(df_rawch_s_eco_global)

# clean for plots 
df_global_sum <- F_clean_forplots(df_global)

# Plotting 
F_plot(df_global_sum, "Global")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Test --------
test <- left_join(df_reg, raw_eco)
head(df_reg)

# Future -----------

## Re-Merge w SV_Eco to plot sp. change per ecoregion map -------- 
df_reg_agg <- df_rawch_s_eco_reg %>% 
  select(rawch_SOY, eco_code, 
         contains("Median")
         #TaxaAggregated_AnnualCrops_Median
         )
  #select(rawch_SOY, eco_code, contains("Median"))

df_reg_agg2 <- F_clean_calc(df_reg_agg)
str(df_reg_agg2)

sv_reg_calc <- merge(sv_eco, df_reg_agg2)
sv_reg_calc <- sv_reg_calc %>% 
  rename(
    #Impact = TaxaAggregated_AnnualCrops_Median
    Impact = Birds_AnnualCrops_Median
    )

# ggplot ##
ggplot()+
  geom_spatvector(data = sv_reg_calc, aes(fill = Impact))+

  # add scico palette to split the colors at 0
  # scale_fill_scico(palette = "bam",
  #                  direction = 1,
  #                  na.value = "white",
  #                  midpoint = 0)+
  labs(
    fill = "Impact (species*years)",
    title = "Biodiversity Impact per Ecoregion")+
  theme_minimal() +
  theme(
    # set plot size and center it
    plot.title = element_text(size = 16, hjust = 0.5),
    # put legend in the bottom right
    legend.position = c(0.15, 0.2),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10))+
  # add the ecoregion outlines
  geom_sf(data = sv_eco, fill = "transparent", color = "gray40", lwd = 0.2)

# save plot
ggsave(filename = paste0(folder_fig_biodiv, "/", "gg_reg_impact.png"),
       width = 14, height = 6, dpi = 300)

# Graveyard -------

## Interactive plot with plotly --------

### interactive bar plot ###
# Create an interactive barplot with error bars using plotly
# library(plotly)
# plot <- plot_ly(df, 
#                 x = ~Taxa, 
#                 y = ~Median, 
#                 type = 'bar', 
#                 error_y = ~list(type = 'data', 
#                                 array = error_high,   # Upper error (upper95 - Median)
#                                 arrayminus = error_low)) %>%  # Lower error (Median - lower95)
#   layout(title = "Interactive Barplot with Error Bars",
#          xaxis = list(title = "Taxa"),
#          yaxis = list(title = "Loss"))
# 
# # Show the plot
# plot

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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Loop Zonal Stats over all layers of "r_world" ---------
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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Got rid of this bc it doesn't make any sense to clip to Brazil if the point of doing ecoregions 
# was to avoid political boundaries

## Filter to Brazil/Cerrado ------

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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## ggplot with function ---------------------

# got rid of bc I'm only doing this once in the clean script, keeping it here in case I want to plot more layers

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
  
  
  ggsave(plot = p, filename = paste0(folder_fig_biodiv, "/", save_title),
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



