# Title: 3a_processResults_biodiv.R
# Purpose: Get impacts to biodiversity at the WWF ecoregion level by using the characterization factors
# designed by Chaudhary et al. 2015 at the regional and global level (from marginal transition).

# Author: Nick Manning 
# Created on: April 2024
# Last edited: July 2025

# REQUIRES:
## SIMPLE-G Result files as 'SpatRasters' from 'processResults_SIMPLEG_2.R'
## WWF Ecoregions shapefile 
## Chaudhary et al. 2015 Characterization Factor XLSX sheets - See Section 2 for Details

# LINKS:
## Chaudhary et al., 2015, SI2 "Quantifying Land Use Impacts on Biodiversity: Combining Species–Area Models and Vulnerability Indicators". DOI: 10.1021/acs.est.5b02507
## WWF Ecoregions Download: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world

# NEXT:
## Convert to Jupyter Notebook

# # # # # # # # # # # # # # # # # # # # # # # # 
rm(list = ls())
getwd()

# 0) Load Libraries & Set Constants ----------------------

### Libraries ###
# Loading, Cleaning, and Plotting Data
library(ggplot2)
library(dplyr)
library(terra)
library(sf)
library(tidyterra)
library(scico)
library(treemap)

# Calulating Biodiversity Impacts from CFs 
library(rio)

# Mapping Biodiversity Impacts
library(classInt)
library(RColorBrewer)

### Constants ###

# SET PCT -  NEED TO DO MANUALLY FOR NOW! 
pct <- "_m" # either "_l" , "_m" , or "_h"
pct_model <- "m"
pct_title <- " - Med"
crop <- "soy" # either maize, soy, or sm (Soy+Maize)
layer_choice <- "rawch_SOY"


# set folders 
date_string <- "2024-11-15"

folder_der <- "../Data_Derived/"
folder_der <- paste0(folder_der, date_string, "/")

folder_fig <- "../Figures/"
folder_fig <- paste0(folder_fig, date_string, "/")

# Create Biodiversity plotting folder 
folder_fig_biodiv <- paste0(folder_fig, pct_model,  "/biodiversity/")
files_fig_biodiv <- list.dirs(folder_fig_biodiv)

# Check to see if Biodiversity folder (per scenario) exists
if (!(any(grepl(date_string, files_fig_biodiv)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_fig_biodiv))
  
  cat("Biodiversity Folder", date_string, "created.\n")
} else {
  cat("A Biodiversity figures folder with the string", date_string, "in its name already exists.\n")
}

# Create Biodiversity results folder
folder_results_biodiv <- paste0("../Results/SIMPLEG-", date_string,  "/", pct_model, "/")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1) Plotting Change -----

## 1.1) Load Shapefiles ------

## Clean WWF Shapefile  ##

# # # # # # # # # # # # # # # # # # # # # # # # #
## Uncomment (Highlight then Ctrl + C) to run ##
# # # # # # # # # # # # # # # # # # # # # # # # #


## ONLY NEED TO RUN THIS ONCE TO FIX BROKEN ECOREGION BOUNDARIES ## 
## IGNORE IF THE CORRECTED SHP FILE WAS PROVIDED ## 


# ## WWF Ecoregions ##
# raw_eco <- st_read("../Data_Source/wwf_ecoregions/wwf_terr_ecos.shp")
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
# 
# # plot - can't plot with eco_code bc they're all unique.
# ggplot(data = eco) +
#   geom_sf(aes(fill = eco_code), show.legend = F)
# 
# # save
# st_write(eco, "../Data_Source/wwf_ecoregions/agg_wwf_terr_ecos_fixed_ecocode.shp")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

### 1.1.1 Load Corrected Ecoregions here -----
# raw_eco <- st_read("../Data_Source/wwf_ecoregions/wwf_terr_ecos.shp")
eco <- st_read("../Data_Source/wwf_ecoregions/agg_wwf_terr_ecos_fixed_ecocode.shp")


### 1.1.2 Load SIMPLE-G -------------

# load Raster - Maize+Soy - World
r_ms_w <- readRDS(file = paste0(folder_der, "r", pct, "_World", ".rds"))


## 1.2) Zonal Stat Agg. (SUM) to Ecoregion ----------------------

# convert ecoregions to spatial vector
sv_eco <- terra::vect(eco)

# set CRS to the same as the Source SpatVector 
crs(r_ms_w) <- crs(sv_eco)

# get just the soy layer
rawch_s <- r_ms_w %>% subset("rawch_SOY")

# Convert from kha to ha (*1000) then from ha to m2 (*10000) before zonal stats
rawch_s_eco <- rawch_s*1000*10000


# Calculate Zonal Stats for soy - sum of the raw change (i.e. transformation) to soy per ecoregion
## we use extract() with "sum" since we are looking for spatraster values in a spatvector ##

# set an ID column to get a pseudo-ID for each ecoregion
sv_eco$ID <- 1:nrow(sv_eco) 

# get pseudo-ID and values as a DF for using as a key later 
df_sv_eco <- as.data.frame(sv_eco)

# returns a df with an ID (ecoregion) and sum - this is where the assumption is 
df_rawch_ecoreg <- terra::extract(rawch_s_eco, sv_eco, na.rm = T, fun = sum)

# merge by ID with the previous df
df_rawch_ecoreg <- merge(df_rawch_ecoreg, df_sv_eco[, c("ID", "eco_code")], by = "ID")


# 2) Calculate Biodiversity from CFs -----

# NOTE: These come from Chaudhary 2015 Supp. Info. 2
# Link: DOI: 10.1021/acs.est.5b02507
# NOTE: I did some manual work in Excel (as follows):
## 1) removed all other sheets except for trans_marginal_regional and trans_marginal_global
## 2) within each taxa, I removed all other land cover classes except "annual crops"
## 3) made each column name tidy by unmerging cells and moving the land cover and stat (median, uppper95, lower95) to the title; e.g. Mammals_AnnualCrops_Median

# load in regional and global CFs for each ecoregion
cf_regional <- rio::import("../Data_Source/CFs/Chaudhary2015_SI2_cSAR_CFsEcoregions.xlsx",
                           which = "Trans_marginal_regional", 
                           skip = 3)

cf_global <- rio::import("../Data_Source/CFs/Chaudhary2015_SI2_cSAR_CFsEcoregions.xlsx",
                         which = "Trans_marginal_global",
                         skip = 3)



# remove "Plants" Column from regional for these reasons:
## match with Global
## Error Bars much too large
## follow Chai et al., 2024
cf_regional <- cf_regional %>% select(-contains("Plants"))

# join with the data on Soybean Change (m2) per ecoregion
df_rawch_s_eco_reg <- left_join(cf_regional, df_rawch_ecoreg)
df_rawch_s_eco_global <- left_join(cf_global, df_rawch_ecoreg)


## 2.1) Functions ----

# first function to clean data
F_clean_calc <- function(df){

  df <- df %>% 
    # remove any faulty eco_codes that don't match the XXYYYY pattern of X = Capital Letter and Y = Digit 
    filter(grepl("^[A-Z]{2}[0-9]{4}", eco_code)) %>% # NOTE: use !grepl outside of fxn to see which regions get lost
    # remove "ID" column
    select(-ID)

  # Clean raw change column by setting any NA's to 0
  df <- df %>% 
    # Convert NA to 0
    mutate(rawch_SOY = replace_na(rawch_SOY, 0))
    
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
      error_low = pmax(0, Median - lower95),   # Difference between Median and lower95, return 0 if negative
      error_high = upper95 - Median   # Difference between upper95 and Median
    )
  
  return(df2)

}

## Plotting Fxn's
# create custom color scheme for the bar and doughnut plots
custom_colors <- c("Reptiles" = "#C77CFF",  # violet
                   "Mammals" = "#00BFC4",  # darkcyan
                   "Birds" = "#7CAE00",  # yellowgreen
                   "Amphibians" = "#F8766D")  # lightcoral "#F8766D"

# this function plots the tree maps, bar plots, and doughnut plots for extinction stats
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
  ggplot(df, aes(x=Taxa, y=Median, fill = Taxa)) + 
    geom_bar(stat = "identity")+ #fill = "gray20") +
    # flip axes
    coord_flip()+
    # add errorbar
    geom_errorbar(aes(
      x = Taxa, 
      ymin = error_low, 
      ymax = error_high
    ), 
    width=0.4, colour="gray20", alpha=0.9, linewidth=0.5)+
    theme_minimal()+
    # add custom colors 
    scale_fill_manual(values = custom_colors) +
    theme(
      axis.text = element_text(size = 16),
      axis.title = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 18)
    )  
  
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
    scale_fill_manual(values = custom_colors)+ 
    #scale_fill_brewer(palette=4) +
    coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
    xlim(c(2, 4))+ # Try to remove that to see how to make a pie chart
    theme_void() +
    theme(legend.position = "none") #+
    
  
  # save
  ggsave(
    filename = paste0(folder_fig_biodiv, 
                      s, "_doughnut", ".png"),
    dpi = 300, width = 8, height = 8)
}


## 2.2) Plot Regional Extinctions -----

# clean 
df_reg <- F_clean_calc(df_rawch_s_eco_reg)

# clean for plots 
df_reg_sum <- F_clean_forplots(df_reg)


# Plotting 
F_plot(df_reg_sum, "Regional")


## 2.3) Plot Global Extinctions ---------

# clean 
df_global <- F_clean_calc(df_rawch_s_eco_global)

# clean for plots 
df_global_sum <- F_clean_forplots(df_global)

# Plotting 
F_plot(df_global_sum, "Global")

## 2.4) Save Reg. & Global Stats --------
save(df_global_sum, file = paste0(folder_results_biodiv, "df_global_sum", pct, ".RData"))
save(df_reg_sum, file = paste0(folder_results_biodiv, "df_reg_sum", pct, ".RData"))


# 3) Mapping Biodiv. Impacts --------

## 3.1) Mapping Function -----

### FUNCTION INPUTS ###
# plot_sv =   Spatial Vector merged with the df containing our results (e.g. 'sv_reg_calc') 
# layer_choice =   The layer (i.e. taxa) this map focuses on. Will loop over 'sv_names', (e.g. Mammals_AnnualCrops_Median) 
# scale =   Type of extinction, either "Regional" or "Global 
# breaks_n =   Number of breaks, here we opted for 7 
# breaks_type =   Type of Mapping unit for data breaks to use with the ClassIntervals() function. We opted for "fisher" but can be "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "jenks", "dpih", "headtails", "maximum", or "box"

# More on Fisher Algorithm:
## The "fisher" style uses the algorithm proposed by W. D. Fisher (1958) and discussed by Slocum et
## al. (2005) as the Fisher-Jenks algorithm; added here thanks to Hisaji Ono. This style will subsample
## by default for more than 3000 observations. This style should always be preferred to "jenks" as it
## uses the original Fortran code and runs nested for-loops much faster. -- From ClassInt Package Details

## Fisher-Jenks Algorithms "classify an array of 'n' numeric values into 'k' classes such that the sum of the squared deviations from the class means" -- GeoDMS, https://geodms.nl/docs/fisher's-natural-breaks-classification-complexity-proof.html

# mapping function that creates a map for each layer in the stack  (i.e. each taxa)
F_map <- function(layer_choice, plot_sv, scale, breaks_n, breaks_type){
  
  # set taxa name based on column name
  taxa <- sub("_.*", "", layer_choice)
  
  # set colors 
  colors <- brewer.pal(n = breaks_n, name = "YlGnBu")  # Adjust the palette name as needed
  
  # get layer values
  attribute_data  <- plot_sv %>% 
    values() %>% 
    pull(!!sym(layer_choice)) %>% 
    round(digits = 0)
  
  # Calculate breaks 
  breaks <- classIntervals(attribute_data, n = breaks_n, style = breaks_type)
  type_breaks <- breaks$brks
  
  # Classify the attribute based on breaks
  cut_data <- cut(attribute_data, breaks = type_breaks, include.lowest = TRUE, labels = FALSE)
  
  # Add the classified data back to the SpatVector
  plot_sv$cut_data <- cut_data
  
  # Create labels for the breaks
  cut_labels <- paste0("[", pmax(round(type_breaks[-length(type_breaks)], 0), 0), 
                       ", ", round(type_breaks[-1], 0), "]")
  
  ## ggplot ## 
  ggplot()+
    geom_spatvector(data = plot_sv, aes(fill = factor(cut_data)))+
    scale_fill_manual(values = colors, 
                      #name =  # str_to_title(breaks_type) , 
                      labels = cut_labels)+
    labs(
      fill = "Species*Years",
      title = paste0(scale, " Biodiversity Impact per Ecoregion (",taxa,")"))+
    theme_minimal() +
    theme(
      # set plot title size and center it
      plot.title = element_text(size = 16, hjust = 0.5),
      # put legend in the bottom right
      legend.position = 'none') # comment out if you want the legend
  # Un-commment below to re-add legend 
      # legend.position = c(0.15, 0.2),
      # legend.title = element_text(size = 14),
      # legend.text = element_text(size = 10))
  
  # save plot
  ggsave(filename = paste0(folder_fig_biodiv, "/", "gg_",
                           str_to_lower(scale),
                           "_impact_",
                           str_to_lower(taxa),
                           "_nolegend.png"),
         width = 14, height = 6, dpi = 300)
  
}

## 3.2) Clean & Map -------- 

### 3.2.1 Regional ------------- 
# get only the median columns
df_reg_agg <- df_rawch_s_eco_reg %>% 
  select(rawch_SOY, eco_code, ID, 
         contains("Median"),
         -TaxaAggregated_AnnualCrops_Median
  )

# clean and re-merge with Spatial Vector of Ecoregions (sv_eco)
df_reg_agg2 <- F_clean_calc(df_reg_agg)
sv_reg_calc <- merge(sv_eco, df_reg_agg2)

# get list of columns / attributes for the things we want to map
sv_names <- names(sv_reg_calc)[str_detect(names(sv_reg_calc), "AnnualCrops")] # get all columns containing "AnnualCrops" string (we removed "Aggregated" in the previous section)

# Loop over each layer in sv_reg_calc and make a map of the impact per taxa
for (layer_name in sv_names) {
  F_map(
    plot_sv = sv_reg_calc, 
    layer_choice = layer_name, 
    scale = "Regional", 
    breaks_n = 7, 
    breaks_type = "fisher"
  )
  }

### 3.2.2 Global -----------
# get spatial vector from either global or regional
df_global_agg <- df_rawch_s_eco_global %>% 
  select(rawch_SOY, eco_code, ID, 
         contains("Median"),
         -TaxaAggregated_AnnualCrops_Median
         #TaxaAggregated_AnnualCrops_Median
  )

# clean and re-merge
df_global_agg2 <- F_clean_calc(df_global_agg)
sv_global_calc <- merge(sv_eco, df_global_agg2)

# Loop over each layer in sv_global_calc and make a map of the impact per taxa
for (layer_name in sv_names) {
  F_map(
    plot_sv = sv_global_calc, 
    layer_choice = layer_name, # uses the same names as regional
    scale = "Global", 
    breaks_n = 7, 
    breaks_type = "fisher"
  )
}

# END ################################