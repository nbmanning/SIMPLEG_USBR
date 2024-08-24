# Title: processResults_SIMPLEG_2_maizesoy_MS.R
# Purpose: Read and plot SIMPLE-G results from text format across US, World, BR, and Cerrado

# Initial date: Aug 23, 2024
# Last edited: Aug 2024

# REQUIRES:
## SIMPLE-G Result files as '.txt' from 'processResults_SIMPLEG_1.R'
## RUN processResults_SIMPLEG_1.R
## RUN aggStats_Mapbiomas.R


# NOTES:
## Create the following folders in your local 'Results' directory:
### 'raster' which houses the results as rasters to pull into a GIS
### 'summary_tables' which houses a table with the stats for each area (min, mean, median, 1st & 3rd Quartiles, max, and NA's)
### 'stat_summary' which houses the raw values for changes in cropland area and production as an R data file to bring into another script   

# Next Steps -----
## Add code from impexp
## Add spatial code for US, Brazil, Cerrado
## Add summary statistics code
## Run to make sure everything works without clamp code

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 0: Load Libraries & Set Constants ---- 
rm(list = ls())

## Libraries -----
library(tidyverse)
library(raster) # use for initial raster stack and basic plotting
library(terra) # use to wrangle geospatial data and plot
library(RColorBrewer) # use for adding colorblind-friendly color palettes 
library(rasterVis) # use for easy violin plot 
library(reshape2) # use for melting data to then use ggplot
library(sf)
library(tidyterra) # plot using ggplot() instead of base R with 'terra'
library(ggspatial) # N arrow and Scale Bar w tidyterrra
#library(rworldmap) # getting simple BR Border


## Constants ------

# NOTE: change this when you change the result file to one of three TXT files
## hi: enter "_hi" ;
## med: enter "_m" ;
## lo: enter "_lo" ;
## out / default; enter ""


### Loading & Saving ###
# Define the model date 
# NOTE: Assumes the results are downloaded and saved in YYYY-MM-DD format
date_string <- "2024-03-03"
date_string_nodash <- gsub("-", "", date_string)

# Set model version & parameter flexibility
datafile_version <- "sg1x3x10_v2402_US_Heat"
pct <- "_m" # change when you change 'datafile'

#pct_title <- " - Med" # for plotting, either " - High" or " - Low" or "" or "- Med"
pct_title <- "" # note: changed Aug 2024 by setting -med to nothing, as it is the default

# create vars to house results
folder_der <- "../Data_Derived/"
folder_der <- paste0(folder_der, date_string, "/")

folder_fig <- "../Figures/"
folder_fig <- paste0(folder_fig, date_string, "/")

folder_results <- paste0("../Results/SIMPLEG-", date_string, "/")
folder_stat <- paste0(folder_results, "stat_summary/")

### Plotting ###
size_title = 2.0
size_labels = 1.5
size_axis_nums = 1.0


# 0: Functions -------

## 0.1: ImpExp Functions --------

## 0.2: Spatial Analysis for Areas of Interest ------- 
# Fxn to Create and Save Violin Plots 
# Note that the SI code is similar but includes histograms and violin plots for more variables 
F_p_violin <- function(df, area){

  ## subset and change names ##
  
  # separate each for Maize and Soy
  df_pct_maizesoy <- df %>% 
    subset(c("pct_LND_MAZ", "pct_LND_SOY")) 
  names(df_pct_maizesoy) <- c("Maize", "Soy")
  
  df_rawch_maizesoy <- df %>% 
    subset(c("rawch_MAZ", "rawch_SOY"))
  names(df_rawch_maizesoy) <- c("Maize", "Soy")
  
  # violin plots for % change and raw change for maize and soy 
  # set size_title, size_labels, and size_axis_nums in the "Constants" section
  p1 <- bwplot(df_pct_maizesoy, 
               main = list(paste(area, "% Change Maize & Soy", pct_title), cex = size_title),
               ylab = list("% Change", cex = size_labels),
               scales=list(
                 x = list(rot=45, cex = size_labels),
                 y = list(cex = size_axis_nums))
  )
  
  p2 <- bwplot(df_rawch_maizesoy, 
               main = list(paste(area, "Raw Change Maize & Soy", pct_title), cex = size_title),
               ylab = list("Area (1000 ha)", cex = size_labels),
               scales=list(
                 x = list(rot=45, cex = size_labels),
                 y = list(cex = size_axis_nums))
  )
  
  # save figures as PNGs - filename will include the area (e.g. US, Brazil) and model elasticity version (e.g. l, m, or h)
  png(filename = paste0(folder_fig, str_to_lower(area), pct, "_bw", "_pctchange", "_maizesoy", ".png"))
  plot(p1)
  dev.off()
  
  png(filename = paste0(folder_fig, str_to_lower(area), pct, "_bw", "_rawchange", "_maizesoy", ".png"))
  plot(p2)
  dev.off()
  
  # also plot these figures in the code window
  return(p1)
  return(p2)
}


# Fxn to incorporate both of these into one function
# Note that we exclude the clamping to 50,000 here as we fixed this step in our analysis
F_aoi_prep <- function(shp, area_name){
  
  ## Clip to AOI Extent ##
  # get extent as terra object for plotting
  ext_area <- vect(ext(shp))
  
  # crop and masking to just the extent of interest
  r_aoi <- terra::crop(r, ext_area, mask = T) 
  r_aoi <- mask(r_aoi, shp)
  
  ## NICK: Not sure if we need any of this since we aren't clamping anymore - this other section just re-orders the layers
  # ## Call Count and Clamp functions to cap the grid cells at 50,000 ##
  # # QLAND #
  # F_count_invalid(r_aoi, "new_QLAND")
  # r_aoi_new_qland <- F_clamp(r_aoi, "new_QLAND")
  # 
  # # QCROP #
  # F_count_invalid(r_aoi, "new_QCROP")
  # r_aoi_new_qcrop <- F_clamp(r_aoi, "new_QCROP")
  # 
  # # Crop Specific
  # F_count_invalid(r_aoi, "new_LND_MAZ")
  # r_aoi_new_maize <- F_clamp(r_aoi, "new_LND_MAZ")
  # 
  # # 
  # F_count_invalid(r_aoi, "new_LND_SOY")
  # r_aoi_new_soy <- F_clamp(r_aoi, "new_LND_SOY")
  # 
  # # get other layers
  # r_aoi_pct_qland <- r_aoi %>% subset("pct_QLAND")
  # r_aoi_rawch_qland <- r_aoi %>% subset("rawch_QLAND")
  # 
  # r_aoi_pct_qcrop <- r_aoi %>% subset("pct_QCROP")
  # r_aoi_rawch_qcrop <- r_aoi %>% subset("rawch_QCROP")
  # 
  # r_aoi_pct_maize <- r_aoi %>% subset("pct_LND_MAZ")
  # r_aoi_rawch_maize <- r_aoi %>% subset("rawch_MAZ")
  # 
  # r_aoi_pct_soy <- r_aoi %>% subset("pct_LND_SOY")
  # r_aoi_rawch_soy <- r_aoi %>% subset("rawch_SOY")
  # 
  # # re-stack and re-order
  # r_aoi <- c(
  #   r_aoi_new_qland, r_aoi_pct_qland, r_aoi_rawch_qland,
  #   r_aoi_new_qcrop, r_aoi_pct_qcrop, r_aoi_rawch_qcrop,
  #   r_aoi_new_maize, r_aoi_pct_maize, r_aoi_rawch_maize,
  #   r_aoi_new_soy, r_aoi_pct_soy, r_aoi_rawch_soy
  # )
  # 
  # save clipped and clamped raster with new AOI 
  saveRDS(r_aoi, file = paste0(folder_der, "r", pct, "_", area_name, ".rds"))
  
  # return as result
  return(r_aoi)
}

# Fxn to calculate total % Change
F_calc_pct_change <- function(final, raw_ch){
  
  # we don't have initial, so we calculate it here
  initial = final - raw_ch
  
  # then we calculate percent change (results are in %)
  pct_change = ((final - initial)/initial)*100
  print(paste0("% Change is: ", pct_change, "%"))
}


# Fxn to get summary of data, call the violin fxn, and plot a basic map
F_EDA <- function(r_aoi, area_name){  
  # get and save a summary table
  table_area <- summary(r_aoi, size = 1000000000) # set size to not use a sample
  print(table_area)
  
  # set variable for file path
  tables_file <- paste0(folder_results, "summary_tables/")
  
  # essentially says, "if no file name contains the search string, create a folder with that string"
  if (!(any(grepl("summary_tables", folder_results)))) {
    dir.create(tables_file)
    # print if the fuile already exists or we created a new one
    cat("Folder ", tables_file, "created.\n")
  } else {
    cat("Folder ", tables_file, "already exists.\n")
  }
  
  # actually create the table as a CSV
  write.csv(table_area, file = paste0(folder_results, "summary_tables/", 
                                      "table_", area_name, pct, "_", date_string_nodash, ".csv"))
  
  print("Totals for Casc. Effect Graph and for Total Change")
  
  # Print total change values then calculate % Change
  cat("\n\nTotal Land Change (1000 ha)\n\n")
  print(global(r_aoi$new_QLAND, fun = "sum", na.rm = T))
  print(global(r_aoi$rawch_QLAND, fun = "sum", na.rm = T))
  
  # Calc % change by grabbing the total (sum) values and running the % change function
  F_calc_pct_change(
    final = (global(r_aoi$new_QLAND, fun = "sum", na.rm = T))[[1]],
    raw_ch = (global(r_aoi$rawch_QLAND, fun = "sum", na.rm = T))[[1]]
  )
  
  # print the total change in crop production
  cat("\n\nTotal Production Change (1000 tons CE)\n\n")
  print(global(r_aoi$rawch_QCROP, fun = "sum", na.rm = T))
  
  # print the total changes in crop production for maize
  cat("\n\n Maize Results\n\n")
  print(global(r_aoi$new_LND_MAZ, fun = "sum", na.rm = T))
  print(global(r_aoi$rawch_MAZ, fun = "sum", na.rm = T))
  F_calc_pct_change(
    final = (global(r_aoi$new_LND_MAZ, fun = "sum", na.rm = T))[[1]],
    raw_ch = (global(r_aoi$rawch_MAZ, fun = "sum", na.rm = T))[[1]]
  )
  
  
  # print the total changes in crop production for soy  
  cat("\n\n Soy Results\n\n")
  print(global(r_aoi$new_LND_SOY, fun = "sum", na.rm = T))
  print(global(r_aoi$rawch_SOY, fun = "sum", na.rm = T))
  F_calc_pct_change(
    final = (global(r_aoi$new_LND_SOY, fun = "sum", na.rm = T))[[1]],
    raw_ch = (global(r_aoi$rawch_SOY, fun = "sum", na.rm = T))[[1]]
  )
  
  # Call EDA fxn to get and save violin plots 
  F_p_violin(r_aoi, area_name)
  
  # Plot basic initial maps - removed as this takes a lot of time 
  # terra::plot(r_aoi, 
  #             axes = F#, 
  #             #type = "interval"
  # )
}

# 1: Import / Export Plot --------

## TO-DO 8/24/24 ##################################

# see barplot_impexp_maizesoy.R
# 5: Facet Plot 2 ---
# plot with the individual plots next to one another
# labels give "A" and "B"
(p <- plot_grid(p_imp, p_exp, labels = "AUTO"))
ggsave(paste0(folder_fig, "bar_impexp.png"),
       p,
       width = 12, height = 6)

# 2: Load Shapefiles & SIMPLE-G Raster -----------------------------------------------------------

### RUN 'processResults_SIMPLEG_1.R' FIRST TO CREATE RASTER AND SHAPEFILES ###

# load files and maize-soy raster
load("../Data_Derived/shp_usbr.RData")
shp_ecoregions <- st_read("../Data_Source/wwf_ecoregions/agg_wwf_terr_ecos.shp")
shp_countries <- shp_world %>% dplyr::select(name_long)

r <- readRDS(file = paste0(folder_der, "r_maizesoy", pct, ".rds"))

# print cropland area in ha by getting the sum of each grid-cell value
print(global(r$new_QLAND, fun = "sum", na.rm = T))

# 3: Edit Stack & Check Values -------------------

## 3.1: Calc & Add Raw Change from % and New -------
# Formula: new - (new / ((pct_change/100)+1))

# subset 
r_pct_qland <- subset(r, "pct_QLAND")
r_new_qland <- subset(r, "new_QLAND")

r_pct_qcrop <- subset(r, "pct_QCROP")
r_new_qcrop <- subset(r, "new_QCROP")

r_pct_maize <- subset(r, "pct_LND_MAZ")
r_new_maize <- subset(r, "new_LND_MAZ")

r_pct_soy <- subset(r, "pct_LND_SOY")
r_new_soy <- subset(r, "new_LND_SOY")

# NOTE: rawch = Raw Change
r_rawch_qcrop <- r_new_qcrop - (r_new_qcrop / ((r_pct_qcrop/100)+1))
r_rawch_qland <- r_new_qland - (r_new_qland / ((r_pct_qland/100)+1))

r_rawch_maize <- r_new_maize - (r_new_maize / ((r_pct_maize/100)+1))
r_rawch_soy <- r_new_soy - (r_new_soy / ((r_pct_soy/100)+1))

# add raw change layers back into stack
r <- c(
  r, 
  r_rawch_qcrop, r_rawch_qland,
  r_rawch_maize, r_rawch_soy
)

r

# set names 
names(r) #NOTE: make sure everything is in the right order below! 
names(r) <- c(
  "pct_QLAND", 
  "new_QLAND", 
  "pct_QCROP", 
  "new_QCROP",
  
  "pct_LND_MAZ",
  "pct_LND_SOY",
  "new_LND_MAZ",
  "new_LND_SOY",
  
  "rawch_QCROP", 
  "rawch_QLAND",
  "rawch_MAZ",
  "rawch_SOY"
)

# 4: World Results ---------

## 4.1: World EDA -----
# make a quick plot of the global results
# terra::plot(r, axes = F)

# Call fxn to clip and prep data 
r_row <- F_aoi_prep(shp = shp_world, area_name = "World")

# call fxn to create EDA plots and generate stats of the clipped data 
F_EDA(r_aoi = r_row, area_name = "World")

## 4.2: World Interval Plot --------

F_ggplot_interval <- function(df, title_text, title_legend, save_title){
  
  # plot 
  p <- ggplot() +
    geom_spatraster(data = df, maxcell = Inf, aes(fill = cats)) +
    scale_fill_whitebox_d(palette = "pi_y_g", direction = 1)+
    
    #geom_sf(data = vect(shp_ecoregions), color = "gray60", fill = "transparent", lwd = 0.1)+
    geom_sf(data = vect(shp_countries), color = "gray60", fill = "transparent", lwd = 0.1)+
    
    theme_minimal()+ 
    labs(
      fill = title_legend,
      title = title_text,
    )+
    
    #coord_sf(crs = "ESRI:53042")+ #Winkel-Tripel 
    #coord_sf(crs = "ESRI:53030")+ # Robinson
    
    theme(
      plot.title = 
        element_text(
          hjust = 0.5, 
          size = 22
        ),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 12)
    )  
  
  # save plot
  ggsave(plot = p, filename = paste0(folder_fig, "/", save_title),
         width = 14, height = 6, dpi = 300)
  
  return(p)
  
}

# example from: https://cloud.r-project.org/web/packages/tidyterra/tidyterra.pdf
# With discrete values
test3 <-  r_row %>%
  subset("rawch_QLAND")

# create df
factor <- test3 %>%
  # add column with break intervals
  mutate(
    cats =
      cut(rawch_QLAND,
          # manually set break intervals here 
          breaks = c(-5, -2.5, -1, -0.5, -0.01,
                     0.01, 0.1, 0.25, 0.5, 1))
    )

# run function to create plot
F_ggplot_interval(
  df = factor, 
  title_text = "Raw Change in Cropland Area",
  title_legend = "Area (1000 ha)",
  save_title = "gg_world_rawch_croplandarea.png")


# 4: US Results ---------
## TO-DO 8/24/24 ##################################

## 4.1: US EDA -------


## 4.2: US Interval Plot -------



# 5: Brazil Results --------

## 5.1: Brazil EDA -------


## 5.2: Brazil Interval Plot -------


# 6: Cerrado Results ----------

## 6.1: Brazil EDA -------


## 6.2: Brazil Interval Plot -------


# 7: Summary Statistics --------


# 8: Transition Results -----
# set relevant vegetation class categories 
list_from_lv3 <- c("Forest Formation", "Savanna Formation", "Wetland",
                   "Grassland", "Pasture", "Forest Plantation",
                   "Mosaic of Agriculture and Pasture",
                   "Magrove", "Flooded Forest",
                   "Shrub Restinga", "Other Non Forest Natural Formation", "Wooded Restinga",
                   "Perennial Crops")

# load clean and long MapBiomas Collection 8 data
#  NOTE: for steps on cleaning, see aggStats_MapBiomas.R Section 1
load(file = paste0(folder_derived, "mapb_col8_clean_long.Rdata"))

# filter Mapbiomas data to only focus on transitions to "Soybeans" & From-To's that do not stay the same
df <- df %>% 
  filter(to_level_4 == "Soy Beans") %>%
  filter(to_level_4 != from_level_4)

## 8.1: Facet Map of Cerrado Transition ----

### 8.1.1: Prep Spatial Data ---------

# NOTE: Municipality & Cerrado Shapefiles come from 'geobr' package 

# Load municipality shapefile 
# Read all municipalities in the country at a given year 
shp_muni <- read_municipality(code_muni="all", year=2018)

# Load Cerrado shapefile
shp_br_cerr <- read_biomes(
  year = 2019,
  simplified = F,
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

# get municipalities that are at all within the Cerrado
shp_muni_in_cerr <- st_intersection(shp_muni, shp_br_cerr)

# get just the codes column and keep as shapefile
shp_code_muni_in_cerr <- shp_muni_in_cerr %>% select(code_muni)

# get territory codes for municipalities in intersection as numeric
muni_codes_cerr <- shp_muni_in_cerr$code_muni

# filter to only municipalities in Cerrado
df_cerr <- df %>% 
  filter(geocode %in% muni_codes_cerr) %>% 
  filter(biome == "Cerrado")

### 8.1.2: Aggregate -------

# get aggregate sum of the entire Cerrado for stats
agg_cerr <- df_cerr %>% 
  aggregate(ha ~ year, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# get agg sum of certain 'from' classes for mapping
agg_cerrmuni_fromveg <- df_cerr %>%
  filter(from_level_3 %in% list_from_lv3) %>% 
  aggregate(ha ~ year + geocode, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# make shape -- from veg 
shp_cerrmuni_fromveg <- shp_code_muni_in_cerr %>% 
  left_join(agg_cerrmuni_fromveg,
            join_by(code_muni == geocode)) %>% 
  mutate(year = year(year)) %>% 
  filter(year >= 2012 & year <= 2017)

### 8.1.3: Plot Facet Map -------
F_facet<-function(data, aoi, class, file_name){
  # plot
  p <- ggplot(data)+
    geom_sf(mapping = aes(fill = ha), color= NA)+
    scale_fill_distiller(palette = "YlOrRd", direction = 1)+
    facet_wrap("year")+
    coord_sf()+
    theme_minimal()+
    
    labs(
      title = paste("Land Transition Across", aoi),
      subtitle = paste(class),
      fill = "Transition (ha)")+
    
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  # save
  ggsave(filename = paste0(folder_plot, file_name),
         plot = p,
         width = 8, height = 8,
         dpi = 300)
  
  return(p)
}

# Run Fxn
F_facet(shp_cerrmuni_fromveg, 
        aoi = "Cerrado", 
        class = "From Relevant Vegetation Classes to Soybean", 
        file_name = "cerr_fromveg.png")


### 8.1.4: Save Stats -----
# print stats
print(agg_cerr %>% filter(year(year) >= 2013 & year(year) <= 2015))
print(agg_cerr_fromveg %>% filter(year(year) >= 2013 & year(year) <= 2015))

# save as variable 
stat_mapb_agg_trans_cerr <- agg_cerr %>% filter(year(year) == 2010 | year(year) >= 2013 & year(year) <= 2015)
stat_mapb_agg_trans_cerr_fromveg <- agg_cerr_fromveg %>% filter(year(year) == 2010 | year(year) >= 2013 & year(year) <= 2015)

# save as .Rdata file
save(
  stat_mapb_agg_trans_br, stat_mapb_agg_trans_br_fromveg,
  stat_mapb_agg_trans_cerr, stat_mapb_agg_trans_cerr_fromveg,
  file = paste0(folder_stat, "mapb_agg_land_trans_br_and_cerr.RData"))

## 8.2: Plot Line Plot of Cerrado Transition ------

# filter to only include the relevant classes 
classes_few <- c("Temporary Crops", "Forest Formation", "Mosaic of Agriculture and Pasture", 
                 "Pasture", "Savanna Formation", "Grassland")

df_cerr <- df_cerr %>% 
  aggregate(ha ~ year + biome + from_level_3 + to_level_4, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

df_cerr <- filter(df_cerr, from_level_3 %in% classes_few)


# plot line plot in Mha
ggplot(df_cerr, aes(x=year, y=ha/1000000, color = from_level_3)) +
  geom_line() +
  geom_point(fill = "white", size = 1.2) +
  xlab("")+
  scale_x_date(date_labels = "%Y")+
  labs(
    #title = "From X to Soybean", # remove title to plot with transition map
    y = "Land Change from Previous Year (Mha)",
    color = "From-To Transitions"
  )+
  # add vertical line in 2012
  geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
             linetype="dotted", linewidth=0.5)+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 17, hjust = 0.5)
  )

# save 
ggsave(paste("../Figures/trans_mapbiomas/cerr_to_soybean.png"), 
       width = 14, height = 7)


