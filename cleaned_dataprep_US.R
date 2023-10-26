# Title: dataprep_US.R
# Author: Nick Manning
# Purpose: Import US yield, production, and area harvested data from USDA and
# do some EDA and mapping to determine the best way to display this data
# Creation Date: 8/24/23
# Last Updated: October 2023

# Links: 
## StackExchange link for plotting fifty_states: https://stackoverflow.com/questions/50499363/how-to-draw-the-outline-of-multiple-us-states-in-r
## str_split help: https://statisticsglobe.com/extract-substring-before-or-after-pattern-in-r
## melting data link: 

# Requires: 
## CSV of yield data from Iman (SIMPLEG_USBR\Data_Source\US_yield_data\USyielddata.csv)


# Sections: 

# 0: Import Libraries & Set Constants

# 1: Tidy & Plot USDA Data
## This section imports and tidies the data, as well as produced initial maps of 
## 2011 to 2012 change 

# 2: Box Plots
## This section plots the data in ways outside of maps, such as boxplots  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  


# 0: Import Libraries & Set Constants ---------------------------------------
rm(list = ls())

## Load Libraries -----
library(dplyr)
library(ggplot2)
library(sf) 
library(stringr) # for str_pad() & str_to_title()
library(tigris) # for FIPS database
library(fiftystater) # for us state map; note: had to install from GitHub for my current R, version 4.2.2 
library(RColorBrewer)
library(classInt) # for mapping and setting breaks 
library(reshape2)


## Set Constants -----

## paths and files 
file_path <- "../Data_Source/US_yield_data/"

## states of interest 
mw_st_abv <- c("ND", "SD", "NE","KS", "MO", "IA", "MN", "WI", "IL", "IN", "OH", "MI")

mw_st_full = c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
          "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN")


# 1: Tidy & Plot USDA Data ------------------------------------------------------

## 1.1: Import and Tidy Data ---------------------

# download us counties shapefile to merge here (only have points as of now)
counties <- tigris::counties(
  state = str_to_title(mw_st_full),
  # NOTE: comment out cb and resolution below for finer resolution
  cb = T,
  resolution = "5m" # could be "500k" or "20m"
) %>% select("GEOID", "STUSPS", "NAME")


# import USDA data from CSV
getwd()

df_raw <- read.csv(paste0(file_path, "USyieldData.csv"))

df <- df_raw

# explore data
names(df)

# set "." to NA and check how many, then remove FIPS with no long/lat
df[df == "."] <- NA
sum(is.na(df$longitude))

# drop NA's in long/lat
df <- df %>% subset(!is.na(longitude))

# make it so all FIPS are 5 characters with 0 leading any 4 digit FIPS
df$fips <- as.character(df$fips)
df$fips <- str_pad(df$fips, 5, pad = "0")

# add the shapefile and set as 'sf' type
df <- df %>% left_join(counties, by = c("fips" = "GEOID"))
df <- st_as_sf(df)

# filter to only MW states
df <- df %>% filter(STUSPS %in% mw_st_abv)
colnames(df)

# transform all to numeric, they were in character
df <- df %>% 
  rename("state" = "STUSPS", "name" = "NAME") %>% 
  mutate_at(vars(contains('corn')), as.numeric) %>% 
  mutate_at(vars(contains('soybeans')), as.numeric) 
  

# select only area harvested, production, and yield
df <- df %>% 
  select("year", "state", "name", "fips", 
         "cornArea", "cornAreaPlanted", "cornProd", "cornYield",
         "soybeansArea", "soybeansAreaPlanted", "soybeansProd", "soybeansYield") %>% 
  rename("cornAreaHarvested" = "cornArea",
         "soybeansAreaHarvested" = "soybeansArea")
str(df)

# create corn/soy data by combining production and area harvest, then calculating yield 
df <- df %>% 
  mutate(
    cornSoyAreaHarvested = cornAreaHarvested + cornAreaHarvested,
    cornSoyAreaPlanted = cornAreaPlanted + soybeansAreaPlanted,
    cornSoyProd = cornProd + soybeansProd,
    cornSoyYield = cornSoyProd/cornSoyAreaPlanted
  )


## 1.2: Initial Mapping --------------
# load mw states for outline
# Stack Exchange link: https://stackoverflow.com/questions/50499363/how-to-draw-the-outline-of-multiple-us-states-in-r
fifty_states <- fifty_states

# get sf of all fifty states
sf_fifty <- sf::st_as_sf(fifty_states, coords = c("long", "lat")) %>% 
  group_by(id, piece) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>% 
  ungroup()

# filter to US-MW
midwest <- sf_fifty %>%
  filter(id %in% tolower(mw_st_full))

# set CRS 
midwest <- st_set_crs(midwest, 4269)

# get map of county outlines 
ggplot(df)+
  geom_sf(aes(color = state))+
  theme_bw()+
  labs(title = "Counties in US-Midwest", color = "State")

# save 
ggsave("../Figures/counties_usmw.png")


## 1.3 Mapping Yield, Prod, Area ---------

# set constants and filter data 
yr_one <- 2012
yr_min <- 2007
yr_range <- 2010:2013

col_border <- "lightgray"

# get one year and filter to 2007 and beyond (for 9 facets)
df <- df %>% filter(year >= yr_min)

df_yr <- df %>% filter(year == yr_one)

df_range <- df %>% filter(year %in% yr_range)




## 1.4: Plot Changes from 2011-2012 in corn, soy, corn/soy --------

### 1.4.1 Calculate Changes ----
df_diff <- df_range %>% 
  group_by(state, name) %>%
  mutate(
    # Difference = 2012 - 2011 per county
    # cornSoyDiffYield = cornSoyYield - lag(cornSoyYield),
    # cornSoyDiffProd = cornSoyProd - lag(cornSoyProd),
    # cornSoyDiffArea = cornSoyAreaHarvested - lag(cornSoyAreaHarvested),
    
    # Percent Change = ( (2012value - 2011value) / 2011value ) *100
    
    # cornSoy
    cornSoyDiffPctYield = ((cornSoyYield - lag(cornSoyYield))/lag(cornSoyYield))*100,
    cornSoyDiffPctProd = ((cornSoyProd - lag(cornSoyProd))/lag(cornSoyProd))*100,
    cornSoyDiffPctAreaHarvested = ((cornSoyAreaHarvested - lag(cornSoyAreaHarvested))/lag(cornSoyAreaHarvested))*100,
    cornSoyDiffPctAreaPlanted = ((cornSoyAreaPlanted - lag(cornSoyAreaPlanted))/lag(cornSoyAreaPlanted))*100,
    
    # Corn
    cornDiffPctYield = ((cornYield - lag(cornYield))/lag(cornYield))*100,
    cornDiffPctProd = ((cornProd - lag(cornProd))/lag(cornProd))*100,
    cornDiffPctAreaHarvested = ((cornAreaHarvested - lag(cornAreaHarvested))/lag(cornAreaHarvested))*100,
    cornDiffPctAreaPlanted = ((cornAreaPlanted - lag(cornAreaPlanted))/lag(cornAreaPlanted))*100,
    
    # Soy
    soyDiffPctYield = ((soybeansYield - lag(soybeansYield))/lag(soybeansYield))*100,
    soyDiffPctProd = ((soybeansProd - lag(soybeansProd))/lag(soybeansProd))*100,
    soyDiffPctAreaHarvested = ((soybeansAreaHarvested - lag(soybeansAreaHarvested))/lag(soybeansAreaHarvested))*100,
    soyDiffPctAreaPlanted = ((soybeansAreaPlanted - lag(soybeansAreaPlanted))/lag(soybeansAreaPlanted))*100
  ) %>% 
  select(year, state, name,
         cornDiffPctYield, cornDiffPctProd, cornDiffPctAreaHarvested, cornDiffPctAreaPlanted,
         soyDiffPctYield, soyDiffPctProd, soyDiffPctAreaHarvested, soyDiffPctAreaPlanted,
         cornSoyDiffPctYield, cornSoyDiffPctProd, cornSoyDiffPctAreaHarvested, cornSoyDiffPctAreaPlanted
         ) %>% 
  na.omit()


# get just the changes from 2011-2012
df_diff_2012 <- df_diff %>% filter(year == yr_one)


## 1.4.2: Create Plotting Fxn
F_plot_gg_diffpct <- function(data, var, yr){
  
  # filter and set variables
  data <- data %>% filter(year == yr)
  y_var <- as.character(var)
  
  # get "soy" from "soyDiffPctYield
  y_var_crop <- as.character(sub("Diff.*","",y_var))
  
  # get "Yield" from soyDiffPctYield
  y_var_metric <- as.character(sub(".*Pct", "", y_var))
  
  # if/else to fix metric for plotting
  if (y_var_metric == "AreaHarvested"){y_var_metric <- "Area Harvested"}
  if (y_var_metric == "Prod"){y_var_metric <- "Production"}
  if (y_var_metric == "AreaPlanted"){y_var_metric <- "Area Planted"}
  
  # set classes with equal intervals breaks for consistency
  # NOTE: for fisher breaks use "class <- classIntervals(round(data[[y_var]]), n = 11, style = "fisher")"
  class <- classIntervals(
    data[[y_var]], 
    fixedBreaks = 
      c(-100, -80, -60, -40, -20, -1,1, 20, 40, 60, 80, 100),  
    style = "fixed")
  
  # set new column with the breaks for mapping, this bins the data 
  data$DiffCut <- cut(data[[y_var]], class$brks, include.lowest = T)
  
  # plot changes
  p <- ggplot(data)+
    # fill with the binned data
    geom_sf(aes(fill = DiffCut), col = "darkgray", linewidth = 0.02)+
    theme_bw()+
    # add colorblind-friendly palette and NA value color
    scale_fill_brewer(palette = "PiYG", direction = 1, drop = F, na.value="gray50")+
    labs(
      title = paste("US-MW % Change in",
        str_to_title(paste(
          y_var_crop, y_var_metric,
          "from", yr-1, "to", yr))),
      fill = "% Change"
      )

  # save figure
  ggsave(paste0("../Figures/shock_eda/",
                "gg_", y_var, "_", yr-1, yr,
                ".png"), 
         plot = p)
  
  return(p)
  
}

# test
F_plot_gg_diffpct(df_diff, "cornSoyDiffPctAreaPlanted", yr_one)

# get all columns except the constants
p_colnames <- colnames(df_diff)
p_colnames <- p_colnames[! p_colnames %in% c('year', 'state', 'name', 'geometry')]

# run fxn over all columns
lapply(p_colnames, F_plot_gg_diffpct, data = df_diff, yr = yr_one)


# # # # # # # # # # # # # ## # # # # 

# 2: Box Plots --------------------------------------------------

## 2.1: Prep Data by Melting --------------------
# Melt link: https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
 
# get just one year of data and change from sf to df
df_diffpct_melt <- df_diff %>%
  filter(year == yr_one) %>% 
  st_drop_geometry()

# melt to change to long
df_diffpct_melt <- melt(df_diffpct_melt, id.vars = c("year", "state", "name"),
                      variable.name = "DiffPctVar", value.name = "DiffPct")

# get just the crop name and remove "DiffPct" 
df_diffpct_melt <- df_diffpct_melt %>% 
  mutate(
    # remove Diff and everything that comes after, then save as lowercase; e.g. "cornDiffPctYield" becomes "corn"
    crop = str_to_lower(sub("Diff.*", "", DiffPctVar)),
    crop = as.factor(crop),
    
    # remove DiffPct; e.g. cornAreaPlanted     
    DiffPctVar = gsub(pattern = "DiffPct", replacement = "", x = DiffPctVar),
    
    # add a space between the words and change to title case; e.g. cornYield becomes Corn Yield
    DiffPctVar_t = str_to_title(gsub('([[:upper:]])', ' \\1', DiffPctVar)),
    DiffPctVar_t = sub("Corn Soy", "CornSoy", DiffPctVar_t)
    )

## 2.2 : Boxplot all columns -------------
# Plot with all columns 
ggplot(data = df_diffpct_melt, aes(x=str_to_lower(crop), y=DiffPct)) + 
  geom_boxplot(aes(fill=DiffPctVar_t))+
  geom_jitter(alpha = 0.05, aes(color = crop))+
  guides(color = "none")+
  theme_light()+
  labs(
    y = paste("% Difference from",
              df_diffpct_melt$year-1, "to",
              df_diffpct_melt$year),
    fill = "Crop Variable"
  )+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text())

# save 
ggsave("../Figures/shock_eda/box_all_diffpct.png", 
       width = 12, height = 8)


## 2.3 Boxplot with just Yield -----

# filter to yield
df_diffpct_melt_y <- df_diffpct_melt %>% 
  filter(str_detect(DiffPctVar, "Yield"))


# plot just yield 
ggplot(data = df_diffpct_melt_y, aes(x=str_to_lower(crop), y=DiffPct)) + 
  geom_boxplot()+
  geom_jitter(alpha = 0.25, aes(color = crop))+ # add transparent points
  guides(color = "none")+
  theme_bw()+
  labs(
    y = "% Difference",
    title = "% Change in Yield from 2011 to 2012 in the US-MW"
  )+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text())

# save
ggsave("../Figures/box_yield_diffpct.png", 
       width = 12, height = 8)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  

# GRAVEYARD ----------------------------------------------------------------------------------------------------

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# 
# 
# ## Graveyard 1: Facet Wrap -------------------
# ### 1.3.2: facet_wrap by year ---------
# 
# # facet_wrap by year
# 
# # plotting function
# F_plot_facet <- function(data, var){
#   y_var <- as.character(var)
#   p <- ggplot(data)+
#     geom_sf(# %>% filter(year == 2010), 
#       aes(fill = .data[[y_var]]), 
#       #col = "lightgrey", 
#       lwd = 0, col = NA)+
#     #geom_sf(data = midwest, col = col_border, lwd = 1, fill = NA)+
#     theme_bw()+
#     theme(
#       # #remove ticks
#       # axis.ticks = element_blank(),
#       # axis.text= element_blank(), 
#       # axis.line = element_blank(),
#       # panel.border = element_blank(),
#       # # make transparent
#       # panel.grid.major = element_line(color='transparent'),
#       # panel.grid.minor = element_line(color='transparent'),
#       # panel.background = element_blank(),
#       # plot.background = element_rect(fill = "transparent",color='transparent'),
#       
#       plot.title = element_text(size=18, hjust = 0.5),
#       legend.position="bottom", legend.box = "horizontal", 
#       #legend.justification='right',
#       #legend.title = element_text(size=13), #change legend title font size
#       #legend.text = element_text(size=11)
#     )+
#     scale_fill_distiller(palette = "Greens", direction = 1
#     )+
#     labs(title = paste("US-MW", var))+
#     facet_wrap("year")
#   
#   ggsave(paste0("../Figures/shock_eda/",
#                 y_var, "_",
#                 min(data$year),max(data$year),
#                 ".png"), 
#          plot = p)
#   
#   return(p)
#   
# }
# 
# ### 1.3.2: Apply Facet Wrap ------
# F_plot_facet(df_range, "soybeansProd")
# 
# # get prod, area, yield vars
# var_names <- c("cornAreaHarvested", "cornProd", "cornYield", "cornAreaPlanted",
#                "cornAreaHarvested", "soybeansProd",  "soybeansYield", "soybeansAreaPlanted",
#                "cornSoyAreaHarvested", "cornSoyProd", "cornSoyYield", "cornSoyAreaPlanted"
# ) 
# 
# # # # UNCOMMENT TO RUN OVER ALL VARIABLES # # #
# # lapply(var_names, F_plot_facet, data = df_range)
# 
# 
# ### 1.4.2 Plot using facet_wrap ----
# F_plot_facet(df_diff, "cornSoyDiffPctYield")
# 
# 
# 
# 
# 
# 
# ## Graveyard 2: Map Single Year Single Var --------
# 
# # Function for one year of interest and one variable
# F_plot_single <- function(data, var, yr){
#   
#   y_var <- as.character(var)
#   data <- data %>% filter(year == yr)
#   
#   p <- ggplot(data)+
#     geom_sf(# %>% filter(year == 2010), 
#       aes(fill = .data[[y_var]]), 
#       #col = "lightgrey", 
#       lwd = 0, col = NA)+
#     #geom_sf(data = midwest, col = col_border, lwd = 1, fill = NA)+
#     theme_bw()+
#     theme(
#       # #remove ticks
#       # axis.ticks = element_blank(),
#       # axis.text= element_blank(), 
#       # axis.line = element_blank(),
#       # panel.border = element_blank(),
#       # # make transparent
#       # panel.grid.major = element_line(color='transparent'),
#       # panel.grid.minor = element_line(color='transparent'),
#       # panel.background = element_blank(),
#       # plot.background = element_rect(fill = "transparent",color='transparent'),
#       
#       plot.title = element_text(size=18, hjust = 0.5),
#       legend.position="bottom", legend.box = "horizontal", 
#       #legend.justification='right',
#       #legend.title = element_text(size=13), #change legend title font size
#       #legend.text = element_text(size=11)
#     )+
#     scale_fill_distiller(palette = "Greens", direction = 1
#     )+
#     labs(title = paste(yr, "US-MW", var))
#   
#   ggsave(paste0("../Figures/shock_eda/",
#                 y_var, "_", yr,
#                 ".png"), 
#          plot = p)
#   
#   return(p)
#   
# }
# 
# 
# 
# 
# 
# ### Run and apply function ------
# F_plot_single(df_range, "cornSoyProd", yr = yr_one)
# 
# # get prod, area, yield vars
# var_names <- c("cornAreaHarvested", "cornProd", "cornYield", "cornAreaPlanted", 
#                "cornAreaHarvested", "soybeansProd",  "soybeansYield", "soybeansAreaPlanted",
#                "cornSoyAreaHarvested", "cornSoyProd", "cornSoyYield", "cornSoyAreaPlanted") 
# 
# # # # UNCOMMENT TO RUN OVER ALL VARIABLES # # #
# 
# #### TO-DO: Fix Legend Labels and Title ###
# 
# #lapply(var_names, F_plot_single, data = df_range, yr = 2010)
# 
# # Graveyard 3: Time Series & Trends --------
# 
