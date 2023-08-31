# Title: dataprep_US.R
# Author: Nick Manning
# Purpose: Import US yield, production, and area harvested data from USDA and
# do some EDA and mapping to determine the best way to display this data
# Creation Date: 8/24/23
# Last Updated: August 2023

# Links: 

# Requires: 
## CSV of yield data from Iman (SIMPLEG_USBR\Data_Source\US_yield_data\USyielddata.csv)

# Steps (check meeting notes): 
# 0: Import Libraries & Set Constants
# 1: USDA Data from Iman
  ## 1.1: Import and Tidy Data
  ## 1.2: Initial Mapping
  ## 1.3: Initial EDA
    ###  1.3.1: Barplots
    ### 1.3.2: Time Series & Trends

# 2: Use tidyUSDA to get data & plot (get inspo from 1_data_import.R)
  ## 2.1 Yield 
  ## 2.2 Area Harvested
  ## 2.3 Production

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  


# 0: Import Libraries & Set Constants ---------------------------------------
rm(list = ls())

# libraries
library(dplyr)
library(ggplot2)
library(sf) 
library(stringr) # for str_pad
library(tigris) # for FIPS database
library(fiftystater) # for us state map; note: had to install from GitHub for my current R, version 4.2.2 
library(RColorBrewer)
library(classInt) # for mapping and setting breaks 

#library(terra)

# constants

## paths and files 
file_path <- "../Data_Source/US_yield_data/"

## states of interest 
mw_st_abv <- c("ND", "SD", "NE","KS", "MO", "IA", "MN", "WI", "IL", "IN", "OH", "MI")

mw_st_full = c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS", "MISSOURI",
          "IOWA", "MINNESOTA", "WISCONSIN", "ILLINOIS", "INDIANA", "OHIO", "MICHIGAN")

mw_st_tigris <- str_to_title(mw_st_full)


# 1: USDA Data from Iman -----------------------------------

## 1.1: Import and Tidy Data ---------------------

# import data
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

# download us counties shapefile to merge here (only have points as of now)
counties <- tigris::counties(
  state = mw_st_tigris,
  # NOTE: comment out cb and resolution below for finer resolution
  cb = T,
  resolution = "5m" # could be "500k" or "20m"
) %>% select("GEOID", "STUSPS", "NAME")

# add the shapefile and set as 'sf' type
df <- df %>% left_join(counties, by = c("fips" = "GEOID"))
df <- st_as_sf(df)

# filter to only MW states
df <- df %>% filter(STUSPS %in% mw_st_abv)
colnames(df)

# transform all to numeric, they were in character
df <- df %>% rename("state" = "STUSPS", "name" = "NAME") %>% 
  mutate_at(c("cornArea",              "cornAreaIrrig",        
              "cornAreaNonIrrig",      "cornAreaPlanted",       "cornPrice",            
              "cornProd",              "cornProdIrrig",         "cornProdNonIrrig",     
              "cornYield",             "cornYieldIrrig",        "cornYieldNonIrrig",    
              "soybeansArea",          "soybeansAreaIrrig",     "soybeansAreaNonIrrig", 
              "soybeansAreaPlanted",   "soybeansPrice",         "soybeansProd",         
              "soybeansProdIrrig",     "soybeansProdNonIrrig",  "soybeansYield",        
              "soybeansYieldIrrig",    "soybeansYieldNonIrrig"), as.numeric)

# select only area harvested, production, and yield
df2 <- df %>% select("year", "state", "name", "fips", 
                     "cornArea", "cornProd", "cornYield",
                     "soybeansArea", "soybeansProd", "soybeansYield")

# create corn/soy data by combining production and area harvest, then calculating yield 
df2 <- df2 %>% 
  mutate(
    cornsoyArea = cornArea + soybeansArea,
    cornsoyProd = cornProd + soybeansProd,
    cornsoyYield = cornsoyProd/cornsoyArea
  )

# write as shapefile for easier plotting - OMIT; DIDN'T WORK
# st_write(df2, dsn = paste0(file_path, "usda_cornsoy_hpy.tif"), driver = "ESRI Shapefile")

# set as sf for mapping - OMIT BECAUSE THIS JUST GETS POINTS FOR EACH COUNTY
# df <- st_as_sf(df, coords = c("longitude", "latitude"))
# df<- st_set_crs(x = df, value = 4326) # NOTE: maybe "EPSG:4326"??

# get data aggregated to state level (OMIT; DIDN'T WORK) -------
# 
# df_st_prodarea <- df2 %>% 
#   #select(-c("longitude", "latitude", "fips", "name")) %>% 
#   na.omit() %>% 
#   group_by(year, state) %>% 
#   summarise_at(vars(matches("Prod|Area")), sum)
# 
# df_st_yield <- df2 %>% 
#   #select(-c("longitude", "latitude", "fips", "name")) %>% 
#   na.omit() %>% 
#   group_by(year, state) %>% 
#   summarise_at(vars(matches("yield")), mean)
# 
# 
# ggplot(df_st_prodarea)+
#   geom_sf(aes(fill = soybeansProd))
# 
# # not worth it to have state level data to plot!!! 
# df_st <- left_join(st_drop_geometry(df_st_prodarea), 
#                    st_drop_geometry(df_st_yield), by = c("year", "state"))


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
ggplot(df2)+
  geom_sf(aes(color = state))+
  theme_bw()+
  labs(title = "Counties in US-Midwest", color = "State")

#save 
ggsave("../Figures/shock_eda/counties.png")


## 1.3 Mapping Yield, Prod, Area ---------

# set constants and filter data 
yr_one <- 2012
yr_min <- 2007
yr_range <- 2010:2013

col_border <- "lightgray"

# get one year and filter to 2007 and beyond (for 9 facets)
df2 <- df2 %>% filter(year >= yr_min)

df2_yr <- df2 %>% filter(year == yr_one)

df2_range <- df2 %>% filter(year %in% yr_range)

### 1.3.1: Map Single Year Single Var --------

# Test fxn 

# ggplot()+
#   geom_sf(data = df2,# %>% filter(year == 2010), 
#           aes(fill = soybeansYield), 
#           #col = "lightgrey", 
#           lwd = 0.1)+
#   geom_sf(data = midwest, col = col_border, lwd = 0.3, fill = NA)+
#   theme_bw()+
#   theme(
#     #axis.ticks = element_blank(),
#     #axis.text= element_blank(), 
#     axis.line = element_blank(),
#     panel.border = element_blank(),
#     panel.grid.major = element_line(color='transparent'),
#     panel.grid.minor = element_line(color='transparent'),
#     panel.background = element_blank(),
#     plot.background = element_rect(fill = "transparent",color='transparent'),
#     
#     plot.title = element_text(size=18, hjust = 0.5),
#     legend.position="bottom", legend.box = "horizontal", 
#     #legend.justification='right',
#     legend.title = element_text(size=13), #change legend title font size
#     legend.text = element_text(size=11)
#   )+
#   scale_fill_distiller(palette = "Greens", direction = 1
#   )+
#   labs(title = paste("US-MW", "soybeansYield", yr))


# Function for one year of interest and one variable
F_plot_single <- function(data, var, yr){
  
  y_var <- as.character(var)
  data <- data %>% filter(year == yr)
  
  p <- ggplot(data)+
    geom_sf(# %>% filter(year == 2010), 
      aes(fill = .data[[y_var]]), 
      #col = "lightgrey", 
      lwd = 0, col = NA)+
    #geom_sf(data = midwest, col = col_border, lwd = 1, fill = NA)+
    theme_bw()+
    theme(
      # #remove ticks
      # axis.ticks = element_blank(),
      # axis.text= element_blank(), 
      # axis.line = element_blank(),
      # panel.border = element_blank(),
      # # make transparent
      # panel.grid.major = element_line(color='transparent'),
      # panel.grid.minor = element_line(color='transparent'),
      # panel.background = element_blank(),
      # plot.background = element_rect(fill = "transparent",color='transparent'),
      
      plot.title = element_text(size=18, hjust = 0.5),
      legend.position="bottom", legend.box = "horizontal", 
      #legend.justification='right',
      #legend.title = element_text(size=13), #change legend title font size
      #legend.text = element_text(size=11)
    )+
    scale_fill_distiller(palette = "Greens", direction = 1
    )+
    labs(title = paste(yr, "US-MW", var))
  
  ggsave(paste0("../Figures/shock_eda/",
                y_var, "_", yr,
                ".png"), 
         plot = p)
  
  return(p)
  
}

### 1.3.1: Apply Single Year Single Var ------
F_plot_single(df2_range, "cornsoyProd", yr = 2012)

# get prod, area, yield vars
var_names <- c("cornArea", "cornProd", "cornYield", 
               "soybeansArea", "soybeansProd",  "soybeansYield",
               "cornsoyArea", "cornsoyProd", "cornsoyYield") 

# # # UNCOMMENT TO RUN OVER ALL VARIABLES # # #
##### TO-DO: Fix Legend Labels and Title ----
#lapply(var_names, F_plot_single, data = df2_range, yr = 2010)


### 1.3.2: facet_wrap by year ---------
# facet_wrap by year
# ggplot(df2)+
#   geom_sf(# %>% filter(year == 2010), 
#           aes(fill = soybeansYield), 
#           #col = "lightgrey", 
#           lwd = 0, col = NA)+
#   #geom_sf(data = midwest, col = col_border, lwd = 1, fill = NA)+
#   theme_bw()+
#   theme(
#     axis.ticks = element_blank(),
#     axis.text= element_blank(), 
#     axis.line = element_blank(),
#     panel.border = element_blank(),
#     panel.grid.major = element_line(color='transparent'),
#     panel.grid.minor = element_line(color='transparent'),
#     panel.background = element_blank(),
#     plot.background = element_rect(fill = "transparent",color='transparent'),
#     
#     plot.title = element_text(size=18, hjust = 0.5),
#     legend.position="bottom", legend.box = "horizontal", 
#     #legend.justification='right',
#     legend.title = element_text(size=13), #change legend title font size
#     legend.text = element_text(size=11)
#   )+
#   scale_fill_distiller(palette = "Greens", direction = 1
#   )+
#   labs(title = paste("US-MW soybeanYield"))+
#   facet_wrap("year")


# plotting function
F_plot_facet <- function(data, var){
  y_var <- as.character(var)
  p <- ggplot(data)+
    geom_sf(# %>% filter(year == 2010), 
      aes(fill = .data[[y_var]]), 
      #col = "lightgrey", 
      lwd = 0, col = NA)+
    #geom_sf(data = midwest, col = col_border, lwd = 1, fill = NA)+
    theme_bw()+
    theme(
      # #remove ticks
      # axis.ticks = element_blank(),
      # axis.text= element_blank(), 
      # axis.line = element_blank(),
      # panel.border = element_blank(),
      # # make transparent
      # panel.grid.major = element_line(color='transparent'),
      # panel.grid.minor = element_line(color='transparent'),
      # panel.background = element_blank(),
      # plot.background = element_rect(fill = "transparent",color='transparent'),
      
      plot.title = element_text(size=18, hjust = 0.5),
      legend.position="bottom", legend.box = "horizontal", 
      #legend.justification='right',
      #legend.title = element_text(size=13), #change legend title font size
      #legend.text = element_text(size=11)
    )+
    scale_fill_distiller(palette = "Greens", direction = 1
    )+
    labs(title = paste("US-MW", var))+
    facet_wrap("year")
  
  ggsave(paste0("../Figures/shock_eda/",
                y_var, "_",
                min(data$year),max(data$year),
                ".png"), 
         plot = p)
  
  return(p)
  
}

### 1.3.2: Apply Facet Wrap ------
F_plot_facet(df2_range, "soybeansProd")

# get prod, area, yield vars
var_names <- c("cornArea", "cornProd", "cornYield", 
               "soybeansArea", "soybeansProd",  "soybeansYield",
               "cornsoyArea", "cornsoyProd", "cornsoyYield"
               ) 

lapply(var_names, F_plot_facet, data = df2_range)

## 1.4: Plot Changes from 2011-2012 in corn, soy, corn/soy --------

### 1.4.1 Calculate Changes ----
df_diff <- df2_range %>% 
  group_by(state, name) %>%
  mutate(
    # Difference = 2012 - 2011 per county
    csDiffYield = cornsoyYield - lag(cornsoyYield),
    csDiffProd = cornsoyProd - lag(cornsoyProd),
    csDiffArea = cornsoyArea - lag(cornsoyArea),
    
    # Percent Change = ( (2012value - 2011value) / 2011value ) *100
    csDiffPctYield = ((cornsoyYield - lag(cornsoyYield))/lag(cornsoyYield))*100,
    csDiffPctProd = ((cornsoyProd - lag(cornsoyProd))/lag(cornsoyProd))*100,
    csDiffPctArea = ((cornsoyArea - lag(cornsoyArea))/lag(cornsoyArea))*100
  ) %>% 
  select(year, state, name, 
         #csDiffYield, csDiffProd, csDiffArea, 
         csDiffPctYield, csDiffPctProd, csDiffPctArea) %>% 
  na.omit()


# get just the changes from 2011-2012
df_diff_2012 <- df_diff %>% filter(year == 2012)


# NOTE: one major outlier. Let's fix this by setting a class interval

#classes <- classIntervals(df_diff_2012$csDiffPctProd, n = 11, style = "jenks")

classes <- classIntervals(round(df_diff_2012[["csDiffPctProd"]]), n = 11, style = "fisher")

#classes <- classIntervals(round(df_diff_2012$csDiffPctProd), style = "quantile", n = 11)

classes <- classIntervals(round(df_diff_2012$csDiffPctArea),
                          fixedBreaks =  
                            #c(min(df_diff_2012$csDiffPctYield), 
                            #seq(-100, 100, 20),
                          c(-40, -30, -20, -10, -1, 1, 10, 20, 30, 40),
                            #max(df_diff_2012$csDiffPctYield)),
                          style = "fixed")


# see breaks
classes$brks

# add breaks column
df_diff_2012 <- df_diff_2012 %>%
  mutate(csDiffPctAreaCut = cut(csDiffPctArea, classes$brks, include.lowest = T))

# plot with new breaks
ggplot(df_diff_2012)+
  geom_sf(aes(fill = csDiffPctAreaCut), col = NA)+
  theme_bw()+
  scale_fill_brewer(palette = "PiYG", direction = 1)#+
  #labs(title = csDiffPctClass)

# Create Fxn
F_plot_gg_diffpct <- function(data, var, yr){
  
  # filter and set variables
  data <- data %>% filter(year == yr)
  y_var <- as.character(var)
  
  # set classes
  #class <- classIntervals(round(data[[y_var]]), n = 11, style = "fisher")
  
  class <- classIntervals(round(data[[y_var]]),
                            fixedBreaks = 
                          #seq(-100, 100, 20),
                           c(-100, -80, -60, -40, -20, -1,1, 20, 40, 60, 80, 100),  
                          #c(-40, -30, -20, -10, -1, 1, 10, 20, 30, 40),
                            style = "fixed")
  
  # set new column with the breaks for mapping
  # data <- data %>%
  #   mutate(DiffCut = cut(y_var, class$brks, include.lowest = T))
  data$DiffCut <- cut(data[[y_var]], class$brks, include.lowest = T)
  
  # plot changes
  p <- ggplot(data)+
    geom_sf(aes(fill = DiffCut), col = NA)+
    theme_bw()+
    scale_fill_brewer(palette = "PiYG", direction = 1, drop = F)+
    labs(
      title = paste0(yr-1, "-", yr, " US-MW ", y_var),
      fill = y_var
    )
  
  # save figure
  ggsave(paste0("../Figures/shock_eda/",
                "gg_", y_var, "_", yr-1, yr,
                ".png"), 
         plot = p)
  
  return(p)
  
}




# NOTE: Change colorscale if including area, fix ###NEXT##### 
F_plot_gg_diffpct(df_diff, "csDiffPctArea", 2012)
F_plot_gg_diffpct(df_diff, "csDiffPctProd", 2012)


# get all columns except the constants
y <- colnames(df_diff)
y <- y[! y %in% c('year', 'state', 'name', 'geometry')]
y

# run fxn over all columns
lapply(y, F_plot_gg_diffpct, data = df_diff, yr = 2012)

### 1.4.2 Plot using facet_wrap ----
F_plot_facet(df_diff, "csDiffPctYield")


### PLOTTING GOALS: ### 
# facet_wrap yield, prod, area for corn, soybean, corn/soy for 2006-2015
# plotting yield, prod, area for corn, soybean, corn/soy for 2010 alone 
# should have 18 plots total

###### USING tmap ########
library(tmap)
library(tmaptools)


tm_shape(df_diff_2012)+
  tm_fill("csDiffPctProd",
          breaks = c(-100, -80, -60, -40, -20, -1,1, 20, 40, 60, 80, 100),
          palette = "PiYG", direction = 1,
          midpoint = 0,
          )+
  #tm_compass(position = c("left", "bottom"), size = 0.8)+
  #tm_scale_bar(position = c("left", "bottom"), width = 0.15)+
  tm_layout(main.title = paste("Change in", "Yield", "from 2011 to 2012"),
            frame = F, legend.outside = T, legend.outside.position = "right",
            #legend.outside.size = 1.5
            )

F_plot_tmap_diffpct <- function(data, var, yr){
  
  # filter and set variables
  data <- data %>% filter(year == yr)
  y_var <- as.character(var)
  
  # plot
  p <- tm_shape(data())+
    tm_fill(y_var,
            breaks = c(-100, -80, -60, -40, -20, -1,1, 20, 40, 60, 80, 100),
            palette = "PiYG", direction = 1,
            midpoint = 0,
    )+
    #tm_compass(position = c("left", "bottom"), size = 0.8)+
    #tm_scale_bar(position = c("left", "bottom"), width = 0.15)+
    tm_layout(main.title = paste("Change in", y_var, "from 2011 to 2012"),
              frame = F, legend.outside = T, legend.outside.position = "right",
              #legend.outside.size = 1.5
    )
  # save figure
  ggsave(paste0("../Figures/shock_eda/",
                "tmap_", y_var, "_", yr-1, yr,
                ".png"), 
         plot = p)
  
  return(p)
  
}  

# get all columns except the constants
y <- colnames(df_diff)
y <- y[! y %in% c('year', 'state', 'name', 'geometry')]
y

# run fxn over all columns
lapply(y, F_plot_tmap_diffpct, data = df_diff, yr = 2012)


################################

## 1.3: Initial EDA
###  1.3.1: Barplots
### 1.3.2: Time Series & Trends


# 2: Use tidyUSDA to get data & plot (get inspo from 1_data_import.R) ------
##2.0 Acres Planted (test)
# year_range <- 2009:2018
# list_year_range <- as.character(year_range)

### IN THE FUTURE! Spend an hour trying this! ###########

## 2.1 Yield 
#tidy_yield <- 
## 2.2 Area Harvested
## 2.3 Production