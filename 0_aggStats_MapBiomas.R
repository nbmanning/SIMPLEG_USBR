# Title: aggStats_MapBiomas_extra.R
# Purpose: Calculate aggregated transition stats from MapBiomas to compare to our cropland area changes

# Creation Date: 12/4/23
# Last edited: Dec 2023

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
rm(list = ls())

# 0: Load Libraries & Set Constants -----

## Libraries 
library(tidyverse)

library(stringi) # removing accents
library(geobr) # load BR shapefiles 
library(sf) # st_intersection and crs
library(RColorBrewer) # maps 

## Constants 
# date_string <- "2024-03-03"
date_string <- "2024-11-15"

# looks like the last date for this attempt was 01-30
folder_stat <- paste0("../Results/SIMPLEG-", date_string, "/stat_summary/")
folder_plot <- "../Figures/trans_mapbiomas/"
folder_source <- "../Data_Source/MapBiomas/"
folder_derived <- "../Data_Derived/"

## Shapefiles 

# NOTE: Can skip to "1.3: Save/Load" after running once

# 1: Load in MapBiomas Transition ------
# Load collection 8 data in tabular form 
csv_br_trans_m <- read.csv(paste0(folder_source, "SOURCE_transonly_col8_mapbiomas_municip.csv"), encoding = "UTF-8")

## 1.1: Tidy -----

df <- csv_br_trans_m

# get rid of all accents

#unique(df$biome)
df$state <- stri_trans_general(str = df$state,  id = "Latin-ASCII")
df$biome <- stri_trans_general(str = df$biome,  id = "Latin-ASCII")
names(df)

# select levels and years to reduce df size 
df <- dplyr::select(df, c("state","municipality", "geocode", "biome", 
                          "from_level_3", "to_level_3",
                          "from_level_4", "to_level_4",
                          #"X1985.1986", "X1986.1987", "X1987.1988", "X1988.1989", "X1989.1990", 
                          #"X1990.1991", "X1991.1992", "X1992.1993", "X1993.1994", "X1994.1995", "X1995.1996", "X1996.1997", "X1997.1998", "X1998.1999",    
                          "X1999.2000", "X2000.2001", "X2001.2002", "X2002.2003", "X2003.2004",    "X2004.2005",    "X2005.2006",   
                          "X2006.2007",    "X2007.2008",    "X2008.2009",    "X2009.2010",   "X2010.2011", "X2011.2012",    "X2012.2013",   
                          "X2013.2014",    "X2014.2015",    "X2015.2016",  "X2016.2017",    "X2017.2018",   
                          "X2018.2019",    "X2019.2020",    "X2020.2021"))

# remove all but the last four digits of all the columns 
names(df) <- str_sub(names(df), - 4, - 1)
names(df)

# rename columns - BEWARE HERE, this is manual for now, if you change the 'select' above then you need to change this as well 
colnames(df)[colnames(df) %in% c("tate", "lity", "code", "iome", "el_3", "el_3",  "el_4", "el_4")] <- c("state", "municipality", "geocode", "biome", 
                                                                       "from_level_3", "to_level_3",
                                                                       "from_level_4", "to_level_4")
names(df)

# save as a clean df to come back to 
df_clean <- df


## 1.2: Make 'long' -----
# gather to make into a long dataset; change the number if you changed 'select' above
ncol(df)
df <- gather(df,"year","ha",9:ncol(df))     



## 1.3: Save / Load -----
save(df_clean, file = paste0(folder_derived, "mapb_col8_clean.Rdata"))
save(df, file = paste0(folder_derived, "mapb_col8_clean_long.Rdata"))

# NOTE: THIS INCLUDES ALL 

# Load 
load(file = paste0(folder_derived, "mapb_col8_clean.Rdata"))
load(file = paste0(folder_derived, "mapb_col8_clean_long.Rdata"))