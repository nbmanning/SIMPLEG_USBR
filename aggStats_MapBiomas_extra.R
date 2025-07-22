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

## 1.4: Filter ---------
# filter to only "Temporary Crops" & From-To's that do not stay the same
df <- df %>% 
  filter(to_level_4 == "Soy Beans") %>%
  filter(to_level_4 != from_level_4)

# 2: Get Muni Codes within Cerrado ---------

## 2.1: Load / Save Shapefiles -------

# Load municipality shapefile 
# Read all municipalities in the country at a given year
shp_muni <- read_municipality(code_muni="all", year=2018)

# Load Cerrado shapefile
shp_br_cerr <- read_biomes(
  year = 2019,
  simplified = F,
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

# Save 
save(shp_muni, file = paste0(folder_source, "shp_muni.Rdata"))
save(shp_br_cerr, file = paste0(folder_source, "shp_br_cerr.Rdata"))

# Load
load(file = paste0(folder_source, "shp_muni.Rdata"))
load(file = paste0(folder_source, "shp_br_cerr.Rdata"))


## 2.2: Get Codes by Intersecting Cerrado & Muni -----

# check CRS for each
# str(shp_muni)
# st_crs(shp_muni)
# 
# str(shp_br_cerr)
# st_crs(shp_br_cerr)


# get municipalities that are at all within the Cerrado
shp_muni_in_cerr <- st_intersection(shp_muni, shp_br_cerr)
#plot(shp_muni_in_cerr)

# get just the codes column
shp_code_muni_in_cerr <- shp_muni_in_cerr %>% select(code_muni)
shp_code_muni_br <- shp_muni %>% select(code_muni)

# get territory codes for municipalities in intersection as numeric
muni_codes_cerr <- shp_muni_in_cerr$code_muni
muni_codes_br <- shp_muni$code_muni

## 2.3 Save ----
save(muni_codes_cerr, file = paste0(folder_derived, "munci_codes_cerr.Rdata"))
save(muni_codes_br, file = paste0(folder_derived, "munci_codes_br.Rdata"))


##### Set "FROM" list - lvl3 ------------
list_from_lv3 <- c("Forest Formation", "Savanna Formation", "Wetland",
                   "Grassland", "Pasture", "Forest Plantation",
                   "Mosaic of Agriculture and Pasture",
                   "Magrove", "Flooded Forest",
                   "Shrub Restinga", "Other Non Forest Natural Formation", "Wooded Restinga",
                   "Perennial Crops")

# 3: Brazil ------

df_br <- df

df_br <- df %>% 
  filter(geocode %in% muni_codes_br)

## 3.1: Aggregate ---------

# keep 'from-to' classes
# df_br_class <- df_br %>% 
#   filter(from_level_3 %in% list_from_lv3) %>% 
#   aggregate(ha ~ year + from_level_3 + to_level_3 + fromto, sum) %>% 
#   mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# just get aggregate sum
agg_br <- df_br %>% 
  aggregate(ha ~ year, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

agg_brmuni <- df_br %>% 
  aggregate(ha ~ year + geocode, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))


# get agg sum of certain 'from' classes
agg_br_fromveg <- df_br %>%
  filter(from_level_3 %in% list_from_lv3) %>% 
  aggregate(ha ~ year, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

agg_brmuni_fromveg <- df_br %>%
  filter(from_level_3 %in% list_from_lv3) %>% 
  aggregate(ha ~ year + geocode, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

## 3.2 plot maps -----

# plot
F_facet<-function(data, aoi, class, file_name){
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
  
  ggsave(filename = paste0(folder_plot, file_name),
         plot = p,
         width = 8, height = 8,
         dpi = 300)
  
  return(p)
  
}

### all agg ----------
# make shape -- all agg
shp_brmuni <- shp_code_muni_br %>% 
  left_join(agg_brmuni,
            join_by(code_muni == geocode)) %>% 
  mutate(year = year(year)) %>% 
  filter(year >= 2012 & year <= 2017)

F_facet(shp_brmuni, aoi = "Brazil", class = "From All Classes to Soybean", file_name = "br_allagg.png")


### from veg ---------
# make shape -- from veg 
shp_brmuni_fromveg <- shp_code_muni_br %>% 
  left_join(agg_brmuni_fromveg,
            join_by(code_muni == geocode)) %>% 
  mutate(year = year(year)) %>% 
  filter(year >= 2012 & year <= 2017)

# plot
F_facet(shp_brmuni_fromveg, aoi = "Brazil", class = "From Relevant Vegetation Classes to Soybean", file_name = "br_fromveg.png")

## 3.3 line plots --------

F_line<-function(data, aoi, class, file_name){
  p <- ggplot(data, aes(x = year, y = ha/1000000))+
    geom_line()+
    geom_point()+
    theme_minimal()+
    labs(
      title = paste("Annual Land Transition Across", aoi),
      subtitle = paste(class),
      y = "Land Transition (Mha)",
      x = ""
    )+
    geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
               linetype="dotted", linewidth=0.5)
  
  ggsave(filename = paste0(folder_plot, file_name),
         plot = p,
         width = 9, height = 6,
         dpi = 300)
  
  # print stats/data for publication
  print("Land Transitioned:")
  print(data %>% filter(year(year) >= 2013 & year(year) <= 2015))
  
  return(p)
}

### all agg ------
F_line(data = agg_br, aoi = "Brazil", class = "From All Classes to Soybean", 
       file_name = "line_br_allagg.png")

### from veg -----
F_line(data = agg_br_fromveg, aoi = "Brazil", class = "From Relevant Vegetation Classes to Soybean", 
       file_name = "line_br_fromveg.png")


## 3.4: STATS BR Land Trans -----
print(agg_br_fromveg %>% filter(year(year) >= 2013 & year(year) <= 2015))
print(agg_br %>% filter(year(year) >= 2013 & year(year) <= 2015))

stat_mapb_agg_trans_br <- agg_br %>% filter(year(year) == 2010 | year(year) >= 2013 & year(year) <= 2015)
stat_mapb_agg_trans_br_fromveg <- agg_br_fromveg %>% filter(year(year) == 2010 | year(year) >= 2013 & year(year) <= 2015)


# 4: Cerrado --------
#df_cerr <- filter(df, biome == "Cerrado")

df_cerr <- df %>% 
  filter(geocode %in% muni_codes_cerr) %>% 
  filter(biome == "Cerrado")
  

## 4.1: Aggregate ----------

# just get aggregate sum
agg_cerrmuni <- df_cerr %>% 
  aggregate(ha ~ year + geocode, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

agg_cerr <- df_cerr %>% 
  aggregate(ha ~ year, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# get agg sum of certain 'from' classes
agg_cerrmuni_fromveg <- df_cerr %>%
  filter(from_level_3 %in% list_from_lv3) %>% 
  aggregate(ha ~ year + geocode, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

agg_cerr_fromveg <- df_cerr %>% 
  filter(from_level_3 %in% list_from_lv3) %>% 
  aggregate(ha ~ year, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

## 4.2 plot maps -----

### all agg ----------
# make shape -- all agg
shp_cerrmuni <- shp_code_muni_in_cerr %>% 
  left_join(agg_cerrmuni,
            join_by(code_muni == geocode)) %>% 
  mutate(year = year(year)) %>% 
  filter(year >= 2012 & year <= 2017)

F_facet(shp_cerrmuni, aoi = "Cerrado", class = "From All Classes to Soybean", file_name = "cerr_allagg.png")

### from veg ---------
# make shape -- from veg 
shp_cerrmuni_fromveg <- shp_code_muni_in_cerr %>% 
  left_join(agg_cerrmuni_fromveg,
            join_by(code_muni == geocode)) %>% 
  mutate(year = year(year)) %>% 
  filter(year >= 2012 & year <= 2017)

# plot
F_facet(shp_cerrmuni_fromveg, aoi = "Cerrado", class = "From Relevant Vegetation Classes to Soybean", file_name = "cerr_fromveg.png")

## 4.3 line plots --------
### all agg ------
# quick line plots 
F_line(data = agg_cerr, aoi = "Cerrado", class = "From All Classes to Soybean", 
       file_name = "line_cerr_allagg.png")

### from veg -----
F_line(data = agg_cerr_fromveg, aoi = "Cerrado", class = "From Relevant Vegetation Classes to Soybean", 
       file_name = "line_cerr_fromveg.png")


## 4.4: STATS Cerr Land Trans -----
print(agg_cerr %>% filter(year(year) >= 2013 & year(year) <= 2015))
print(agg_cerr_fromveg %>% filter(year(year) >= 2013 & year(year) <= 2015))

stat_mapb_agg_trans_cerr <- agg_cerr %>% filter(year(year) == 2010 | year(year) >= 2013 & year(year) <= 2015)
stat_mapb_agg_trans_cerr_fromveg <- agg_cerr_fromveg %>% filter(year(year) == 2010 | year(year) >= 2013 & year(year) <= 2015)

# 5: SAVE stats ---------
save(
  stat_mapb_agg_trans_br, stat_mapb_agg_trans_br_fromveg,
  stat_mapb_agg_trans_cerr, stat_mapb_agg_trans_cerr_fromveg,
  file = paste0(folder_stat, "mapb_agg_land_trans_br_and_cerr.RData"))


# 6: Line Plots with from-to ----------
classes_few <- c("Temporary Crops", "Forest Formation", "Mosaic of Agriculture and Pasture", 
                 "Pasture", "Savanna Formation", "Grassland")

df_g_to_soy_cerr <- df_cerr %>% 
  aggregate(ha ~ year + biome + from_level_3 + to_level_4, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

df_g_cerr <- filter(df_g_to_soy_cerr, from_level_3 %in% classes_few)

# plot with a few classes remove
ggplot(df_g_cerr, aes(x=year, y=ha/1000000, color = from_level_3)) +
  geom_line() + 
  #geom_point() +
  xlab("")+
  scale_x_date(date_labels = "%m-%Y")+
  labs(title = "From X to Soybean",
       ylab = "Hectares Transitioned from Previous Year")+
  geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
             linetype="dotted", linewidth=0.5)

# fancier for manuscript
ggplot(df_g_cerr, aes(x=year, y=ha/1000000, color = from_level_3)) +
  geom_line() +
  geom_point(fill = "white", size = 1.2) +
  xlab("")+
  scale_x_date(date_labels = "%Y")+
  labs(
    #title = paste("Top", length(ls_top), "Transition Classes"),
    #title = "From X to Soybean",
    y = "Land Change from Previous Year (Mha)",
    color = "From-To Transitions"
  )+
  geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
             linetype="dotted", linewidth=0.5)+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 17, hjust = 0.5),
    #plot.subtitle = element_text(size = 12, hjust = 0.5)
  )

# save 
ggsave(paste("../Figures/trans_mapbiomas/cerr_to_soybean.png"), 
       width = 14, height = 7)

# END ####################################################################################

# TO-DO -----------------
## add Cerrado outline to maps --------

# GRAVEYARD -----------------------------------

## 1: Cerrado Col. 6 Municipality -------
# load Col. 6 municipality level data filtered to Cerrado extent from 1_data_import_clean.R and x_temp_transBRmuni.R
# load(file = "../Data_Derived/MapBiomas/trans_to_soy_BRCerr_frommuni_year.R")

### from 1_data_import_clean.R -------
# load Col. 6 Lvl 4 data from package
# source_mapb_trans_municip <- load_mapbiomas( # takes a long time
#   dataset = "mapbiomas_transition",
#   raw_data = F,
#   geo_level = "municipality",
#   #time_period = "all",
#   language = "eng",
#   #time_id = "year",
#   cover_level = 4
# )

# Set the transition variables of "to_elvel_4" to keep 
# list_lvl4_interest <- c("Savanna Formation", "Grassland", "Pasture", "Soy Beans", 
#                         "Other Temporary Crops", "Mosaic of Agriculture and Pasture",
#                         "Sugar Cane", "Other Non Vegetated Area", "Coffe",
#                         "Other Non Forest Natural Formation", "Citrus", "Rice")


names(source_mapb_trans_municip)
#save(source_mapb_trans_municip, file = "../Data_Source/MapBiomas/source_mapb_trans_municip_lvl3.Rdata")
load(file = "../Data_Source/MapBiomas/source_mapb_trans_municip.Rdata")

list_from_lv3 <- c("Forest Formation", "Savanna Formation", "Wetland",
                   "Grassland", "Pasture", "Forest Plantation",
                   "Mosaic of Agriculture and Pasture",
                   "Magrove", "Flooded Forest",
                   "Shrub Restinga", "Other Non Forest Natural Formation", "Wooded Restinga",
                   "Perennial Crops")


# set temp variable to test with
trans_br <- source_mapb_trans_municip

# get others -- not just soybeans 
# trans_br <- trans_br %>% 
#   filter(to_level_4 %in% list_lvl4_interest)

# remove duplicates 
trans_br <- trans_br %>% 
  filter(to_level_3 != from_level_3)

# break up intervals into start and end year (going from )
trans_br$start_year <- as.numeric(str_sub(trans_br$year, 1, 4)) 
trans_br$end_year <- as.numeric(str_sub(trans_br$year, -4, -1))

str(trans_br)

# get only the non-subsequent intervals
# trans_br_intervals <- trans_br %>%
#   filter(end_year != start_year+1)

# keep only consecutive start/end years and  only "Temp. Crops"
# this means that 2013 captures the 2012-13 harvest year in BR 
trans_br <- trans_br %>% 
  filter(end_year == start_year+1) %>% 
  # select("year", "end_year","start_year","state","municipality","municipality_code",
  #        "from_level_0", "from_level_1", "from_level_2", "from_level_3", "from_level_4",     
  #        "to_level_0", "to_level_1", "to_level_2", "to_level_3", "to_level_4",      
  #        "value")
  select("year", "end_year","start_year","state","municipality","municipality_code",
         "from_level_3", "to_level_3", 
         #"from_level_4", "to_level_4",
         "value") %>% 
  filter(to_level_3 == c("Temporary Crops")) %>%
  filter(to_level_3 != from_level_3)

# aggregate to yearly transition values by combining all FROM classes per municip per year
#note: replaced territory_id for municipality_code
trans_BRmunicip_agg <- aggregate(value ~ municipality_code + municipality + state + end_year + from_level_3 + to_level_3, trans_br, sum)

trans_BRmunicip_agg <- trans_BRmunicip_agg %>% 
  filter(end_year >= 2000 & end_year <= 2019) %>% # UGHHHHH ONLY up to 2019!!!!!!!! 
  rename(
    "yr" = "end_year",
    "trans" = "value") %>%
  #uncomment to remove from-to
  #select(., c("yr","state", "municipality", "municipality_code", "trans")) %>% 
  mutate(country = "Brazil")

agg_trans_BR <- aggregate(value ~ end_year, trans_br, sum)

agg_trans_BR_fromveg <- trans_br %>% 
  filter(from_level_3 %in% list_from_lv3) %>% 
  aggregate(value ~ end_year, sum)



#

## 5: Aggregate to one value per year  -----

# optional: filter to just probable "from" classifications
trans_cerrmuni_fromveg <- trans_cerrmuni %>% 
  filter(from_level_3 %in% list_from_lv3)
#unique(trans_cerrmuni_fromveg$from_level_3)

# agg to one value per entire region per year
agg_trans_cerr <- trans_cerrmuni %>% 
  aggregate(trans ~ yr, ., sum) %>%
  mutate(country = "Brazil")

agg_trans_cerr_fromveg <- trans_cerrmuni_fromveg %>% 
  aggregate(trans ~ yr, ., sum) %>%
  mutate(country = "Brazil")

# agg to one value per municipality per year
agg_trans_cerrmuni <- trans_cerrmuni %>% 
  aggregate(trans ~ yr + municipality_code + municipality, ., sum) %>%
  mutate(country = "Brazil")

agg_trans_cerrmuni_fromveg <- trans_cerrmuni_fromveg %>% 
  aggregate(trans ~ yr + municipality_code + municipality, ., sum) %>%
  mutate(country = "Brazil")


# save as BR trans data 
# R_trans_to_soy_BRCerr_muni <- trans_tosoy_cerrmuni
# save(R_trans_to_soy_BRCerr_muni, file = "../Data_Source/r_data_check/trans_to_soy_BRCerr_frommuni_year.R")

##6: quick line plot ----
ggplot(agg_trans_cerr, aes(x = yr, y = trans))+
  geom_line()+
  geom_point()+
  scale_y_continuous(name = "land transition", 
                     limits = c(20000, 800000))


ggplot(agg_trans_cerr_fromveg, aes(x = yr, y = trans))+
  geom_line()+
  geom_point()+
  scale_y_continuous(name = "land transition", 
                                   limits = c(20000, 800000))

## 7: quick transition map -------
# join to keep 'geom'
agg_trans_cerrmuni <- agg_trans_cerrmuni %>% 
  mutate(municipality_code = as.double(municipality_code))

shp_trans_cerrmuni <- left_join(shp_code_muni_in_cerr, agg_trans_cerrmuni,
                                join_by(code_muni == municipality_code))
str(shp_trans_cerrmuni)

# filter to years and plot
yr1 <- 2012
yr2 <- 2017

shp1 <- shp_trans_cerrmuni %>% filter(yr == 2013)
shp1_2 <- shp_trans_cerrmuni %>% filter(yr >= yr1 & yr <= yr2)

ggplot(shp1_2)+
  geom_sf(mapping = aes(fill = trans), color= NA)+
  scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  facet_wrap("yr")+
  coord_sf()+
  theme_minimal()+
  labs(title = "Using Collection 6 (all) from Datazoom.Amazonia")+
  theme(plot.title = element_text(hjust = 0.5))
