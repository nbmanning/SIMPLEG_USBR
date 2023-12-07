# Title: aggStats_MapBiomas.R
# Purpose: Calculate aggregated transition stats from MapBiomas to compare to our cropland area changes

# Creation Date: 12/4/23
# Last edited: Dec 2023

# NOTE: uses a bunch of code from:
# "@_ThesisCode/code_old_ignore/fall22_spring23_geo866_CEP_attempt/0_3_1_to_soy_trans_statelevel_BRprice.R"
# "@_ThesisCode/code_current/x_temp_transBRmuni.R"
# "@_ThesisCode/code_current/1_data_import_clean.R"

# NOTE: need to make a decision here, we can use collection 6 data from 'datazoom_amazonia' package
# at the municipality level or we can load in the CSV at the "biome / state" level, 
# which I think just means state level. Former makes the most sense for now, since 
# we are also interested in change across all Brazil 

# NOTE: Downloading from 'datazoom.amazonia' needs a authentication code

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
rm(list = ls())

# 0: Load Libraries & Set Constants -----

## Libraries 
library(tidyverse)

library(stringi) # removing accents
library(datazoom.amazonia) # loading municipality col. 6 data
library(geobr) # load BR shapefiles 
library(sf) # st_intersection and crs
library(RColorBrewer) # maps 

## Constants 

## Shapefiles 

# 1: Load in MapBiomas Transition ------
# Load collection 8 data in tabular form 
#csv_br_trans_st <- read.csv("../Data_Source/MapBiomas/SOURCE_TRANSONLY_COL8_MAPBIOMAS_BIOMASxESTADOS.csv", encoding = "UTF-8")
csv_br_trans_m <- read.csv("../Data_Source/MapBiomas/SOURCE_transonly_col8_mapbiomas_municip.csv", encoding = "UTF-8")

# load Col. 6 municipality level data filtered to Cerrado extent from 1_data_import_clean.R and x_temp_transBRmuni.R
#load(file = "../Data_Source/r_data_check/trans_to_soy_BRCerr_frommuni_year.R")

## 1.1: Tidy -----

df <- csv_br_trans_m

# get rid of all accents
unique(df$biome)
df$state <- stri_trans_general(str = df$state,  id = "Latin-ASCII")
df$biome <- stri_trans_general(str = df$biome,  id = "Latin-ASCII")
names(df)
# df <- filter(csv_br_trans_st, biome == "Cerrado")
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

# rename columns - UGLY AND MANUAL
colnames(df)[colnames(df) %in% c("tate", "lity", "code", "iome", "el_3", "el_3",  "el_4", "el_4")] <- c("state", "municipality", "geocode", "biome", 
                                                                       "from_level_3", "to_level_3",
                                                                       "from_level_4", "to_level_4")
names(df)

df_clean <- df

## 1.2: Filter ---------

# filter to only "Temporary Crops" & From-To's that do not stay the same
df <- df %>% 
  filter(to_level_3 == c("Temporary Crops")) %>%
  filter(to_level_3 != from_level_3)

# OPTIONAL: filter from only probable land cover types (excluding Temporary Crops here)
# unique(df$from_level_3)
# 
# list_from_lv3 <- c("Forest Formation", "Savanna Formation", "Wetland",
#                    "Grassland", "Pasture", "Forest Plantation", 
#                    "Mosaic of Agriculture and Pasture",
#                    "Magrove", "Flooded Forest",
#                    "Shrub Restinga", "Other Non Forest Natural Formation", "Wooded Restinga",
#                    "Perennial Crops")
# 
# df2 <- df %>% 
#   filter(from_level_3 %in% list_from_lv3)

## 1.3: Make 'long' -----
# make long & add "fromto" column
ncol(df)
df <- gather(df,"year","ha",9:ncol(df))     

df <- df %>% 
  mutate(fromto = paste0(from_level_3, " to ", to_level_3)) 



# 2: Brazil ------
df_br <- df

## 2.1: Aggregate ---------

list_from_lv3 <- c("Forest Formation", "Savanna Formation", "Wetland",
                   "Grassland", "Pasture", "Forest Plantation",
                   "Mosaic of Agriculture and Pasture",
                   "Magrove", "Flooded Forest",
                   "Shrub Restinga", "Other Non Forest Natural Formation", "Wooded Restinga",
                   "Perennial Crops")


# keep 'from-to' classes
df_br_class <- df_br %>% 
  filter(from_level_3 %in% list_from_lv3) %>% 
  aggregate(ha ~ year + from_level_3 + to_level_3 + fromto, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# just get aggregate sum
df_br_agg <- df_br %>% 
  aggregate(ha ~ year, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

df_br_fromveg_agg <- df_br %>%
  filter(from_level_3 %in% list_from_lv3) %>% 
  aggregate(ha ~ year, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# 3: Cerrado --------
df_cerr <- filter(df, biome == "Cerrado")

df_cerr <- df_cerr %>% 
  filter(geocode %in% muni_codes_cerr)

## 3.1: Aggregate ----------

# keep 'from-to' classes
df_cerr_class <- df_cerr %>% 
  aggregate(ha ~ year + geocode + from_level_3 + to_level_3 + fromto, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# just get aggregate sum
df_cerrmuni_agg <- df_cerr %>% 
  aggregate(ha ~ year + geocode, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

df_cerr_agg <- df_cerr %>% 
  aggregate(ha ~ year, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# get agg sum of certain 'from' classes
df_cerrmuni_fromveg_agg <- df_cerr %>%
  filter(from_level_3 %in% list_from_lv3) %>% 
  aggregate(ha ~ year + geocode, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

df_cerr_fromveg_agg <- df_cerr %>% 
  filter(from_level_3 %in% list_from_lv3) %>% 
  aggregate(ha ~ year, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))


# make shape -- all agg
shp_cerrmuni <- shp_code_muni %>% 
  left_join(df_cerrmuni_agg,
            join_by(code_muni == geocode)) %>% 
  mutate(year = year(year)) %>% 
  filter(year >= 2012 & year <= 2017)

# plot
ggplot(shp_cerrmuni)+
  geom_sf(mapping = aes(fill = ha), color= NA)+
  scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  facet_wrap("year")+
  coord_sf()+
  theme_minimal()+
  labs(title = "Using Collection 8 from MapBiomas")+
  theme(plot.title = element_text(hjust = 0.5))

# make shape -- from veg 
shp_cerrmuni_fromveg <- shp_code_muni %>% 
  left_join(df_cerrmuni_fromveg_agg,
            join_by(code_muni == geocode)) %>% 
  mutate(year = year(year)) %>% 
  filter(year >= 2012 & year <= 2017)

# plot
ggplot(shp_cerrmuni_fromveg)+
  geom_sf(mapping = aes(fill = ha), color= NA)+
  scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  facet_wrap("year")+
  coord_sf()+
  theme_minimal()+
  labs(title = "Using Collection 8 from MapBiomas",
       subtitle = "Land Transition from Relevant Classes")+
  theme(plot.title = element_text(hjust = 0.5))

# quick line plots 
ggplot(df_cerr_agg, aes(x = year, y = ha))+
  geom_line()+
  geom_point()

ggplot(df_cerr_fromveg_agg, aes(x = year, y = ha))+
  geom_line()+
  geom_point()

# STATS: land trans from br and cerr





# END ####################################################################################

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



## 2: from 'x_temp_transBRmuni.R': load municipality shapefile and Cerrado shapefile and intersect -----

### 2.1: Load municipality shapefile -----

# Read all municipalities in the country at a given year
# to-do: change to shp_br_muni
shp_muni <- read_municipality(code_muni="all", year=2018)
#plot(shp_muni)

### 2.2: Load Cerrado shapefile ---------
shp_br_cerr <- read_biomes(
  year = 2019,
  simplified = T,
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

### 2.3: Intersect Cerrado & Muni -----
# str(shp_muni)
# st_crs(shp_muni)
# 
# str(shp_br_cerr)
# st_crs(shp_br_cerr)

# get municipalities that are at all within the Cerrado
shp_muni_in_cerr <- st_intersection(shp_muni, shp_br_cerr)
#plot(shp_muni_in_cerr)

shp_code_muni <- shp_muni_in_cerr %>% select(code_muni)

## 3: get territory codes for municipalities in intersection -----
muni_codes_cerr <- shp_muni_in_cerr$code_muni

## 4: filter all aggregated municipalities to only those within Cerrado -----
# change trans_tosoy to "trans_BRmunicip_agg"
trans_cerrmuni <- trans_BRmunicip_agg %>% 
  filter(municipality_code %in% muni_codes_cerr)

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

shp_trans_cerrmuni <- left_join(shp_code_muni, agg_trans_cerrmuni,
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
