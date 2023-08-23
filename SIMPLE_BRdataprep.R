# title: MapBtoSIMPLEG_dataprep.R
# author: Nick Manning
# purpose: Import and clean muncipality level geospatial and tabular land cover
# data for BR and the Cerrado

# creation date: 7/27/23
# last updated: August 2023

# links: 
## BR GeoTiff Download: https://brasil.mapbiomas.org/en/colecoes-mapbiomas-1?cama_set_language=en
## Collection 7.1 Download: https://brasil.mapbiomas.org/en/statistics


# Requires: 
## Crosswalk CSV for MapBiomas --> SIMPLE-G
## GeoTiff downloaded from MapBiomas (link provided)
## (in-progress) Land Transition Excel sheet downloaded from MapBiomas (link provided)

### To-Do: ###
# Clean up script

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list=ls())
getwd()

# 0: Load Libraries & Set Constants -----------

library(tidyverse) # dplyr(): piping and data manipulation; stringr()
library(datazoom.amazonia) # loading tabular municipality transition data 
library(geobr) # loading Cerrado & BR shapefiles
library(terra)
library(rasterVis)
library(readxl)
library(polyglotr) # translating from Portugeuse to Eng

# data import & export  
path_import <- "../Data_Source/LandChange/Cerrado/"


# 1: Tabular Transition Data (from package) ---------

# load data from datazoom.amazonia
# NOTE: This is Collection 6 data (as of 7/31/23) and ends at 2019
# NOTE: This takes a pretty long time, ~15 minutes or so to download and load

# source_mapb_trans_municip <- load_mapbiomas( # takes a LONG time
#   dataset = "mapbiomas_transition",
#   raw_data = F,
#   geo_level = "municipality",
#   #time_period = "all",
#   language = "eng",
#   #time_id = "year",
#   cover_level = 4
# )

# Alternatively, save the data once then load it here in the future - this is 
# significantly faster, it took around 2 minutes 

#save(source_mapb_trans_municip, file = paste0(path_import, "source_mapb_trans_municip.Rdata"))
load(file = paste0(path_import, "source_mapb_trans_municip.Rdata"))

# set temp variable to test with
trans <- source_mapb_trans_municip

# break up intervals into start and end year (going from )
trans$start_year <- as.numeric(str_sub(trans$year, 1, 4)) 
trans$end_year <- as.numeric(str_sub(trans$year, -4, -1))


# keep only consecutive start/end years
# this means that 2013 captures the 2012-13 harvest year in BR 
trans <- trans %>% 
  filter(end_year == start_year+1) %>% 
  select("year", "end_year","start_year","state","municipality","municipality_code",
         "from_level_0", "from_level_1", "from_level_2", "from_level_3", "from_level_4",     
         "to_level_0", "to_level_1", "to_level_2", "to_level_3", "to_level_4",      
         "value")

# optional - get relevant level 4 "TO" categories that aren't only "soybeans" 

# list_lvl4_interest <- c("Savanna Formation", "Grassland", "Pasture", "Soy Beans", 
#                         "Other Temporary Crops", "Mosaic of Agriculture and Pasture",
#                         "Sugar Cane", "Other Non Vegetated Area", "Coffe",
#                         "Other Non Forest Natural Formation", "Citrus", "Rice")

# trans <- trans %>% 
#   filter(to_level_4 %in% list_lvl4_interest)



# 2: Land Cover Map from MapBiomas GeoTiff Download ----------------

## 2.1: Download and aggregate, resample, and reclassify MapB to SIMPLE-G ------ 
# Download link; replace year with year of interest 
# https://storage.googleapis.com/mapbiomas-public/brasil/collection-71/lclu/coverage/brasil_coverage_2010.tif  

source_mapb <- rast(paste0(path_import, "brasil_coverage_2010.tif"))
mapb <- source_mapb

# FROM IMAN: if it is in degrees (decimal degrees) right now and we want it in 
# 5-arc-minute, then we multiply by 12 (by 60 would get 1-arc-minute, then by
# 12 would equal 5-arc-minute resolution)   

# NOTE: used median here to keep a round number for land cover category

mapb_900 <- aggregate(mapb, fact = 30, na.rm = T, median) # NOTE: takes a while
plot(mapb)
plot(mapb_900)

# create template raster to set resolution to 1/12 a degree 
# note on units: 1/60 of a degree is one arc-minute, so 1/12 is 5-arc-minute
temp_rast <- rast(res=1/12, extent = ext(mapb_900))
mapb_5min = resample(mapb_900, temp_rast, method = "near")
plot(mapb_5min)

# set as factor to get a categorical raster
mapb_5min_factor <- as.factor(mapb_5min)

# link to crosswalk table: https://mapbiomas-br-site.s3.amazonaws.com/downloads/_EN__C%C3%B3digos_da_legenda_Cole%C3%A7%C3%A3o_7.pdf
# read in manual CSV
mapb_pal_csv <- read.csv(paste0(path_import, "mapb_color_palette.csv"))

# set category descriptions and color palette
legend_mapb_cat <- mapb_pal_csv %>% 
  dplyr::select("mapb_id", "mapb_cat") %>% 
  rename("ID" = "mapb_id",
         "category" = "mapb_cat")

legend_mapb_col <- mapb_pal_csv %>% 
  dplyr::select("mapb_id", "mapb_hex") %>% 
  rename("ID" = "mapb_id",
         "color" = "mapb_hex")

# plot 5-arc-minute MapBiomas Map with reclassified 
levels(mapb_5min_factor) <- legend_mapb_cat
coltab(mapb_5min_factor) <- legend_mapb_col
plot(mapb_5min_factor)


## 2.2 Aggregate classes to SIMPLE-G classes -----

# example from: http://www.wvview.org/os_sa/15b_Raster_Analysis_terra.html

# get a from-to table where categorical raster cells are replaced with other
# values. The first column will be changed to the second column

mapb_sg_crosswalk <- as.matrix(select(mapb_pal_csv, c("mapb_id","sg_id")))

mapb_re <- terra::classify(mapb_5min_factor, mapb_sg_crosswalk)

# set category descriptions and color palette
legend_sg_cat <- mapb_pal_csv %>% 
  dplyr::select("sg_id", "sg_cat") %>% 
  rename("ID" = "sg_id",
         "category" = "sg_cat")

legend_sg_col <- mapb_pal_csv %>% 
  dplyr::select("sg_id", "sg_hex") %>% 
  rename("ID" = "sg_id",
         "color" = "sg_hex")

legend_sg_cat <- unique(legend_sg_cat)
legend_sg_col <- unique(legend_sg_col)

# apply SIMPLE-G categories and color palettes
levels(mapb_re) <- legend_sg_cat
coltab(mapb_re) <- legend_sg_col
plot(mapb_re)

## 2.3: Clip BR extent to Cerrado Extent ---

# Load Cerrado Shapefile
shp_br_cerr <- read_biomes(
  year = 2019,
  simplified = T,
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

# get Cerrado extent as terra object 
ext_cerr <- vect(ext(shp_br_cerr))

# crop, mask, and plot 
mapb_re_cerr <- terra::crop(mapb_re_cerr, ext_cerr, mask = T)
mapb_re_cerr <- mask(mapb_re_cerr, shp_br_cerr)
terra::plot(mapb_re_cerr, axes = F)


# 3: (in progress) 7.1 MapBiomas Transition Data  ----------

# note for Nick: check '0_3_1_to_soy_trans_statelevel_BRprice.R' script for help

# accessed from: https://mapbiomas.org/en/statistics

# load in Collection 7.1 
file_municip <- "TABELA-GERAL-COL71-MAPBIOMAS-BIOMASxMUNICIPIOS-v2.xlsx"
file_sheet_trans <- "TRANSICOES_COL7.1"

source_mapb_trans <- read_xlsx(path = paste0(path_import, file_municip), 
                                sheet = file_sheet_trans, 
                                col_names = TRUE)
# get just the first 1000 rows to work with 
trans <- source_mapb_trans
trans_test <- trans %>% slice(1:1000)
colnames(trans_test)

# gather so the year values are in one column (long)
trans_test <- gather(trans_test,"year","trans_ha", "1985-1986":"2020-2021") 

# remove all but the last four digits of all the columns 
# FIRST, keep only rows with sequential years (e.g. no 1985-2021)
# names(trans) <- str_sub(names(trans), - 4, - 1)


# 4: (in progress) 7.1 MapBiomas Cover Data  ----------

# load in Collection 7.1 Cover Data
file_municip <- "TABELA-GERAL-COL71-MAPBIOMAS-BIOMASxMUNICIPIOS-v2.xlsx"
file_sheet_cover <- "COBERTURA_COL7.1"
path_import <- "../Data_Source/LandChange/Cerrado/"


source_mapb_cover <- read_xlsx(path = paste0(path_import, file_municip), 
                               sheet = file_sheet_cover, 
                               col_names = TRUE)

# get just the first 1000 rows to work with 
cover <- source_mapb_cover
cover_test <- cover %>% slice(1:1000)

# optional - get rid of all accents (makes for easier merging / processing in the future)
# library(stringi)
# cover_test$city <- stri_trans_general(str = cover_test$city,  id = "Latin-ASCII")

# split 'city' column into municipalities, states, biomes
cover_test[c('muni', 'state', 'biome')] <- str_split_fixed(cover_test$city, ' - ', 3)

# re-order and select year
cover_test <- cover_test %>% 
  select("feature_id", "city", "muni", "state", "class_id",   
         "level_0", "level_1", "level_2", "level_3", "level_4",    
         "color", "2010")

# 5: Get Iman BR stats for 2010 using IBGE package ------


library(sidrar)
library(polyglotr)

# NOTES on 'sidrar' package:
# "Tabela 1612: Área plantada, área colhida, quantidade produzida, rendimento médio e valor da produção das lavouras temporárias"

## Quantidade produzida = quantity produced (Toneladas)
## Rendimento médio da produção = average production yield (kg/ha)

# codes:
# 109:Área plantada (Hectares) 

# 1000109: Área plantada - percentual do total geral (%) 

# 216:Área colhida (Hectares)

# 1000216: Área colhida - percentual do total geral (%)

# 214: Quantidade produzida (Toneladas)
## Quantity Produced (Tons)

# 112: Rendimento médio da produção (Quilogramas por Hectare)
## Average production yield (kg per ha)

# 215: Valor da produção (Mil Cruzeiros, Mil Cruzados , Mil Cruzados Novos, Mil Cruzeiros Reais [], Mil Reais)

# 1000215: Valor da produção - percentual do total geral (%)

# The geo argument can be one of "Brazil", "Region", "State", "MesoRegion", 
## "MicroRegion", "MetroRegion", "MetroRegionDiv", "IRD", "UrbAglo", "City", "District",
## "subdistrict","Neighborhood","PopArrang". 'geo.filter' lists can/must be named 
## with the same characters.

## 1.5.1: BR Raw Production (Prod & Planted) at State Level ------- 
# get state-level data so we can merge to only those states within the extent of the Cerrado
raw_sidra_state <- get_sidra(x = 1612, 
                       variable =  c(214, 216), # production and yield # or for first six (excluding value of production) c(109, 1000109, 216, 1000216,214, 112) 
                       period = "2010",# list_year_range, #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                       geo = "State", # Brazil, State, or Município
                       geo.filter = NULL,
                       classific = "c81",
                       category = list(c(2713, 2711)), # 2713 = Soja (em grão); 2711 = Milho (corn) (em grão)
                       header = T,
                       format = 3)

raw_sidra_br <- get_sidra(x = 1612, 
                             variable =  c(214, 216), # production and yield # or for first six (excluding value of production) c(109, 1000109, 216, 1000216,214, 112) 
                             period = "2010",# list_year_range, #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                             geo = "Brazil", # Brazil, State, or Município
                             geo.filter = NULL,
                             classific = "c81",
                             category = list(c(2713, 2711)), # 2713 = Soja (em grão); 2711 = Milho (corn) (em grão)
                             header = T,
                             format = 3)
br <- raw_sidra_br
eng <- google_translate(colnames(raw_sidra_br), target_language = "en", source_language = "pt")
colnames(br) <- tolower(eng)

br1 <- br %>% 
  dplyr::select("territorial level", "unit of measurement", "value", "year", "variable", "product from temporary crops") %>% 
  rename("level" = "territorial level", 
         "unit" = "unit of measurement",
         "crop" = "product from temporary crops"
         ) 
# 2010 harvested area of corn (ha)
## 12678875

# 2010 harvested area of soy (ha)
## 23327296

# 2010 production of corn (tons) 
## 55364271

# 2010 production of soy (tons)
## 68756343



# 6: Bring in SPAM 2010 data and clip to Brazil ----

# get units and descriptions for SPAM data here: https://mapspam.info/methodology/

# library(geodata) # didn't work :(
# crop_spam(crop = "soybean", 
#           var = "harv_area", #  "yield", "harv_area" (harvested area), "phys_area" (physical area), "prod" (production) or "val_prod" (value of production)
#           path = paste0(path_import, "SPAM2010_soyb_harea"))

# bring in harvested area (ha)
spam_soy_harea <- rast(paste0(path_import, "dataverse_files/spam2010V2r0_global_H_SOYB_A.tif"))
spam_maize_harea <- rast(paste0(path_import, "dataverse_files/spam2010V2r0_global_H_MAIZ_A.tif"))

# production (mt)
spam_soy_prod <- rast(paste0(path_import, "dataverse_files/spam2010V2r0_global_P_SOYB_A.tif"))
spam_maize_prod <- rast(paste0(path_import, "dataverse_files/spam2010V2r0_global_P_MAIZ_A.tif"))

spam <- c(spam_soy_harea, spam_maize_harea, spam_soy_prod, spam_maize_prod)
plot(spam)

## 6.1: Clip to Brazil to test ----
# Load BR Shapefile
shp_br <- read_country(year = 2010, 
                       simplified = T, 
                       showProgress = T)
# get extent as terra object for plotting
ext_br <- vect(ext(shp_br))

# plot basic BR results by cropping and masking to just BR extent
r_br <- terra::crop(spam, ext_br, mask = T) 
r_br <- mask(r_br, shp_br)
terra::plot(r_br, axes = F, type = "interval")
r_br                       
                       

## 6.2: Get results ----
## area= (total area)/(ha)
#spam_soy_harea <- rast(paste0(path_import, "dataverse_files/spam2010V2r0_global_H_SOYB_A.tif"))
plot(r_br$spam2010V2r0_global_H_SOYB_A)
plot(spam_soy_harea)

# testing with small raster (IGNORE) 
# link (see "Build a spatRaster from Scratch"): http://www.wvview.org/os_sa/15b_Raster_Analysis_terra.html#general-raster-analysis 
# r_test <- rast(ncols = 10, nrows = 10, xmin = 500000, xmax = 500100, ymin = 4200000, ymax = 4200100, crs="+init=EPSG:26917")
# r_test
# 
# plot(r_test)
# vals <- sample(c(0:255), 100, replace=TRUE)
# 
# values(r_test) <- vals
# plot(r_test)
# a <- cellSize(r_test)
# plot(a)

### 6.2.1A: Area (% of grid cell) ------ 
# get area of each grid cell (will be different bc of projection)
r_br_area <- cellSize(r_br$spam2010V2r0_global_H_SOYB_A, unit = "ha")
plot(r_br_area)

# get harvested percentages by dividing by area of each grid cell 
spam_soy_harea_perc <- (r_br$spam2010V2r0_global_H_SOYB_A/r_br_area) #* 100
plot(spam_soy_harea_perc)

spam_maize_harea_perc <- (r_br$spam2010V2r0_global_H_MAIZ_A/r_br_area) #* 100
plot(spam_maize_harea_perc)

### 6.2.1B: Area (% of total) -----
# get sums 
sum_harea_soy <- as.numeric(
  global(classify(r_br$spam2010V2r0_global_H_SOYB_A, cbind(NA, 0)), fun = "sum"))

sum_harea_maize <- as.numeric(
  global(classify(r_br$spam2010V2r0_global_H_MAIZ_A, cbind(NA, 0)), fun = "sum"))

# divide rasters by total sum for each crop
spam_soy_harea_perctotal <- r_br$spam2010V2r0_global_H_SOYB_A / sum_harea_soy
plot(spam_soy_harea_perctotal)

spam_maize_harea_perctotal <- r_br$spam2010V2r0_global_H_MAIZ_A / sum_harea_maize
plot(spam_soy_harea_perctotal)

### 6.2.2: Production (% of total) ------
# get production percentage (% of total per grid cell)
## production= (mt)/(total BR prod) #Q: or should this be total Cerrado prod? BR prod makes more sense, then clip to Cerr after

# first, get total sum of brazil soy or maize
# NOTE: I set all NA's to 0 when calculating the sume of metric tons of crops produced

# get sums 
sum_prod_soy <- as.numeric(
  global(classify(r_br$spam2010V2r0_global_P_SOYB_A, cbind(NA, 0)), fun = "sum"))

sum_prod_maize <- as.numeric(
  global(classify(r_br$spam2010V2r0_global_P_MAIZ_A, cbind(NA, 0)), fun = "sum"))

# divide rasters by total sum for each crop
spam_soy_prod_perc <- r_br$spam2010V2r0_global_P_SOYB_A / sum_prod_soy
plot(spam_soy_prod_perc)

spam_maize_prod_perc <- r_br$spam2010V2r0_global_P_MAIZ_A / sum_prod_maize
plot(spam_maize_prod_perc)


### 6.2.3: Yield (production / harvested area) (mt / ha) ------
spam_soy_yield <- r_br$spam2010V2r0_global_P_SOYB_A / r_br$spam2010V2r0_global_H_SOYB_A
spam_maize_yield <- r_br$spam2010V2r0_global_P_MAIZ_A / r_br$spam2010V2r0_global_H_MAIZ_A

plot(spam_soy_yield)

## 6.3: Combine to Get Percentage Raster Stack ---------
# Combine percentage rasters 
spam_perc <- c(spam_soy_harea_perc, spam_maize_harea_perc, spam_soy_prod_perc, spam_maize_prod_perc)
plot(spam_perc)

spam_perc_total <- c(spam_soy_harea_perctotal, spam_maize_harea_perctotal, spam_soy_prod_perc, spam_maize_prod_perc)
plot(spam_perc_total)

spam_perc_all <- c(spam_soy_harea_perc, spam_maize_harea_perc, spam_soy_harea_perctotal, spam_maize_harea_perctotal, spam_soy_prod_perc, spam_maize_prod_perc, spam_soy_yield, spam_maize_yield)


## 6.4: Plot Brazil -------
# rename layers for final set that includes harvested area % by grid cell 
spam_perc <- spam_perc %>% rename(
  soy_harea_perc_2010 = spam2010V2r0_global_H_SOYB_A,
  maize_harea_perc_2010 = spam2010V2r0_global_H_MAIZ_A,
  soy_prod_perc_2010 = spam2010V2r0_global_P_SOYB_A,
  maize_prod_perc_2010 = spam2010V2r0_global_P_MAIZ_A
)

plot(spam_perc)

# rename layers from _all
names(spam_perc_all) <- c("soy_harea_perc_gridcell","maize_harea_perc_gridcell",
                          "soy_harea_perc_total","maize_harea_perc_total",
                          "soy_prod_perc_total","maize_prod_perc_total",
                          "soy_yield_gridcell", "maize_yield_gridcell")

# plot with 2 columns and 3 rows 
plot(spam_perc_all, nc = 2, nr = 4)

# use the harvested area % of total so we can facet_wrap with 'tidyterra'
spam_perc_total <- spam_perc_total %>% rename(
  soy_harea_perc_2010 = spam2010V2r0_global_H_SOYB_A,
  maize_harea_perc_2010 = spam2010V2r0_global_H_MAIZ_A,
  soy_prod_perc_2010 = spam2010V2r0_global_P_SOYB_A,
  maize_prod_perc_2010 = spam2010V2r0_global_P_MAIZ_A
)

ggplot()+
  geom_spatraster(data = spam_perc_total)+
  facet_wrap(~lyr)

## 6.5: Clip to Cerrado to Plot -------

# Load Cerrado Shapefile
shp_br_cerr <- read_biomes(
  year = 2019,
  simplified = T,
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

# get Cerrado extent as terra object 
ext_cerr <- vect(ext(shp_br_cerr))

# crop, mask, and plot 
r_cerr <- terra::crop(spam_perc_all, ext_cerr, mask = T)
r_cerr <- mask(r_cerr, shp_br_cerr)
terra::plot(r_cerr, axes = F, nc = 4, nr = 2)

## 6.6: Export SPAM BR & Cerrado Data --------

writeRaster(spam_perc_all, paste0(path_import, "spam2010_soyb_maize_h_p_y_br.tif"))
writeRaster(r_cerr, paste0(path_import, "spam2010_soyb_maize_h_p_y_cerr.tif"))


