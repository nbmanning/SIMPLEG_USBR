# Title: compare_SIMPLEG_to_real.R
# Purpose: Calculate aggregated transition stats from MapBiomas to compare to our cropland area changes
# Author: Nick Manning 

## Note: This script generates the stat files to be brought into the comparison plotting script and
## relies on the code that generates SIMPLE-G results (processResults_SIMPLEG) and the MapBiomas code (aggStats_MapBiomas) 

# Creation Date: 1/17/24
# Last edited: Jan 2024

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
rm(list = ls())

# 0: Load Libraries & Set Constants ---------------------------------------------------------------

## Libraries 
library(tidyverse)
library(sidrar)
library(tidyUSDA)

## Constants
folder_source <- "../Data_source/FAOSTAT/"

# set up pre- and post
year_pre <- 2010
year_post <- 2013

# add personal key from USDA-NASS
usda_key = "34BD2DD3-9049-37A1-BC2C-D8A967E25E42"


# 1: Real Area & Production (US/BR; FAOSTAT & USDA) ---------

## 1.1: Area - BR & US -----------------------------------------------
### Source 1: Area Harvested from FAOSTAT ------

# set up FAOSTAT function
# set up new function based on 'area'
F_clean_FAOSTAT <- function(df, new_element){
  new <- df %>% 
    # get only relevant columns
    select(Area.Code..M49., Area, Element, Item, Year, Value) %>% 
    # replace with the same input from the SIMPLE-G results 
    mutate(Element = new_element) %>% 
    # change inputs  
    mutate(Item = case_when(
      Item == "Maize (corn)" ~ "maize",
      Item == "Soya beans" ~ "soy")
    ) %>% 
    # rename columns
    rename(
      "code" = Area.Code..M49.,
      "type" = Element,
      "crop" = Item,
      "value" = Value)
  
  return(new)
}

source_area <- read.csv(paste0(folder_source, "FAOSTAT_BR_US_AreaHarv_CornSoy_20072017.csv"))
area <- F_clean_FAOSTAT(source_area, "area") 
stat_fao_area <- area %>% 
  filter(Year==year_pre | Year == year_post)

### Source 2: US Stats from USDA-NASS ----------
# Area Harvested -- SOY
source_area_usda <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "SOYBEANS - ACRES HARVESTED",
  commodity = "SOYBEANS",
  geographic_level = "NATIONAL",
  year = as.character(c(year_pre:year_post)),
  geometry = F) 

area_usda_soy <- source_area_usda %>% 
  # keep only some of the variables
  dplyr::select(
    year, short_desc, reference_period_desc, Value) %>%
  # rename the Value column
  dplyr::rename(
    area = Value,
    crop = short_desc) %>% 
  filter(reference_period_desc == "YEAR") %>% 
  # convert from acres to ha
  mutate(area = area * 0.40468564,
         crop = "soy",
         country = "US") %>% 
  dplyr::select(-reference_period_desc)

# Area Harvested -- MAIZE
source_area_usda <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "CORN, GRAIN - ACRES HARVESTED",
  commodity = "CORN",
  geographic_level = "NATIONAL",
  year = as.character(c(year_pre:year_post)),
  geometry = F) 

area_usda_maize <- source_area_usda %>% 
  # keep only some of the variables
  dplyr::select(
    year, short_desc, reference_period_desc, Value) %>%
  # rename the Value column
  dplyr::rename(
    area = Value,
    crop = short_desc) %>% 
  filter(reference_period_desc == "YEAR") %>% 
  # convert from acres to ha
  mutate(area = area * 0.40468564,
         crop = "maize",
         country = "US") %>% 
  dplyr::select(-reference_period_desc)

# group soy & maize area together
stat_usda_area <- rbind(area_usda_soy, area_usda_maize)

# spread to add a 'total' column then gather to make long & tidy
stat_usda_area <- stat_usda_area %>% 
  pivot_wider(names_from = crop, values_from = area) %>% 
  mutate(total = maize + soy) %>% 
  pivot_longer(cols = -c(year, country), names_to = "crop", values_to = "area")


## 1.2: Real Production -----------------------------------------------
### Source 1: Production from FAOSTAT ------
source_prod <- read.csv(paste0(folder_source, "FAOSTAT_BR_US_Prod_CornSoy_20072017.csv"))
prod <- F_clean_FAOSTAT(source_prod, "prod") 
stat_fao_prod <- prod %>% 
  filter(Year==year_pre | Year == year_post)

### Source 2: US stats from USDA-NASS -------
# Production
# Production -- SOY
source_prod_usda <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "SOYBEANS - PRODUCTION, MEASURED IN BU",
  commodity = "SOYBEANS",
  geographic_level = "NATIONAL",
  year = as.character(c(year_pre:year_post)),
  geometry = F) 

prod_usda_soy <- source_prod_usda %>% 
  # keep only some of the variables
  dplyr::select(
    year, short_desc, reference_period_desc, Value) %>%
  # rename the Value column
  dplyr::rename(
    prod = Value,
    crop = short_desc) %>% 
  filter(reference_period_desc == "YEAR") %>% 
  # convert from bushels to metric tons source: https://grains.org/markets-tools-data/tools/converting-grain-units/
  mutate(prod = prod*0.0272155,
         crop = "soy",
         country = "US") %>% 
  dplyr::select(-reference_period_desc)

# Area Harvested -- MAIZE
source_prod_usda <- getQuickstat(
  key = usda_key,
  program = "SURVEY",
  data_item = "CORN, GRAIN - PRODUCTION, MEASURED IN BU",
  commodity = "CORN",
  geographic_level = "NATIONAL",
  year = as.character(c(year_pre:year_post)),
  geometry = F) 

prod_usda_maize <- source_prod_usda %>% 
  # keep only some of the variables
  dplyr::select(
    year, short_desc, reference_period_desc, Value) %>%
  # rename the Value column
  dplyr::rename(
    prod = Value,
    crop = short_desc) %>% 
  filter(reference_period_desc == "YEAR") %>% 
  # convert from bushels to metric tons source: https://grains.org/markets-tools-data/tools/converting-grain-units/
  mutate(prod = prod*0.0254,
         crop = "maize",
         country = "US") %>% 
  dplyr::select(-reference_period_desc)

# join prod maize & soy together
stat_usda_prod <- rbind(prod_usda_soy, prod_usda_maize)

# spread to add a 'total' column then gather to make long & tidy
stat_usda_prod <- stat_usda_prod %>% 
  pivot_wider(names_from = crop, values_from = prod) %>% 
  mutate(total = maize + soy) %>% 
  pivot_longer(cols = -c(year, country), names_to = "crop", values_to = "prod")

# 2: Imports & Exports ---------------

## 2.1: Load from SIMPLE-G ----------
# from script 'barplot_impexp.r'
load(file = "../Results/SIMPLEG-2023-10-29/imports_exports/df_impexp.RData")
stat_SG_impexp <- df_impexp %>% mutate(chg_mmt = chg/1000)

## 2.2: Real Imports & Exports (FAOSTAT) -----------------------------------------------

### 2.2.1: Imports -------------

### Source 1: UN Comtrade - inputs = source, element
source_imp <- read.csv(paste0(folder_source, "FAOSTAT_AllCountries_CornSoy_Imports_20072017.csv"))
imp <- F_clean_FAOSTAT(source_imp, "Imports")  

# separate to make sure they both have complete years 
imp_soy <- imp %>% 
  filter(crop == "soy") %>% 
  group_by(Area, code) %>%
  complete(Year = 2007:2017) %>%
  rename("imp_soy" = value) %>%
  select(-type, -crop)


imp_maize <- imp %>% 
  # get complete years 
  filter(crop == "maize") %>% 
  group_by(Area, code) %>% 
  complete(Year = 2007:2017) %>%
  # rename for later merging
  rename("imp_maize" = value) %>% 
  select(-crop, -type)

# re-join to get a df of complete cases 
imp <- left_join(imp_maize, imp_soy, by = c("Area", "code", "Year"))

### 2.2.2 Exports ---------
source_exp <- read.csv(paste0(folder_source, "FAOSTAT_AllCountries_CornSoy_Exports_20072017.csv"))
exp <- F_clean_FAOSTAT(source_exp, "Exports")

# get each crop separately
exp_soy <- exp %>%
  filter(crop == "soy") %>%
  group_by(Area, code) %>%
  complete(Year = 2007:2017) %>%
  rename("exp_soy" = value) %>%
  select(-type, -crop)

exp_maize <- exp %>% 
  filter(crop == "maize") %>%
  group_by(Area, code) %>%
  complete(Year = 2007:2017) %>%
  rename("exp_maize" = value) %>%
  select(-type, -crop)

# re-join 
exp <- left_join(exp_maize, exp_soy, by = c("Area", "code", "Year"))

## 2.3: Join SIMPLE-G and FAOSTAT Imp/Exp -------------

# import crosswalk
cross <- read.csv(paste0(folder_source, "FAOtoSIMPLEG_RegionCrosswalk2.csv"))

# remove periods in column names
names(cross) <- gsub("\\.", "", names(cross))

# keep only columns to add to FAO data
cross <- cross %>% 
  select(UNPOPCode, SIMPLEv2RegionCoden154, RegionName) %>% 
  rename("code" = UNPOPCode, "region" = SIMPLEv2RegionCoden154)

# create 'fao_impexp' by joining together and tidying
fao_impexp <- left_join(imp, exp, by = c("Area", "code", "Year"))

# create tidy import / export data at regional level
stat_fao_impexp_crop <- fao_impexp %>% 
  
  # pivot and create import andexport columns
  pivot_longer(cols = -c(Area, code, Year)) %>% 
  separate(name, c('type', 'crop')) %>% 
  mutate(type = case_when(
    type == 'imp' ~ 'Imports',
    type == 'exp' ~ 'Exports'
  )) %>% 
  
  # replace NA's with 0
  mutate_at(vars('value'), ~replace_na(.,0)) %>%
  
  # join with SIMPLE-G crosswalk info
  left_join(cross) %>%
  
  # remove all without a SIMPLE-G region
  drop_na(region) %>%
  
  # get to sum of impots and exports by region 
  group_by(region, RegionName, type, Year, crop) %>% 
  summarise(value = sum(value)) %>% 
  
  # rename to match SIMPLE-G
  rename(
    "region_abv" = region,
    "region" = RegionName
  )


# change from each crop to soy+maize
fao_impexp <- stat_fao_impexp_crop %>% 
  filter(Year == year_post | Year == year_pre) %>%
  # set to same units as SIMPLE-G
  mutate(value = value/1000) %>% 
  # add a pre-post marker
  mutate(time = case_when(
    Year == year_pre ~ "pre",
    Year == year_post ~ "post"
  ))


# get changes (then divide by 1000)
fao_impexp <- fao_impexp %>% 
  # remove Year to have pre- and post- be the separators
  subset(select = -Year) %>% 
  # separate pre- and post into their own columns
  pivot_wider(
    names_from = "time",
    values_from = "value",
  ) %>% 
  group_by(region_abv, region, type) %>%
  summarize(pre = sum(pre),
            post = sum(post)) %>%
  # # add change and change_mmt
  mutate(chg_fao = post-pre) %>%  
  mutate(chg_mmt_fao = chg_fao/1000) %>% 
  # rename pre and post so we can merge later
  rename(
    "pre_fao" = pre,
    "post_fao" = post
  )

# will use this stat later
stat_fao_impexp <- fao_impexp

## 2.3.1: Clean and Prep for Plotting ----------
fao_impexp <- fao_impexp %>% subset(select = -region)

# calculate sums from crops
df_sg_impexp <-  stat_SG_impexp %>% 
  group_by(region_abv, region, type) %>%
  summarize(pre = sum(pre),
            post = sum(post)) %>% 
  mutate(chg = post-pre,
         chg_mmt = chg/1000)

# join SIMPLE-G with FAOSTAT data
comp_impexp_all <- df_sg_impexp %>% 
  left_join(fao_impexp) %>% 
  # remove any mismatches - removes SSA bc missing from crosswalk
  drop_na() %>% 
  select(region_abv, region, type,
         pre, pre_fao, post, post_fao,
         chg, chg_fao, chg_mmt, chg_mmt_fao) %>% 
  mutate_if(is.numeric, round, 2)

stat_comp_impexp <- comp_impexp_all

# get final df's for plotting 
stat_comp_impexp_plotting <- comp_impexp_all %>% 
  pivot_longer(cols = -c(region_abv, region, type), 
               names_to = "stat", values_to = "value")


# 3: Compare Area & Production (SIDRA) ---------------------------------------------------------------
# Search terms:
## "Tabela 1612: Área plantada, área colhida, quantidade produzida, rendimento médio e valor da produção das lavouras temporárias"


# Units: 
## Quantidade produzida = quantity produced (Toneladas)
## Rendimento médio da produção = average production yield (kg/ha)


# Item Codes:
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

## 3.1: National BR Raw Production (Prod & Planted) ------- 
# get area harvested and production for BR 
raw_sidra_br <- get_sidra(x = 1612, 
                          variable =  c(214, 216), # 214 = Production, 216 = Area Harvested
                          period = as.character(c(year_pre, year_post)),# list_year_range, #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                          geo = "Brazil", # Brazil, State, or Município
                          geo.filter = NULL,
                          classific = "c81",
                          category = list(c(0, 2713, 2711)), # 2713 = Soja (em grão); 2711 = Milho (corn) (em grão)
                          header = T,
                          format = 3)

## STATS BR PRE PROD AREA----------
stat_sidra_area_prod_br <- raw_sidra_br %>% filter(Ano == year_pre | Ano == year_post) %>% dplyr::select(-c(Brasil, `Brasil (Código)`, `Nível Territorial (Código)`)) 
test_br <- stat_sidra_area_prod_br %>% filter(Ano == year_pre) 
test_br

# assuming year_pre is 2010.....
# 2010 production of soy (tons)
## 68756343

# 2010 production of corn (tons) 
## 55364271

# 2010 harvested area of soy (ha)
## 23327296

# 2010 harvested area of corn (ha)
## 12678875


## STATS BR POST PROD AREA --------
test_br <- stat_sidra_area_prod_br %>% filter(Ano == year_post) 
test_br

# assuming year_post is 2013.....
# 2013 production of soy (tons)
## 81724477 

# 2013 production of corn (tons) 
## 80273172

# 2013 harvested area of soy (ha)
## 27906675

# 2013 harvested area of corn (ha)
## 15279652

### Clean BR prod & area ------------
names(stat_sidra_area_prod_br) 

stat_sidra_area_prod_br  <- stat_sidra_area_prod_br %>% 
  dplyr::select("Nível Territorial", "Ano", "Variável", "Produto das lavouras temporárias", "Valor") %>% 
  rename("extent" = "Nível Territorial",
         "year" = "Ano",
         "var" = "Variável",
         "type_ag" = "Produto das lavouras temporárias",
         "value" = "Valor") %>% 
  # spread data to clean
  pivot_wider(names_from = type_ag, values_from = value) %>% 
  # rename again
  rename(
    "soy" = "Soja (em grão)",
    "maize" = "Milho (em grão)",
    "total" = "Total"
  ) %>% 
  ## Approach One ##
  # change NA's to 0
  mutate(across(where(anyNA), ~ replace_na(., 0))) %>% 
  # change total to the sum of soy+maize
  mutate(total = soy + maize) %>% 
  # ## Approach Two ## -- not sure if I should omit all the NA's bc they aren't technically 0
  # na.omit()
  # change back to long data
  pivot_longer(
    cols = -c(extent, year, var),
    names_to = "type_ag",
    values_to = "value"
  ) %>%
  # change to ideal variable names
  mutate(var = case_when(
    var == "Quantidade produzida" ~ "prod",
    var == "Área colhida" ~ "area"
  ))
  


## 3.2: Cerrado Production & Area Harvested -------
# get area harvested and production for BR 
raw_sidra_cerr <- get_sidra(x = 1612, 
                          variable =  c(214, 216), # 214 = Production, 216 = Area Harvested
                          period = as.character(c(year_pre, year_post)),# list_year_range, #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                          geo = "MicroRegion", # Brazil, State, or Município
                          geo.filter = NULL,
                          classific = "c81",
                          category = list(c(0, 2713, 2711)), # 2713 = Soja (em grão); 2711 = Milho (corn) (em grão)
                          header = T,
                          format = 3)
#?get_sidra
#The geo argument can be one of "Brazil", "Region", "State", "MesoRegion", "MicroRegion", "MetroRegion", "MetroRegionDiv",
# "IRD", "UrbAglo", "City", "District","subdistrict","Neighborhood","PopArrang". 
# 'geo.filter' lists can/must be named with the same characters.

# CLEAN
prod_area_cerr <-  raw_sidra_cerr %>% 
  dplyr::select("Microrregião Geográfica (Código)", "Ano", "Variável", "Produto das lavouras temporárias", "Valor") %>% 
  rename("code_micro" = "Microrregião Geográfica (Código)",
         "year" = "Ano",
         "var" = "Variável",
         "type_ag" = "Produto das lavouras temporárias",
         "value" = "Valor") %>% 
  mutate(code_micro = as.double(code_micro)) %>% 
  # spread data to clean
  pivot_wider(names_from = type_ag, values_from = value) %>% 
  # rename again
  rename(
    "soy" = "Soja (em grão)",
    "maize" = "Milho (em grão)",
    "total" = "Total"
  ) %>% 
  ## Approach One ##
  # change NA's to 0
  mutate(across(where(anyNA), ~ replace_na(., 0))) %>% 
  # change total to the sum of soy+maize
  mutate(total = soy + maize) %>% 
  # ## Approach Two ## -- not sure if I should omit all the NA's bc they aren't technically 0
  # na.omit()
  pivot_longer(
    cols = -c(code_micro, year, var),
    names_to = "type_ag",
    values_to = "value"
  )


library(geobr)
source_br_micro <- read_micro_region(year = year_pre, simplified = T)

# get ag stats for each microregion
micro <- source_br_micro %>% left_join(prod_area_cerr)

# load cerr shapefile 
load(file = "../Data_Source/MapBiomas/shp_br_cerr.Rdata")

# check CRS for each - should be TRUE
st_crs(micro) == st_crs(shp_br_cerr)

# get municipalities that are at all within the Cerrado
shp_micro_cerr <- st_intersection(micro, shp_br_cerr)
#plot(shp_micro_cerr)

names(shp_micro_cerr) 

## STATS Cerrado Prod & Area --------------
stat_sidra_area_prod_micro_cerr_sf <- shp_micro_cerr %>% 
  dplyr::select(year, code_micro, var, type_ag, value, name_biome) %>% 
  mutate(var = case_when(
    var == "Quantidade produzida" ~ "prod",
    var == "Área colhida" ~ "area"
  ))
  
stat_sidra_area_prod_cerr <- stat_sidra_area_prod_micro_cerr_sf %>%
  # remove spatial aspect
  st_drop_geometry() %>% 
  group_by(year, name_biome, var, type_ag) %>% 
  summarise(value=sum(value))

## 3.3 Import SIMPLE-G AREA & PROD Results -----------

folder_stat <- "../Results/SIMPLEG-2023-10-29/stat_summary/"

load(file = paste0(folder_stat, "mapb_agg_land_trans_br_and_cerr.RData"))
load(file = paste0(folder_stat, "sg_QLAND_QCROP_US_BR_Cerr.RData"))


# 4: SAVE STATS, SUMMARY, UNITS ------------------------
# USDA - US; Area(ha), Prod('metric tonnes' aka 'mt' aka 'tonnes' aka 1000 kg)
save(stat_usda_area, stat_usda_prod, 
     file = paste0(folder_stat, "usda_area_prod.Rdata"))

# FAO - US & BR; Area(ha), Prod(t == tonnes), Imp/Exp(t --> 1000t)
save(stat_fao_area, stat_fao_prod,
     stat_fao_impexp, stat_fao_impexp_crop,  
     file = paste0(folder_stat, "fao_area_prod.Rdata"))

# SIDRA - BR & Cerrado; Area(ha), Prod(tonnes)
## NOTE: I changed NA's to 0 before calculating total, so this could be an underestimation 
save(stat_sidra_area_prod_br, 
     stat_sidra_area_prod_cerr, 
     stat_sidra_area_prod_micro_cerr_sf,  
     file = paste0(folder_stat, "sidra_area_prod.Rdata"))

# Comparisons - Import/Export(1000tonnes)
save(stat_SG_impexp, 
     stat_comp_impexp, 
     stat_comp_impexp_plotting,
     file = paste0(folder_stat, "sg_fao_impexp.Rdata"))

## From Other Scripts ##
# MAPBIOMAS - BR & Cerrado; New QLAND (Transition)(ha)
# load(file = paste0(folder_stat, "mapb_agg_land_trans_br_and_cerr.RData"))

# SIMPLE-G - QLAND (Cropland Area) & QCROP (Crop Production index)
# load(file = paste0(folder_stat, "sg_QLAND_QCROP_US_BR_Cerr.RData"))


## NOTES ON UNITS -----------

# FAOSTAT:
## Area in hectares ('ha')
## Imp / Exp come as tonnes, divided by 1000 to get SIMPLE-G units (1000tonnes)  
## Prod as tonnes (t) (or mmt = million metric ton)

# USDA-NASS 
## Area comes as acres, conv. to ha
## Prod comes as bushels, conv to metric tonnes

# MAPBIOMAS
## Land Transition is in ha

# SIMPLE-G
## QLAND = cropland area in 1000 ha
## QCROP = crop production in 1000-ton (corn-equivalent)
## Imp / Exp in 1000 tonnes

# SIDRA 
## area = ha
## production = tonnes (? maybe tons?)







