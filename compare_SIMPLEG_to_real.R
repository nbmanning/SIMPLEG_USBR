# Title: compare_SIMPLEG_to_real.R
# Purpose: Calculate aggregated transition stats from MapBiomas to compare to our cropland area changes

# Creation Date: 1/17/24
# Last edited: Jan 2024

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
rm(list = ls())

# 0: Load Libraries & Set Constants ---------------------------------------------------------------

## Libraries 
library(tidyverse)
library(cowplot)
library(sidrar)


## Constants
folder_source <- "../Data_source/FAOSTAT/"

# 1: Import All Source Data ---------------------------------------------------------------

## 1.1: SIMPLE-G Results -----------------------------------------------

## Import / Export
# from script 'barplot_impexp.r'
load(file = "../Results/SIMPLEG-2023-10-29/imports_exports/df_impexp.RData")
stat_sg_impexp <- df_impexp %>% mutate(chg_mmt = chg/1000)
  

## Area & Production
# from script ... 

## 1.2: Real Area - BR & US -----------------------------------------------
### Source 1: Area Harvested from FAOSTAT
source_area <- read.csv(paste0(folder_source, "FAOSTAT_BR_US_AreaHarv_CornSoy_20072017.csv"))
names(source_area)

area <- source_area %>% 
  select(Area, Element, Item, Year, Value) %>% 
  mutate(Element = "area") %>% 
  mutate(Item = case_when(
    Item == "Maize (corn)" ~ "maize",
    Item == "Soya beans" ~ "soy")
  ) %>% 
  rename(
    "type" = Element,
    "crop" = Item,
    "value" = Value
  )

str(area)
stat_fao_area <- area %>% filter(Year==2010 | Year == 2013)

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

## 1.3: Real Production -----------------------------------------------
### Source 1: Production from FAOSTAT
source_prod <- read.csv(paste0(folder_source, "FAOSTAT_BR_US_Prod_CornSoy_20072017.csv"))
prod <- F_clean_FAOSTAT(source_prod, "Production") 
stat_fao_prod <- prod %>% filter(Year==2010 | Year == 2013)

## 1.4: Real Imports & Exports -----------------------------------------------

### 1.4.1: Imports -------------

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

### 1.4.2 Exports ---------
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

### 1.4.3: Imports & Exports -------------

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


# set up pre- and post
year_pre <- 2010
year_post <- 2013

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

stat_fao_impexp <- fao_impexp

## 1.5: Plot Real Imports & Exports -------
col_neg <- "red"
col_pos <- "blue"

# remove US
fao_impexp_nous <- fao_impexp %>% filter(region_abv != "US")

# separate to imports and exports for plotting 
fao_imp_nous <- fao_impexp_nous %>% filter(type =="Imports")
fao_exp_nous <- fao_impexp_nous %>% filter(type =="Exports")


# plot imports using same format as in 'barplot_impexp.R'
(p_imp <- ggplot(fao_imp_nous, aes(x = region_abv, y = chg_mmt_fao))+
    geom_bar(aes(fill = chg_mmt_fao < 0), stat = "identity") + 
    scale_fill_manual(guide = "none", breaks = c(TRUE, FALSE), values=c(col_neg, col_pos))+
    coord_flip()+
    theme_bw()+
    labs(
      title = "Change in Corn-Soy Imports (million metric ton)",
      x = "",
      y = ""
    )+
    theme(plot.title = element_text(hjust = 0.5))
)

# plot exports using same format as in 'barplot_impexp.R'
(p_exp <- ggplot(fao_exp_nous, aes(x = region_abv, y = chg_mmt_fao))+
   # Set color code on a True-False basis
   geom_bar(aes(fill = chg_mmt_fao < 0), stat = "identity") + 
   # if false, one color, if true, another
   scale_fill_manual(guide = "none", breaks = c(TRUE, FALSE), values=c(col_neg, col_pos))+
   coord_flip()+
   theme_bw()+
   labs(
     title = "Change in Corn-Soy Exports (million metric ton)",
     x = "",
     y = ""
   )+
   theme(plot.title = element_text(hjust = 0.5),
         axis.text.y = element_blank()
   )
)

# set up facet plot
# plot with the individual plots next to one another
# labels give "A" and "B"
(p <- plot_grid(p_imp, p_exp, labels = "AUTO"))

# ggsave(paste0(folder_fig, "bar_impexp_fao.png"),
#        p,
#        width = 12, height = 6)

# 2: Compare SIMPLE-G and FAO -----

## 2.1: Join SIMPLE-G and FAOSTAT ---------
fao_impexp <- fao_impexp %>% subset(select = -region)

# calculate sums from crops
df_sg_impexp <-  df_sg_impexp %>% 
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
comp_impexp <- comp_impexp_all %>% 
  pivot_longer(cols = -c(region_abv, region, type), 
               names_to = "stat", values_to = "value")
  
# comp_imp <- comp_impexp %>% 
#   filter(type == "Imports")
# 
# comp_exp <- comp_impexp %>%
#   filter(type == "Exports")

## 2.2: Plot Changes ---------

# Maybe make a new script for plotting??? This one could just be organizing 

# plot as barplot
## link: https://www.datanovia.com/en/lessons/ggplot-barplot/
col1 <- "purple"
col2 <- "green"

### Working Ex --------------

# WORKS #
# #filter
# comp_imp_plot <- comp_impexp %>%
#   filter(type == "Imports") %>%
#   filter(stat == "pre" | stat == "pre_fao") #%>%
# 
# # plot
# p_imp <- ggplot(comp_imp_plot, aes(x = region_abv, y = value)) +
#   geom_col(aes(fill = stat), position = position_dodge(0.8), width = 0.7)+
#   coord_flip()+
#   scale_fill_manual(values = c(col1, col2),
#                     labels=c('SIMPLE-G', 'FAO'))+
#   labs(
#     title = paste("Pre-Simulation", "Imports"),
#     x="", y=""
#   )+
#   theme(legend.position = "none")
# 
# ##### exports plot
# comp_exp_plot <- comp_impexp %>%
#   filter(type == "Exports") %>%
#   filter(stat == "pre" | stat == "pre_fao") #%>%
# 
# # plot
# p_exp <-ggplot(comp_exp_plot, aes(x = region_abv, y = value)) +
#   geom_col(aes(fill = stat), position = position_dodge(0.8), width = 0.7)+
#   coord_flip()+
#   scale_fill_manual(values = c(col1, col2),
#                     labels=c('SIMPLE-G', 'FAO'))+
#   labs(
#     title = paste("Pre-Simulation", "Exports"),
#     x="", y=""
#   )
# 
# ##### together plot
# (p <- plot_grid(p_imp, p_exp, labels = "AUTO"))


#### turn to fxn
F_plot_compare <- function(df, sg_stat, fao_stat, title){
  
  comp_imp_plot <- df %>%
    filter(type == "Imports") %>%
    filter(stat == sg_stat | stat == fao_stat) #%>%
  
  # plot
  p_imp <- ggplot(comp_imp_plot, aes(x = region_abv, y = value)) +
    geom_col(aes(fill = stat), position = position_dodge(0.8), width = 0.7)+
    coord_flip()+
    scale_fill_manual(values = c(col1, col2),
                      labels=c('SIMPLE-G', 'FAO'))+
    labs(
      title = paste(title, "Imports"),
      x="", y=""
    )+
    theme(legend.position = "none")
  
  ##### exports plot
  comp_exp_plot <- df %>%
    filter(type == "Exports") %>%
    filter(stat == sg_stat | stat == fao_stat) #%>%
  
  # plot
  p_exp <-ggplot(comp_exp_plot, aes(x = region_abv, y = value)) +
    geom_col(aes(fill = stat), position = position_dodge(0.8), width = 0.7)+
    coord_flip()+
    scale_fill_manual(values = c(col1, col2),
                      labels=c('SIMPLE-G', 'FAO'))+
    labs(
      title = paste(title, "Exports"),
      x="", y=""
    )
  
  ##### together plot
  (p <- plot_grid(p_imp, p_exp, labels = "AUTO"))
}


### 2.2.1: PRE-stats ---------------
unique(comp_impexp$stat)
F_plot_compare(comp_impexp, "pre", "pre_fao", "Pre-Shock")


### 2.2.2 POST stats -------------
F_plot_compare(comp_impexp, "post", "post_fao", "Post-Shock")

### 2.2.3 CHANGE stats (mmt) ---------------
F_plot_compare(comp_impexp, "chg_mmt", "chg_mmt_fao", "Change (mmt) in")


# 3: Compare Area & Production ---------------------------------------------------------------
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
stat_sidra_area_prod_br <- raw_sidra_br %>% filter(Ano == 2010 | Ano == 2013) %>% dplyr::select(-c(Brasil, `Brasil (Código)`, `Nível Territorial (Código)`)) 
test_br <- stat_sidra_area_prod_br %>% filter(Ano == 2010) 
test_br
# 2010 production of soy (tons)
## 68756343

# 2010 production of corn (tons) 
## 55364271

# 2010 harvested area of soy (ha)
## 23327296

# 2010 harvested area of corn (ha)
## 12678875


## STATS BR POST PROD AREA --------
test_br <- stat_sidra_area_prod_br %>% filter(Ano == 2013) 
test_br

# 2013 production of soy (tons)
## 81724477 

# 2013 production of corn (tons) 
## 80273172

# 2013 harvested area of soy (ha)
## 27906675

# 2013 harvested area of corn (ha)
## 15279652



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
source_br_micro <- read_micro_region(year = 2010, simplified = T)

# get ag stats for each microregion
micro <- source_br_micro %>% left_join(prod_area_cerr)

# load cerr shapefile 
load(file = "../Data_Source/MapBiomas/shp_br_cerr.Rdata")

# check CRS for each - should be TRUE
st_crs(micro) == st_crs(shp_br_cerr)

# get municipalities that are at all within the Cerrado
shp_micro_cerr <- st_intersection(micro, shp_br_cerr)
#plot(shp_micro_cerr)

# # get just the codes column
# shp_code_muni_in_cerr <- shp_muni_in_cerr %>% select(code_muni)
# shp_code_muni_br <- shp_muni %>% select(code_muni)
# 
# # get territory codes for municipalities in intersection as numeric
# muni_codes_cerr <- shp_muni_in_cerr$code_muni
# muni_codes_br <- shp_muni$code_muni

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

## 3.3 Import SIMPLE-G Results -----------
