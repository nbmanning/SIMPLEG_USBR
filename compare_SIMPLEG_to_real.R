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

## Constants
folder_source <- "../Data_source/FAOSTAT/"

# 1: Import All Source Data ---------------------------------------------------------------

## 1.1: SIMPLE-G Results -----------------------------------------------

## Import / Export
# from script 'barplot_impexp.r'
load(file = "../Results/SIMPLEG-2023-10-29/imports_exports/df_impexp.RData")
sg_impexp <- df_impexp %>% mutate(chg_mmt = chg/1000)
  

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
fao_impexp <- fao_impexp %>% 
  
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

fao_impexp <- fao_impexp %>% 
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
  mutate(chg_fao_mmt = chg_fao/1000) %>% 
  # rename pre and post so we can merge later
  rename(
    "pre_fao" = pre,
    "post_fao" = post
  )

## 1.5: Plot Real Imports & Exports -------
col_neg <- "red"
col_pos <- "blue"

# remove US
fao_impexp_nous <- fao_impexp %>% filter(region_abv != "US")

# separate to imports and exports for plotting 
fao_imp_nous <- fao_impexp_nous %>% filter(type =="Imports")
fao_exp_nous <- fao_impexp_nous %>% filter(type =="Exports")


# plot imports using same format as in 'barplot_impexp.R'
(p_imp <- ggplot(fao_imp_nous, aes(x = region_abv, y = chg_fao_mmt))+
    geom_bar(aes(fill = chg_fao_mmt < 0), stat = "identity") + 
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
(p_exp <- ggplot(fao_exp_nous, aes(x = region_abv, y = chg_fao_mmt))+
   # Set color code on a True-False basis
   geom_bar(aes(fill = chg_fao_mmt < 0), stat = "identity") + 
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
fao_impexp <- fao_impexp %>% subset(select = -region)

# calculate sums from crops
sg_impexp <-  sg_impexp %>% 
  group_by(region_abv, region, type) %>%
  summarize(pre = sum(pre),
            post = sum(post)) %>% 
  mutate(chg = post-pre,
         chg_mmt = chg/1000)

# join SIMPLE-G with FAOSTAT data
comp_impexp<- sg_impexp %>% 
  left_join(fao_impexp) %>% 
  # remove any mismatches - removes SSA bc missing from crosswalk
  drop_na() %>% 
  select(region_abv, region, type,
         pre, pre_fao, post, post_fao,
         chg, chg_fao, chg_mmt, chg_fao_mmt) %>% 
  mutate_if(is.numeric, round, 2)

  
# 5: Save import and export df's ---

# 2: Compare Area ---------------------------------------------------------------
# 3: Compare Production ---------------------------------------------------------------
