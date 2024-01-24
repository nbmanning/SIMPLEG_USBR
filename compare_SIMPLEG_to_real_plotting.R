# Title: compare_SIMPLEG_to_real_plotting.R
# Purpose: loads the stats generated in the other compare script and plots them
# Author: Nick Manning 

# Creation Date: 1/24/24
# Last edited: Jan 2024

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
rm(list = ls())

# 0: Load Libraries & Data and Set Constants ---------------------------------------------------------------

## Load Libraries -----
library(tidyverse)
library(cowplot)

## Set Constants ----

### folders 
folder_stat <- "../Results/SIMPLEG-2023-10-29/stat_summary/"
folder_fig <- "../Figures/compare_to_real/"

### plotting 
col_neg <- "red"
col_pos <- "blue"

col1 <- "purple"
col2 <- "green"

### years 
year_pre <- 2010
year_post <- 2013

## Load all STAT data -----
# USDA - US; Area(ha), Prod('metric tonnes' aka 'mt' aka 'tonnes' aka 1000 kg)
load(file = paste0(folder_stat, "usda_area_prod.Rdata"))

# FAO - US & BR; Area(ha), Prod(t == tonnes), Imp/Exp(t --> 1000t)
load(file = paste0(folder_stat, "fao_area_prod.Rdata"))

# SIDRA - BR & Cerrado; Area(ha), Prod(tonnes)
## NOTE: I changed NA's to 0 before calculating total, so this could be an underestimation 
load(file = paste0(folder_stat, "sidra_area_prod.Rdata"))

# Comparisons - Import/Export(1000tonnes)
load(file = paste0(folder_stat, "sg_fao_impexp.Rdata"))

## From Other Scripts ##
# MAPBIOMAS - BR & Cerrado; New QLAND (Transition)(ha)
load(file = paste0(folder_stat, "mapb_agg_land_trans_br_and_cerr.RData"))

# SIMPLE-G - QLAND (Cropland Area) & QCROP (Crop Production index)
load(file = paste0(folder_stat, "sg_QLAND_QCROP_US_BR_Cerr.RData"))



# 1: IMPORTS & EXPORTS ---------------------

# plot as barplot
## link: https://www.datanovia.com/en/lessons/ggplot-barplot/


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

### 1.1: PRE-stats ---------------
F_plot_compare(stat_comp_impexp_plotting, "pre", "pre_fao", "Pre-Shock")

### 1.2 POST stats -------------
F_plot_compare(stat_comp_impexp_plotting, "post", "post_fao", "Post-Shock")

### 1.3 CHANGE stats (mmt) ---------------
F_plot_compare(stat_comp_impexp_plotting, "chg_mmt", "chg_mmt_fao", "Change (mmt) in")






# 2: AREA HARVESTED -----------

# 3: PRODUCTION ------------
