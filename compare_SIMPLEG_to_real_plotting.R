# Title: compare_SIMPLEG_to_real_plotting.R
# Purpose: loads the stats generated in the other compare script and plots them
# Author: Nick Manning 

# Creation Date: 1/24/24
# Last edited: Jan 2024

# STATUS: Working! But we still need to 
## Check units and make sure they are consistent, especially with tons corn equivalent vs. tonnes
## make the graphs look prettier

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
rm(list = ls())

# 0: Load Libraries & Data and Set Constants ---------------------------------------------------------------

## Load Libraries -----
library(tidyverse)
library(cowplot)

## Set Constants ----

### folders 
date_string <- "2024-03-03"

#folder_stat <- "../Results/SIMPLEG-2023-10-29/stat_summary/"
folder_stat <- paste0("../Results/SIMPLEG-", date_string, "/stat_summary/")
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
# # USDA - US; Area(ha), Prod('metric tonnes' aka 'mt' aka 'tonnes' aka 1000 kg)
# load(file = paste0(folder_stat, "usda_area_prod.Rdata"))
# 
# # FAO - US & BR; Area(ha), Prod(t == tonnes), Imp/Exp(t --> 1000t)
# load(file = paste0(folder_stat, "fao_area_prod.Rdata"))
# 
# # SIDRA - BR & Cerrado; Area(ha), Prod(tonnes)
# ## NOTE: I changed NA's to 0 before calculating total, so this could be an underestimation 
# load(file = paste0(folder_stat, "sidra_area_prod.Rdata"))
# 
# # Comparisons - Import/Export(1000tonnes)
load(file = paste0(folder_stat, "sg_fao_impexp.Rdata"))
# 
# ## From Other Scripts ##
# # MAPBIOMAS - BR & Cerrado; New QLAND (Transition)(ha)
# load(file = paste0(folder_stat, "mapb_agg_land_trans_br_and_cerr.RData"))
# 
# # SIMPLE-G - QLAND (Cropland Area) & QCROP (Crop Production index)
# load(file = paste0(folder_stat, "sg_QLAND_QCROP_US_BR_Cerr.RData"))



# 1: IMPORTS & EXPORTS ---------------------

# plot as barplot
## link: https://www.datanovia.com/en/lessons/ggplot-barplot/

## set up function to plot based on 'baplot_impexp.r'
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
    )+
    theme(legend.position = "none")

  
  # plot imports and exports together
  p <- plot_grid(p_imp, 
                  p_exp, 
                  labels = c("A", "B"),
                  align = 'vh',
                  scale = 0.9)
  
  # get legend
  # link: https://wilkelab.org/cowplot/articles/shared_legends.html
  legend <- get_legend(
    p_imp + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom",
            legend.title = element_blank())
  )
  
  # add the legend underneath the row we made earlier. Give it 10%
  # of the height of one plot (via rel_heights).
  (p_with_legend <- plot_grid(p, legend, ncol = 1, rel_heights = c(1, .1)))
  
  return(p_with_legend)
  
  # save 
  ggsave(filename = paste0(folder_fig, "impexp_", sg_stat, ".png"),
         plot = p_with_legend,
         width = 12, height = 6)
  
}

### 1.1: PRE-stats ---------------
F_plot_compare(stat_comp_impexp_plotting, "pre", "pre_fao", "Pre-Shock")

### 1.2 POST stats -------------
F_plot_compare(stat_comp_impexp_plotting, "post", "post_fao", "Post-Shock")

### 1.3 CHANGE stats (mmt) ---------------
F_plot_compare(stat_comp_impexp_plotting, "chg_mmt", "chg_mmt_fao", "Change (mmt) in")



# 2: AREA HARVESTED -----------

# Use stat_comp_impexp_plotting as inspo, but instead
# of stat = pre and stat = pre_fao, change to source = FAO, SIMPLE-G, USDA, etc.

## 2.1: Import ---------

# load clean files 
load(file = paste0(folder_stat, "clean_area_prod.Rdata"))

# make long (as a test)
# test <- stat_clean_area %>%
#   #filter(crop == "total") %>% 
#   filter(year == year_pre | year == year_post) %>% 
#   #filter(extent == "Brazil" | extent == "US") %>% 
#   mutate(
#     timing = case_when(
#       year == year_pre ~ "pre",
#       year == year_post ~ "post"
#     )) %>% 
#   pivot_longer(cols = "area", names_to = "stat", values_to = "value")


# run function to plot 
F_plot_compare <- function(df, ag_stat, title){
  
  # first, pivot to make long 
  df <- df %>%
    #filter(crop == "total") %>% 
    filter(year == year_pre | year == year_post) %>% 
    #filter(extent == "Brazil" | extent == "US") %>% 
    mutate(
      timing = case_when(
        year == year_pre ~ "pre",
        year == year_post ~ "post"
      )) %>% 
    pivot_longer(cols = ag_stat, names_to = "stat", values_to = "value")
  
  
  ##### pre-shock plot
  df_pre <- df %>% filter(timing == "pre")
  
  # plot
  p_area_pre <- ggplot(df_pre, aes(x = extent, y = value)) +
      geom_col(aes(fill = source), position = position_dodge(0.8), width = 0.7)+
      coord_flip()+
      # scale_fill_manual(values = c(col1, col2),
      #                   labels=c('SIMPLE-G', 'FAO'))+
      labs(
        title = paste("Pre-Shock", title),
        x="", y=""
      )
  
  ##### post-shock plot
  df_post <- df %>% filter(timing == "post")
  
  # plot
  (p_area_post <- ggplot(df_post, aes(x = extent, y = value)) +
      geom_col(aes(fill = source), position = position_dodge(0.8), width = 0.7)+
      coord_flip()+
      # scale_fill_manual(values = c(col1, col2),
      #                   labels=c('SIMPLE-G', 'FAO'))+
      labs(
        title = paste("Post-Shock", title),
        x="", y=""
      ))
  
  
  # plot imports and exports together
  p <- plot_grid(p_area_pre + theme(legend.position = "none"), 
                 p_area_post + theme(legend.position = "none"), 
                 labels = c("A", "B"),
                 align = 'vh',
                 scale = 0.9)
  
  # get legend
  # link: https://wilkelab.org/cowplot/articles/shared_legends.html
  legend <- get_legend(
    p_area_post + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom",
            legend.title = element_blank())
  )
  
  # add the legend underneath the row we made earlier with 
  # 10% of the height of one plot (via rel_heights)
  (p_with_legend <- plot_grid(p, legend, ncol = 1, rel_heights = c(1, .1)))
 
  # save 
  ggsave(filename = paste0(folder_fig, ag_stat, "_comp", ".png"),
         plot = p_with_legend,
         width = 12, height = 6)
  
  # return() to plot in env
  return(p_with_legend)
}

F_plot_compare(df = stat_clean_area, ag_stat = "area", title = "Cropland Area")
F_plot_compare <- function(df, ag_stat, title){
  
  # first, pivot to make long 
  df <- df %>%
    #filter(crop == "total") %>% 
    filter(year == year_pre | year == year_post) %>% 
    #filter(extent == "Brazil" | extent == "US") %>% 
    mutate(
      timing = case_when(
        year == year_pre ~ "pre",
        year == year_post ~ "post"
      )) %>% 
    pivot_longer(cols = ag_stat, names_to = "stat", values_to = "value")
  
  
  ##### pre-shock plot
  df_pre <- df %>% filter(timing == "pre")
  
  # plot
  p_area_pre <- ggplot(df_pre, aes(x = extent, y = value)) +
    geom_col(aes(fill = source), position = position_dodge(0.8), width = 0.7)+
    coord_flip()+
    # scale_fill_manual(values = c(col1, col2),
    #                   labels=c('SIMPLE-G', 'FAO'))+
    labs(
      title = paste("Pre-Shock", title),
      x="", y=""
    )
  
  ##### post-shock plot
  df_post <- df %>% filter(timing == "post")
  
  # plot
  (p_area_post <- ggplot(df_post, aes(x = extent, y = value)) +
      geom_col(aes(fill = source), position = position_dodge(0.8), width = 0.7)+
      coord_flip()+
      # scale_fill_manual(values = c(col1, col2),
      #                   labels=c('SIMPLE-G', 'FAO'))+
      labs(
        title = paste("Post-Shock", title),
        x="", y=""
      ))
  
  
  # plot imports and exports together
  p <- plot_grid(p_area_pre + theme(legend.position = "none"), 
                 p_area_post + theme(legend.position = "none"), 
                 labels = c("A", "B"),
                 align = 'vh',
                 scale = 0.9)
  
  # get legend
  # link: https://wilkelab.org/cowplot/articles/shared_legends.html
  legend <- get_legend(
    p_area_post + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom",
            legend.title = element_blank())
  )
  
  # add the legend underneath the row we made earlier with 
  # 10% of the height of one plot (via rel_heights)
  (p_with_legend <- plot_grid(p, legend, ncol = 1, rel_heights = c(1, .1)))
  
  # save 
  ggsave(filename = paste0(folder_fig, ag_stat, "_comp", ".png"),
         plot = p_with_legend,
         width = 12, height = 6)
  
  # return() to plot in env
  return(p_with_legend)
}
F_plot_compare(df = stat_clean_prod, ag_stat = "prod", title = "Ag Production")
