# Title: compare_SIMPLEG_to_real_plotting.R
# Purpose: loads the stats generated in the other compare script and plots them
# Author: Nick Manning 

# Creation Date: 1/24/24
# Last edited: Jan 2024

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
rm(list = ls())

# 0: Load Libraries & Data and Set Constants ---------------------------------------------------------------

## Load Libraries 
library(tidyverse)
library(cowplot)

## Load all STAT data 



## Constants



# 1: IMPORTS & EXPORTS ---------------------

# 5: PLOTTING IMPORTS / EXPORTS-----------------

# 5.1 Imports & Exports -------
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
unique(stat_comp_impexp_plotting$stat)
F_plot_compare(stat_comp_impexp_plotting, "pre", "pre_fao", "Pre-Shock")


### 2.2.2 POST stats -------------
F_plot_compare(stat_comp_impexp_plotting, "post", "post_fao", "Post-Shock")

### 2.2.3 CHANGE stats (mmt) ---------------
F_plot_compare(stat_comp_impexp_plotting, "chg_mmt", "chg_mmt_fao", "Change (mmt) in")








# 2: AREA HARVESTED -----------

# 3: PRODUCTION ------------

