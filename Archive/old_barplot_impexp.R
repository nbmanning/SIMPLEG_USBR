# Title: barplot_impexp.R
# Purpose: (OLD) Read import and export data and re-create barplots in R
# USE barplot_impexp_maizesoy.R instead!

# Created by: Nick Manning 
# Created on: Dec 2023
# Last edited: Dec 2023

# REQUIRES:
## SIMPLE-G Result files as '.CSV' 

# 0: Load Libraries & Set Constants ---- 
rm(list = ls())

## Libraries ##
library(tidyverse)
library(ggplot2)
library(cowplot)

## Files ##
getwd()
folder_data <- "../Results/SIMPLEG-2023-10-29/imports_exports/"
folder_fig <- "../Figures/"
# 1: Imports ------

## tidy -----
# Load in data
imp_maize <- read.csv(paste0(folder_data, "imp_maize.csv"))
imp_soy <- read.csv(paste0(folder_data, "imp_soy.csv"))

# MANUAL Rename - always double-check
cols <- c("region_abv", "pct_chg", "pre", "post", "chg", "region")

names(imp_soy) <- cols
names(imp_maize) <- cols

# Set Crop Col
imp_soy$crop <- "soy"
imp_maize$crop <- "maize"

# Join
imp <- rbind(imp_soy, imp_maize)

#imp <- select(imp, "region_abv", "region", "chg", "crop")

# Remove spaces and numbers before regions
## regex: remove anything up to and including the first space
imp$region_abv <- gsub(".*\\ ", "", imp$region_abv)

# save for joining 
df_imp <- imp 

# get sum by region
imp <- aggregate(imp$chg, list(imp$region), FUN=sum)
names(imp) <- c("region", "chg")
imp$chg_mmt <- imp$chg/1000

# exclude us
imp_nous <- imp %>% filter(region != "United States")


## plot --------

### set colors here -------
col_neg <- "red"
col_pos <- "blue"

(p_imp <- ggplot(imp_nous, aes(x = region, y = chg_mmt))+
  geom_bar(aes(fill = chg_mmt < 0), stat = "identity") + 
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

# save
ggsave(paste0(folder_fig, "bar_imp.png"),
       width = 6, height = 8)


# 2: Exports ------------
## tidy -----
# Load in data
exp_maize <- read.csv(paste0(folder_data, "exp_maize.csv"))
exp_soy <- read.csv(paste0(folder_data, "exp_soy.csv"))

# Rename
cols <- c("region_abv", "pct_chg", "pre", "post", "chg", "region")

names(exp_soy) <- cols
names(exp_maize) <- cols

# Set Crop Col
exp_soy$crop <- "soy"
exp_maize$crop <- "maize"

# Join
exp <- rbind(exp_soy, exp_maize)
#exp <- select(exp, "region_abv", "region", "chg", "crop")

# Remove spaces and numbers before regions
## regex: remove anything up to and including the first space
exp$region_abv <- gsub(".*\\ ", "", exp$region_abv)

# Save for later 
df_exp <- exp 

# get sum by region
exp <- aggregate(exp$chg, list(exp$region), FUN=sum)
names(exp) <- c("region", "chg")
exp$chg_mmt <- exp$chg/1000

# exclude us
exp_nous <- exp %>% filter(region != "United States")

## plot --------
# plot vertical barplot
#helpful link: https://stackoverflow.com/questions/48463210/how-to-color-code-the-positive-and-negative-bars-in-barplot-using-ggplot
(p_exp <- ggplot(exp_nous, aes(x = region, y = chg_mmt))+
   # Set color code on a True-False basis
   geom_bar(aes(fill = chg_mmt < 0), stat = "identity") + 
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

# save
ggsave(paste0(folder_fig, "bar_exp.png"),
       width = 6, height = 8)


# 3: Facet Plot ------------
# add facet column
imp$type <- "Imports"
exp$type <- "Exports"
impexp <- rbind(imp, exp)

impexp_nous <- impexp %>% filter(region != "United States")

# plot with facets
ggplot(impexp_nous, aes(x = region, y = chg_mmt))+
  geom_bar(aes(fill = chg_mmt < 0), stat = "identity") + 
  scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c(col_neg, col_pos))+
  coord_flip()+
  facet_wrap("type")+
  theme_bw()+
  labs(
    title = "Change in Corn-Soy Exports (million metric ton)",
    x = "",
    y = ""
  )+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(folder_fig, "bar_impexp_f.png"),
       width = 12, height = 8)

# 4: Facet Plot 2 ------
# plot with the individual plots next to one another
# labels give "A" and "B"
(p <- plot_grid(p_imp, p_exp, labels = "AUTO"))
ggsave(paste0(folder_fig, "bar_impexp.png"),
       p,
       width = 12, height = 6)

# 5: Save import and export df's -----------------
# add labels and join
df_imp$type <- "Imports"
df_exp$type <- "Exports"
df_impexp <- rbind(df_imp, df_exp)

# save
save(df_impexp, file = "../Results/SIMPLEG-2023-10-29/imports_exports/df_impexp.RData")

