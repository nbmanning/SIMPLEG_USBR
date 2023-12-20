# Title: barplot_impexp.R
# Purpose: Read impor and export data and re-create barplots in R

# Created by: Nick Manning 
# Created on: Dec 2023
# Last edited: Dec 2023

# REQUIRES:
## SIMPLE-G Result files as '.CSV' 

# Load Libraries & Set Constants ---- 
rm(list = ls())

## Libraries ##
library(tidyverse)
library(ggplot2)

## Files ##
getwd()
folder <- "../Results/SIMPLEG-2023-10-29/imports_exports/"

# 1: Imports ------

## tidy -----
# Load in data
imp_maize <- read.csv(paste0(folder, "imp_maize.csv"))
imp_soy <- read.csv(paste0(folder, "imp_soy.csv"))

# Rename
cols <- c("region_abv", "pct_chg", "pre", "post", "chg", "region")

names(imp_soy) <- cols
names(imp_maize) <- cols

# Set Crop Col
imp_soy$crop <- "soy"
imp_maize$crop <- "maize"

# Join
imp <- rbind(imp_soy, imp_maize)
imp <- select(imp, "region_abv", "region", "chg", "crop")

# Remove spaces and numbers before regions
## regex: remove anything up to and including the first space
imp$region_abv <- gsub(".*\\ ", "", imp$region_abv)

# get sum by region
imp <- aggregate(imp$chg, list(imp$region), FUN=sum)
names(imp) <- c("region", "chg")
imp$chg_mmt <- imp$chg/1000


## plot --------
col_neg <- "red"
col_pos <- "blue"

(p_imp <- ggplot(imp, aes(x = region, y = chg_mmt))+
  geom_bar(aes(fill = chg_mmt < 0), stat = "identity") + 
  scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c(col_neg, col_pos))+
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
ggsave("../Figures/bar_imp.png",
       width = 6, height = 8)


# 2: Exports ------------
# Load in data
exp_maize <- read.csv(paste0(folder, "exp_maize.csv"))
exp_soy <- read.csv(paste0(folder, "exp_soy.csv"))

# Rename
cols <- c("region_abv", "pct_chg", "pre", "post", "chg", "region")

names(exp_soy) <- cols
names(exp_maize) <- cols

# Set Crop Col
exp_soy$crop <- "soy"
exp_maize$crop <- "maize"

# Join
exp <- rbind(exp_soy, exp_maize)
exp <- select(exp, "region_abv", "region", "chg", "crop")

# Remove spaces and numbers before regions
## regex: remove anything up to and including the first space
exp$region_abv <- gsub(".*\\ ", "", exp$region_abv)

# get sum by region
exp <- aggregate(exp$chg, list(exp$region), FUN=sum)
names(exp) <- c("region", "chg")
exp$chg_mmt <- exp$chg/1000


## plot --------
(p_exp <- ggplot(exp, aes(x = region, y = chg_mmt))+
  geom_bar(aes(fill = chg_mmt < 0), stat = "identity") + 
  scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c(col_neg, col_pos))+
  coord_flip()+
  theme_bw()+
  labs(
    title = "Change in Corn-Soy Exports (million metric ton)",
    x = "",
    y = ""
  )+
  theme(plot.title = element_text(hjust = 0.5))
)

# save
ggsave("../Figures/bar_exp.png",
       width = 6, height = 8)


# 3: Facet Plot ------------
imp$type <- "Imports"
exp$type <- "Exports"
impexp <- rbind(imp, exp)

ggplot(impexp, aes(x = region, y = chg_mmt))+
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

ggsave("../Figures/bar_impexp_f.png",
       width = 12, height = 8)

# 4: Facet Plot 2 ------
library(cowplot)

(p <- plot_grid(p_imp, p_exp, labels = "AUTO"))
ggsave("../Figures/bar_impexp.png", p,
       width = 12, height = 6)
