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

## Files ##
getwd()
folder <- "../Results/SIMPLEG-2023-10-29/imports_exports/"

# 1: Imports ------
# Load in data
imp_maize
imp_soy

# Join

# Calculate Differences

# Calculate Change

# Plot



# link: https://itecnote.com/tecnote/r-make-all-positive-value-bar-graph-the-same-color-theme-as-bar-graph-with-negative-values-in-ggplot/
dtf1 <- data.frame(ID = c(1:10),Diff = c(-5:4))
dtf1$colour <- ifelse(dtf1$Diff < 0, "firebrick1","steelblue")
dtf1$hjust <- ifelse(dtf1$Diff > 0, 1.3, -0.3)

# 2: Exports ------------
