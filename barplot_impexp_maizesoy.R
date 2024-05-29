# Title: barplot_impexp.R
# Purpose: Read import and export data and re-create barplots in R

# Created by: Nick Manning 
# Created on: March 2024
# Last edited: March 2024

# REQUIRES:
## SIMPLE-G Result file as '.xlsx' 

# 0: Load Libraries & Set Constants ---- 
rm(list = ls())

## Libraries ##
library(tidyverse)
library(ggplot2)
library(cowplot)
library(rio)


## Files ##
getwd()

date_string <- "2024-03-03"

folder_results <- paste0("../Results/SIMPLEG-", date_string, "/imports_exports/")

folder_fig <- paste0("../Figures/", date_string, "/")


## Set Model Pct Here ##
pct_model <- "m"


# 0: Check folders --------------  
# Check if folders exist; if not, create
# List all files and folders in the current directory
files_results <- list.dirs(folder_results)
files_fig <- list.dirs(folder_fig)

# Check if any imp/exp folder exists in the search string folder 

if (!(any(grepl(date_string, files_results)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_results))
  
  cat("Results Folder", date_string, "created.\n")
} else {
  cat("A folder with the string", date_string, "in its name already exists.\n")
}


# check for figures folder
if (!(any(grepl(date_string, files_fig)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_fig))
  
  cat("Figure Folder", date_string, "created.\n")
} else {
  cat("A folder with the string", date_string, "in its name already exists.\n")
}

# 0: Import Source Data ----------
# Load in data as xlsx (diff from previous)
source_path <- paste0(files_results, "regional_results.xlsx")
data_list <- import_list(source_path)


# 1: Tidy ------

# Load in data as xlsx (diff from previous)
# source_path <- paste0(files_results, "regional_results.xlsx")
# data_list <- import_list(source_path)

# function to get one sheet of data and clean it
## var == the name of the sheet we want
## pct == the model type for elasticity; enter either "l" for low, "m" for medium, or "h" for high

# CHANGE TO USE 'data' AS THE ARGUMENT SO THIS CAN BE LOOPED
F_clean_sheet <- function(var, pct){
  # var <-  "Corn Exp"
  # pct <- "l"
  # 
  # get one sheet 
  data <- data_list[[var]]
  
  # Extract just the first letter of each column name
  setting <- substr(colnames(data), 1, 1)
  
  # Get the combined model setting (l,m, or h) and variable; e.g. Pct-m
  data <- rbind(setting, data)
  
  # Combine the first two rows with a hyphen in between
  combined_row <- paste(data[2, ], data[1, ], sep = "-")
  
  # Add the combined row as the first row
  data <- rbind(combined_row, data[-c(1, 2), ])
  
  #get model info then remove 
  model_info <- data[2, ]
  data <- data[-2, ]
  
  # Update column names to model variable 
  colnames(data) <- as.character(unlist(data[1, ]))
  
  colnames(data)[1] <- "region_abv"
  
  # Remove the duplicated row
  data <- data[-1, ]
  
  
  # Assuming your data frame is named data
  # Filter columns based on certain character in row 2
  char_to_keep <- pct  # NOTE: Change this to the model setting you want to keep
  
  # Save pre-existing column names
  pre_existing_colnames <- colnames(data)
  
  # Filter columns based on predefined character in column names
  columns_to_keep <- c(TRUE, sapply(pre_existing_colnames[-1], function(col_name) substr(col_name, nchar(col_name), nchar(col_name)) == char_to_keep))
  
  # Subset the data frame to keep the desired columns
  data <- data[, columns_to_keep]
  
  # Remove spaces and numbers before regions
  ## regex: remove anything up to and including the first space
  data$region_abv <- gsub(".*\\ ", "", data$region_abv)
  data$variable <- var
  
  # split column to get crop and type 
  data <- data %>% 
    mutate(crop = str_extract(variable, "^[^ ]+"),
           type = str_extract(variable, "[^ ]+$"))
  
  # add column based on var 
  data <- data %>% 
    mutate(modeltype = case_when(
      pct == "l" ~ "low",
      pct == "m" ~ "med",
      pct == "h" ~ "high"
    ))
  
  # rename columns - can ADD TO FUNCTION USING SOMETHING LIKE paste0(-,pct)
  data <- data %>% 
    rename(
      "pct_chg" = paste0("Pct-", pct),
      "pre" = paste0("Pre-", pct),
      "post" = paste0("Post-", pct),
      "chg" = paste0("CH-", pct)
    )
  
  # convert certain columns to numeric
  data <- data %>%
    #mutate_at(vars(columns_to_convert), as.numeric)
    mutate_at(vars(c("pct_chg", "pre", "post", "chg")), as.numeric) %>% 
    mutate(chg_mmt = chg/1000)

  return(data)
}




test <- F_clean_sheet("Soy Exp", "m")
test2 <- F_clean_sheet("Soy Exp", "l")

# 2 Run Fxn & Join --------

# reset model variable here if you want to re-run with different amounts 
# pct_model <- "m"

## 2.1: Exports -------
# Get Exports  
exp_soy <- F_clean_sheet(var = "Soy Exp", pct = pct_model)
exp_corn <- F_clean_sheet(var = "Corn Exp", pct = pct_model)
exp_cornsoy <- rbind(exp_soy, exp_corn)

# Rename to change 
exp <- exp_cornsoy 


# Get Exports  
exp_soy <- F_clean_sheet(var = "Soy Exp", pct = pct_model)
exp_corn <- F_clean_sheet(var = "Corn Exp", pct = pct_model)
exp_cornsoy <- rbind(exp_soy, exp_corn)

# get sum by region
exp <- aggregate(exp_cornsoy$chg, list(exp_cornsoy$region_abv), FUN=sum)

# rename
names(exp) <- c("region_abv", "chg")

# get million metric tons 
exp$chg_mmt <- (exp$chg)/1000

# exclude us
exp_nous <- exp %>% filter(region_abv != "US")

## 2.2 Imports ----------
# Get Imports  
imp_soy <- F_clean_sheet(var = "Soy Imp", pct = pct_model)
imp_corn <- F_clean_sheet(var = "Corn Imp", pct = pct_model)
imp_cornsoy <- rbind(imp_soy, imp_corn)

# get sum by region
imp <- aggregate(imp_cornsoy$chg, list(imp_cornsoy$region_abv), FUN=sum)

# rename
names(imp) <- c("region_abv", "chg")

# get million metric tons 
imp$chg_mmt <- (imp$chg)/1000

# exclude us
imp_nous <- imp %>% filter(region_abv != "US")


# 3: Separate Bar Plots --------
col_neg <- "red"
col_pos <- "blue"

# plot vertical barplot
#helpful link: https://stackoverflow.com/questions/48463210/how-to-color-code-the-positive-and-negative-bars-in-barplot-using-ggplot

F_ggplot_bar_vert_sep <- function(df, y_var, title_text, save_text){
  
  # need to do to use character evaluation
  y_var <- rlang::sym(y_var)
  
  # plot
  (p <- ggplot(df, aes(x = region_abv, y = !! y_var ))+
    # Set color code on a True-False basis
    geom_bar(aes(fill = !! y_var < 0), stat = "identity") + 
    # if false, one color, if true, another
    scale_fill_manual(guide = "none", breaks = c(TRUE, FALSE), values=c(col_neg, col_pos))+
    coord_flip()+
    theme_bw()+
    labs(
      title = title_text,
      x = "",
      y = ""
    )+
    theme(
      plot.title = element_text(hjust = 0.5),
      # remove y-axis text (use when we're merging exp and imp graphs)
      #axis.text.y = element_blank()
    )
  )
  # save
  ggsave(paste0(folder_fig, save_text),
         width = 6, height = 8)
  
  return(p)
  
}


## 3.1 Exports -----------------
### Corn-Soy Exports ------
(p_exp <- F_ggplot_bar_vert_sep(
  df = exp_nous,
  y_var = "chg_mmt",
  title_text = "Change in Corn-Soy Exports (million metric ton)",
  save_text = "bar_exp_fxn.png"
))


### Corn Exports ------------
exp_corn_nous <- exp_corn %>% filter(region_abv != "US")
(p_exp_corn <- F_ggplot_bar_vert_sep(
  df = exp_corn_nous,
  y_var = "chg_mmt",
  title_text = "Change in Corn Exports (million metric ton)",
  save_text = "bar_exp_corn.png"
))


### Soy Exports ------------
exp_soy_nous <- exp_soy %>% filter(region_abv != "US")
(p_exp_soy <- F_ggplot_bar_vert_sep(
  df = exp_soy_nous,
  y_var = "chg_mmt",
  title_text = "Change in Soy Exports (million metric ton)",
  save_text = "bar_exp_soy.png"
))


## 3.2 Imports ------------------
### Corn-Soy Imports ------
(p_imp <- F_ggplot_bar_vert_sep(
  df = imp_nous,
  y_var = "chg_mmt",
  title_text = "Change in Corn-Soy Imports (million metric ton)",
  save_text = "bar_imp_fxn.png"
))


### Corn Imports ------------
imp_corn_nous <- imp_corn %>% filter(region_abv != "US")
(p_imp_corn <- F_ggplot_bar_vert_sep(
  df = imp_corn_nous,
  y_var = "chg_mmt",
  title_text = "Change in Corn Imports (million metric ton)",
  save_text = "bar_imp_corn.png"
))


### Soy Imports ------------
imp_soy_nous <- imp_soy %>% filter(region_abv != "US")
(p_imp_soy <- F_ggplot_bar_vert_sep(
  df = imp_soy_nous,
  y_var = "chg_mmt",
  title_text = "Change in Soy Imports (million metric ton)",
  save_text = "bar_imp_soy.png"
))

# 4: Facet Plot ------------
F_ggplot_bar_facet <- function(df, y_var, facet_var, title_text, save_text){
  # set y variable to use in ggplot- need to do to use character evaluation
  y_var <- rlang::sym(y_var)
  
  # plot 
  p <- ggplot(df, aes(x = region_abv, y = !! y_var))+
    geom_bar(aes(fill = !! y_var < 0), stat = "identity") + 
    scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c(col_neg, col_pos))+
    coord_flip()+
    facet_wrap({{facet_var}})+
    theme_bw()+
    labs(
      title = title_text,
      x = "",
      y = ""
    )+
    theme(plot.title = element_text(hjust = 0.5))
  
  # save
  ggsave(paste0(folder_fig, save_text),
         width = 12, height = 8)
  
  # plot in environment 
  return(p)
}

## 4.1 Corn & Soy Facets -------

### Exports --------
exp_cornsoy_nous <- exp_cornsoy %>% filter(region_abv != "US")

# plot with facets
(F_ggplot_bar_facet(
  df = exp_cornsoy_nous,
  y_var = "chg_mmt",
  facet_var = "crop",
  title_text = "Change in Corn & Soy Exports (million metric ton)",
  save_text = "bar_f_exp_cornsoy.png"
))

### Imports ------------
imp_cornsoy_nous <- imp_cornsoy %>% filter(region_abv != "US")

# plot with facets
(F_ggplot_bar_facet(
  df = imp_cornsoy_nous,
  y_var = "chg_mmt",
  facet_var = "crop",
  title_text = "Change in Corn & Soy Imports (million metric ton)",
  save_text = "bar_f_imp_cornsoy.png"
))

### Corn v. Soy ------------
imp_cornsoy$type <- "Imports"
exp_cornsoy$type <- "Exports"

impexp_cornsoy <- rbind(imp_cornsoy, exp_cornsoy)
impexp_cornsoy_nous <- impexp_cornsoy %>% filter(region_abv!= "US")

# plot just soy imp & exp with facets
(F_ggplot_bar_facet(
  df = impexp_cornsoy_nous %>% filter(crop == "Soy"),
  y_var = "chg_mmt",
  facet_var = "type",
  title_text = "Change in Soy Exports & Imports (million metric ton)",
  save_text = "bar_f_impexp_soy.png"
))

# plot just corn imp & exp with facets
(F_ggplot_bar_facet(
  df = impexp_cornsoy_nous %>% filter(crop == "Corn"),
  y_var = "chg_mmt",
  facet_var = "type",
  title_text = "Change in Corn Exports & Imports (million metric ton)",
  save_text = "bar_f_impexp_corn.png"
))

## 4.2 Agg imports & Exports Facet -------

# add facet column
imp$type <- "Imports"
exp$type <- "Exports"
impexp <- rbind(imp, exp)

impexp_nous <- impexp %>% filter(region_abv != "US")

# plot with facets
(F_ggplot_bar_facet(
  df = impexp_nous,
  y_var = "chg_mmt",
  facet_var = "type",
  title_text = "Change in Corn and Soy Imports & Exports (million metric ton)",
  save_text = "bar_f_impexp.png"
))


# 5: Facet Plot 2 ------
# plot with the individual plots next to one another
# labels give "A" and "B"
(p <- plot_grid(p_imp, p_exp, labels = "AUTO"))
ggsave(paste0(folder_fig, "bar_impexp.png"),
       p,
       width = 12, height = 6)


# 6: Save import and export df's -----------------
# add labels and join

df_impexp_cornsoy <- rbind(imp_cornsoy, exp_cornsoy)
df_impexp <- rbind(imp, exp)

# save
save(df_impexp_cornsoy, file = paste0(files_results, "df_impexp_cornsoy.RData"))
save(df_impexp, file = paste0(files_results, "df_impexp.RData"))


# 7: Calc Totals --------------------
F_calc_totals <- function(data){
  ## add row for total changes ##
  # Calculate sum for columns A and B, and mean for column C
  summarised_data <- data %>%
    summarise(
      pre = sum(pre),
      post = sum(post),
    ) 
  
  # get character values from the dataset   
  summarised_data <- summarised_data %>% 
    mutate(
    # ca
      pct_chg = ((post-pre)/pre)*100,
      chg = post-pre,
      chg_mmt = chg/1000,
      
      region_abv = "Total",
      variable = data$variable[1],
      crop = data$crop[1],
      type = data$type[1],
      modeltype = data$modeltype[1]
    )
  
  
  # Add a row at the bottom with the total values
  data <- summarised_data %>%
    bind_rows(data, .)
  
  return(data)
}

test_total <- F_calc_totals(test)

## 7.1 Corn & Soy Imports & Exports ----------
sheets <- names(data_list)
data_clean <- lapply(X = sheets, FUN = F_clean_sheet, pct = "m")
names(data_clean) <- names(data_list)
data_clean <- lapply(X = data_clean, FUN = F_calc_totals)

rio::export(
  data_clean, 
  file = paste0(folder_results, 'regional_results_clean_', pct_model, '.xlsx'))
