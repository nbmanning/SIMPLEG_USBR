# Title: processResults_SIMPLEG_2_maizesoy_MS.R
# Purpose: Read and plot SIMPLE-G results from text format across US, World, BR, and Cerrado

# Initial date: Aug 23, 2024
# Last edited: Aug 2024

# NOTES ------------------------------------- 
# REQUIRES:
## RUN processResults_SIMPLEG_1.R to get the r_maizesoy.Rdata and shp_usbr.RData files for Section 2
## RUN aggStats_Mapbiomas.R to get mapb_col8_clean_long.Rdata for Section 8
## 'regional_results.xlsx' from SIMPLE-G output files for Section 1

# FOLDER STRUCTURE:
## Create the following folders in your local 'Results' directory:
### 'raster' which houses the results as rasters to pull into a GIS
### 'summary_tables' which houses a table with the stats for each area (min, mean, median, 1st & 3rd Quartiles, max, and NA's)
### 'stat_summary' which houses the raw values for changes in cropland area and production as an R data file to bring into another script   


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 0: Load Libraries & Set Constants ------------------------------------------------------------------------
rm(list = ls())

## Libraries -----
# imp-exp plots
#library(tidyverse)
library(rio)
library(cowplot)
library(dplyr)
library(stringr)
library(ggplot2)
library(stringr)
library(tidyr)
library(openxlsx)

# SIMPLE-G maps 
library(raster) # use for initial raster stack and basic plotting
library(terra) # use to wrangle geospatial data and plot
library(RColorBrewer) # use for adding colorblind-friendly color palettes 
library(rasterVis) # use for easy violin plot 
library(reshape2) # use for melting data to then use ggplot
library(sf)
library(tidyterra) # plot using ggplot() instead of base R with 'terra'
library(ggspatial) # N arrow and Scale Bar w tidyterrra

# transition maps
library(geobr)

## Constants ------

# NOTE: change this when you change the result file to one of three TXT files
## hi: enter "_hi" ;
## med: enter "_m" ;
## lo: enter "_lo" ;
## out / default; enter ""


### Loading & Saving ###
# Define the model date 
# NOTE: Assumes the results are downloaded and saved in YYYY-MM-DD format
date_string <- "2024-11-15"
date_string_nodash <- gsub("-", "", date_string)

# Set model version & parameter flexibility
datafile_version <- "sg1x3x10_v2411_US_Heat"
pct <- "_m" # change when you change 'datafile'
pct_model <- "m" # for the imp/exp cleaning

#pct_title <- " - Med" # for plotting, either " - High" or " - Low" or "" or "- Med"
pct_title <- "" # note: changed Aug 2024 by setting -med to nothing, as it is the default

# create vars to house results
folder_der <- "../Data_Derived/"
folder_der_date <- paste0(folder_der, date_string, "/")

folder_fig <- "../Figures/"
folder_fig <- paste0(folder_fig, date_string, "/")

folder_results <- paste0("../Results/SIMPLEG-", date_string, "/")
folder_results_impexp <- paste0("../Results/SIMPLEG-", date_string, "/imports_exports/")

folder_stat <- paste0(folder_results, "stat_summary/")

### Plotting ###
size_title = 2.0
size_labels = 1.5
size_axis_nums = 1.0


## Create Folders -----

files_results <- list.dirs(folder_results)
files_results_impexp <- list.dirs(folder_results_impexp)

files_fig <- list.dirs(folder_fig)

# Check if a folder exists for this set of results (e.g. if date_string == '2024-03-03') 

if (!(any(grepl(date_string, files_results)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_results))
  
  cat("Results Folder", date_string, "created.\n")
} else {
  cat("A results folder with the string", date_string, "in its name already exists.\n")
}


# do the same but specifically for imp/exp folder
if (!(any(grepl(date_string, files_results_impexp)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_results_impexp))
  
  cat("Imp/Exp Results Folder", date_string, "created.\n")
} else {
  cat("An Imp/Exp results folder with the string", date_string, "in its name already exists.\n")
}


# check for figures folder
if (!(any(grepl(date_string, files_fig)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_fig))
  
  cat("Figure Folder", date_string, "created.\n")
} else {
  cat("A figures folder with the string", date_string, "in its name already exists.\n")
}

# 0: Functions ------------------------------------------------------------------------

# list functions used multiple times here

## 0.1: ImpExp Functions --------

# Import and clean each Regional Result sheet from 'regional_results.xlsx'
F_clean_sheet <- function(var, pct){
  
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


# Plotting Fxn
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
        # remove y-axis text (use when merging exp and imp graphs)
        #axis.text.y = element_blank()
      )
  )
  # save
  ggsave(paste0(folder_fig, save_text),
         width = 6, height = 8)
  
  return(p)
  
}


## 0.2: Spatial Analysis for Areas of Interest ------- 

## NOTE: The plotting functions for each individual AOI are in their individual sections; e.g. Section 4.2 for plotting global results ##

# Fxn to Create and Save Violin Plots 
## NOTE: the SI code is similar but includes histograms and violin plots for more variables 
F_p_violin <- function(df, area){

  ## subset and change names ##
  
  # separate each for Maize and Soy
  df_pct_maizesoy <- df %>% 
    subset(c("pct_LND_MAZ", "pct_LND_SOY")) 
  names(df_pct_maizesoy) <- c("Maize", "Soy")
  
  df_rawch_maizesoy <- df %>% 
    subset(c("rawch_MAZ", "rawch_SOY"))
  names(df_rawch_maizesoy) <- c("Maize", "Soy")
  
  # violin plots for % change and raw change for maize and soy 
  # set size_title, size_labels, and size_axis_nums in the "Constants" section
  p1 <- bwplot(df_pct_maizesoy, 
               main = list(paste(area, "% Change Maize & Soy", pct_title), cex = size_title),
               ylab = list("% Change", cex = size_labels),
               scales=list(
                 x = list(rot=45, cex = size_labels),
                 y = list(cex = size_axis_nums))
  )
  
  p2 <- bwplot(df_rawch_maizesoy, 
               main = list(paste(area, "Raw Change Maize & Soy", pct_title), cex = size_title),
               ylab = list("Area (kha)", cex = size_labels),
               scales=list(
                 x = list(rot=45, cex = size_labels),
                 y = list(cex = size_axis_nums))
  )
  
  # save figures as PNGs - filename will include the area (e.g. US, Brazil) and model elasticity version (e.g. l, m, or h)
  png(filename = paste0(folder_fig, str_to_lower(area), pct, "_bw", "_pctchange", "_maizesoy", ".png"))
  plot(p1)
  dev.off()
  
  png(filename = paste0(folder_fig, str_to_lower(area), pct, "_bw", "_rawchange", "_maizesoy", ".png"))
  plot(p2)
  dev.off()
  
  # also plot these figures in the code window
  return(p1)
  return(p2)
}


# Fxn to incorporate both of these into one function
# Note that we exclude the clamping to 50,000 here as we fixed this step in our analysis
F_aoi_prep <- function(shp, area_name){
  
  ## Clip to AOI Extent ##
  # get extent as terra object for plotting
  ext_area <- vect(ext(shp))
  
  # set CRS of extent spatial vector
  crs(ext_area) <- crs(shp)
  
  # change so r (results raster) becomes the CRS of our shapefile 
  # we made a new variable her to not change the global r variable in our environment
  r_func <- r
  crs(r_func) <- crs(ext_area)
  
  # crop and masking to just the extent of interest
  r_aoi <- terra::crop(r_func, ext_area, mask = T) 
  r_aoi <- mask(r_aoi, shp)
  
  ## NICK: Not sure if we need any of this since we aren't clamping anymore - this other section just re-orders the layers
  # ## Call Count and Clamp functions to cap the grid cells at 50,000 ##
  # # QLAND #
  # F_count_invalid(r_aoi, "new_QLAND")
  # r_aoi_new_qland <- F_clamp(r_aoi, "new_QLAND")
  # 
  # # QCROP #
  # F_count_invalid(r_aoi, "new_QCROP")
  # r_aoi_new_qcrop <- F_clamp(r_aoi, "new_QCROP")
  # 
  # # Crop Specific
  # F_count_invalid(r_aoi, "new_LND_MAZ")
  # r_aoi_new_maize <- F_clamp(r_aoi, "new_LND_MAZ")
  # 
  # # 
  # F_count_invalid(r_aoi, "new_LND_SOY")
  # r_aoi_new_soy <- F_clamp(r_aoi, "new_LND_SOY")
  # 
  # # get other layers
  # r_aoi_pct_qland <- r_aoi %>% subset("pct_QLAND")
  # r_aoi_rawch_qland <- r_aoi %>% subset("rawch_QLAND")
  # 
  # r_aoi_pct_qcrop <- r_aoi %>% subset("pct_QCROP")
  # r_aoi_rawch_qcrop <- r_aoi %>% subset("rawch_QCROP")
  # 
  # r_aoi_pct_maize <- r_aoi %>% subset("pct_LND_MAZ")
  # r_aoi_rawch_maize <- r_aoi %>% subset("rawch_MAZ")
  # 
  # r_aoi_pct_soy <- r_aoi %>% subset("pct_LND_SOY")
  # r_aoi_rawch_soy <- r_aoi %>% subset("rawch_SOY")
  # 
  # # re-stack and re-order
  # r_aoi <- c(
  #   r_aoi_new_qland, r_aoi_pct_qland, r_aoi_rawch_qland,
  #   r_aoi_new_qcrop, r_aoi_pct_qcrop, r_aoi_rawch_qcrop,
  #   r_aoi_new_maize, r_aoi_pct_maize, r_aoi_rawch_maize,
  #   r_aoi_new_soy, r_aoi_pct_soy, r_aoi_rawch_soy
  # )
  # 
  # save clipped and clamped raster with new AOI 
  saveRDS(r_aoi, file = paste0(folder_der_date, "r", pct, "_", area_name, ".rds"))
  
  # return as result
  return(r_aoi)
}

# Fxn to calculate total % Change
F_calc_pct_change <- function(final, raw_ch){
  
  # we don't have initial, so we calculate it here
  initial = final - raw_ch
  
  # then we calculate percent change (results are in %)
  pct_change = ((final - initial)/initial)*100
  print(paste0("% Change is: ", pct_change, "%"))
}

# create function 
F_clean_summary_tables <- function(area_name, pct){
  
  filename <- paste0(folder_results, "summary_tables/table_", area_name, pct, "_10e6_", date_string_nodash)
  csv2 <- read.csv(file = paste0(filename, ".csv"))
  
  df2 <- csv2  %>% 
    select(-X) %>% 
    # remove all whitespace
    mutate(across(where(is.character), ~ str_replace_all(., " ", ""))) %>% 
    # rename by removing the "X.." or "X." 
    rename_with(~ str_replace_all(., "X\\.\\.?", "")) %>% 
    # split the first column to get the stats
    separate_wider_delim(new_QLAND, delim = ":", names = c("stat", "new_QLAND"), too_few = "align_start") %>%
    # remove everything before the colon (including the colon)
    mutate(across(where(is.character), ~ str_replace(., "^.*?:", ""))) %>% 
    
    # remove everything before the colon (including the colon)
    mutate(across(where(is.character), ~ str_replace(., "^.*?:", ""))) %>% 
    # convert to numeric
    mutate(across(!stat, as.numeric)) %>% 
    # get rid of the total new land columns - we're only interested in change
    select(-c(new_QLAND, new_QCROP, new_LND_MAZ, new_LND_SOY)) %>%
    # add region column 
    mutate(reg = area_name) %>% 
    # rename columns for easy export
    rename(
      #"new_QLAND" new_QCROP new_LND_MAZ new_LND_SOY
      "Stat" = "stat",
      "Percent Change in Total Land Area" = "pct_QLAND",
      "Raw Change in Total Land Area" = "rawch_QLAND", 
      "Percent Change in Total Crop Production" = "pct_QCROP",
      "Raw Change in Total Crop Production" = "rawch_QCROP",
      
      "Percent Change in Maize Area" = "pct_LND_MAZ",
      "Raw Change in Maize Area" = "rawch_MAZ", 
      "Percent Change in Soy Area" = "pct_LND_SOY",
      "Raw Change in Soy Area" = "rawch_SOY",
      "Region" = "reg") %>% 
    # remove all apostrophes (e.g. to get NA's to NAs)
    # Remove apostrophes from 'stat' column
    mutate(Stat = str_replace_all(Stat, "'", "")) %>%
    #!  # divide by 1000000 to get the accurate values (IF USING 10e6 VERSION)
    mutate(across(
      where(is.numeric),
      ~ if_else(Stat != "NAs", . / 1000000, .)
    ))
  
  df2_round <- df2 %>% 
    # Round each column to 3 decimal places if it is negative or greater than 0.01, else keep it the same
    mutate(across(
      where(is.numeric),
      ~ if_else(. < 0 | . > 0.01, round(., 3), .)
    ))
  

  
  # export the clean dataframe to an excel sheet so we can load it and work on spacing
  rio::export(df2_round, file = paste0(filename, ".xlsx"))
  # also export the whole values in case one rounds weird
  rio::export(df2, file = paste0(filename, "_no_round", ".xlsx"))
  
  
  ## Part 2: Auto-Generate Column Widths ##
  # load xlsx file 
  wb <- loadWorkbook(paste0(filename, ".xlsx"))
  # get sheet names 
  wb_sheet <- names(wb)
  # Adjust column widths based on column name lengths
  for (sheet in wb_sheet) {
    # Get column names
    col_names <- colnames(read.xlsx(wb, sheet))
    
    # Set column widths to match the length of each column name
    widths <- nchar(col_names) + 2 # Adding some extra space for readability
    setColWidths(wb, sheet, cols = 1:length(col_names), widths = widths)
  }
  
  # Save the updated workbook
  saveWorkbook(wb, paste0(filename, ".xlsx"), overwrite = TRUE)
}

# Fxn to get summary of data, call the violin fxn, and plot a basic map
F_EDA <- function(r_aoi, area_name){  
  # get and save a summary table
  table_area <- summary(r_aoi, size = Inf) # set size to not use a sample
  print(table_area)
  
  table_area_10e6 <- summary(r_aoi*1000000, size = Inf) 
  
  # set variable for file path
  tables_file <- paste0(folder_results, "summary_tables/")
  
  # essentially says, "if no file name contains the search string, create a folder with that string"
  if (!(any(grepl("summary_tables", folder_results)))) {
    dir.create(tables_file)
    # print if the fuile already exists or we created a new one
    cat("Folder ", tables_file, "created.\n")
  } else {
    cat("Folder ", tables_file, "already exists.\n")
  }
  
  # actually create the table as a CSV
  write.csv(table_area, file = paste0(folder_results, "summary_tables/", 
                                      "table_", area_name, pct, "_", date_string_nodash, ".csv"))
  
  # actually create the table as a CSV
  write.csv(table_area_10e6, file = paste0(folder_results, "summary_tables/", 
                                      "table_", area_name, pct, "_10e6_",  date_string_nodash, ".csv"))
  
  
  # clean tables 
  F_clean_summary_tables(area_name, pct = pct)
  
  ## Change Section ##
  print("Totals for Casc. Effect Graph and for Total Change")
  
  # Print total change values then calculate % Change
  cat("\n\nTotal Land Change (kha)\n\n")
  print(global(r_aoi$new_QLAND, fun = "sum", na.rm = T))
  print(global(r_aoi$rawch_QLAND, fun = "sum", na.rm = T))
  
  # Calc % change by grabbing the total (sum) values and running the % change function
  F_calc_pct_change(
    final = (global(r_aoi$new_QLAND, fun = "sum", na.rm = T))[[1]],
    raw_ch = (global(r_aoi$rawch_QLAND, fun = "sum", na.rm = T))[[1]]
  )
  
  # print the total change in crop production
  cat("\n\nTotal Production Change (1000 tons CE)\n\n")
  print(global(r_aoi$rawch_QCROP, fun = "sum", na.rm = T))
  
  # print the total changes in crop production for maize
  cat("\n\n Maize Results\n\n")
  print(global(r_aoi$new_LND_MAZ, fun = "sum", na.rm = T))
  print(global(r_aoi$rawch_MAZ, fun = "sum", na.rm = T))
  F_calc_pct_change(
    final = (global(r_aoi$new_LND_MAZ, fun = "sum", na.rm = T))[[1]],
    raw_ch = (global(r_aoi$rawch_MAZ, fun = "sum", na.rm = T))[[1]]
  )
  
  
  # print the total changes in crop production for soy  
  cat("\n\n Soy Results\n\n")
  print(global(r_aoi$new_LND_SOY, fun = "sum", na.rm = T))
  print(global(r_aoi$rawch_SOY, fun = "sum", na.rm = T))
  F_calc_pct_change(
    final = (global(r_aoi$new_LND_SOY, fun = "sum", na.rm = T))[[1]],
    raw_ch = (global(r_aoi$rawch_SOY, fun = "sum", na.rm = T))[[1]]
  )
  
  # Call EDA fxn to get and save violin plots 
  F_p_violin(r_aoi, area_name)
  
  # Plot basic initial maps - removed as this takes a lot of time 
  # terra::plot(r_aoi, 
  #             axes = F#, 
  #             #type = "interval"
  # )
}

# 1: Import / Export Plot ------------------------------------------------------------------------

## 1.1: Clean Import & Export Data -------

# Load in data as xlsx (diff from previous)
# source_path <- paste0(files_results, "regional_results.xlsx")
# data_list <- import_list(source_path)

# function to get one sheet of data and clean it
## var == the name of the sheet we want
## pct == the model type for elasticity; enter either "l" for low, "m" for medium, or "h" for high


## 1.2: Run Fxn & Join --------

# reset model variable here if you want to re-run with different amounts 
# pct_model <- "m"

# Load in data as xlsx (diff from previous) 
# NOTE: Changed code here so there's no need to manually move 'regional_results.xlsx' from download folder to imports_exports
source_path <- paste0(folder_results, "regional_results.xlsx")
data_list <- import_list(source_path)

### 1.2.1: Exports -------
# Get Exports  
exp_soy <- F_clean_sheet(var = "Soy Exp", pct = pct_model)
exp_corn <- F_clean_sheet(var = "Corn Exp", pct = pct_model)
exp <- rbind(exp_soy, exp_corn)

# get sum by region
exp <- aggregate(exp$chg, list(exp$region_abv), FUN=sum)

# rename
names(exp) <- c("region_abv", "chg")

# get million metric tons 
exp$chg_mmt <- (exp$chg)/1000

# exclude us
exp_nous <- exp %>% filter(region_abv != "US")
#print(paste("Total Change in Exports (Mmt) (Excluding US): ", sum(exp_nous$chg_mmt)))

### 1.2.2 Imports ----------
# Get Imports  
imp_soy <- F_clean_sheet(var = "Soy Imp", pct = pct_model)
imp_corn <- F_clean_sheet(var = "Corn Imp", pct = pct_model)
imp <- rbind(imp_soy, imp_corn)

# get sum by region
imp <- aggregate(imp$chg, list(imp$region_abv), FUN=sum)

# rename
names(imp) <- c("region_abv", "chg")

# get million metric tons 
imp$chg_mmt <- (imp$chg)/1000

# exclude us
imp_nous <- imp %>% filter(region_abv != "US")

## 1.3: Vertical Barplots ------

# Run fxn to plot vertical barplot

## helpful link: https://stackoverflow.com/questions/48463210/how-to-color-code-the-positive-and-negative-bars-in-barplot-using-ggplot

col_neg <- "red"
col_pos <- "blue"


# get Corn-Soy Exports
(p_exp <- F_ggplot_bar_vert_sep(
  df = exp_nous,
  y_var = "chg_mmt",
  title_text = "Change in Corn-Soy Exports (million metric ton)",
  save_text = "bar_exp_fxn.png"
))

# get Corn-Soy Imports
(p_imp <- F_ggplot_bar_vert_sep(
  df = imp_nous,
  y_var = "chg_mmt",
  title_text = "Change in Corn-Soy Imports (million metric ton)",
  save_text = "bar_imp_fxn.png"
))

# plot with the individual plots next to one another
# labels give "A" and "B"
(p <- plot_grid(p_imp, p_exp, labels = "AUTO"))

# save 
ggsave(paste0(folder_fig, "bar_impexp.png"),
       p,
       width = 12, height = 6)

## 1.4: Print Results for MS (excluding US) ------
# US Reductions in Corn/SoyExports 
print(paste("Total Change in US Soy Exports (Mmt) (Excluding US): ", exp_soy$chg_mmt[exp_soy$region_abv == "US"]))
print(paste("Total Change in US Corn Exports (Mmt) (Excluding US): ", exp_corn$chg_mmt[exp_corn$region_abv == "US"]))
exp_soy$chg_mmt[exp_soy$region_abv == "US"] + exp_corn$chg_mmt[exp_corn$region_abv == "US"]


# Total Exp
print(paste("Total Change in Exports (Mmt) (Excluding US): ", sum(exp_nous$chg_mmt)))
# Soy/Corn Exp
print(paste("Total Change in Soy Exports (Excluding US): ", 
            sum(exp_soy[!(exp_soy$region_abv %in% "US"),]$chg_mmt)))
print(paste("Total Change in Corn Exports (Excluding US): ", 
            sum(exp_corn[!(exp_corn$region_abv %in% "US"),]$chg_mmt)))


# Total Imp
print(paste("Total Change in Imports (Mmt) (Excluding US): ", sum(imp_nous$chg_mmt)))
# Soy/Corn Imp
print(paste("Total Change in Soy Imports (Excluding US): ", 
            sum(imp_soy[!(imp_soy$region_abv %in% "US"),]$chg_mmt)))
print(paste("Total Change in Corn Imports (Excluding US): ", 
            sum(imp_corn[!(imp_corn$region_abv %in% "US"),]$chg_mmt)))

# US Reductions in Corn/SoyExports 
print(paste("Total Change in US Soy Imports (Mmt) (US Only): ", imp_soy$chg_mmt[imp_soy$region_abv == "US"]))
print(paste("Total Change in US Corn Imports (Mmt) (US Only): ", imp_corn$chg_mmt[imp_corn$region_abv == "US"]))

imp_soy$chg_mmt[imp_soy$region_abv == "US"] + imp_corn$chg_mmt[imp_corn$region_abv == "US"]

## 1.5: Create Clean Results Sheet for Casc Effects Plot ----

# Function for summarizing data - sums for .. and mean for ..
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

# clean data by running through each sheet with the above function 
sheets <- names(data_list)
data_clean <- lapply(X = sheets, FUN = F_clean_sheet, pct = "m")
names(data_clean) <- names(data_list)
data_clean <- lapply(X = data_clean, FUN = F_calc_totals)

# save clean sheet 
rio::export(
  data_clean, 
  file = paste0(folder_results, 'regional_results_clean_', pct_model, '.xlsx'))


### 1.5.1 Global Price Changes ----
paste("Global Soy Price Change: ", mean(data_clean$`Soy Exp Price index`$pct_chg))
paste("Global Corn Price Change: ", mean(data_clean$`Corn Exp Price index`$pct_chg))


# 2: Load Shapefiles & SIMPLE-G Raster ------------------------------------------------------------------------

### RUN 'processResults_SIMPLEG_1.R' FIRST TO CREATE RASTER AND SHAPEFILES ###

# load files and maize-soy raster
load("../Data_Derived/shp_usbr.RData")
shp_ecoregions <- st_read("../Data_Source/wwf_ecoregions/agg_wwf_terr_ecos.shp")
shp_countries <- shp_world %>% dplyr::select(name_long)

r <- readRDS(file = paste0(folder_der_date, "r_maizesoy", pct, ".rds"))

# print cropland area in ha by getting the sum of each grid-cell value
print(global(r$new_QLAND, fun = "sum", na.rm = T))

# 3: Edit Stack & Check Values ------------------------------------------------------------------------

## 3.1: Calc & Add Raw Change from % and New -------
# Formula: new - (new / ((pct_change/100)+1))

# subset 
r_pct_qland <- subset(r, "pct_QLAND")
r_new_qland <- subset(r, "new_QLAND")

r_pct_qcrop <- subset(r, "pct_QCROP")
r_new_qcrop <- subset(r, "new_QCROP")

r_pct_maize <- subset(r, "pct_LND_MAZ")
r_new_maize <- subset(r, "new_LND_MAZ")

r_pct_soy <- subset(r, "pct_LND_SOY")
r_new_soy <- subset(r, "new_LND_SOY")

# NOTE: rawch = Raw Change
r_rawch_qcrop <- r_new_qcrop - (r_new_qcrop / ((r_pct_qcrop/100)+1))
r_rawch_qland <- r_new_qland - (r_new_qland / ((r_pct_qland/100)+1))

r_rawch_maize <- r_new_maize - (r_new_maize / ((r_pct_maize/100)+1))
r_rawch_soy <- r_new_soy - (r_new_soy / ((r_pct_soy/100)+1))

# add raw change layers back into stack
r <- c(
  r, 
  r_rawch_qcrop, r_rawch_qland,
  r_rawch_maize, r_rawch_soy
)

r

# set names 
names(r) #NOTE: make sure everything is in the right order below! 
names(r) <- c(
  "pct_QLAND", 
  "new_QLAND", 
  "pct_QCROP", 
  "new_QCROP",
  
  "pct_LND_MAZ",
  "pct_LND_SOY",
  "new_LND_MAZ",
  "new_LND_SOY",
  
  "rawch_QCROP", 
  "rawch_QLAND",
  "rawch_MAZ",
  "rawch_SOY"
)

# 4: World Results  ------------------------------------------------------------------------

## 4.1: World EDA -----
# make a quick plot of the global results
# terra::plot(r, axes = F)

 

# Call fxn to clip and prep data 
r_row <- F_aoi_prep(shp = shp_world, area_name = "World")

# call fxn to create EDA plots and generate stats of the clipped data 
F_EDA(r_aoi = r_row, area_name = "World")

## 4.2: World Interval Plot --------

F_ggplot_interval <- function(df, title_text, title_legend, save_title){
  
  # plot 
  p <- ggplot() +
    geom_spatraster(data = df, maxcell = Inf, aes(fill = cats)) +
    scale_fill_whitebox_d(palette = "pi_y_g", direction = 1)+
    
    #geom_sf(data = vect(shp_ecoregions), color = "gray60", fill = "transparent", lwd = 0.1)+
    geom_sf(data = vect(shp_countries), color = "gray30", fill = "transparent", lwd = 0.2)+
    
    theme_minimal()+ 
    labs(
      fill = title_legend,
      title = title_text,
    )+
    
    #coord_sf(crs = "ESRI:53042")+ #Winkel-Tripel 
    #coord_sf(crs = "ESRI:53030")+ # Robinson
    
    theme(
      plot.title = 
        element_text(
          hjust = 0.5, 
          size = 22
        ),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 12)
    )  
  
  # save plot
  ggsave(plot = p, filename = paste0(folder_fig, "/", save_title),
         width = 14, height = 6, dpi = 300)
  
  return(p)
  
}

# example from: https://cloud.r-project.org/web/packages/tidyterra/tidyterra.pdf
# With discrete values
test3 <-  r_row %>%
  subset("rawch_QLAND")

# create df
factor <- test3 %>%
  # add column with break intervals
  mutate(
    cats =
      cut(rawch_QLAND,
          # manually set break intervals here 
          breaks = c(-5, -2.5, -1, -0.5, -0.01,
                     0.01, 0.1, 0.25, 0.5, 1))
    )

# run function to create plot
F_ggplot_interval(
  df = factor, 
  title_text = "Global Change in Cropland Area",
  title_legend = "Area (kha)",
  save_title = "gg_world_rawch_croplandarea.png")

## 4.3: World Results w/o US ------
# get extent as terra object for plotting
us_vect <- vect(shp_us)

# set CRS of extent spatial vector
crs(us_vect) <- crs(r)

## 
r_no_us <- mask(r, us_vect, inverse = T)
summary(r_no_us*1000000, size = Inf)
summary(r*1000000, size = Inf)

plot(r_no_us$rawch_QLAND)
F_EDA(r_aoi = r_no_us, area_name = "Rest of World")

# 4: US Results ------------------------------------------------------------------------

## 4.1: US EDA -------

# Call fxn to clip data 
r_us <- F_aoi_prep(shp = shp_us, area_name = "US")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_us, area_name = "US")

## 4.2: US Interval Plot -------
F_ggplot_us_interval <- function(df, title_text, title_legend, save_title){
  
  # plot 
  #"atlas", "high_relief", "arid", "soft", "muted", 
  #"purple", "viridi", "gn_yl", "pi_y_g", "bl_yl_rd", "deep"
  p <- ggplot() +
    geom_spatraster(data = df, maxcell = Inf, aes(fill = cats)) +
    #scale_fill_wiki_d(na.value = "white")
    scale_fill_whitebox_d(palette = "pi_y_g", direction = 1, drop = F)+
    
    geom_sf(data = vect(shp_us), color = "gray30", fill = "transparent", lwd = 0.2)+
    coord_sf(crs = "EPSG:2163")+ # Robinson
    
    theme_minimal()+
    labs(
      fill = title_legend,
      title = title_text,
    )+
    
    #coord_sf(crs = "ESRI:53042")+ #Winkel-Tripel 
    #coord_sf(crs = "ESRI:53030")+ # Robinson
    
    theme(
      plot.title = 
        element_text(
          hjust = 0.5, 
          size = 40
        ),
      legend.title = element_text(size = 24),
      legend.position = c(0.9, 0.25),
      legend.text = element_text(size = 14)
    )  
  
  ggsave(plot = p, filename = paste0(folder_fig, "/", save_title),
         width = 16, height = 8, dpi = 300)
  
  return(p)
  
  
}


# example from: https://cloud.r-project.org/web/packages/tidyterra/tidyterra.pdf
# With discrete values
test3 <-  r_us %>%
  subset("rawch_QLAND")

# create df
factor <- test3 %>%
  mutate(
    cats =
      cut(rawch_QLAND,
          breaks = c(-5, -3, -1, -0.1, -0.01,
                     0.01, 0.25, 0.5, 1, 2))
    )

# run Fxn
F_ggplot_us_interval(
  df = factor, 
  title_text = "Change in US Cropland Area",
  title_legend = "Area (kha)",
  save_title = "gg_us_rawch_croplandarea_2163.png")


## 4.3 US Prod Plot for SI ------
test3 <-  r_us %>%
  subset("rawch_QCROP")

# create df
factor <- test3 %>%
  mutate(
    cats =
      cut(rawch_QCROP,
          breaks = c(-10, -5, -1, -0.1, -0.01,
                     0.01, 0.5, 1, 2, 3))
          # uncomment to match breaks with cropland expansion
          # breaks = c(-5, -3, -1, -0.1, -0.01,
          #            0.01, 0.25, 0.5, 1, 2))
  )

# run Fxn
F_ggplot_us_interval(
  df = factor, 
  title_text = "Raw Change in Crop Production",
  title_legend = "Prod. (tons CE)",
  save_title = "gg_us_rawch_cropprod_2163.png")



## 4.4: US Corn+Soy Changes (regional_results.xlsx) ----

### prod -----
ms_us_prod_pre <- data_clean$`Soy Production`$pre[data_clean$`Soy Production`$region_abv == "US"] + 
  data_clean$`Corn Production`$pre[data_clean$`Corn Production`$region_abv == "US"]

ms_us_prod_post <- data_clean$`Soy Production`$post[data_clean$`Soy Production`$region_abv == "US"] + 
  data_clean$`Corn Production`$post[data_clean$`Corn Production`$region_abv == "US"]

(ms_us_prod_pct_chg <- (ms_us_prod_post - ms_us_prod_pre) / ms_us_prod_pre)  

### area -----
ms_us_area_pre <- data_clean$`Soy Area`$pre[data_clean$`Soy Area`$region_abv == "US"] + 
  data_clean$`Corn Area`$pre[data_clean$`Corn Area`$region_abv == "US"]

ms_us_area_post <- data_clean$`Soy Area`$post[data_clean$`Soy Area`$region_abv == "US"] + 
  data_clean$`Corn Area`$post[data_clean$`Corn Area`$region_abv == "US"]

(ms_us_area_raw_chg <- ms_us_area_post - ms_us_area_pre)  

### exports -------
ms_us_exp_pre <- data_clean$`Soy Exp`$pre[data_clean$`Soy Exp`$region_abv == "US"] + 
  data_clean$`Corn Exp`$pre[data_clean$`Corn Exp`$region_abv == "US"]

ms_us_exp_post <- data_clean$`Soy Exp`$post[data_clean$`Soy Exp`$region_abv == "US"] + 
  data_clean$`Corn Exp`$post[data_clean$`Corn Exp`$region_abv == "US"]

ms_us_exp_pct_chg <- (ms_us_exp_post - ms_us_exp_pre) / ms_us_exp_pre  


# 5: Brazil Results --------

## 5.1: Brazil EDA -------
# Call fxn to clip data 
r_br <- F_aoi_prep(shp = shp_br, area_name = "Brazil")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_br, area_name = "Brazil")


## 5.2: Brazil + Cerrado Continuous Plot -------

# set up function for both 
F_ggplot_brcerr <- function(df, area, brks, pal, legend_title, p_title, save_title){
  
  # plot
  p <- ggplot()+
    geom_spatraster(data = df, maxcell = Inf)+
    
    # use continuous palette
    scale_fill_whitebox_c(
      #palette = "viridi", direction = 1,
      palette = pal,
      breaks = brks
    )+
    labs(
      fill = legend_title,
      title = p_title
    )+
    theme_minimal() +
    
    theme(
      # set plot size and center it 
      plot.title = element_text(size = 24, hjust = 0.5),
      # put legend in the bottom right 
      #legend.position = c(0.15, 0.2),
      legend.position = c(0.1, 0.15),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 10))#+
    
    
    
    # option to plot all the states containing any Cerrado biome
    #geom_sf(data = shp_cerr_states, color = "gray70", fill = "transparent", lwd = 0.2)+
    
    # option to plot all Brazilian states 
    #geom_sf(data = shp_br_states, color = "gray70", fill = "transparent", lwd = 0.1)+ 
    
    # option to plot the outline of the Cerrado
    #geom_sf(data = shp_cerr, color = "black", fill = "transparent", lwd = 0.3)#+
    
    # otion to plot BR country outline
    #geom_sf(data = shp_br_border, color = "gray20", fill = "transparent", lwd = 0.4)#+
  
  
  # set conditional width & height & outlines 
  if(area== "Cerrado"){
    w = 12
    h = 8
    
    # add outlines based on the AOI
    p <- p + 
      # option to plot all the states containing any Cerrado biome
      geom_sf(data = shp_cerr_states, color = "gray70", fill = "transparent", lwd = 0.2)+ 
      # option to plot the outline of the Cerrado
      geom_sf(data = shp_cerr, color = "black", fill = "transparent", lwd = 0.3)
  }
    
  else{
    w = 14
    h = 7
    
    # add outlines based on the AOI
    p <- p +
      # option to plot BR country outline
      geom_sf(data = shp_br_border, color = "gray20", fill = "transparent", lwd = 0.4)+
      # option to plot the outline of the Cerrado
      geom_sf(data = shp_cerr, color = "black", fill = "transparent", lwd = 0.3)
  }
  
  ggsave(plot = p, filename = paste0(folder_fig, "/", save_title),
         width = w, height = h, dpi = 300)
  
  return(p)
}

# call Fxn for Brazil
F_ggplot_brcerr(df = r_br %>% subset("rawch_QLAND"),
                brks = waiver(), 
                area = "Brazil",
                pal = "gn_yl", 
                legend_title = "Area (kha)",
                p_title = paste("Change in BR Cropland Area", pct_title),
                save_title = "gg_br_rawch_croplandarea.png")

## 5.4 BR Prod Plot for SI -----
F_ggplot_brcerr(df = r_br %>% subset("rawch_QCROP"),
                brks = waiver(), 
                area = "Brazil",
                pal = "gn_yl", 
                legend_title = "Prod. (tons CE)",
                p_title = paste("Change in BR Crop Production", pct_title),
                save_title = "gg_br_rawch_cropprod.png")



# 6: Cerrado Results ----------

## 6.1: Cerrado EDA -------
# Call fxn to clip data 
r_cerr <- F_aoi_prep(shp = shp_cerr, area_name = "Cerrado")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_cerr, area_name = "Cerrado")


## 6.2: Cerrado Plot -------
## NOTE: Cerrado is slightly different as a scale bar and N arrow are very helpful here
p2 <- F_ggplot_brcerr(
  df = r_cerr %>% subset("rawch_QLAND"),
  area = "Cerrado",
  brks = waiver(),
  pal = "gn_yl", 
  legend_title = "Area (kha)",
  p_title = paste("Change in Cerrado Cropland Area", pct_title),
  save_title = "gg_cerr_rawch_croplandarea.png")

# call variable to save base map
p2

# Add scale bar and N arrow manually 
library(ggspatial)
p2 <- p2 +       
  annotation_scale(location = "br", width_hint = 0.4) +  # Scale bar at the bottom right
  annotation_north_arrow(location = "br", which_north = "true",  # North arrow at the bottom right
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_minimal())

p2
ggsave(plot = p2, filename = paste0(folder_fig, "/", "gg_cerr_rawch_croplandarea_nscale.png"),
       width = 12, height = 8, dpi = 300)

## 6.3 Cerrado Prod Plot fo SI --------
p3 <- F_ggplot_brcerr(
  df = r_cerr %>% subset("rawch_QCROP"),
  area = "Cerrado",
  brks = waiver(),
  pal = "gn_yl", 
  legend_title = "Production\n(1000-tons CE)",
  p_title = paste("Change in Cerrado Crop Production Index", pct_title),
  save_title = "gg_cerr_rawch_cropprod.png")

# call variable to save base map
p3

# Add scale bar and N arrow manually 
library(ggspatial)
p3 <- p3 +       
  annotation_scale(location = "br", width_hint = 0.4) +  # Scale bar at the bottom right
  annotation_north_arrow(location = "br", which_north = "true",  # North arrow at the bottom right
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_minimal())

p3
ggsave(plot = p3, filename = paste0(folder_fig, "/", "gg_cerr_rawch_cropprod_nscale.png"),
       width = 12, height = 8, dpi = 300)

# 7: Maize/Soy Summary Statistics --------
## 7.1: Sum of total changes for CSV ----
# Fxn to get the total changes
F_sum <- function(df, layer){
  test2 <- df %>% subset(layer)
  global(test2, fun = "sum", na.rm = T)[1,]
}

# Fxn to create df of changes and calculate "PRE" values
F_area_stats <- function(df, extent_text){
  # create column of labels for easier recall
  labels <- c(
    "new_cropland_area", 
    "raw_change_cropland_area",
    "new_crop_production", 
    "raw_change_crop_production",
    
    "new_soy_area",
    "raw_change_soy_area",
    
    "new_maize_area",
    "raw_change_maize_area"
  )
  
  # get layers from input df using sum function created above
  values <- c(
    F_sum(df, "new_QLAND"),
    F_sum(df, "rawch_QLAND"),
    F_sum(df, "new_QCROP"),
    F_sum(df, "rawch_QCROP"),
    
    F_sum(df, "new_LND_SOY"),
    F_sum(df, "rawch_SOY"),
    F_sum(df, "new_LND_MAZ"),
    F_sum(df, "rawch_MAZ")
    
  )
  
  # create data frame from the other layers and their labels 
  df2 <- data.frame(labels, values)
  
  # pivot wide to create 'pre' data, then pivot long to make tidy
  df2 <- df2 %>% 
    
    # make wide so each variable is its own column for easier math
    pivot_wider(names_from = "labels", values_from = "values") %>% 
    
    # calculate PRE values
    mutate(
      extent = extent_text,
      pre_cropland_area = new_cropland_area - raw_change_cropland_area,
      pre_crop_production = new_crop_production - raw_change_crop_production,
      
      pre_soy_area = new_soy_area - raw_change_soy_area,
      pre_maize_area = new_maize_area - raw_change_maize_area
    ) %>% 
    
    # make long again so the column headers are Extent, Labels, Values
    pivot_longer(cols = -extent, names_to = "labels", values_to = "values")
}

# run fxn for each AOI
stat_SG_US_maizesoy <- F_area_stats(r_us, "US")  
stat_SG_BR_maizesoy <- F_area_stats(r_br, "Brazil")  
stat_SG_Cerrado_maizesoy <- F_area_stats(r_cerr, "Cerrado")  

# combine to one df
stat_SG_summary_maizesoy <- rbind(stat_SG_US_maizesoy, stat_SG_BR_maizesoy, stat_SG_Cerrado_maizesoy)

# save
write.csv(stat_SG_summary_maizesoy, 
          file = paste0(
            folder_stat, "sg", pct, "_stat_summary_maizesoy_US_BR_Cerr_", date_string_nodash, ".csv"),
          row.names = F)

## 7.2: Changes calculated for MS Abstract -------

### t1 ----
# Text:  Mean area of corn and soy land expansion per grid-cell in the Cerrado (32.2 ha) was ~1.6 times higher than in Brazil as a whole (24.2 ha).
t_c1 <- as.numeric(terra::global(r_cerr$rawch_QLAND, fun = "mean", na.rm = T))
t_br1 <- as.numeric(terra::global(r_br$rawch_QLAND, fun = "mean", na.rm = T))

t_comp1 <- t_c1 / t_br1

t_c1 / t_br1

paste0("Mean area of corn and soy land expansion per grid-cell in the Cerrado, (",
      round(t_c1*1000, 1), 
      " ha) was", 
      round(t_comp1, 2), 
      " times higher than in Brazil as a whole (", 
      round(t_br1*1000, 1),
      " ha).")

### t2 ----

## SOY ## 
# We found, on average, that a 1 ha decrease in the amount of cropland dedicated to soybean in the US leads to a 0.20 increase in Cerrado soybean cropland. 
t_s_c2 <- as.numeric(global(r_cerr$rawch_SOY, fun = "sum", na.rm = T))
t_s_us2 <- as.numeric(global(r_us$rawch_SOY, fun = "sum", na.rm = T))
t_s_comp2 <- t_s_c2 / t_s_us2

paste0("We found, on average, that a 1 ha decrease in the amount of cropland dedicated to soybean in the US leads to a ",
       format(round(t_s_comp2*-1, 2), nsmall = 2),  
       " ha increase in Cerrado soybean cropland.")

t_s_br2 <- as.numeric(global(r_br$rawch_SOY, fun = "sum", na.rm = T))
t_s_comp2_usbr <- t_s_br2 / t_s_us2

paste0("We found, on average, that a 1 ha decrease in the amount of cropland dedicated to soybean in the US leads to a ",
       format(round(t_s_comp2_usbr*-1, 2), nsmall = 2),  
       " ha increase in Brazil soybean cropland.")


## C+S ##
t_c2 <- as.numeric(global(r_cerr$rawch_QLAND, fun = "sum", na.rm = T))
t_us2 <- as.numeric(global(r_us$rawch_QLAND, fun = "sum", na.rm = T))
t_comp2 <- t_c2 / t_us2

paste0("We found, on average, that a 1 ha decrease in the amount of cropland in the US leads to a ",
       format(round(t_comp2*-1, 2), nsmall = 2),  
       " ha increase in Cerradocropland.")

t_br2 <- as.numeric(global(r_br$rawch_QLAND, fun = "sum", na.rm = T))
t_comp2_usbr <- t_br2 / t_us2

paste0("We found, on average, that a 1 ha decrease in the amount of cropland in the US leads to a ",
       format(round(t_comp2_usbr*-1, 2), nsmall = 2),  
       " ha increase in Brazil cropland.")

# 8: Transition Results: Doesn't Change with new Model Runs -----

# NOTE: It is useful to clear the environment and re-run Sections 0 and 2 before running this section

# set relevant vegetation class categories
list_from_lv3 <- c("Forest Formation", "Savanna Formation", "Wetland",
                   "Grassland", "Pasture", "Forest Plantation",
                   "Mosaic of Agriculture and Pasture",
                   "Magrove", "Flooded Forest",
                   "Shrub Restinga", "Other Non Forest Natural Formation", "Wooded Restinga",
                   "Perennial Crops")

# load clean and long MapBiomas Collection 8 data
#  NOTE: for steps on cleaning, see aggStats_MapBiomas.R Section 1
load(file = paste0(folder_der, "mapb_col8_clean_long.Rdata"))

# filter Mapbiomas data to only focus on transitions to "Soybeans" & From-To's that do not stay the same
df <- df %>%
  filter(to_level_4 == "Soy Beans") %>%
  filter(to_level_4 != from_level_4)

## 8.1: Facet Map of Cerrado Transition ----

### 8.1.1: Prep Spatial Data ---------

# NOTE: Municipality & Cerrado Shapefiles come from 'geobr' package

# Load municipality shapefile
# Read all municipalities in the country at a given year
shp_muni <- read_municipality(code_muni="all", year=2018)

# get municipalities that are at all within the Cerrado
shp_muni_in_cerr <- st_intersection(shp_muni, shp_cerr)

# get just the codes column and keep as shapefile
shp_code_muni_in_cerr <- shp_muni_in_cerr %>%  dplyr::select(code_muni)

# get territory codes for municipalities in intersection as numeric
muni_codes_cerr <- shp_muni_in_cerr$code_muni

# filter to only municipalities in Cerrado
df_cerr <- df %>%
  filter(geocode %in% muni_codes_cerr) %>%
  filter(biome == "Cerrado")

### 8.1.2: Aggregate -------

# PICK UP HERE ##
# make years into XXXX-XXXX format, make years larger
# make a panel with Land Trnasition Facet + Line Plot (A, B)

# get aggregate sum of the entire Cerrado for stats
agg_cerr <- df_cerr %>%
  aggregate(ha ~ year, sum) %>%
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# get agg sum of certain 'from' classes for mapping
agg_cerrmuni_fromveg <- df_cerr %>%
  filter(from_level_3 %in% list_from_lv3) %>%
  aggregate(ha ~ year + geocode, sum) %>%
  mutate(year = as.numeric(year)) 

  #mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# make shape -- from veg
shp_cerrmuni_fromveg <- shp_code_muni_in_cerr %>%
  left_join(agg_cerrmuni_fromveg,
            join_by(code_muni == geocode)) %>%
  #mutate(year = year(year)) %>%
  filter(year >= 2012 & year <= 2017) %>% 
  mutate(years = paste0(year-1,"-",year))

### 8.1.3: Plot Facet Map -------
F_facet<-function(data, aoi, class, file_name){
  # plot
  p <- ggplot(data)+
    geom_sf(mapping = aes(fill = ha/1000), color= NA)+
    scale_fill_distiller(palette = "YlOrRd", direction = 1)+
    facet_wrap("years")+
    coord_sf()+
    theme_minimal()+

    labs(
      title = paste("Land Transition Across", aoi),
      subtitle = paste(class),
      fill = "Transition (kha)")+

    theme(
      plot.title = element_text(hjust = 0.5, size = 32),
      plot.subtitle = element_text(hjust = 0.5, size = 20),
      legend.position = "top",
      strip.text.x = element_text(size = 14)#,
      #legend.key.size = unit(0.8, "cm")
    )

  # save
  ggsave(filename = paste0(folder_fig, file_name),
         plot = p,
         width = 8, height = 8,
         dpi = 300)

  return(p)
}

# Run Fxn
F_facet(shp_cerrmuni_fromveg,
        aoi = "Cerrado",
        class = "From Relevant Vegetation Classes to Soybean",
        file_name = "cerr_fromveg.png")


### 8.1.4: Save Stats -----
# print stats
print(agg_cerr %>% filter(year(year) >= 2013 & year(year) <= 2015))

## 8.2: Plot Line Plot of Cerrado Transition ------

# set calculated r_cerr as a variable (note: unit is kha)
sg_cerr_rawch_soy <- as.numeric(global(r_cerr$rawch_SOY, fun = "sum", size = Inf, na.rm = T))

# filter to only include the relevant classes
classes_few <- c(
  #"Temporary Crops", 
  "Forest Formation", "Mosaic of Agriculture and Pasture",
                 "Pasture", "Savanna Formation", "Grassland")

df_cerr_agg <- df_cerr %>%
  aggregate(ha ~ year + biome + from_level_3 + to_level_4, sum) %>%
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

df_cerr_agg_from3 <- filter(df_cerr_agg, from_level_3 %in% classes_few)


# --------------
# load what will become "df" - generated from aggStats_MapBiomas
load(file = paste0(folder_der, "mapb_col8_clean_long.Rdata"))

# list of "Relevant Vegetation Classes"

# filter level 4 data to only "To-Soybeans"
df <- df %>% 
  filter(to_level_4 == "Soy Beans") %>%
  filter(to_level_4 != from_level_4)

# filter to Cerrado
# needs: muni_codes_cerr
df_cerr <- df %>% 
  filter(geocode %in% muni_codes_cerr) %>% 
  filter(biome == "Cerrado")

# filter to only land that came from one of the RVCs-to-Soybean
agg_cerr_fromveg <- df_cerr %>% 
  filter(from_level_3 %in% list_from_lv3) %>% 
  aggregate(ha ~ year, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d')) %>% 
  mutate(
    biome = "Cerrado",
    from_level_3 = "Sum of RVCs",
    to_level_4 = "Soy Beans"
  )

# add to "from3" df
df_cerr_RVC <- rbind(df_cerr_agg_from3, agg_cerr_fromveg)

# Plot -- Add horizontal line here at the SIMPLE-G Results
# plot line plot in Mha
ggplot(df_cerr_RVC, aes(x=year, y=ha/1000000, color = from_level_3)) +
  geom_line() +
  geom_point(fill = "white", size = 1.2) +
  xlab("")+
  scale_x_date(date_labels = "%Y")+
  labs(
    #title = "From X to Soybean", # remove title to plot with transition map
    y = "Land Change from Previous Year (Mha)",
    color = "From-To Transitions"
  )+
  # add vertical line in 2012
  geom_vline(xintercept = as.Date("2013-01-01"), color = "red",
             linetype="dotted", linewidth=0.5)+
  # add horizontal line where we calculated Cerrado transition 
  geom_hline(yintercept = sg_cerr_rawch_soy/1000, color = "black",
             linetype="dotted", linewidth=0.5)+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 17, hjust = 0.5)
  )

# save
ggsave(paste0(folder_fig, "cerr_to_soybean.png"),
       width = 14, height = 7)

# now plot ONLY RVCs
ggplot(agg_cerr_fromveg, aes(x=year, y=ha/1000000, color = from_level_3)) +
  geom_line() +
  geom_point(fill = "white", size = 1.2) +
  xlab("")+
  scale_x_date(date_labels = "%Y")+
  labs(
    #title = "From X to Soybean", # remove title to plot with transition map
    y = "Land Change from Previous Year (Mha)",
    color = "From-To Transitions"
  )+
  # add vertical line in 2012
  geom_vline(aes(xintercept = as.Date("2013-01-01"), color = "Start of Post-Drought Period"),
             linetype="dotted", linewidth=0.5)+
  
  # add horizontal line where we calculated Cerrado transition 
  geom_hline(aes(yintercept = sg_cerr_rawch_soy/1000, color = "SIMPLE-G Estimate"),
             linetype="dashed", linewidth = 1)+
  theme_bw()+
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
  )+
  scale_color_manual(
    values = c("SIMPLE-G Estimate" = "blue", 
               "Sum of RVCs" = "black", 
               "Start of Post-Drought Period" = "red"))


# save
ggsave(paste0(folder_fig, "cerr_to_soybean_RVC.png"),
       width = 14, height = 7)


# # grabbed from aggStats_MapBiomas.R on 11/20
# F_line<-function(data, aoi, class, file_name){
#   p <- ggplot(data, aes(x = year, y = ha/1000000))+
#     geom_line()+
#     geom_point()+
#     theme_minimal()+
#     labs(
#       title = paste("Annual Land Transition Across", aoi),
#       subtitle = paste(class),
#       y = "Land Transition (Mha)",
#       x = ""
#     )+
#     # add horizontal line where we calculated Cerrado transition 
#     geom_hline(yintercept = sg_cerr_rawch_soy/1000, color = "red",
#                linetype="dotted", linewidth=0.5)+
#     
#     geom_vline(xintercept = as.Date("2013-01-01"), color = "red",
#                linetype="dotted", linewidth=0.5)
#   
#   ggsave(filename = paste0(folder_fig, file_name),
#          plot = p,
#          width = 9, height = 6,
#          dpi = 300)
#   
#   # print stats/data for publication
#   print("Land Transitioned:")
#   print(data %>% filter(year(year) >= 2012 & year(year) <= 2017))
#   
#   return(p)
# }
# 
# 
# F_line(data = agg_cerr_fromveg, aoi = "Cerrado", 
#        class = "From Relevant Vegetation Classes to Soybean", 
#        file_name = "line_cerr_fromveg.png")
# 
# print(agg_cerr_fromveg %>% filter(year(year) >= 2013 & year(year) <= 2015))
# 
