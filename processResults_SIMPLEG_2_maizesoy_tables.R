# Title: processResults_SIMPLEG_2_maizesoy_tables.R
# Purpose: Generate clean tables for the Supplementary Information section
# Initial date: Aug 2024
# Author: Nick Manning 
# Last edited: Aug 2024

# REQUIRES:
## 'terra' sumamry tables generated from the F_EDA function in processResults_SIMPLEG_2_maizesoy.R

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# libraries 
library(tidyverse)
library(rio) # writing the excel file
library(openxlsx) # edit column widths

# constants 
date_string <- "2024-03-03"
date_string_nodash <- gsub("-", "", date_string)
pct <- "_m"

folder_results <- paste0("../Results/SIMPLEG-", date_string, "/")


# write manual code to clean one set
region <- "US"
csv <- read.csv(file = paste0(folder_results, "summary_tables/table_", region, pct, "_", date_string_nodash, ".csv"))
df <- csv  %>% 
  select(-X) %>% 
  # remove all whitespace
  mutate(across(where(is.character), ~ str_replace_all(., " ", ""))) %>% 
  # rename by removing the "X.." or "X." 
  rename_with(~ str_replace_all(., "X\\.\\.?", "")) %>% 
  # split the first column to get the stats
  separate_wider_delim(new_QLAND, delim = ":", names = c("stat", "new_QLAND"), too_few = "align_start") %>%
  # remove everything before the colon (including the colon)
  mutate(across(where(is.character), ~ str_replace(., "^.*?:", ""))) %>% 
  # convert to numeric
  mutate(across(!stat, as.numeric)) %>% 
  # get rid of the total new land columns - we're only interested in change
  select(-c(new_QLAND, new_QCROP, new_LND_MAZ, new_LND_SOY)) %>% 
  # rename columns for easy export
  rename(
    #"new_QLAND" new_QCROP new_LND_MAZ new_LND_SOY
    "Percent Change in Total Land Area" = "pct_QLAND",
    "Raw Change in Total Land Area" = "rawch_QLAND", 
    "Percent Change in Total Crop Production" = "pct_QCROP",
    "Raw Change in Total Crop Production" = "rawch_QCROP",
    
    "Percent Change in Maize Area" = "pct_LND_MAZ",
    "Raw Change in Maize Area" = "rawch_MAZ", 
    "Percent Change in Soy Area" = "pct_LND_SOY",
    "Raw Change in Soy Area" = "rawch_SOY",
    "_" = "stat")

rio::export(df, file = paste0(folder_results, "summary_tables/cleantable_", region, pct, "_", date_string_nodash, ".xlsx"))

## Auto-Generate Column Widths ## 
# load xlsx file 
wb <- loadWorkbook(paste0(folder_results, "summary_tables/cleantable_", region, pct, "_", date_string_nodash, ".xlsx"))
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
saveWorkbook(wb, paste0(folder_results, "summary_tables/cleantable_", region, pct, "_", date_string_nodash, ".xlsx"), overwrite = TRUE)


# translate to function for all result code 
region <- "US"
F_clean_summary_tables <- function(region, pct){
  
  csv2 <- read.csv(file = paste0(folder_results, "summary_tables/table_", region, pct, "_", date_string_nodash, ".csv"))
  
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
    # convert to numeric
    mutate(across(!stat, as.numeric)) %>% 
    # get rid of the total new land columns - we're only interested in change
    select(-c(new_QLAND, new_QCROP, new_LND_MAZ, new_LND_SOY)) %>%
    # add region column 
    mutate(reg = region) %>% 
    # rename columns for easy export
    rename(
      #"new_QLAND" new_QCROP new_LND_MAZ new_LND_SOY
      "Percent Change in Total Land Area" = "pct_QLAND",
      "Raw Change in Total Land Area" = "rawch_QLAND", 
      "Percent Change in Total Crop Production" = "pct_QCROP",
      "Raw Change in Total Crop Production" = "rawch_QCROP",
      
      "Percent Change in Maize Area" = "pct_LND_MAZ",
      "Raw Change in Maize Area" = "rawch_MAZ", 
      "Percent Change in Soy Area" = "pct_LND_SOY",
      "Raw Change in Soy Area" = "rawch_SOY",
      "_____" = "stat",
      "region" = "reg")
  
  rio::export(df2, file = paste0(folder_results, "summary_tables/cleantable_", region, pct, "_", date_string_nodash, ".xlsx"))
  
  ## Part 2: AutoGenerate COlumn Widths ##
  # load xlsx file 
  wb <- loadWorkbook(paste0(folder_results, "summary_tables/cleantable_", region, pct, "_", date_string_nodash, ".xlsx"))
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
  saveWorkbook(wb, paste0(folder_results, "summary_tables/cleantable_", region, pct, "_", date_string_nodash, ".xlsx"), overwrite = TRUE)
}

F_clean_summary_tables("US", pct = pct)
F_clean_summary_tables("Brazil", pct = pct)
F_clean_summary_tables("Cerrado", pct = pct)
F_clean_summary_tables("World", pct = pct)
