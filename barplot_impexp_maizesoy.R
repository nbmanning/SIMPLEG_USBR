# Title: barplot_impexp.R
# Purpose: Read import and export data and re-create barplots in R

# Created by: Nick Manning 
# Created on: March 2024
# Last edited: March 2024

# REQUIRES:
## SIMPLE-G Result files as '.CSV' 

# 0: Load Libraries & Set Constants ---- 
rm(list = ls())

## Libraries ##
library(tidyverse)
library(ggplot2)
library(cowplot)
library(rio)


## Files ##
getwd()

search_string <- "2024-03-03"

folder_results <- paste0("../Results/SIMPLEG-", search_string, "/imports_exports/")

folder_fig <- paste0("../Figures/", search_string, "/")

# 0: Check folders --------------  
# Check if folders exist; if not, create
# List all files and folders in the current directory
files_results <- list.dirs(folder_results)
files_fig <- list.dirs(folder_fig)

# Check if any imp/exp folder exists in the search string folder 
if (!(any(files_results))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_results))
  
  cat("Results Folder", search_string, "created.\n")
} else {
  cat("A folder with the string", search_string, "in its name already exists.\n")
}


# check for figures folder
if (!(any(files_fig))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_fig))
  
  cat("Figure Folder", search_string, "created.\n")
} else {
  cat("A folder with the string", search_string, "in its name already exists.\n")
}

# 0: Import Source Data ----------
# Load in data as xlsx (diff from previous)
source_path <- paste0(files_results, "regional_results.xlsx")
data_list <- import_list(source_path)


# 1: Imports (NONE??)------

## tidy -----
# Load in data as xlsx (diff from previous)
source_path <- paste0(files_results, "regional_results.xlsx")
data_list <- import_list(source_path)

# Load in data
# imp_maize <- read.csv(paste0(folder_results, "imp_maize.csv"))
# imp_soy <- read.csv(paste0(folder_results, "imp_soy.csv"))

# imp_maize <- datalist[[]]

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

## 2.1 tidy -----

# # Load in data
# exp_maize <- read.csv(paste0(folder_results, "exp_maize.csv"))
# exp_soy <- read.csv(paste0(folder_results, "exp_soy.csv"))

# raw_exp_maize <- data_list[["Corn Exp"]]
# df <- raw_exp_maize
# 
# # Extract just the first letter of each column name
# setting <- substr(colnames(df), 1, 1)
# 
# # Get the combined model setting (l,m, or h) and variable; e.g. Pct-m
# df <- rbind(setting, df)
# 
# # Combine the first two rows with a hyphen in between
# combined_row <- paste(df[2, ], df[1, ], sep = "-")
# 
# # Add the combined row as the first row
# df <- rbind(combined_row, df[-c(1, 2), ])
# 
# #get model info then remove 
# model_info <- df[2, ]
# df <- df[-2, ]
# 
# # Update column names to model variable 
# colnames(df) <- as.character(unlist(df[1, ]))
# 
# colnames(df)[1] <- "region_abv"
# 
# # Remove the duplicated row
# df <- df[-1, ]
# 
# 
# # Assuming your data frame is named df
# # Filter columns based on certain character in row 2
# char_to_keep <- "l"  # NOTE: Change this to the model setting you want to keep
# 
# # Save pre-existing column names
# pre_existing_colnames <- colnames(df)
# 
# # Filter columns based on predefined character in column names
# columns_to_keep <- c(
#   TRUE, 
#   sapply(
#     # apply this function to all except the first row 
#     pre_existing_colnames[-1], 
#     # function is using one col_name, make a T/F if it contains the char_to_keep
#     function(col_name) substr(
#       col_name, nchar(col_name), nchar(col_name))
#     == char_to_keep
#   )
# )
# 
# # Subset the data frame to keep the desired columns
# df <- df[, columns_to_keep]
# 
# # Remove spaces and numbers before regions
# ## regex: remove anything up to and including the first space
# df$region_abv <- gsub(".*\\ ", "", df$region_abv)
# 
# # add column with model type and remove from column names
# df$modeltype <- "Low"


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

# test <- test %>% 
#   #rename_with(~ gsub("-(\\d{2})$", "", .x), contains("-"))
#   rename_with(~ gsub("-\\d{2}$", "", .x), contains("-"))

# # remove model type from all columns 
# # Define the string to remove from column names
# string_to_remove <- "-l"  # Change this to the string you want to remove
# 
# # Remove the specified string from column names
# new_colnames <- gsub(string_to_remove, "", pre_existing_colnames)
# 
# # Update the column names in the data frame
# colnames(df) <- new_colnames





# Rename
#cols <- c("region_abv", "pct_chg", "pre", "post", "chg", "region")
#cols <- c("region_abv", "pct_chg", "pre", "post", "chg")

#names(exp_soy) <- cols
#exp_maize <- df
#names(exp_maize) <- cols

# Set Crop Col
#exp_soy$crop <- "soy"
#exp_maize$crop <- "maize"

## 2.2 Run Fxn & Join --------
pct_model <- "m"
exp_soy <- F_clean_sheet(var = "Soy Exp", pct = pct_model)
exp_maize <- F_clean_sheet(var = "Corn Exp", pct = pct_model)
exp_cornsoy <- rbind(exp_soy, exp_maize)
#exp <- select(exp, "region_abv", "region", "chg", "crop")

# Remove spaces and numbers before regions
## regex: remove anything up to and including the first space
#exp$region_abv <- gsub(".*\\ ", "", exp$region_abv)

# Rename to change 
exp <- exp_cornsoy 

## 2.3 Rename and Agg ----------
names(exp)

# # rename columns - can ADD TO FUNCTION USING SOMETHING LIKE paste0(-,pct)
# exp <- exp %>% 
#   rename(
#     "pct_chg" = "Pct-m",
#     "pre" = "Pre-m",
#     "post" = "Post-m",
#     "chg" = "CH-m"
#   )
# 
# # convert certain columns to numeric
# exp <- exp %>%
#   #mutate_at(vars(columns_to_convert), as.numeric)
#   mutate_at(vars(c("pct_chg", "pre", "post", "chg")), as.numeric)

#cols <- c("region_abv", "pct_chg", "pre", "post", "chg")

# get million metric tons 
#exp$chg_mmt <- (exp$chg)/1000

# get sum by region
exp <- aggregate(exp$chg, list(exp$region_abv), FUN=sum)
names(exp) <- c("region", "chg")

# get million metric tons 
exp$chg_mmt <- (exp$chg)/1000

# exclude us
exp_nous <- exp %>% filter(region != "US")

## 2.4 Plot --------
col_neg <- "red"
col_pos <- "blue"

# plot vertical barplot
#helpful link: https://stackoverflow.com/questions/48463210/how-to-color-code-the-positive-and-negative-bars-in-barplot-using-ggplot

### Corn-Soy Exports ------
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
    theme(
      plot.title = element_text(hjust = 0.5),
      # remove y-axis text (use when we're merging exp and imp graphs)
      #axis.text.y = element_blank()
    )
)

# save
ggsave(paste0(folder_fig, "bar_exp.png"),
       width = 6, height = 8)


### Corn Exports ------------
exp_maize_nous <- exp_maize %>% filter(region_abv != "US")
(p_exp_corn <- ggplot(exp_maize_nous, aes(x = region_abv, y = chg_mmt))+
   # Set color code on a True-False basis
   geom_bar(aes(fill = chg_mmt < 0), stat = "identity") + 
   # if false, one color, if true, another
   scale_fill_manual(guide = "none", breaks = c(TRUE, FALSE), values=c(col_neg, col_pos))+
   coord_flip()+
   theme_bw()+
   labs(
     title = "Change in Corn Exports (million metric ton)",
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
ggsave(paste0(folder_fig, "bar_exp_corn.png"),
       width = 6, height = 8)

### Soy Exports ------------
exp_soy_nous <- exp_soy %>% filter(region_abv != "US")
(p_exp_soy <- ggplot(exp_soy_nous, aes(x = region_abv, y = chg_mmt))+
    # Set color code on a True-False basis
    geom_bar(aes(fill = chg_mmt < 0), stat = "identity") + 
    # if false, one color, if true, another
    scale_fill_manual(guide = "none", breaks = c(TRUE, FALSE), values=c(col_neg, col_pos))+
    coord_flip()+
    theme_bw()+
    labs(
      title = "Change in Soy Exports (million metric ton)",
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
ggsave(paste0(folder_fig, "bar_exp_soy.png"),
       width = 6, height = 8)

# 3: Facet Plot ------------

## 3.1 Corn & Soy Imports Facet ---------

## 3.2 Corn & Soy Exports Facet -------
exp_cornsoy_nous <- exp_cornsoy %>% filter(region_abv != "US")

# plot with facets
ggplot(exp_cornsoy_nous, aes(x = region_abv, y = chg_mmt))+
  geom_bar(aes(fill = chg_mmt < 0), stat = "identity") + 
  scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c(col_neg, col_pos))+
  coord_flip()+
  facet_wrap("crop")+
  theme_bw()+
  labs(
    title = "Change in Corn & Soy Exports (million metric ton)",
    x = "",
    y = ""
  )+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(folder_fig, "bar_exp_cornsoy.png"),
       width = 12, height = 8)

## 3.3 Agg imports & Exports Facet -------
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
