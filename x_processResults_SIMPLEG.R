# x_processResults_SIMPLEG.R

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
rm(list = ls())
library(tidyverse)
library(terra) # use to wrangle geospatial data and plot
library(RColorBrewer) # use for adding colorblind-friendly color palettes 
library(patchwork) # use to arrange maps into result figures  
library(rasterVis)
library(reshape2)

#library(geobr) # use to load BR & Cerrado extent shapefiles
#library(tigris) # use to load US and US-MW shapefiles
#library(stringr) # use to manipulate result .txt file
#library(raster) # use for initial raster stack and basic plotting

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# set constants 
pct <- "_hi" # change when you change 'datafile'; either "" or "_lo" or "_hi"
pct_title <- "- High" # for plotting, either " - High" or " - Low"

# NOTE: will need to local location
folder <- "../Results/SIMPLEG-2023-10-29/"
der_folder <- "../Data_Derived/"

# load data from other processResults script so I can just focus on mapping here
load(file = "../Data_Derived/shp_usbr.RData")
r <- readRDS(paste0(der_folder,"r", pct, ".rds"))

# 1: US RESULTS ----------------------------

## 2.1 Clip to US Extent ----------------------------
# get extent as terra object for plotting
ext_us <- vect(ext(shp_us))

# plot basic us results by cropping and masking to just us extent
r_us <- crop(r, ext_us, mask = T)
r_us <- mask(r_us, shp_us)

## 2.2 Subset and do EDA ----------------------------
# subset to each band
r_us_pct_qland <- r_us %>% subset("pct_QLAND")
r_us_new_qland <- r_us %>% subset("new_QLAND")
r_us_pct_qcrop <- r_us %>% subset("pct_QCROP")
r_us_new_qcrop <- r_us %>% subset("new_QCROP")

### Summaries ----------------------------
table_us <- summary(r_us, size = 1000000)
table_us
write.csv(t, file = paste0(folder, "table_us_102923", pct, ".csv"))

### EDA Plots ----------------------------
# terra::boxplot(r_us %>% subset(c("pct_QLAND", "pct_QCROP")))
# terra::boxplot(r_us %>% subset(c("new_QLAND", "new_QCROP")))
 
terra::hist(r_us)
terra::hist(log(r_us))

# Violin Plots 
F_p_violin <- function(df, area){
  
  # subset
  df_pct <- df %>% subset(c("pct_QLAND", "pct_QCROP"))
  df_new <- df %>% subset(c("new_QLAND", "new_QCROP"))
  
  # plot
  p1 <- bwplot(df_pct, 
               main = paste(area, "% Change", pct_title),
               ylab = "% Change")
  p2 <- bwplot(df_new, 
               main = paste(area, "Post-Sim Values", pct_title),
               ylab = "Area (ha)")
  plot(p1)
  plot(p2)
  }

F_p_violin(r_us, "US")


## 2.3: Plot US Results ----------------------------

## plot all together ##
terra::plot(r_us, axes = F, type = "continuous")

## plot individually ##
par(mfrow=c(2,2), oma = c(0,0,2,0))
# r_us_pct_qland@ptr[["names"]][[1]]

# set new continuous color scheme
col_split <- colorRampPalette(brewer.pal(9, "YlOrRd"))(50)

### Create Functions -------
F_EDA_us_pct_map <- function(layer, title){
  
  par(mfrow=c(2,2), oma = c(0,0,0,0))
  terra::plot(layer,
            type = "continuous",
            col = rev(col_split),
            main = paste(title, "- default", pct_title), 
            plg = list(x="bottomright"))

  terra::plot(layer,
            type = "interval",
            breaks = c(seq(-25, 5, by = 5)),
            col = rev(brewer.pal(9, "YlOrRd")),
            main = paste(title, "- breaks; seq()", pct_title), 
            plg = list(x="bottomright"))

  terra::plot(layer,
            type = "interval",
            breakby = "cases",
            col = rev(brewer.pal(9, "YlOrRd")),
            main = paste(title, "- breaks; equal quantile", pct_title), 
            plg = list(x="bottomright"))

  terra::plot(layer,
            type = "interval",
            breakby = "eqint",
            col = rev(brewer.pal(9, "YlOrRd")),
            main = paste(title, "- breaks; equal interval", pct_title), 
            plg = list(x="bottomright"))
  }



F_EDA_us_new_map <- function(layer, title){
  
  par(mfrow=c(2,2), oma = c(0,0,0,0))
  terra::plot(layer,
              type = "continuous",
              col = col_split,
              main = paste(title, "- default", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breaks = c(seq(0, 250000, by = 50000)),
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; seq()", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "cases",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal quantile", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "eqint",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal interval", pct_title), 
              plg = list(x="bottomright"))
}

### Plot EDA --------
F_EDA_us_pct_map(r_us_pct_qland, "% Change in Cropland Area")
F_EDA_us_pct_map(r_us_pct_qcrop, "% Change in Crop Index")

F_EDA_us_new_map(r_us_new_qland, "Post-Sim Values of Cropland Area")
F_EDA_us_new_map(r_us_new_qcrop, "Post-Sim Values of Crop Index")

### Plot Best Map ---------
par(mfrow=c(1,4), oma = c(0,0,0,0))
### % Change in Cropland Area ###
terra::plot(r_us_pct_qland,
            type = "interval",
            #breaks = c(seq(-25, 5, by = 5)),
            #breaks = c(-25, -22.5, -20, -17.5, -15, -10, -5, 0, 2.5, 5),
            breaks = c(-25, -22.5, -20, -17.5, -15, -5, 0, 5),
            col = brewer.pal(7, "RdYlGn"),
            #col = brewer.pal(9, "YlOrRd"), # try negative to positive 
            main = paste("US % Change in Cropland Area", pct_title), 
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")
#north(cbind(-121, 29))


### % Change in Crop Index ###
terra::plot(r_us_pct_qcrop,
            type = "interval",
            #breaks = c(seq(-25, 5, by = 5)),
            breaks = c(-25, -22.5, -20, -17.5, -15, -5, 0, 5),
            col = brewer.pal(7, "RdYlGn"),
            #col = brewer.pal(9, "YlOrRd"),
            main = paste("US % Change in Crop Index", pct_title), 
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Cropland Area ###
terra::plot(r_us_new_qland/1000,
            type = "interval",
            breaks = c(0, 0.1, 1, 10, 50, 100, 250, 300),
            #col = brewer.pal(9, "YlOrRd"),
            col = brewer.pal(7, "YlGn"),
            main = paste("US Post-Simulation Crop Area", pct_title), 
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Crop Index ###
terra::plot(r_us_new_qcrop/1000,
            type = "interval",
            breaks = c(0, 0.1, 1, 10, 50, 100, 250, 300),
            #col = brewer.pal(9, "YlOrRd"),
            col = brewer.pal(9, "YlGn"),
            main = paste("US Post-Simulation Crop Index", pct_title), 
            plg = list(x="bottomright"))
lines(shp_us_mw, lwd = 0.8, lty = 3, col = "darkgray")


# 2: BRAZIL RESULTS ----------------------------

## 2.1 Clip to Brazil Extent ----------------------------

# get extent as terra object for plotting
ext_br <- vect(ext(shp_br))

# plot basic BR results by cropping and masking to just BR extent
r_br <- terra::crop(r, ext_br, mask = T) 
r_br <- mask(r_br, shp_br)

## 2.2 Subset and do EDA ----------------------------

r_br_pct_qland <- r_br %>% subset("pct_QLAND")
r_br_new_qland <- r_br %>% subset("new_QLAND")
r_br_pct_qcrop <- r_br %>% subset("pct_QCROP")
r_br_new_qcrop <- r_br %>% subset("new_QCROP")

### Summaries ----------------------------
table_br <- summary(r_br, size = 1000000) # set size to not use a sample
table_br
write.csv(t, file = paste0(folder, "table_br_102923", pct, ".csv"))

### Boxplots ----------------------------
F_p_violin(r_br, "Brazil")
hist(log(r_br))
## 2.3: Plot BR Results ----------------------------
terra::plot(r_br, axes = F, type = "interval")

### Create Functions -------
F_EDA_br_pct_map <- function(layer, title){
  
  par(mfrow=c(2,2), oma = c(0,0,0,0))
  terra::plot(layer,
              type = "continuous",
              col = col_split,
              main = paste(title, "- default", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breaks = c(seq(0, 4, by = 0.5)),
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; seq()", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "cases",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal quantile", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "eqint",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal interval", pct_title), 
              plg = list(x="bottomright"))
}



F_EDA_br_new_map <- function(layer, title){
  
  par(mfrow=c(2,2), oma = c(0,0,0,0))
  terra::plot(layer,
              type = "continuous",
              col = col_split,
              main = paste(title, "- default", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breaks = c(seq(0, 450000, by = 50000)),
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; seq()", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "cases",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal quantile", pct_title), 
              plg = list(x="bottomright"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "eqint",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal interval", pct_title), 
              plg = list(x="bottomright"))
}

### Plot EDA --------
F_EDA_br_pct_map(r_br_pct_qland, "% Change in Cropland Area")
F_EDA_br_pct_map(r_br_pct_qcrop, "% Change in Crop Index")

F_EDA_br_new_map(r_br_new_qland, "Post-Sim Values of Cropland Area")
F_EDA_br_new_map(r_br_new_qcrop, "Post-Sim Values of Crop Index")

### Plot Best Map ---------
par(mfrow=c(1,4), oma = c(0,0,0,0))

### % Change in Cropland Area ###
terra::plot(r_br_pct_qland,
            type = "interval",
            breaks = c(seq(0, 4, by = 0.5)),
            #col = brewer.pal(9, "YlGn"),
            col = brewer.pal(9, "YlOrRd"),
            main = paste("Brazil % Change in Cropland Area", pct_title), 
            plg = list(x="bottomright"))
lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
#north(cbind(-65, -25))

### % Change in Crop Index ###
terra::plot(r_br_pct_qcrop,
            type = "interval",
            breaks = c(seq(0, 4, by = 0.5)),
            #col = brewer.pal(9, "YlGn"),
            col = brewer.pal(9, "YlOrRd"),
            main = paste("Brazil % Change in Crop Index", pct_title), 
            plg = list(x="bottomright"))
lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Cropland Area ###
terra::plot(r_br_new_qland/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 350, 450),
            #col = brewer.pal(9, "YlOrRd"),
            col = brewer.pal(9, "YlGn"),
            main = paste("Brazil Post-Simulation Crop Area", pct_title), 
            plg = list(x="bottomright"))
lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Crop Index ###
terra::plot(r_br_new_qcrop/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 350, 450),
            #col = brewer.pal(9, "YlOrRd"),
            col = brewer.pal(9, "YlGn"),
            main = paste("Brazil Post-Simulation Crop Index", pct_title), 
            plg = list(x="bottomright"))
lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")




# 3: CERRADO RESULTS ----------------------------

## 3.1 Clip to Cerrado Extent ----------------------------
# get Cerrado extent as terra object 
ext_cerr <- vect(ext(shp_br_cerr))

# crop and mask
r_cerr <- terra::crop(r, ext_cerr, mask = T)
r_cerr <- mask(r_cerr, shp_br_cerr)

## 3.2 Subset and do EDA ----------------------------
r_cerr_pct_qland <- r_cerr %>% subset("pct_QLAND")
r_cerr_new_qland <- r_cerr %>% subset("new_QLAND")
r_cerr_pct_qcrop <- r_cerr %>% subset("pct_QCROP")
r_cerr_new_qcrop <- r_cerr %>% subset("new_QCROP")

### Summaries ----------------------------
table_cerr <- summary(r_cerr, size = 1000000) # set size to not use a sample
table_cerr
write.csv(table_cerr, file = paste0(folder, "table_cerr_102923", pct, ".csv"))

### Boxplots ----------------------------
F_p_violin(r_cerr, "Cerrado")
hist(r_cerr)
hist(log(r_cerr))

## 3.3: Plot Cerrado Results ----------------------------
### Create Functions -------
F_EDA_cerr_pct_map <- function(layer, title){
  
  par(mfrow=c(2,2), oma = c(0,0,0,0))
  terra::plot(layer,
              type = "continuous",
              col = col_split,
              main = paste(title, "- default"), 
              plg = list(x="topleft"))
  
  terra::plot(layer,
              type = "interval",
              breaks = c(seq(0, 4, by = 0.5)),
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; seq()"), 
              plg = list(x="topleft"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "cases",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal quantile"), 
              plg = list(x="topleft"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "eqint",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal interval"), 
              plg = list(x="topleft"))
}



F_EDA_cerr_new_map <- function(layer, title){
  
  par(mfrow=c(2,2), oma = c(0,0,0,0))
  terra::plot(layer,
              type = "continuous",
              col = col_split,
              main = paste(title, "- default"), 
              plg = list(x="topleft"))
  
  terra::plot(layer,
              type = "interval",
              breaks = c(seq(0, 450000, by = 100000)),
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; seq()"), 
              plg = list(x="topleft"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "cases",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal quantile"), 
              plg = list(x="topleft"))
  
  terra::plot(layer,
              type = "interval",
              breakby = "eqint",
              col = brewer.pal(9, "YlOrRd"),
              main = paste(title, "- breaks; equal interval"), 
              plg = list(x="topleft"))
}

### Plot EDA --------
F_EDA_cerr_pct_map(r_cerr_pct_qland, "% Change in Cropland Area")
F_EDA_cerr_pct_map(r_cerr_pct_qcrop, "% Change in Crop Index")

F_EDA_cerr_new_map(r_cerr_new_qland, "Post-Sim Values of Cropland Area")
F_EDA_cerr_new_map(r_cerr_new_qcrop, "Post-Sim Values of Crop Index")

### Plot Best Map ---------
par(mfrow=c(1,4), oma = c(0,0,0,0))

### % Change in Cropland Area ###
terra::plot(r_cerr_pct_qland,
            type = "interval",
            breaks = c(seq(0, 4, by = 0.5)),
            #col = brewer.pal(9, "YlGn"),
            col = brewer.pal(9, "YlOrRd"),
            main = paste("Cerrado % Change in Cropland Area", pct_title), 
            plg = list(x="topleft"))
lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")
north(cbind(-59, -22))
sbar(d = 400, type = "bar", xy = "bottomright", divs = 4, below = "km")

### % Change in Crop Index ###
terra::plot(r_cerr_pct_qcrop,
            type = "interval",
            breaks = c(seq(0, 4, by = 0.5)),
            #col = brewer.pal(9, "YlGn"),
            col = brewer.pal(9, "YlOrRd"),
            main = paste("Cerrado % Change in Crop Index", pct_title), 
            plg = list(x="topleft"))
lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Cropland Area ###
terra::plot(r_cerr_new_qland/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 350, 450),
            #col = brewer.pal(9, "YlOrRd"),
            col = brewer.pal(9, "YlGn"),
            main = paste("Cerrado Post-Simulation Crop Area", pct_title), 
            plg = list(x="topleft"))
lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")

### Post-Sim Crop Index ###
terra::plot(r_cerr_new_qcrop/1000,
            type = "interval",
            breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 350, 450),
            #col = brewer.pal(9, "YlOrRd"),
            col = brewer.pal(9, "YlGn"),
            main = paste("Cerrado Post-Simulation Crop Index", pct_title), 
            plg = list(x="topleft"))
lines(shp_br_cerr_states, lwd = 0.8, lty = 3, col = "darkgray")














# GRAVEYARD ------------
# tidyterra --------
library(tidyterra)
r_df <- r %>% 
  mutate(cut_pctQland = cut(pct_QLAND, breaks = 9))

rr <- raster(r)
r_df <- rr %>% 
  mutate(cut_pctQland = cut(pct_QLAND, breaks = 9))

library(rasterVis)
r_us_pct <- r_us %>% subset(c("pct_QLAND", "pct_QCROP")) 

gplot(r_us_pct)+geom_tile(aes(fill = value))+facet_wrap(~variable)

# violin ggplots (included in rasterVis)--------

# F_p_violin <- function(df){
#   df2 <- as.data.frame(df)
#   
#   df2_new <- as.data.frame(df) %>% 
#     dplyr::select(new_QLAND, new_QCROP) %>% 
#     melt(variable.name = "layer", value.name = "new_value")
#   
#   p_v_1 <- ggplot(df2_new, aes(x = layer, y = new_value))+
#     geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
#     theme_bw()+
#     labs(title = "New Values of Area & Index")
#   
#   df2_pct <- as.data.frame(df) %>% 
#     dplyr::select(pct_QLAND, pct_QCROP) %>% 
#     melt(variable.name = "layer", value.name = "pct_change")
#   
#   p_v_2 <- ggplot(df2_pct, aes(x = layer, y = pct_change))+
#     geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
#     theme_bw()+labs(title = "% Change of Area & Index")
#   
#   plot(p_v_1)
#   plot(p_v_2)
#   
# }