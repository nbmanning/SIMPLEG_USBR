# x_plot_lmh.R

# purpose: create plot of us, cerrado, brazil l,m,h scenarios
# changed to do this manually 

# author: Nick Manning

# date created: October 2025

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list=ls())
library(ggplot2)
library(terra)
library(tidyverse)
library(gridExtra)
library(tidyterra)

# 0) Load Shapefiles -----
load("../Data_Derived/shp_usbr.RData")
folder <- "../Data_Derived/2024-11-15/"
folder_fig <- "../Figures/2024-11-15/"

# 1) Load Clipped Result Rasters -------
l_cerrado <- rast(read_rds(paste0(folder, "r_l_Cerrado.rds")))
m_cerrado <- rast(read_rds(paste0(folder, "r_m_Cerrado.rds")))
h_cerrado <- rast(read_rds(paste0(folder, "r_h_Cerrado.rds")))

l_brazil <- rast(read_rds(paste0(folder, "r_l_Brazil.rds")))
m_brazil <- rast(read_rds(paste0(folder, "r_m_Brazil.rds")))
h_brazil <- rast(read_rds(paste0(folder, "r_h_Brazil.rds")))

l_us <- rast(read_rds(paste0(folder, "r_l_US.rds")))
m_us <- rast(read_rds(paste0(folder, "r_m_US.rds")))
h_us <- rast(read_rds(paste0(folder, "r_h_US.rds")))

# 2) Set Plotting Fxn's
## 2.1) US --------
F_ggplot_us_interval <- function(df, title_text, title_legend, save_title){
  
  # plot 
  #"atlas", "high_relief", "arid", "soft", "muted", 
  #"purple", "viridi", "gn_yl", "pi_y_g", "bl_yl_rd", "deep"
  p <- ggplot() +
    geom_spatraster(data = df, maxcell = Inf, aes(fill = cats)) +
    #scale_fill_wiki_d(na.value = "white")
    scale_fill_whitebox_d(palette = "pi_y_g", direction = 1, drop = F)+
    
    geom_sf(data = vect(shp_us), color = "gray30", fill = "transparent", lwd = 0.2)+
    #coord_sf(crs = "EPSG:2163")+ # Robinson
    coord_sf(crs = "EPSG:4326")+ # WGS 1984
    
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
      legend.position = c(0.9, 0.3),
      legend.text = element_text(size = 14)
    )  
  
  ggsave(plot = p, filename = paste0(folder_fig, "/", save_title),
         width = 16, height = 8, dpi = 300)
  
  return(p)
}


## 2.2) Brazil/Cerrado----
F_ggplot_brcerr_continuous <- function(df, area, brks, pal, legend_title, p_title, save_title){
  
  # plot
  p <- ggplot()+
    geom_spatraster(data = df, maxcell = Inf)+
    #coord_sf(crs = "EPSG:5880")+ # SIRGAS 2000 /Brazil Polyconic
    
    # use continuous palette
    scale_fill_whitebox_c(
      #palette = "viridi", direction = 1,
        palette = pal,
        breaks = brks,
        labels = brks,
        limits = range(brks)  # Ensures consistent scale across plots
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
      geom_sf(data = shp_cerr_states, color = "gray50", fill = "transparent", lwd = 0.2)+ 
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

F_ggplot_brcerr_discrete <- function(df, area, brks, pal, legend_title, p_title, save_title){
  
  # Convert raster values to factor based on breaks
  df_discrete <- classify(df, brks, include.lowest = TRUE, right = FALSE)
  
  # Create labels for the breaks
  brk_labels <- paste0(head(brks, -1), "–", tail(brks, -1))
  
  # Plot
  p <- ggplot() +
    geom_spatraster(data = df_discrete, maxcell = Inf) +
    
    # Use discrete palette
    scale_fill_whitebox_d(
      palette = pal,
      labels = brk_labels,
      na.translate = FALSE,
      drop = F
    ) +
    labs(
      fill = legend_title,
      title = p_title
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, hjust = 0.5),
      legend.position = c(0.1, 0.15),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 10)
    )
  
  # Add outlines based on area
  if(area == "Cerrado"){
    w = 12
    h = 8
    p <- p + 
      geom_sf(data = shp_cerr_states, color = "gray50", fill = "transparent", lwd = 0.2) + 
      geom_sf(data = shp_cerr, color = "black", fill = "transparent", lwd = 0.3)
  } else {
    w = 14
    h = 7
    p <- p +
      geom_sf(data = shp_br_border, color = "gray20", fill = "transparent", lwd = 0.4) +
      geom_sf(data = shp_cerr, color = "black", fill = "transparent", lwd = 0.3)
  }
  
  ggsave(plot = p, filename = paste0(folder_fig, "/", save_title),
         width = w, height = h, dpi = 300)
  
  return(p)
}

# 3) Plot -------
## 3.1) Continuous -----

### USA ###

### Brazil #########
p_l_brazil <- F_ggplot_brcerr_continuous(df = l_brazil %>% subset("rawch_SOY"),
                           #brks = waiver(),
                           brks = c(0, 0.25, 0.5), 
                           area = "Brazil",
                           pal = "gn_yl", 
                           legend_title = "Area (kha)",
                           p_title = paste("Change in BR Soybean Cropland Area - Low"),
                           save_title = "_test_gg_br_rawch_soy_croplandarea.png")

p_m_brazil <- F_ggplot_brcerr_continuous(df = m_brazil %>% subset("rawch_SOY"),
                                         #brks = waiver(),
                                         brks = c(0, 0.25, 0.5), 
                                         area = "Brazil",
                                         pal = "gn_yl", 
                                         legend_title = "Area (kha)",
                                         p_title = paste("Change in BR Soybean Cropland Area - Med"),
                                         save_title = "_test_gg_br_rawch_soy_croplandarea.png")
p_h_brazil <- F_ggplot_brcerr_continuous(df = h_brazil %>% subset("rawch_SOY"),
                                         #brks = waiver(),
                                         brks = c(0, 0.25, 0.5), 
                                         area = "Brazil",
                                         pal = "gn_yl", 
                                         legend_title = "Area (kha)",
                                         p_title = paste("Change in BR Soybean Cropland Area - High"),
                                         save_title = "_test_gg_br_rawch_soy_croplandarea.png")
p_brazil <- grid.arrange(grobs = plots, ncol = 3)

library(patchwork)
combined_plot_brazil <- (p_l_brazil + p_m_brazil + p_h_brazil) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

combined_plot_brazil

### Cerrado ######
p_l_cerrado <- F_ggplot_brcerr_continuous(df = l_cerrado %>% subset("rawch_SOY"),
                                         #brks = waiver(),
                                         brks = c(0, 0.25, 0.5), 
                                         area = "Cerrado",
                                         pal = "gn_yl", 
                                         legend_title = "Area (kha)",
                                         p_title = paste("Change in BR Soybean Cropland Area - Low"),
                                         save_title = "_test_gg_br_rawch_soy_croplandarea.png")

p_m_cerrado <- F_ggplot_brcerr_continuous(df = m_cerrado %>% subset("rawch_SOY"),
                                         #brks = waiver(),
                                         brks = c(0, 0.25, 0.5), 
                                         area = "Cerrado",
                                         pal = "gn_yl", 
                                         legend_title = "Area (kha)",
                                         p_title = paste("Change in BR Soybean Cropland Area - Med"),
                                         save_title = "_test_gg_br_rawch_soy_croplandarea.png")
p_h_cerrado <- F_ggplot_brcerr_continuous(df = h_cerrado %>% subset("rawch_SOY"),
                                         #brks = waiver(),
                                         brks = c(0, 0.25, 0.5), 
                                         area = "Cerrado",
                                         pal = "gn_yl", 
                                         legend_title = "Area (kha)",
                                         p_title = paste("Change in BR Soybean Cropland Area - High"),
                                         save_title = "_test_gg_br_rawch_soy_croplandarea.png")

combined_plot_cerrado <- (p_l_cerrado + p_m_cerrado + p_h_cerrado) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

combined_plot_cerrado

### USA ######

### LOW ###
tmp <-  l_us %>%
  subset("rawch_SOY")

# create df
factor <- tmp %>%
  mutate(
    cats =
      cut(rawch_SOY,
          breaks = c(-5, -3, -1, -0.1, -0.01,
                     0.01, 0.25, 0.5, 1, 2))
  )

# run Fxn
p_l_us <- F_ggplot_us_interval(
  df = factor, 
  title_text = "Change in US Soybean Cropland Area",
  title_legend = "Area (kha)",
  save_title = "test_gg_us_rawch_soy_croplandarea_4326.png")

### MED ###
tmp <-  m_us %>%
  subset("rawch_SOY")

# create df
factor <- tmp %>%
  mutate(
    cats =
      cut(rawch_SOY,
          breaks = c(-5, -3, -1, -0.1, -0.01,
                     0.01, 0.25, 0.5, 1, 2))
  )

# run Fxn
p_m_us <- F_ggplot_us_interval(
  df = factor, 
  title_text = "Change in US Soybean Cropland Area",
  title_legend = "Area (kha)",
  save_title = "test_gg_us_rawch_soy_croplandarea_4326.png")

### HIGH ###
tmp <-  h_us %>%
  subset("rawch_SOY")

# create df
factor <- tmp %>%
  mutate(
    cats =
      cut(rawch_SOY,
          breaks = c(-5, -3, -1, -0.1, -0.01,
                     0.01, 0.25, 0.5, 1, 2))
  )

# run Fxn
p_h_us <- F_ggplot_us_interval(
  df = factor, 
  title_text = "Change in US Soybean Cropland Area",
  title_legend = "Area (kha)",
  save_title = "test_gg_us_rawch_soy_croplandarea_4326.png")

# combine
p_l_us <- p_l_us + theme(legend.position = "none")
p_h_us <- p_h_us + theme(legend.position = "none")

combined_plot_us <- (p_l_us + p_m_us + p_h_us) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

combined_plot_us



# # Generate plots
# plots <- mapply(plot_wrapper, rasters, names(rasters), SIMPLIFY = FALSE)
# 
# # Arrange in 3x3 grid
# grid_plot <- grid.arrange(grobs = plots, ncol = 3)
# 
# # Optionally save
# ggsave("raster_grid_plot.png", grid_plot, width = 16, height = 12, dpi = 300)

# CODE ########################

# Load required libraries
library(terra)
library(tidyterra)
library(ggplot2)
library(dplyr)

# Define breaks and labels
breaks <- c(-5, -3, -1, -0.1, -0.01, 0.01, 0.25, 0.5, 1, 2)
labels <- paste(head(breaks, -1), tail(breaks, -1), sep = " to ")

# Helper function to convert raster to tidy data frame
raster_to_df <- function(raster, varname, scenario_label) {
  # Cut values into categories
  cats <- cut(values(raster[[varname]]),
              breaks = breaks,
              labels = labels,
              include.lowest = TRUE,
              right = FALSE)
  
  # Add categorized values to raster
  raster$cats <- cats
  
  # Convert to data frame
  df <- as.data.frame(raster, xy = TRUE, na.rm = FALSE)
  
  # Keep only coordinates and 'cats'
  df <- df %>%
    select(x, y, cats) %>%
    mutate(scenario = scenario_label)
  
  return(df)
}

# Prepare data frames for each scenario
df_l <- raster_to_df(l_us %>% subset("rawch_SOY"), 
                     varname = "rawch_SOY", 
                     scenario_label = "Low")

df_m <- raster_to_df(m_us %>% subset("rawch_SOY"), "rawch_SOY", "Medium")
df_h <- raster_to_df(h_us %>% subset("rawch_SOY"), "rawch_SOY", "High")

# Combine all into one data frame
df_all <- bind_rows(df_l, df_m, df_h)

# Plot faceted map
p_facet <- ggplot(df_all) +
  geom_tile(aes(x = x, y = y, fill = cats)) +
  scale_fill_whitebox_d(
    palette = "pi_y_g",
    direction = 1,
    na.translate = FALSE
  ) +
  facet_wrap(~ scenario) +
  geom_sf(data = vect(shp_us), color = "gray30", fill = NA, lwd = 0.2, inherit.aes = FALSE) +
  coord_sf(crs = "EPSG:4326") +
  theme_minimal() +
  labs(
    fill = "Area (kha)",
    title = "Change in US Soybean Cropland Area by Scenario",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 18),
    axis.text = element_blank()
  )

p_facet

# Save the plot
ggsave("facet_plot_us_soybean.png", p_facet, width = 20, height = 10, dpi = 300)

