# title: trans_BRcerr_lineplots.R
# author: Nick Manning
# date created: 9/4/23

# last updated: September 2023

# Goals: 
# 2000-2019 Time Series of Conversions in Cerrado with 2012 (July) indicated
## Natural Forest → Soybean / Crops
## Pasture → Soybean / Crops 
## Grassland → Soybean / Crops 

# links: 
# download the HUGE MapBiomas CSV that this was based on from: https://mapbiomas.org/en/statistics 
## link was "LAND COVER AND TRANSITIONS BY BIOMES & STATES (COLLECTION 8)"
## sheet was 'TRANSICOES_COL8'

# NOTES: 
# copied plotting code from "Thesis2\@_ThesisCode\code_old_ignore\fall22_spring23_geo866_CEP_attempt\0_3_to_soy_trans_BRprice.R"

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# load libraries 
rm(list = ls())

library(dplyr) # data manipulation 
library(stringr) # changing colnames
library(tidyr) # gathering data
library(lubridate) # creating 'year' types 
library(ggplot2) # plotting
library(stringi) # removing accents

# plotting map
library(maps)
library(ggthemes)


# set constants 
yr <- 2011
yr_range <- 2000:2019

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# UPDATE 12/7: Use Cerrado Municipality level data instead of state level data
# import from aggStats_MapBiomas.R

# load clean, long, df 
load(file = "../Data_Derived/mapb_col8_clean_long.Rdata")

# load municipality codes for Cerrado 
load(file = "../Data_Derived/muni_codes_cerr.Rdata")

# df_g is only Cerrado - level 3 now 
df_g <- df
df_g <- df_g %>% 
  filter(geocode %in% muni_codes_cerr) %>% 
  filter(to_level_3 == "Temporary Crops") %>%
  filter(to_level_3 != from_level_3) %>% 
  select(!c(from_level_4, to_level_4)) %>% 
  mutate(fromto = paste0(from_level_3, " to ", to_level_3)) %>% 
  filter(biome == "Cerrado")

# get csv of all the unique columns
names_df_g <- df_g %>% dplyr::select(from_level_3, to_level_3, fromto) %>% distinct()
write.csv(names_df_g, "../Data_Source/MapBiomas/names_fromto_lvl3.csv", row.names = F)

# Scale up to CERRADO scale ------------------------
df_g_cerr <- df_g %>% 
  aggregate(ha ~ year + biome + to_level_3 + from_level_3 + fromto, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# PLOTTING -------------

# plot all categories to temp crops
ggplot(df_g_cerr, aes(x=year, y=ha, color = from_level_3)) +
  geom_line() + 
  #geom_point() +
  xlab("")+
  scale_x_date(date_labels = "%m-%Y")+
  labs(title = "From X to Temporary Crop",
       ylab = "Hectares Transitioned from Previous Year")+
  geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
             linetype="dotted", linewidth=0.5)

# remove insignificant classes; first, see all the classes
unique(df_g_cerr$from_level_3)

# get a few top classes instead of all of them
# as.factor(df_g_to_soy_cerr$from_level_3)
# classes_few <- c("Other Temporary Crops", "Mosaic of Agriculture and Pasture", "Cotton",
#                  "Pasture", "Savanna Formation", "Grassland", "Sugar Cane", "Other Non Vegetated Area", 
#                  "Rice", "Forest Formation", "Wetland")

classes_few <- c(
  "Forest Formation",
  "Forest Plantation",                
  "Grassland", 
  "Mosaic of Agriculture and Pasture",
  #"Non Observed",
  #"Other Non Vegetated Area",         
  "Pasture",
  "Perennial Crops",
  #"River, Lake and Ocean",
  "Savanna Formation",                
  "Urban Infrastructure",
  "Wetland"
)  

df_g_few <- df_g_cerr %>% filter(from_level_3 %in% classes_few)

# plot with a few classes remove
ggplot(df_g_few, aes(x=year, y=ha/1000000, color = from_level_3)) +
  geom_line() + 
  #geom_point() +
  #scale_x_date(date_labels = "%m-%Y")+
  labs(title = "From X to Soybean",
       ylab = "Hectares Transitioned from Previous Year",
       x = "")+
  geom_vline(xintercept = 2012, color = "red",
             linetype="dotted", linewidth=0.5)

# select certain classes for plotting 
# fewer 
classes_fewer <- c(
  "Forest Formation",
  "Mosaic of Agriculture and Pasture",
  "Pasture",
  "Savanna Formation", 
  "Grassland",
  "Perennial Crops"
  )

df_g_fewer <- filter(df_g_cerr, from_level_3 %in% classes_fewer)

ggplot(df_g_fewer, aes(x=year, y=ha/1000000, color = fromto)) +
  geom_line(lwd = 0.8) + 
  geom_point(fill = "white", size = 1.2) +
  xlab("")+
  scale_x_date(date_labels = "%Y")+
  labs(
    title = "Transitions to Temporary Crops",
    y = "Land Change from Previous Year (Mha)")+
  geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
             linetype="dotted", linewidth=1)+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "top",
        plot.title = element_text(size = 18, hjust = 0.5),
        #plot.title = element_text(hjust = 0.5)
        )

# save 
ggsave(paste("../Figures/trans_mapbiomas/cerr_to_lvl3_tempcrops.png"), 
       width = 16, height = 8)

# END #########################################################################

# GRAVEYARD --------------------------------

#1: Code  From LEVEL 4 SCRIPT -------
# # Plot Specific "From" to Only Pasture Soybean -------------
# df_g_to_pastsoy_cerr <- df_g_to_pastsoy %>% 
#   aggregate(ha ~ year + biome + to_level_3 + from_level_3 + fromto, sum) %>% 
#   mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d')) %>% 
#   filter(year(year) %in% yr_range) %>% 
#   filter(biome == "Cerrado")
# 
# # get only a few classes
# classes_mult_few <- c("Other Temporary Crops", "Mosaic of Agriculture and Pasture", "Cotton",
#                  "Pasture", "Savanna Formation", "Grassland", "Sugar Cane", "Other Non Vegetated Area", 
#                  "Rice", "Forest Formation", "Wetland")
# 
# # filter to select classes
# df_g_to_pastsoy_cerr <- df_g_to_pastsoy_cerr %>% 
#   filter(from_level_3 %in% classes_mult_few)# %>% 
#   #filter(fromto != "Pasture to Pasture") # %>%   mutate(rank = rank(-ha))
# 
# # get top 10 categories
# ls_top <- df_g_to_pastsoy_cerr %>%
#   # filter to only the interesting "from" classes
#   #filter(from_level_3 %in% classes_few) %>% 
#   #filter(fromto != "Pasture to Pasture") %>% 
#   # get the top converted "n" classes in 2013 as a list
#   filter(year(year) == 2013) %>% 
#   arrange(desc(ha)) %>% 
#   slice_head(n = 6) %>% 
#   pull(fromto) %>% 
#   as.list
# 
# df_g_to_pastsoy_cerr_topn <- df_g_to_pastsoy_cerr %>% 
#   filter(fromto %in% ls_top)
# 
# # plot
# ggplot(df_g_to_pastsoy_cerr_topn, aes(x=year, y=ha, color = fromto)) +
#   geom_line() + 
#   geom_point(fill = "white", size = 0.8) +
#   xlab("")+
#   scale_x_date(date_labels = "%Y")+
#   labs(title = paste("Top", length(ls_top), "Transition Classes"),
#        ylab = "Hectares Transitioned from Previous Year",
#        color = "From-To Transitions")+
#   geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
#              linetype="dotted", linewidth=0.5)+
#   theme_bw()+
#   theme(legend.title = element_blank(),
#         legend.text = element_text(size = 12),
#         legend.position = "top")+
#   guides(color = guide_legend(nrow = 2))
# 
# 
# # Top "TO" categories -------------
# df_g_to_all <- df_g %>% 
#   group_by(year, to_level_3) %>%
#   na.omit() %>% 
#   filter(to_level_3 != from_level_3) %>% 
#   summarize(total_trans = sum(ha)) %>% 
#   filter(year %in% yr_range) %>% 
#   mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))
# 
# # get top 10 categories
# ls_top <- df_g_to_all %>%
#   # filter to only the interesting "from" classes
#   #filter(from_level_3 %in% classes_few) %>% 
#   #filter(fromto != "Pasture to Pasture") %>% 
#   # get the top converted "n" classes in 2013 as a list
#   filter(year(year) == 2013) %>% 
#   arrange(desc(total_trans)) %>% 
#   slice_head(n = 4) %>% 
#   pull(to_level_3) %>% 
#   as.list
# 
# df_g_to_all_topn <- df_g_to_all %>% 
#   filter(to_level_3 %in% ls_top)
# 
# # plot
# ggplot(df_g_to_all_topn, aes(x=year, y=total_trans/1000000, color = to_level_3)) +
#   geom_line() +
#   geom_point(fill = "white", size = 0.8) +
#   xlab("")+
#   scale_x_date(date_labels = "%Y")+
#   labs(
#     title = paste("Top", length(ls_top), "Transition Classes"),
#     #subtitle = "From All Classes to X",
#     y = "Land Change from Previous Year (Mha)",
#     color = "From-To Transitions"
#     )+
#   geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
#              linetype="dotted", linewidth=0.5)+
#   theme_bw()+
#   theme(
#     legend.title = element_blank(),
#     legend.text = element_text(size = 16),
#     legend.position = "top",
#     axis.title.y = element_text(size = 10),
#     plot.title = element_text(size = 14, hjust = 0.5),
#     #plot.subtitle = element_text(size = 12, hjust = 0.5)
#     )
# 
# # save 
# ggsave(paste("../Figures/trans_mapbiomas/top_",length(ls_top), "_fromAlltoX.png"), 
#        width = 16, height = 8)
# 
# 
# # Top "from-to" categories ------------
# df_g_fromto <- df_g %>% 
#   group_by(year, fromto) %>%
#   na.omit() %>% 
#   summarize(total_trans = sum(ha)) %>% 
#   filter(year %in% yr_range) %>% 
#   mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))
# 
# # get top 10 categories
# ls_top <- df_g_fromto %>%
#  # get the top converted "n" classes in 2013 as a list
#   filter(year(year) == 2013) %>% 
#   arrange(desc(total_trans)) %>% 
#   slice_head(n = 10) %>% 
#   pull(fromto) %>% 
#   as.list
# 
# df_g_fromto_topn <- df_g_fromto %>% 
#   filter(fromto %in% ls_top)
# 
# # plot
# ggplot(df_g_fromto_topn, aes(x=year, y=total_trans/1000000, color = fromto)) +
#   geom_line() +
#   geom_point(fill = "white", size = 0.8) +
#   xlab("")+
#   scale_x_date(date_labels = "%Y")+
#   labs(title = paste("Top", length(ls_top), "Transition Classes"),
#        subtitle = "Only FromTo Classes",
#        y = "Land Change from Previous Year (Mha)",
#        color = "From-To Transitions")+
#   geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
#              linetype="dotted", linewidth=0.5)+
#   theme_bw()+
#   theme(
#     #legend.title = element_blank(),
#     #legend.text = element_text(size = 12),
#     #legend.position = "top"
#         )
# 
# # save 
# ggsave(paste("../Figures/trans_mapbiomas/top_",length(ls_top), "_fromto.png"), 
#        width = 16, height = 8)
# 
# 
# # Specific "from" and "to" categories -------------
# 
# ## Set Specific Categories
# 
# # names_from <- c("Forest Formation", "Grassland", "Mosaic of Agriculture and Pasture",
# #                 "Pasture", "Other Temporary Crops", "Other Perennial Crops",
# #                 "Savanna Formation", "Soy Beans", "Sugar Cane", "Wetland")
# 
# names_from <- c("Forest Formation", "Grassland", "Pasture", "Savanna", "Wetland")
# 
# #names_from <- c("Forest Formation", "Grassland", "Savanna", "Wetland")
# 
# 
# names_to <- c("Mosaic of Agriculture and Pasture", "Pasture",
#              "Other Temporary Crops", "Other Perennial Crops",
#              "Soy Beans", "Sugar Cane")
# 
# # filter by this section
# df_g_from_and_to <- df_g %>% 
#   filter(to_level_3 %in% names_to) %>% 
#   filter(from_level_3 %in% names_from) %>% 
#   filter(fromto != "Pasture to Mosaic of Agriculture and Pasture") %>% 
#   group_by(year, fromto) %>% 
#   na.omit() %>% 
#   summarise(total_trans = sum(ha)) %>% 
#   filter(year %in% yr_range) %>% 
#   mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))
# 
# # get list of top "n"
# ls_top <- df_g_from_and_to %>%
#   # get the top converted "n" classes in 2013 as a list
#   filter(year(year) == 2013) %>% 
#   arrange(desc(total_trans)) %>% 
#   slice_head(n = 8) %>% 
#   pull(fromto) %>% 
#   as.list
# 
# # filter from list
# df_g_from_and_to_topn <- df_g_from_and_to %>% 
#   filter(fromto %in% ls_top) 
# 
# # plot
# ggplot(df_g_from_and_to_topn, aes(x=year, y=total_trans/1000000, color = fromto)) +
#   geom_line() +
#   geom_point(fill = "white", size = 0.8) +
#   xlab("")+
#   scale_x_date(date_labels = "%Y")+
#   labs(title = paste("Top", length(ls_top), "Transition Classes"),
#        subtitle = "Selected From and To Classes",
#        y = "Land Change from Previous Year (Mha)",
#        color = "From-To Transitions")+
#   geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
#              linetype="dotted", linewidth=0.5)+
#   theme_bw()+
#   theme(
#     legend.title = element_blank(),
#     legend.text = element_text(size = 10),
#     legend.position = "top"
#         )
# 
# # save 
# ggsave("../Figures/trans_mapbiomas/selected_from_and_to.png", 
#        width = 16, height = 8)
# 
# 
# # PLOT: Specific "from-to" categories ---------
# names_fromto <-c(
#   "Forest Formation to Mosaic of Agriculture and Pasture",
#   "Forest Formation to Pasture",
#   "Forest Formation to Soy Beans",
#   "Forest Formation to Other Temporary Crops",
#   "Pasture to Mosaic of Agriculture and Pasture",
#   "Pasture to Other Temporary Crops",
#   "Pasture to Soy Beans",
#   "Pasture to Sugar Cane",
#   "Savanna Formation to Mosaic of Agriculture and Pasture",
#   "Savanna Formation to Other Temporary Crops",
#   "Savanna Formation to Pasture",
#   "Savanna Formation to Soy Beans"
#   )
# 
# 
# df_g_specific_fromto <- df_g %>% 
#   filter(fromto %in% names_fromto) %>% 
#   group_by(year, fromto) %>% 
#   na.omit() %>% 
#   summarise(total_trans = sum(ha)) %>% 
#   filter(year %in% yr_range) %>% 
#   mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))
# 
# # get list of top "n"
# ls_top <- df_g_specific_fromto %>%
#   # get the top converted "n" classes in 2013 as a list
#   filter(year(year) == 2013) %>% 
#   arrange(desc(total_trans)) %>% 
#   slice_head(n = 8) %>% 
#   pull(fromto) %>% 
#   as.list
# 
# # filter from list
# df_g_specific_fromto_topn <- df_g_specific_fromto %>% 
#   filter(fromto %in% ls_top) 
# 
# # plot
# ggplot(df_g_specific_fromto, aes(x=year, y=total_trans/1000000, color = fromto)) +
#   geom_line() +
#   geom_point(fill = "white", size = 0.8) +
#   xlab("")+
#   scale_x_date(date_labels = "%Y")+
#   labs(title = paste("Top", length(ls_top), "Transition Classes"),
#        subtitle = "Selected From and To Classes",
#        y = "Land Change from Previous Year (Mha)",
#        color = "From-To Transitions")+
#   geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
#              linetype="dotted", linewidth=0.5)+
#   theme_bw()+
#   theme(
#     legend.title = element_blank(),
#     legend.text = element_text(size = 10),
#     legend.position = "top",
#     plot.title = element_text(size = 14, hjust = 0.5),
#     plot.subtitle = element_text(size = 12, hjust = 0.5)
#   )
# 
# # save 
# ggsave("../Figures/trans_mapbiomas/selected_fromto.png", 
#        width = 16, height = 8)
# 
# 
# 
