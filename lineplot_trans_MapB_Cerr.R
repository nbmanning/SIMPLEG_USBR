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

# import cover and transition CSV's
getwd()
#csv_br_cover <- read.csv("current/mapb_summed_cover.csv")

#csv_br_trans <- read.csv("current/mapb_trans.csv", encoding = "UTF-8")
csv_br_trans <- read.csv("../Data_Source/MapBiomas/SOURCE_TRANSONLY_COL8_MAPBIOMAS_BIOMASxESTADOS.csv", encoding = "UTF-8")

# get rid of all accents
unique(csv_br_trans$biome)
csv_br_trans$state <- stri_trans_general(str = csv_br_trans$state,  id = "Latin-ASCII")
csv_br_trans$biome <- stri_trans_general(str = csv_br_trans$biome,  id = "Latin-ASCII")

#cover <- csv_br_cover
df <- filter(csv_br_trans, biome == "Cerrado")

df <- dplyr::select(df, c("state","biome", "to_level_4", "from_level_4", "X1985.1986", "X1986.1987", "X1987.1988", "X1988.1989", "X1989.1990", 
                                #"X1990.1991", "X1991.1992", "X1992.1993", "X1993.1994", "X1994.1995", "X1995.1996", "X1996.1997", "X1997.1998", "X1998.1999",    
                                "X1999.2000", "X2000.2001", "X2001.2002", "X2002.2003", "X2003.2004",    "X2004.2005",    "X2005.2006",   
                                "X2006.2007",    "X2007.2008",    "X2008.2009",    "X2009.2010",   "X2010.2011", "X2011.2012",    "X2012.2013",   
                                "X2013.2014",    "X2014.2015",    "X2015.2016",  "X2016.2017",    "X2017.2018",   
                                "X2018.2019",    "X2019.2020",    "X2020.2021"))

# remove all but the last four digits of all the columns 
names(df) <- str_sub(names(df), - 4, - 1)
names(df)

# rename columns 
colnames(df)[colnames(df) %in% c("tate", "iome", "el_4", "el_4")] <- c("state", "biome","to_level_4","from_level_4")
names(df)

# gather data
ncol(df)
# make long & add "fromto" column
df_g <- gather(df,"year","ha",5:40)     
df_g <- df_g %>% 
  mutate(fromto = paste0(from_level_4, " to ", to_level_4)) %>% 
  # Important! Removes all classes that stay the same
  filter(to_level_4 != from_level_4) 
  
  

# get csv of all the unique columns
names_df_g <- df_g %>% dplyr::select(from_level_4, to_level_4, fromto) %>% distinct()
write.csv(names_df_g, "../Data_Source/MapBiomas/names_fromto.csv", row.names = F)

# Filter to pasture and soybean
#unique(df_g$to_level_4)
df_g_to_pastsoy <- filter(df_g, to_level_4 %in% c("Soy Beans", "Pasture"))
df_g_to_soy <- filter(df_g, to_level_4 == "Soy Beans")


# Scale up to CERRADO scale ------------------------
df_g_to_soy_cerr <- df_g_to_soy %>% 
  aggregate(ha ~ year + biome + to_level_4 + from_level_4 + fromto, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

df_g_to_pastsoy_cerr <- df_g_to_pastsoy %>% 
  aggregate(ha ~ year + biome + to_level_4 + from_level_4 + fromto, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

df_g_to_all <- df_g %>% 
  group_by(year, to_level_4) %>%
  na.omit() %>% 
  summarize(total_trans = sum(ha)) %>% 
  filter(to_level_4 %in% 
           c("Pasture", "Soy Beans", "Other Temporary Crops",
             "Forest Plantation", "Mosaic of Agriculture and Pasture",
             "Sugar Cane", "Rice", "Cotton","Other Perennial Crops",
             "Coffe", "Citrus")) %>% 
  filter(year %in% yr_range) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# remove "from soybeans -> soybeans"
#df_g_to_soy_cerr <- df_g_to_soy_cerr[!grepl("Soy Beans", df_g_to_soy_cerr$from_level_4),]
#df_g_to_pastsoy_cerr <- df_g_to_pastsoy_cerr[!grepl("Soy Beans", df_g_to_pastsoy_cerr$from_level_4),]

# add ggrepel label - DIDN'T WORK - I got lost in what lines are biggest dfition
#df_g_to_soy_cerr$label[which(df_g_to_soy_cerr$from_level_4 == max(df_g_to_soy_cerr$from_level_4))] <- df_g_to_soy_cerr$group[which(df_g_to_soy_cerr$from_level_4 == max(df_g_to_soy_cerr$from_level_4))]



# PLOTTING -------------

# plot basic map ----------
# get state data
us_states <- map_data("state")

# set subregion value
us_states$subregion <- ifelse(
  us_states$region %in% c(
    "north dakota", "south dakota", "nebraska", "kansas",
    "missouri", "iowa", "minnesota", "wisconsin", "illinois", 
    "indiana", "ohio", "michigan"),
  "mw", NA)

# plot and save
(p <- ggplot(data = us_states,
             # fill with the subregion of mw states
             mapping = aes(x = long, y = lat,
                           group = group, 
                           fill = subregion))+ 
    # add color to the map
    geom_polygon(color = "gray90", linewidth = 0.1) +
    # change projection
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    # remove legend
    guides(fill = "none")+
    # get rid of lat/long box
    ggthemes::theme_map()
  )

ggsave("../Figures/trans_mapbiomas/usmw_map.png", plot = p)


# original; from X to Soybean ---------
ggplot(df_g_to_soy_cerr, aes(x=year, y=ha, color = from_level_4)) +
  geom_line() + 
  #geom_point() +
  xlab("")+
  scale_x_date(date_labels = "%m-%Y")+
  labs(title = "From X to Soybean",
       ylab = "Hectares Transitioned from Previous Year")+
  geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
             linetype="dotted", linewidth=0.5)

# remove insignificant classes; first, see all the classes
unique(df_g_to_soy_cerr$from_level_4)

# get a few top classes instead of all of them
# as.factor(df_g_to_soy_cerr$from_level_4)

classes_few <- c("Other Temporary Crops", "Mosaic of Agriculture and Pasture", "Cotton",
                 "Pasture", "Savanna Formation", "Grassland", "Sugar Cane", "Other Non Vegetated Area", 
                 "Rice", "Forest Formation", "Wetland")

df_g_to_soy_cerr <- filter(df_g_to_soy_cerr, from_level_4 %in% classes_few)

# plot with a few classes remove
ggplot(df_g_to_soy_cerr, aes(x=year, y=ha/1000000, color = from_level_4)) +
  geom_line() + 
  #geom_point() +
  xlab("")+
  scale_x_date(date_labels = "%m-%Y")+
  labs(title = "From X to Soybean",
       ylab = "Hectares Transitioned from Previous Year")+
  geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
             linetype="dotted", linewidth=0.5)

### fewer 
classes_fewer <- c("Other Temporary Crops", "Mosaic of Agriculture and Pasture", "Cotton",
                   "Pasture", "Savanna Formation", "Grassland")
df_g_to_soy_cerr <- filter(df_g_to_soy_cerr, from_level_4 %in% classes_fewer)

ggplot(df_g_to_soy_cerr, aes(x=year, y=ha/1000000, color = fromto)) +
  geom_line(lwd = 0.8) + 
  geom_point(fill = "white", size = 1.2) +
  xlab("")+
  scale_x_date(date_labels = "%Y")+
  labs(
    title = "Transitions to Soybean",
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



# NEW: Plot Specific "From" to Only Pasture Soybean -------------
df_g_to_pastsoy_cerr <- df_g_to_pastsoy %>% 
  aggregate(ha ~ year + biome + to_level_4 + from_level_4 + fromto, sum) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d')) %>% 
  filter(year(year) %in% yr_range)

# get only a few classes
classes_mult_few <- c("Other Temporary Crops", "Mosaic of Agriculture and Pasture", "Cotton",
                 "Pasture", "Savanna Formation", "Grassland", "Sugar Cane", "Other Non Vegetated Area", 
                 "Rice", "Forest Formation", "Wetland")

# filter to select classes
df_g_to_pastsoy_cerr <- df_g_to_pastsoy_cerr %>% 
  filter(from_level_4 %in% classes_mult_few)# %>% 
  #filter(fromto != "Pasture to Pasture") # %>%   mutate(rank = rank(-ha))

# get top 10 categories
ls_top <- df_g_to_pastsoy_cerr %>%
  # filter to only the interesting "from" classes
  #filter(from_level_4 %in% classes_few) %>% 
  #filter(fromto != "Pasture to Pasture") %>% 
  # get the top converted "n" classes in 2013 as a list
  filter(year(year) == 2013) %>% 
  arrange(desc(ha)) %>% 
  slice_head(n = 6) %>% 
  pull(fromto) %>% 
  as.list

df_g_to_pastsoy_cerr_topn <- df_g_to_pastsoy_cerr %>% 
  filter(fromto %in% ls_top)

# plot
ggplot(df_g_to_pastsoy_cerr_topn, aes(x=year, y=ha, color = fromto)) +
  geom_line() + 
  geom_point(fill = "white", size = 0.8) +
  xlab("")+
  scale_x_date(date_labels = "%Y")+
  labs(title = paste("Top", length(ls_top), "Transition Classes"),
       ylab = "Hectares Transitioned from Previous Year",
       color = "From-To Transitions")+
  geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
             linetype="dotted", linewidth=0.5)+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "top")+
  guides(color = guide_legend(nrow = 2))


# Top "TO" categories -------------
df_g_to_all <- df_g %>% 
  group_by(year, to_level_4) %>%
  na.omit() %>% 
  filter(to_level_4 != from_level_4) %>% 
  summarize(total_trans = sum(ha)) %>% 
  filter(year %in% yr_range) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# get top 10 categories
ls_top <- df_g_to_all %>%
  # filter to only the interesting "from" classes
  #filter(from_level_4 %in% classes_few) %>% 
  #filter(fromto != "Pasture to Pasture") %>% 
  # get the top converted "n" classes in 2013 as a list
  filter(year(year) == 2013) %>% 
  arrange(desc(total_trans)) %>% 
  slice_head(n = 4) %>% 
  pull(to_level_4) %>% 
  as.list

df_g_to_all_topn <- df_g_to_all %>% 
  filter(to_level_4 %in% ls_top)

# plot
ggplot(df_g_to_all_topn, aes(x=year, y=total_trans/1000000, color = to_level_4)) +
  geom_line() +
  geom_point(fill = "white", size = 0.8) +
  xlab("")+
  scale_x_date(date_labels = "%Y")+
  labs(
    title = paste("Top", length(ls_top), "Transition Classes"),
    #subtitle = "From All Classes to X",
    y = "Land Change from Previous Year (Mha)",
    color = "From-To Transitions"
    )+
  geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
             linetype="dotted", linewidth=0.5)+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "top",
    axis.title.y = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 0.5),
    #plot.subtitle = element_text(size = 12, hjust = 0.5)
    )

# save 
ggsave(paste("../Figures/trans_mapbiomas/top_",length(ls_top), "_fromAlltoX.png"), 
       width = 16, height = 8)


# Top "from-to" categories ------------
df_g_fromto <- df_g %>% 
  group_by(year, fromto) %>%
  na.omit() %>% 
  summarize(total_trans = sum(ha)) %>% 
  filter(year %in% yr_range) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# get top 10 categories
ls_top <- df_g_fromto %>%
 # get the top converted "n" classes in 2013 as a list
  filter(year(year) == 2013) %>% 
  arrange(desc(total_trans)) %>% 
  slice_head(n = 10) %>% 
  pull(fromto) %>% 
  as.list

df_g_fromto_topn <- df_g_fromto %>% 
  filter(fromto %in% ls_top)

# plot
ggplot(df_g_fromto_topn, aes(x=year, y=total_trans/1000000, color = fromto)) +
  geom_line() +
  geom_point(fill = "white", size = 0.8) +
  xlab("")+
  scale_x_date(date_labels = "%Y")+
  labs(title = paste("Top", length(ls_top), "Transition Classes"),
       subtitle = "Only FromTo Classes",
       y = "Land Change from Previous Year (Mha)",
       color = "From-To Transitions")+
  geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
             linetype="dotted", linewidth=0.5)+
  theme_bw()+
  theme(
    #legend.title = element_blank(),
    #legend.text = element_text(size = 12),
    #legend.position = "top"
        )

# save 
ggsave(paste("../Figures/trans_mapbiomas/top_",length(ls_top), "_fromto.png"), 
       width = 16, height = 8)


# Specific "from" and "to" categories -------------

## Set Specific Categories

# names_from <- c("Forest Formation", "Grassland", "Mosaic of Agriculture and Pasture",
#                 "Pasture", "Other Temporary Crops", "Other Perennial Crops",
#                 "Savanna Formation", "Soy Beans", "Sugar Cane", "Wetland")

names_from <- c("Forest Formation", "Grassland", "Pasture", "Savanna", "Wetland")

#names_from <- c("Forest Formation", "Grassland", "Savanna", "Wetland")


names_to <- c("Mosaic of Agriculture and Pasture", "Pasture",
             "Other Temporary Crops", "Other Perennial Crops",
             "Soy Beans", "Sugar Cane")

# filter by this section
df_g_from_and_to <- df_g %>% 
  filter(to_level_4 %in% names_to) %>% 
  filter(from_level_4 %in% names_from) %>% 
  filter(fromto != "Pasture to Mosaic of Agriculture and Pasture") %>% 
  group_by(year, fromto) %>% 
  na.omit() %>% 
  summarise(total_trans = sum(ha)) %>% 
  filter(year %in% yr_range) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# get list of top "n"
ls_top <- df_g_from_and_to %>%
  # get the top converted "n" classes in 2013 as a list
  filter(year(year) == 2013) %>% 
  arrange(desc(total_trans)) %>% 
  slice_head(n = 8) %>% 
  pull(fromto) %>% 
  as.list

# filter from list
df_g_from_and_to_topn <- df_g_from_and_to %>% 
  filter(fromto %in% ls_top) 

# plot
ggplot(df_g_from_and_to_topn, aes(x=year, y=total_trans/1000000, color = fromto)) +
  geom_line() +
  geom_point(fill = "white", size = 0.8) +
  xlab("")+
  scale_x_date(date_labels = "%Y")+
  labs(title = paste("Top", length(ls_top), "Transition Classes"),
       subtitle = "Selected From and To Classes",
       y = "Land Change from Previous Year (Mha)",
       color = "From-To Transitions")+
  geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
             linetype="dotted", linewidth=0.5)+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "top"
        )

# save 
ggsave("../Figures/trans_mapbiomas/selected_from_and_to.png", 
       width = 16, height = 8)


# PLOT: Specific "from-to" categories ---------
names_fromto <-c(
  "Forest Formation to Mosaic of Agriculture and Pasture",
  "Forest Formation to Pasture",
  "Forest Formation to Soy Beans",
  "Forest Formation to Other Temporary Crops",
  "Pasture to Mosaic of Agriculture and Pasture",
  "Pasture to Other Temporary Crops",
  "Pasture to Soy Beans",
  "Pasture to Sugar Cane",
  "Savanna Formation to Mosaic of Agriculture and Pasture",
  "Savanna Formation to Other Temporary Crops",
  "Savanna Formation to Pasture",
  "Savanna Formation to Soy Beans"
  )


df_g_specific_fromto <- df_g %>% 
  filter(fromto %in% names_fromto) %>% 
  group_by(year, fromto) %>% 
  na.omit() %>% 
  summarise(total_trans = sum(ha)) %>% 
  filter(year %in% yr_range) %>% 
  mutate(year = as.Date(paste(year, 1, 1), '%Y %m %d'))

# get list of top "n"
ls_top <- df_g_specific_fromto %>%
  # get the top converted "n" classes in 2013 as a list
  filter(year(year) == 2013) %>% 
  arrange(desc(total_trans)) %>% 
  slice_head(n = 8) %>% 
  pull(fromto) %>% 
  as.list

# filter from list
df_g_specific_fromto_topn <- df_g_specific_fromto %>% 
  filter(fromto %in% ls_top) 

# plot
ggplot(df_g_specific_fromto, aes(x=year, y=total_trans/1000000, color = fromto)) +
  geom_line() +
  geom_point(fill = "white", size = 0.8) +
  xlab("")+
  scale_x_date(date_labels = "%Y")+
  labs(title = paste("Top", length(ls_top), "Transition Classes"),
       subtitle = "Selected From and To Classes",
       y = "Land Change from Previous Year (Mha)",
       color = "From-To Transitions")+
  geom_vline(xintercept = as.Date("2012-01-01"), color = "red",
             linetype="dotted", linewidth=0.5)+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "top",
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  )

# save 
ggsave("../Figures/trans_mapbiomas/selected_fromto.png", 
       width = 16, height = 8)


# Brazil Production Stats - What % of crops are corn & soy? --------------

# downloaded from https://www.fao.org/faostat/en/#data/QCL
## Countries == Brazil, United States of America
## Elements == Area HArvested, Yield, Production Quantity
## Items = Crops, primary > (List)
## Years = 2010, 2011, 2012, 2013

raw_prod <- read.csv("../Data_Source/FAOSTAT/FAOSTAT_BRUS_AllCrop_20102013.csv")

names(raw_prod)

prod <- raw_prod %>%
  dplyr::select(Area, Element, Item, Year, Unit, Value)

# Set Year of Interest
yr <- 2011

# get the total area harvested and production for the whole year
prod_total <- prod %>% 
  filter(Element != "Yield") %>% 
  group_by(Area, Element, Year) %>% 
  #filter(Year == yr) %>% 
  summarize(TotalProd = sum(Value))


# get just the soybeans (soya beans) and corn ()
prod_cornsoy <- prod %>% 
  ## Get to just corn soy
  filter(Element != "Yield") %>% 
  filter(Item %in% c("Soya beans", 
                     #"Sugar cane",
                     "Maize (corn)"
                     )) %>% 
  
  ## add corn soy by year
  #filter(Year == yr) %>% 
  group_by(Area, Element, Year) %>% 
  summarize(TotalCornSoy = sum(Value)) %>% 
  
  ## create percentages 
  left_join(prod_total) %>% 
  mutate(PctCornSoy = round((TotalCornSoy / TotalProd) * 100, 2)) %>% 
  arrange(Area, Element, desc(Year))



# See which items have the largest Pct
prod_pct <- prod %>% 
  filter(Element != "Yield") %>% 
  left_join(prod_total) %>% 
  mutate(Pct = round((Value / TotalProd) * 100, 1)) %>% 
  filter(Year == yr) %>% 
  arrange(Area, Element, desc(Pct))


## Production Stats ----------

# BR 2011 Sugarcane = 74% of Production by tons but only 14% of Area Harvested?? 
# BR 2011 CornSoy = 54.3% of Area Harvested and 13.2% of Production

# US 2011 

