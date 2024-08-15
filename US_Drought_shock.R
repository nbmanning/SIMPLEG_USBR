# Title: US_Drought_shock.R

# Purpose: Calculate the shock for US-Brazil telecoupling in SIMPLE-G

# Initial script by: Iman Haqiqi(ihaqiqi@purdue.edu)
# Initial date: March 27, 2024

# Edited by: Nick Manning 
# Last edit date: Aug 2024


# inputs:
# --- USDA Causes of Loss, 
# --- USDA Prevented and Failed Acres
# --- SIMPLEG coordinates

# outputs:
# --- y12.har for use in SIMPLEG

# NOTES:



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  

# 0: Load Libraries & Set Constants -----

# load required libraries for raster analysis
library(terra)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Set folder locations 
folder_shock <- "../US_Drought_Shock/"

# 1: Create a data frame with FIPS and xy coordinates -----------

# US coordinates and FIPS
us.xy.file <- paste0(folder_shock, "SIMPLEG_LonLat.csv")
tmp <- read.csv(us.xy.file, header=T, sep=",")

# re-generate coordinates to ensure decimal accuracy
tmp$x <- round(tmp$x * 120)/120
tmp$y <- round(tmp$y * 120)/120

#create a SpatRaster and plot
tmp.ras <- rast(tmp, type="xyz")
plot(tmp.ras)
plot(tmp.ras$nlcd_2011_L82)

# select the relevant variables; CRP & the amount of cropland in 2010
us.xy <- tmp[c("x","y","FIPS", "VCRP_2010", "QLND_2010")]

# 2: Create a data frame of loss by FIPS ----
# read the USDA RMA Causes of Loss
loss.file <- paste0(folder_shock, "colsom12.txt")
loss.dt <- read.csv(loss.file, header =F, sep="|")
head(loss.dt)

#read the column labels
loss.id <- paste0(folder_shock, "codes.csv")
id <- read.csv(loss.id, header =F, sep=",")
id

names(loss.dt) <- id$V1
names(loss.dt) 

# generate FIPS codes
loss.dt$FIPS <- loss.dt$State_Code * 1000 + loss.dt$County_Code

unique(loss.dt$Cause_Description)

loss.df <- loss.dt[c("FIPS", "Indemnity_Amount")]

loss.aggr <- aggregate( . ~ FIPS, data =loss.df, FUN= sum, na.rm=T )
head(loss.aggr)
dim(loss.aggr)

heat.dt <- loss.dt[loss.dt$Cause_Description == "Drought" |
                    loss.dt$Cause_Description == "Heat"   ,]

heat.aggr <- aggregate( . ~ FIPS, FUN= sum, na.rm=T, 
                       data =heat.dt[c("FIPS", "Indemnity_Amount")])
names(heat.aggr) <- c("FIPS", "Indemnity_Heat")

head(heat.aggr)
dim(heat.aggr)


# 3: Create a data frame of prevented and failed acres -----------

# read the prevented and failed acres
acres.file <- paste0(folder_shock, "PreventedFailed_2012.csv")
acres.dt <- read.csv(acres.file, header =T, sep=",")
names(acres.dt)


# generate FIPS codes
acres.dt$FIPS <- acres.dt$State.Code* 1000 + acres.dt$County.Code

summary(acres.dt)

acres.df <- acres.dt[c("FIPS", "Planted.Acres" , "Failded.Acres", 
                   "Prevented.Acres" , "Not.Planted.Acres", 
                   "Planted.and.Failed.Acres")]

acres.df <- acres.df %>% mutate_all(as.numeric)
summary(acres.df)

acres.aggr <- aggregate( . ~ FIPS,  data =acres.df, FUN= sum, na.rm=T )

head(acres.aggr)
dim(acres.aggr)


# 4: Merge data sets and calculate the rates (averages) -----------

# read the prevented and failed acres
fips.df <- merge(acres.aggr, loss.aggr, by = "FIPS", all.x=T)
fips.df <- merge(fips.df,    heat.aggr, by = "FIPS", all.x=T)

head(fips.df)

#fips.df$crop.acres <- fips.df$Planted.and.Failed.Acres + fips.df$Not.Planted.Acres
fips.df$crop.acres <- fips.df$Planted.and.Failed.Acres 

fips.df$IndemAcre <- fips.df$Indemnity_Amount / fips.df$crop.acres
fips.df$HeatAcre  <- fips.df$Indemnity_Heat   / fips.df$crop.acres

#  merge datasets
df.xyz <- merge(us.xy, fips.df[c("FIPS","IndemAcre", "HeatAcre")], by = "FIPS", all.x=T)
dim(df.xyz)
summary(df.xyz)

# set NA to 0
df.xyz[is.na(df.xyz)] <- 0

# get raster
s <- rast(df.xyz[-1], type="xyz")
s
#s <- rast(df.xyz[c("x","y","Loss_Ratio")], type="xyz")
plot(s)

# calculate the loss in % - max out shock at 99%
s$shock <- (1000/100)*(s$IndemAcre * s$QLND_2010) /s$VCRP_2010
s$shock[s$shock >= 99] = 99

s$heat <- 10*(s$HeatAcre * s$QLND_2010) /s$VCRP_2010
s$heat[s$heat >= 99] = 99

plot(s$shock)
plot(s$heat)

t=rast(res=1/12)
crs(s) <- crs(t)

# change the projection
u <- terra::project(s, "+init=epsg:2163")
v <- vect(paste0(folder_shock, "cb_2018_us_state_500k/"))
v <- crop(v,s)
w <- terra::project(v, "+init=epsg:2163")

plot(u$shock,  
     breaks=c(0,5,10,50,99),
     col = c( "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"),
     #main = "Percentage of crop production loss in 2012", 
     #type= "continuous", # no need for continuous if we provide breaks, or vice-versa
     axes =F,
     #legend.width=2, # legend.width is not a graphical parameter
     pax=list(side=1:4,retro=T),
     plg=list(title="(percentage loss)\n", x= "bottom", horiz=T))
plot(w, add=T)
text(w, "STUSPS", cex=0.7)


# 5: Plot Shock using tidyterra ---------

# Define breaks and colors using RColorBrewer
breaks <- c(0, 5, 10, 50, 99)
#colors <- c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")
colors <- brewer.pal(n = length(breaks) - 1, name = "YlOrRd")

# Cut the 'value' column into categories based on the breaks
# Necessary as it is continuous at the moment
u_cut <- u %>%
  mutate(category = cut(shock, breaks = breaks, include.lowest = TRUE))

# plot
ggplot()+
  geom_spatraster(data = u_cut %>% subset("category"), aes(fill = category))+
  scale_fill_manual(values = colors, name = "Percent Loss", na.value = "white") +
  geom_spatvector(data = w, fill = NA)+
  #geom_spatvector_text(data = w, aes(label = STUSPS))+
  theme_void() +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(direction = "horizontal"))

# save
ggsave("../Figures/SIMPLEG_Shock.png",
       height = 10, width = 15, dpi = 300)

# 6: Create Shock File for SIMPLE-G global -----------
xygID  <- rast(paste0(folder_shock, "grid_id_xyg.tif"))
plot(xygID)

nGrid <- 1316744

x <- resample(-s$heat, xygID)
x[is.na(x)] <- 0
plot(x)

y <- c(xygID, x*xygID/xygID)
plot(y)
y

my.df <- as.data.frame(y, xy=T)
my.df <- my.df[!is.na(my.df$grid_id_xyg),]
my.df <- my.df[ order(my.df$grid_id_xyg),]
head(my.df)

my.df$heat[my.df$heat < -80] = -80

all.equal(my.df$grid_id_xyg, c(1:nGrid))


v <- "y12" 
print(v)
txt <- paste0(nGrid, ' real row_order header "',v,'" longname "US 2012 Drought Shock" ;')

# NOTE: using the folder_shock file doesn't work with this for some reason - need to manually move y12.txt and y12.har from "code" folder, or the folder based on getwd()
#txt.file = paste0(folder_shock, v,".txt")
txt.file <- paste0(v,".txt")

write(txt, txt.file)

my.var <- "heat"

write.table(my.df[ , my.var], txt.file, 
            row.names=F, col.names=F, quote=F, sep=",", dec = ".", append=T)

### Not sure what this line does but it brings everything together
# original (uncomment to run)
system(paste0("txt2har.exe ", v,".txt ", v,".har"))

