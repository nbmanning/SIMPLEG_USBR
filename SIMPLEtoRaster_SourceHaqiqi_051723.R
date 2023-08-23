
# Read and plot SIMPLE-G results from text format
# By: Iman Haqiqi
# Aug 2019

# note: use the following command to define the files in your TABLO code, 
# File (New,text,SSE)
# I used GP 12.0.001

# ---- required libraries ---- 
library(raster)
library(stringr)


  varNames   <- "SMPL/out/wbm_simplegScarcity_var.txt"
  varNamesdf <- read.table(varNames, comment.char = " ", sep="\t", header=F)
  datNames   <- as.character(varNamesdf[which(varNamesdf$V1!="!"),])
  var.list   <- (varNamesdf[which(varNamesdf$V1!="!"   &
                                  varNamesdf$V1!="LON" &
                                  varNamesdf$V1!="LAT") ,] )


  datafile   <- "SMPL/out/wbm_simplegScarcity_pct.txt"
  old.lines  <- readLines(datafile)
  new.lines <- 
     old.lines[which(str_sub(old.lines, 1,1) != " " & 
                     str_sub(old.lines, 1,1) != "!" &
                     str_sub(old.lines, 1,1) != ""  )]
  newfile = "SMPL/out/temp.txt"
  
  writeLines(new.lines, newfile, sep="\n")
  
  dat <- read.table(newfile, sep=",", header=T)
  names(dat) <- c("grid", datNames)
  dat$x      <- as.numeric(dat$LON/120) # LON = 120x longitude
  dat$y      <- as.numeric(dat$LAT/120) # LAT = 120x latitude
  
  dat$irrgEff    <- log10(dat$QWEQPT + 1)
  

v = dat[1:75651,]
head(v)
coordinates(v) = ~x+y
gridded(v) = T
prct_ras = stack(v)

prct_ras$QLand_r[prct_ras$QLand_r > 500] = 500

writeRaster(prct_ras$QLand_i, "StoW/qLand_i_diff_p_2010-2015.tif", format="GTiff", overwrite=TRUE)
writeRaster(prct_ras$QLand_r, "StoW/qLand_r_diff_p_2010-2015.tif", format="GTiff", overwrite=TRUE)
writeRaster(prct_ras$EFF, "StoW/irrgationEff_diff_p_2010-2015.tif", format="GTiff", overwrite=TRUE)





