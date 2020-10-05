#install RGEE
remotes::install_github("r-spatial/rgee", force = TRUE)
ee_install()
ee_Initialize()

#other libraries that we need
library(tidyverse)
library(reticulate)
library(sf)
library(sp)

#Import python modules RGEE works in a python environment
py_install("numpy")
np = import("numpy")


py_install("pandas")
pd = import("pandas")

#read in our ROI- Worcester Ecoregion
eco_mask <- st_read('Ecoregion.shp',quiet = TRUE) %>% st_transform(4326)
eco_mask_ee <- sf_as_ee(eco_mask)



#Bring in the imagery from RGEE
s3 <- ee$ImageCollection('LANDSAT/LT05/C01/T1_32DAY_EVI')
s3 <-  s3$filterDate(ee$Date('2000-01-01'), ee$Date('2003-01-01'))$filterBounds(eco_mask_ee)


#Show the amount of images in the collection
nbrImages_s3 <- s3$size()$getInfo()
nbrImages_s3


#Mapping the clip function over the collection
s3 <- s3$map(function(image){image$clip(eco_mask_ee)})
s3_1 <- s3$map(function(image){image$clip(eco_mask_ee)})

#Selecting the EVI band which holds the information
s3 <-  s3$select("EVI")

#Get information about the collection
s3$getInfo()


#get more information on what's stored in the bands
# s3$bandNames()$size()$getInfo()
#-----------------------------------------------------------
# setup for the loop.  In case the loop crashes or doesn't work,
#you must rerun the above chunk first before running this chunk again.

nimages <- s3$size()$getInfo()
ic_date <- ee_get_date_ic(s3)

s2_img_list <- list()
latlng <- list()
lats <- list()
lngs <- list()
evi_values <- list()
s2_names <- list()

for(index in seq_len(nimages)) {
  py_index <- index - 1
  s2_img <- ee$Image(s3$toList(1, py_index)$get(0))
  s2_name <- s2_img$get('system:index')$getInfo()
  s2_img <- s2_img$select("EVI")$rename(s2_name)
  s2_img_list[[index]] <- ee$Image$pixelLonLat()$addBands(s2_img)
  s2_img_list[[index]] <-  s2_img_list[[index]]$reduceRegion(reducer = ee$Reducer$toList(),
                                                        geometry  = eco_mask_ee,
                                                        maxPixels = 1e6,
                                                        scale = 10,
                                                        bestEffort = TRUE)
  lats[[index]] <-  np$array((ee$Array(s2_img_list[[index]]$get("latitude"))$getInfo()))
  lngs[[index]]  <- np$array((ee$Array(s2_img_list[[index]]$get("longitude"))$getInfo()))
  evi_values[[index]] <-  np$array((ee$Array(s2_img_list[[index]]$get(s2_name))$getInfo()))

}

#Create test copy of list to name and evaluate
lats_test <- lats

names(lats_test) <- ic_date$id

#Will evaluate to false since the names have change

identical(lats_test[1], lats_test[2])


#Cerate back-up copy to test lat_long equality

lats_test_2 <- lats

identical(lats_test_2[1], lats_test_2[13])


#Cerate back-up copy to test lat_long equality

lngs_test <- lngs

identical(lngs_test[1], lngs_test[13])

names(lngs_test) <- ic_date$id

length(evi_values)

#Create copy of evi values and rename the columns of the elements
 evis <- evi_values
names(evis) <- ic_date$id

lats3 <- unlist(lats[1])
lng3 <- unlist(lngs[1])

#Convert list to matrix and create dataframe out of the lats, longs, and evi values

evis_df <- data.frame(x = lng3,y = lats3,lapply(evis, "length<-", max(lengths(evis))))

library(raster)
XYZ_2 <- rasterFromXYZ(evis_df, crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

raster::plot(XYZ_2, 8)

XYZ_SP<- rasterFromXYZ(evis_df, crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
raster::plot(XYZ_2, 8)

#-------------------------------------------------------------------
#Extrating phenology using Green Brown Package
#Demo code
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index
library(greenbrown)
data(ndvimap)
plot(ndvimap, 8)

# calculate phenology metrics (this can take some time!)
# Parallel computing??
#took a couple hours-- How can we reduce time!!

phenmap <- PhenologyRaster(XYZ_2, start=c(2000, 1), freq=12,
	tsgf="TSGFspline", approach="Deriv")
# Select method by defining 'tsgf' (temporal smoothing and gap filling) and
# by 'approach' (method to summarize phenology metrics).
# See \code{\link{Phenology}} for examples and a comparison of methods.
#----------------------------------------------------------------------------------

plot(phenmap)
par(mar = c(2, 2, 2, 2))
par(mar = c(1, 1, 1, 1))
plot(phenmap, grep("SOS.2001", names(phenmap))) # start of season 2000
plot(phenmap, grep("EOS.2000", names(phenmap))) # end of season 2000
plot(phenmap, grep("LOS.2000", names(phenmap))) # length of season 2000
plot(phenmap, grep("POP.2000", names(phenmap))) # position of peak value 2000
plot(phenmap, grep("POT.2000", names(phenmap))) # position of trough value 2000
plot(phenmap, grep("MGS.2000", names(phenmap))) # mean growing season value 2000
plot(phenmap, grep("PEAK.2000", names(phenmap))) # peak value 2000
plot(phenmap, grep("TROUGH.2000", names(phenmap))) # trough value 2000
plot(phenmap, grep("MSP.2000", names(phenmap))) # mean spring value 2000
plot(phenmap, grep("MAU.2000", names(phenmap))) # mean autumn value 2000
plot(phenmap, grep("RSP.2000", names(phenmap))) # rate of spring greenup 2000
plot(phenmap, grep("RAU.2000", names(phenmap))) # rate of autumn senescence 2000

# calculate trends on length of season using TrendRaster
losmap <- subset(phenmap, grep("LOS", names(phenmap)))
plot(losmap)
lostrend <- TrendRaster(losmap, start=c(2000, 1), freq=1)
plot(lostrend)

# classify trends in length of season
lostrend.cl <- TrendClassification(lostrend)
plot(lostrend.cl, col=brgr.colors(3), breaks=c(-1.5, -0.5, 0.5, 1.5))
# only a few pixels have a positive trend in the length of growing season





















































d2 <- data(ndvi)
phenmap2 <- PhenologyRaster(ndvimap, start=c(2000, 1), freq=12,
                           tsgf="TSGFspline", approach="Deriv")
# Select method by defining 'tsgf' (temporal smoothing and gap filling) and
# by 'approach' (method to summarize phenology metrics).
# See \code{\link{Phenology}} for examples and a comparison of methods.
#----------------------------------------------------------------------------------









