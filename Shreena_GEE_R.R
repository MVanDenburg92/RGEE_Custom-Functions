#Goal: This script aims to connect to Google earth engine, creates EVI foe everymoth for 2 years, and build a raster stack.


#Import rgee library into R from github as well as other packages needed

remotes::install_github("r-spatial/rgee", force = TRUE)
install.packages('geojsonio')
install.packages('mapview')
install.packages("greenbrown", repos="http://R-Forge.R-project.org")

library(geojsonio)
library(rgee)
library(greenbrown)
library(sf)
library(mapview)

#initialize and authenticate GEE

ee_Initialize(email = 'shreenapyakurel@gmail.com')

#import Image collection

EVI_dat <- ee$ImageCollection('LANDSAT/LE07/C01/T1_32DAY_EVI')$select('EVI')

#Group images by composite date
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)






