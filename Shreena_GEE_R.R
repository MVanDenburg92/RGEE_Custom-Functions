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

#from miles branch load shapefile for ecoregion
library(sf)
eco_mask <- st_read('Ecoregion.shp',quiet = TRUE) %>% st_transform(3857)

#create region as geomerty feature
eco_mask2 <- eco_mask %>% sf_as_ee(.)
region <- eco_mask2$geometry()$bounds()


#group image by composite date
EVI_dat <- EVI_dat$map(function(img) {
  moy <- ee$Date(img$get('system:time_start'))$getRelative('month', 'year')
  img$set('moy', moy)
})
distinctMOY <- EVI_dat$filterDate('2000-01-01', '2001-01-01')


#Define a filter that identifies which images from the complete collection match the MOY from the distinct MOY collection.

filter <- ee$Filter$equals(leftField = 'moy', rightField = 'moy')

#combine all images into a feature collection
join <- ee$Join$saveAll('moy_matches')
joinEVI_dat <- ee$ImageCollection(join$apply(distinctMOY, EVI_dat, filter))

#for each image get the median value
median_EVI <- joinEVI_dat$map(function(img) {
  MOY_EVI = ee$ImageCollection$fromImages(
    img$get('MOY_matches')
  )
  MOY_EVI$reduce(ee$Reducer$median())
})

#define Visual params
visParams = list(
  min = 0.0,
  max = 9000.0,
  bands = "EVI_median",
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)

#creare RGB images for annimation frames
rgbVis <- median_EVI$map(function(img) {
  do.call(img$visualize, visParams) %>%
    ee$Image$clip(eco_mask2)
})

#define gif params
gifParams <- list(
  region = region,
  dimensions = 600,
  crs = 'EPSG:3857',
  framesPerSecond = 1
)

print(rgbVis$getVideoThumbURL(gifParams))
browseURL(rgbVis$getVideoThumbURL(gifParams))

#---------------------------------------------------------------------------------------------------------------


mask <- system.file("shp/arequipa.shp", package = "rgee") %>%
  st_read(quiet = TRUE) %>%
  sf_as_ee()
region <- mask$geometry()$bounds()


col <- ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')



col <- col$map(function(img) {
  doy <- ee$Date(img$get('system:time_start'))$getRelative('day', 'year')
  img$set('doy', doy)
})
distinctDOY <- col$filterDate('2013-01-01', '2014-01-01')


filter <- ee$Filter$equals(leftField = 'doy', rightField = 'doy')

join <- ee$Join$saveAll('doy_matches')
joinCol <- ee$ImageCollection(join$apply(distinctDOY, col, filter))

comp <- joinCol$map(function(img) {
  doyCol = ee$ImageCollection$fromImages(
    img$get('doy_matches')
  )
  doyCol$reduce(ee$Reducer$median())
})

visParams = list(
  min = 0.0,
  max = 9000.0,
  bands = "NDVI_median",
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)



rgbVis <- comp$map(function(img) {
  do.call(img$visualize, visParams) %>%
    ee$Image$clip(mask)
})

gifParams <- list(
  region = region,
  dimensions = 600,
  crs = 'EPSG:3857',
  framesPerSecond = 10
)

print(rgbVis$getVideoThumbURL(gifParams))
browseURL(rgbVis$getVideoThumbURL(gifParams))
