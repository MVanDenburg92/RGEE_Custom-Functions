library(rgee)
library(geospaar)
library(sp)
library(greenbrown)

#
ee_install()
ee_Initialize()



library(reticulate)
# earthengine_python <- Sys.getenv("EARTHENGINE_PYTHON", unset = NA)
# Sys.setenv(RETICULATE_PYTHON = earthengine_python)
# reticulate::py_config()
# library(rgee)
# ee_check()
ee_Initialize()
reticulate::py_config()
remotes::install_github("r-spatial/rgee", force = TRUE)
# 4/3wGFipl797YDrGzUazAjNTj7k_qKQ1LGbJV8Oyew9AOKZqbWZU7a9fA
ee_Initialize()
ee_Initialize(email = 'shreenapyakurel@gmail.com')
ee_clean_pyenv()
ee_install()
ee_check()
# ee_reattach() # reattach ee as a reserve word
ee_Initialize(email = "shreenapyakurel@gmail.com" , drive = TRUE)


#create polygon
coords_2 <- matrix(c(-71.8215424722187, 42.267434489848135,
                     -71.8215424722187, 42.26733574244465,
                     -71.8213366126768, 42.26733574244465,
                     -71.8213366126768, 42.267434489848135,
                     -71.8215424722187, 42.267434489848135),
                   ncol = 2, byrow = TRUE)
P1 <-   Polygon(coords_2)
Ps1 <- SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(Ps1, axes = TRUE)
ps2 <- st_as_sf(Ps1)
polygon <- ee$Geometry$Polygon(
  list(
    c(-71.8215424722187, 42.267434489848135),
    c(-71.8215424722187, 42.26733574244465),
    c(-71.8213366126768, 42.26733574244465),
    c(-71.8213366126768, 42.267434489848135),
    c(-71.8215424722187, 42.267434489848135)
  )
)


Map$centerObject(polygon());
Map$addLayer(polygon, {}, 'polygon')

#create start date and end date
startDate <-  ee$Date('2000-01-01');
endDate <-  ee$Date('2003-01-01');


#Load EVI image
EVI <- ee$ImageCollection('LANDSAT/LT05/C01/T1_32DAY_EVI')$select("EVI")%>%
  ee$ImageCollection$filterDate(startDate, endDate) %>%
  ee$ImageCollection$filterBounds(polygon)


#print how many image in image collection
print(EVI$size()$getInfo())


tmp <- tempdir()

## Using drive
drive_files <- ee_imagecollection_to_local(
  ic = EVI,
  region = polygon,
  scale = 30,
  dsn = file.path(tmp, "drive_")
)


#Convert image collection to raster and export all image
ee_imagecollection_to_local(
  EVI,
  polygon,
  dsn = NULL,
  via = "drive",
  scale = NULL,
  maxPixels = 1e+09,
  container = "rgee_backup",
  quiet = FALSE
)

#import all image as raster stack
fs <- dir(tempdir(), pattern = "r.*.tif", full.names = TRUE)
s <- fs %>% lapply(raster) %>% stack


#Green brown (metrics)





