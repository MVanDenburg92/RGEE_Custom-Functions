
library(greenbrown)
library(sf)
library(sp)
library(tidyverse)

library(raster)
library(qdapRegex)
library(lubridate)

library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)
library(parallel)
library(gsubfn)



# devtools::install_github("rstudio/reticulate")

#Install if you don't have already
# remotes::install_github("r-spatial/rgee")


library(rgee)


# ee_install()
ee_Initialize(email = 'miles.vandenburg@gmail.com')

library(reticulate)

# ee_check()

# install.packages(c('processx','ps'))
# packageVersion('rgee')
# rgee::ee_check()
# reticulate::py_config()

# ee_clean_pyenv()
# ee_clean_credentials()
# ee_Initialize()


# ee = import("ee")          # Import the Earth Engine library
# ee$Initialize() 



#Bring on the RGEE Objects and Vectors

#Smaller ecoregion


x = 'Ecoregion_Small.shp'
band_name = 'EVI'
img_collection = "LANDSAT/LT05/C01/T1_32DAY_EVI"
start = "2000-01-01"
end = "2008-01-01"

gee_extract <- function(x = 'Ecoregion_Small.shp', band_name = 'EVI', img_collection = "LANDSAT/LT05/C01/T1_32DAY_EVI", start = "2000-01-01", end = "2008-01-01") {
  
  eco_mask2 <- st_read(x,quiet = TRUE) %>% st_transform(4326) 
  
  # plot(eco_mask2)
  # 
  eco_mask_ee2 <- sf_as_ee(eco_mask2)
  
  
  ##Bring in the imagery from RGEE
  # 
  s4 <- ee$ImageCollection(img_collection)
  # start <- '2000-01-01'
  # end <- '2008-01-01'
  # 
  s4 <-  s4$filterDate(ee$Date(start),ee$Date(end))$filterBounds(eco_mask_ee2)
  
  # s4info1 <- s4$   getInfo()
  
  # band_name <- s4info1[["bands"]][[1]][["id"]]
  
  
  #Show the amount of images in the collection
  nbrImages_s4 = s4$size() %>% ee$Number()
  
  
  #Mapping the clip function over the collection
  s4 = s4$map(function(image){image$clip(eco_mask_ee2)})
  
  # s4info2 <- s4$getInfo()
  
  #Selecting the EVI band which holds the information
  s4 = s4$select(band_name)
  
  #Get information about the collection 
  # s4info3 <- s4$getInfo()
  
  
  
  
  ## SMALLER REGIONS
  
  #Get setup for the loop.  In case the loop crashes or doesn't work, you must rerun the above chunk first before running this chunk again. 
  
  np = import("numpy")      # Import Numpy 
  
  
  # nimages2 <- s4$size()$getInfo()  
  
  nimages2 <- s4 %>% ee$FeatureCollection$size() %>% ee$Number()
  ic_date2 <- ee_get_date_ic(s4)
  
  
  s4_img_list <- list()
  latlng2 <- list()
  lats2 <- list()
  lngs2 <- list()
  evi_values4 <- list()
  s4_names <- list()
  
  evi_values5 <- list()
  evi_values6 <- list()
  
  # nimages2$getInfo()
  
  
  for(i in seq(nimages2$getInfo())){
    # as.numeric(i)
    py_index <- i - 1
    s4_img <- ee$Image(s4 %>% ee$FeatureCollection$toList(1, py_index) %>% ee$List$get(0))
    s4_name <- s4_img %>% ee_get_date_img()
    s4_names[[i]] <- as.character(s4_name$time_start)
    s4_img <- s4_img$select(band_name)$rename(as.character(s4_name$time_start))
    s4_img_list[[i]] <- ee$Image$pixelLonLat()$addBands(s4_img)
    s4_img_list[[i]] <-  s4_img_list[[i]]$reduceRegion(reducer = ee$Reducer$toList(),
                                                       geometry  = eco_mask_ee2,
                                                       maxPixels = 1e6,
                                                       scale = 30,
                                                       bestEffort = TRUE)
  }
  
  func_dos <- function(q) {
    lats2 <- np$array((ee$Array(s4_img_list[[q]]$get("latitude")) %>% ee$Array$getInfo()))
    lngs2 <- np$array((ee$Array(s4_img_list[[q]]$get("longitude")) %>% ee$Array$getInfo()))
    
    data.frame(x = lngs2,y = lats2)
    evi_values4 <- list()
    
    for(index in  seq(nimages2$getInfo())) {
      evi_values4[[index]] <-  ee$List(s4_img_list[[index]]$get(s4_names[[index]]))$getInfo()
      
    }
    
    #Convert list elements of evi_values into num py arrays
    evi_values5 <- lapply(evi_values4,function(x){
      np$array((x))
    })
    
    eviss4 <- evi_values5
    names(eviss4) <- ic_date2$id
    evis_df_s4 <- data.frame(x = lngs2,y = lats2,lapply(eviss4, "length<-", max(lengths(eviss4))))
  }
  s <- func_dos(1)
  
}


###################################
evis_df_s4 <- gee_extract()
##############################
