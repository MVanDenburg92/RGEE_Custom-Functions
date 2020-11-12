
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


shape = 'Ecoregion_Small.shp'
band_name = 'EVI'
img_collection = "LANDSAT/LT05/C01/T1_32DAY_EVI"
start = "2000-01-01"
end = "2008-01-01"

gee_extract <- function(shape = 'Ecoregion_Small.shp', band_name = 'EVI', img_collection = "LANDSAT/LT05/C01/T1_32DAY_EVI", start = "2000-01-01", end = "2008-01-01") {
  
  eco_mask2 <- st_read(shape,quiet = TRUE) %>% st_transform(4326) 
  
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
backup <- evis_df_s4

dataframe_processing <- function(){
  
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

pos <- grep(pattern = "NA", evis_df_s4)
while(length(pos)>0){
  for(t in pos){
    if(t < ncol(evis_df_s4)){
      evis_df_s4[t] <- (evis_df_s4[t+1]-evis_df_s4[t-1])/2
    }else
      evis_df_s4[t] <- evis_df_s4[ncol(evis_df_s4)-1]
  }
  pos <- grep(pattern = "NA", x = evis_df_s4)
  
}


coords3 <- evis_df_s4 %>% dplyr::select(x, y) 

evis_df_s4 <- evis_df_s4 %>% select(-c(x, y))

evis_df_s4[evis_df_s4 < 0] = 0

evis_df_s4_2 <- data.frame(coords3,evis_df_s4)

#add reformatted dates

colnames_df <-  names(evis_df_s4_2)

#Extract dates from colummns
dates <- gsubfn:::strapplyc(colnames_df, "[0-9]{8,}", simplify = TRUE)
dates <- lubridate:::ymd(dates[-c(1:2)])

names(evis_df_s4_2)[3:length(evis_df_s4_2)] <- as.character(dates)



evis_df_s4_2 <- evis_df_s4_2 %>%
  mutate_all(as.numeric)


#Extract years from dates 
y2 <- data.frame(Date=(seq(dates[1], dates[length(dates)], by = 'year')))
y2$Year <-year(y2$Date)
years <- y2$Year


all_pixels_2 <- evis_df_s4_2 %>% dplyr:::select(-c(x, y)) %>% arrange() %>% slice(1:nrow(evis_df_s4_2)) %>% t() 


# #create time series 
EVIseries2 <- ts(all_pixels_2, start=c(years[1], 1), end=c(years[length(years)], 12), frequency=12)


#Create empty lists for lapply
Yt_interpolate_m3 <- list()
Yt_interpolate_m4 <- list()
Phen_2000_2007_3 <- list()


#Apply Tspp # time series pre-processing ---interpolating across whole data set.  Use lapply to retain time series information. 
ww <- ncol(EVIseries2)

# system.time(
# Yt_interpolate_m4 <- lapply(seq(ww), function(x){
#   Yt_interpolate_m3[[x]]  <- TsPP(EVIseries2[,x], tsgf=TSGFspline)
# }))

# user  system elapsed 
# 7.65    0.04    8.01 


model.mse <- function(x){
  Yt_interpolate_m3[[x]]  <- greenbrown::TsPP(EVIseries2[,x], tsgf=TSGFspline)
}




model_phen <- function(x){
  Phen_2000_2007_3[[x]] <- greenbrown:::Phenology(Yt_interpolate_m4[[x]], approach="White")
}

UseCores <- detectCores() - 2


clust <- makeCluster(UseCores)
parallel:::clusterEvalQ(cl = clust, library("greenbrown"))
parallel:::clusterExport(cl = clust, c("EVIseries2","Yt_interpolate_m3"), envir=environment())
Yt_interpolate_m4 <- parLapply(clust, seq(ww), model.mse)

# user  system elapsed 
# 0.03    0.01    5.69 



  # clust <- makeCluster(UseCores)
clusterEvalQ(clust, library("greenbrown"))
clusterExport(clust, c('Phen_2000_2007_3', 'Yt_interpolate_m4'),envir=environment())
Phen_2000_2007_3 <- parLapply(clust, seq(ww), model_phen)

#4 cores
# #   user  system elapsed 
# 0.24    0.07   39.80 

#6 cores

# user  system elapsed 
# 0.22    0.03   31.72 


stopCluster(clust)
getDoParWorkers()
registerDoSEQ()
stopImplicitCluster()



#Create empty list containers for loop
sos2 <- list()
eos2 <- list()
los2 <- list()
pop2 <- list()


for(i in seq(ncol(EVIseries2))){
  sos2[[i]] <-  as.numeric( Phen_2000_2007_3[[i]][["sos"]])
  eos2[[i]] <-  as.numeric( Phen_2000_2007_3[[i]][["eos"]])
  los2[[i]] <-  as.numeric( Phen_2000_2007_3[[i]][["los"]])
  pop2[[i]] <-  as.numeric( Phen_2000_2007_3[[i]][["pop"]])
  
}

plot(Yt_interpolate_m4[[1]])

sos2 <- unlist(sos2)
eos2 <- unlist(eos2)
los2 <- unlist(los2)
pop2 <- unlist(pop2)


coords3 <-evis_df_s4_2 %>% dplyr:::select(x, y) 
coords4 <- coords3 %>% slice(rep(1:n(), each = 8))



DF_phenmet_test2 <- data.frame(coords4,sos2, eos2, los2, pop2) %>% cbind(years)

DF_phenmet_testarranged2 <- DF_phenmet_test2 %>% arrange(years)


DF_phenmet_testarranged_sf2 = st_as_sf(DF_phenmet_testarranged2, coords = c("x", "y"), crs = 4326)


year_chars2 <- as.character(years)
#year_chars[1]

separated_rasters2 <- list()
# x <- length(years)

for(g in seq(years)){
  separated_rasters2[[g]] <- lapply(years[g], function(x){DF_phenmet_testarranged2 %>% filter(years == year_chars2[g])})
}

DF_phenmet_testarranged2 %>% filter(years == year_chars2[1])


rasterList2 <- brick()

rasterList2 <- lapply(1:length(separated_rasters2), function(x){rasterFromXYZ(separated_rasters2[[x]][[1]])})

rasterBrick2 <- brick(rasterList2)
  
}

rasterBrick_out <- dataframe_processing()

plot(rasterBrick_out)
