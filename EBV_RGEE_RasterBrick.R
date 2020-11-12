library(greenbrown)
library(sf)
library(sp)
library(tidyverse)

library(raster)
library(qdapRegex)
library(lubridate)

library(doParallel) 
library(foreach)
library(parallel)
library(gsubfn)


evis_df_s4 <- evis_df_s4

evis_df_s4 <- backup


dataframe_processing <- function(){
  
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


rasterBrick_out <- dataframe_processing()

# plot(rasterBrick_out)


