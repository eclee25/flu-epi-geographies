## helper functions to clean data for flu clustering model
library(tidyverse)

#### clean response data ####
import_resp_cluster <- function(resp_dir, year, agegroup){ 
  ## import flu cluster
  fn <- list.files(resp_dir, pattern = paste("louvain_robust_reduced_colorcorrected", agegroup, year-1, sep = "_"), include.dirs = FALSE) ## should only be one agegroup-year file chosen

  if(length(fn)==1){
    print(paste("read", fn))
    cluster <- read_csv(paste(resp_dir, fn, sep = "/"), na = c("nan", "na", "", "NA", "NAN", " "))  %>%
      dplyr::rename(cluster = modularity_class, fips = node) %>%
      dplyr::mutate(fips = ifelse(nchar(fips)==4, paste0("0", fips), fips)) %>% 
      dplyr::mutate(year = year)
  } else{
    print("Error: greater than one filename was identified.")
    cluster <- data.frame()
  }
  
  return(cluster)
}

#### clean covariate data ####
import_cov_hsa <- function(cov_dir){
  ## import health service area crosswalk for fips counties
  hsa <- read_csv(paste0(cov_dir, "/health_service_areas.csv"), skip = 1, col_types = "_cc", col_names = c("X_hsa", "fips")) %>%
    dplyr::mutate(fips = ifelse(nchar(fips)==4, paste0("0", fips), fips))
  return(hsa)
}

import_cov_hhs <- function(cov_dir){
  ## import hhs region crosswalk for fips counties
  hhs <- read_csv(paste0(cov_dir, "/HHS_regions.csv"), skip = 1, col_types = "cc", col_names = c("fips", "hhs")) %>%
    dplyr::mutate(fips = ifelse(nchar(fips)==4, paste0("0", fips), fips))
  return(hhs)
}

import_cov_koeppen <- function(cov_dir){
  ## import koeppen classification for fips counties
  koep <- read_csv(paste0(cov_dir, "/koeppen_county.csv"), skip = 1, col_types = "_cc", col_names = c("fips", "X_koep")) %>%
    dplyr::mutate(fips = ifelse(nchar(fips)==4, paste0("0", fips), fips)) 
  koep$X_koep <- dplyr::recode(koep$X_koep, 'Cfa'=0, 'Cwa'=0, 'Csa'=1, 'Csb'=1, 'Cfb'=2, 'Cfc'=2, 'Dfa'= 3, 'Dfb'=3, 'Dwa'=3,
                'Dwb'=3, 'Dsb'=3, 'Dfc'=4, 'Dfd'=4, 'Dwc'=4, 'Dwd'=4, 'Dsc'= 4, 'BWh'=5, 'BWk'=5, 
                'BSh'=5, 'BSk'=5, 'Am'=6,'Aw'=6, 'Af'=6, 'As'=6, 'ET'=7)
  koep <- koep %>% distinct(fips, X_koep)

  return(koep)
}

import_cov_urban_2003 <- function(cov_dir){
  ## import rural urban codes for 2003
  urban03 <- read_csv(paste0(cov_dir, "/rural_urban_codes_2003.csv"), skip = 1, col_types = "_cc", col_names = c("fips", "X_urban")) %>%
    dplyr::mutate(fips = ifelse(nchar(fips)==4, paste0("0", fips), fips)) %>%
    dplyr::mutate(year = 2003)
  ## use 2003 codes for 2002-2004
  urban <- bind_rows(urban03 %>% dplyr::mutate(year = 2002), 
                      urban03,
                      urban03 %>% dplyr::mutate(year = 2004))
  return(urban)
}

import_cov_urban_2006 <- function(cov_dir){
  ## import rural urban codes for 2003
  urban06 <- read_csv(paste0(cov_dir, "/rural_urban_codes_cdc_2006.csv"), skip = 1, col_types = "_cc", col_names = c("fips", "X_urban")) %>%
    dplyr::mutate(fips = ifelse(nchar(fips)==4, paste0("0", fips), fips)) %>%
    dplyr::mutate(year = 2006)
  ## use 2003 codes for 2002-2004
  urban <- bind_rows(urban06 %>% dplyr::mutate(year = 2005), 
                      urban06,
                      urban06 %>% dplyr::mutate(year = 2007),
                      urban06 %>% dplyr::mutate(year = 2008),
                      urban06 %>% dplyr::mutate(year = 2009))
  return(urban)
}

import_cov_commuting <- function(cov_dir){
  ## import commuting community partition
  commuting <- read_csv(paste0(cov_dir, "/commuting_partition.csv"), skip = 1, col_types = "cc", col_names = c("fips", "X_commut")) %>%
    dplyr::mutate(fips = ifelse(nchar(fips)==4, paste0("0", fips), fips)) 
  return(commuting)
}

import_cov_airtraffic <- function(cov_dir){
  ## import air traffic community partition 
  airtraffic <- read_csv(paste0(cov_dir, "/air_traffic_partition.csv"), skip = 1, col_types = "_cci", col_names = c("fips", "X_airtraf", "year")) %>%
    dplyr::mutate(fips = ifelse(nchar(fips)==4, paste0("0", fips), fips)) %>%
    dplyr::mutate(year = year+1)
  return(airtraffic)
}

## change to distance from source location
import_cov_srcLocDist <- function(cov_dir){
  ## see scales/R_export/origin_locations/fluseason_source_locations_Lee.csv
  cov <- read_csv(paste0(cov_dir, "/scales_cov_df.csv")) %>% dplyr::rename(lat = X_latitude)
  cov2 <- cov %>%
    distinct(fips, lat, lon) 
  srcFips <- data.frame(year = 2003:2009, fips = c("48007", "48505", "23023", "04023", "01003", "48323", "12055")) %>%
    left_join(cov2, by = c("fips")) %>%
    dplyr::rename(srcLat = lat, srcLon = lon)
  srcLocDist <- left_join(cov %>% dplyr::select(fips, year, lat, lon), srcFips %>% dplyr::select(-fips), by = c("year")) %>%
    rowwise %>%
    dplyr::mutate(X_srcLocDist = distance_function(srcLat, srcLon, lat, lon)) %>% 
    ungroup %>%
    dplyr::select(fips, year, X_srcLocDist)
  return(srcLocDist)
}

distance_function <- function(srcPt1, srcPt2, pt1, pt2){
  return(sqrt(((srcPt1-pt1)^2) + ((srcPt2-pt2)^2)))
}

testset_2clusters <- function(Y, testProp){
  Y0 <- sample(which(Y=="flu0"), size = ceiling(testProp*length(which(Y=="flu0"))))
  Y1 <- sample(which(Y=="flu1"), size = ceiling(testProp*length(which(Y=="flu1"))))
  testindexes <- c(Y0, Y1)
  return(testindexes)
}
testset_3clusters <- function(Y, testProp){
  Y0 <- sample(which(Y=="flu0"), size = ceiling(testProp*length(which(Y=="flu0"))))
  Y1 <- sample(which(Y=="flu1"), size = ceiling(testProp*length(which(Y=="flu1"))))
  Y2 <- sample(which(Y=="flu2"), size = ceiling(testProp*length(which(Y=="flu2"))))
  testindexes <- c(Y0, Y1, Y2)
  return(testindexes)
}
testset_4clusters <- function(Y, testProp){
  Y0 <- sample(which(Y=="flu0"), size = ceiling(testProp*length(which(Y=="flu0"))))
  Y1 <- sample(which(Y=="flu1"), size = ceiling(testProp*length(which(Y=="flu1"))))
  Y2 <- sample(which(Y=="flu2"), size = ceiling(testProp*length(which(Y=="flu2"))))
  Y3 <- sample(which(Y=="flu3"), size = ceiling(testProp*length(which(Y=="flu3"))))
  testindexes <- c(Y0, Y1, Y2, Y3)
  return(testindexes)
}

doRandomForest <- function(train, testindexes){
  # Train & test a model; return predicted values on test samples
  ycol <- which(names(train)=="cluster")
  params <- list(x = train[-testindexes,-ycol],
                 y = as.factor(train[-testindexes, ycol] %>% unlist %>% unname),
                 xtest = train[testindexes, -ycol],
                 ytest = as.factor(train[testindexes, ycol] %>% unlist %>% unname), 
                 proximity = TRUE,
                 keep.forest = TRUE,
                 localImp = TRUE)
  fit <- do.call(randomForest, params)
  return(fit)
}