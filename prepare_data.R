library(tidyverse)
library(randomForest)

# setwd("grant")
source("utils.R")
modyears <- 2003:2009

for(modyear in modyears){
  print(paste(modyear, "analyses *********************"))

  sampling <- "base" ## base, up, down
  agegroup <- "total" ## total, adult, child
  cov_dir <- "data"
  resp_dir <- "data/flu_clusters_Apr28/Partitions"
  # fig_dir <- "figures/flu_clusters_Apr28"
  gendata_dir <- "generated_data"
  
  #### cleap=n response data ####
  resp <- import_resp_cluster(resp_dir, modyear, agegroup)

  #### clean covariate data ####
  hsa <- import_cov_hsa(cov_dir)
  hhs <- import_cov_hhs(cov_dir) ## should be imported in cov2 also
  koep <- import_cov_koeppen(cov_dir) %>% ## koeppen workaround 12/7/18
    group_by(fips) %>% summarise(X_koep = first(X_koep)) %>% ungroup
  urban03 <- import_cov_urban_2003(cov_dir)
  urban06 <- import_cov_urban_2006(cov_dir)
  commut <- import_cov_commuting(cov_dir)
  airtraf <- import_cov_airtraffic(cov_dir)
  srcloc <- import_cov_srcLocDist(cov_dir)
  cov2 <- read_csv(paste0(cov_dir, "/scales_cov_df.csv"))

  #### prepare model data ####
  if(modyear %in% 2002:2004){
    urban <- urban03
  } else{
    urban <- urban06
  }

  full_df <- left_join(resp, hsa, by = c("fips")) %>%
    left_join(koep, by = c("fips")) %>%
    left_join(urban, by = c("fips", "year")) %>%
    left_join(commut, by = c("fips")) %>%
    left_join(airtraf, by = c("fips", "year")) %>%
    left_join(srcloc, by = c("year", "fips")) %>%
    left_join(cov2, by = c("fips", "year")) %>%
    dplyr::filter(year == modyear) %>%
    dplyr::filter(!is.na(st)) %>%
    dplyr::filter(!is.na(cluster)) %>%
    dplyr::mutate(fips_st = as.factor(paste0("s",fips_st)), 
                  regionID = as.factor(paste0("r",regionID)),
                  X_urban = as.factor(paste0("u", X_urban)), 
                  X_koep = as.factor(paste0("k",X_koep)),
                  cluster = as.factor(paste0("flu",cluster)),
                  X_hsa = as.factor(paste0("h",X_hsa)),
                  X_commut = as.factor(paste0("cm",X_commut)),
                  X_airtraf = as.factor(paste0("at",X_airtraf)))

  allmod_df <- full_df %>%  
    dplyr::select(-X_pollution, -X_humidity) %>%
    dplyr::select(cluster, starts_with("X_"), starts_with("O_"), fips, fips_st, regionID) %>%
    dplyr::select(-X_hsa)  ## has too many categories

  ## continuous vars only - model_id 15
  model_id <- 15
  dat15 <- allmod_df %>% 
    dplyr::select(cluster, X_srcLocDist, X_latitude, X_poverty, X_child, X_adult, X_hospaccess, X_housdensity, X_vaxcovI, X_vaxcovE, X_H3A, X_B, X_priorImmunity, X_anomHumidity, X_singlePersonHH, X_logpopdensity, O_imscoverage, O_insured, O_careseek) 
  write_csv(dat15, paste0(gendata_dir, "/modeldata_id", model_id, "_", modyear, ".csv"))
  
  ## continuous and categorical vars - model_id 16
  model_id <- 16 
  dat16 <- allmod_df %>% 
    dplyr::select(cluster, fips_st, regionID, X_koep, X_urban, X_commut, X_airtraf, X_srcLocDist, X_latitude, X_poverty, X_child, X_adult, X_hospaccess, X_housdensity, X_vaxcovI, X_vaxcovE, X_H3A, X_B, X_priorImmunity, X_anomHumidity, X_singlePersonHH, X_logpopdensity, O_imscoverage, O_insured, O_careseek)
  write_csv(dat16, paste0(gendata_dir, "/modeldata_id", model_id, "_", modyear, ".csv"))
}