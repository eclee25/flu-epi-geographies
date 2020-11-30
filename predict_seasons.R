#### see how well a model from one year can predict cluster similarity in a different year
library(tidyverse)
library(randomForest)

source("utils.R")
model_id <- 15
perctestset <- 15

cov_dir <- "data"
resp_dir <- "data/flu_clusters_Apr28/Partitions"
gendata_dir <- "generated_data"
modyears <- 2003:2008

fig_dir <- paste0("figures/flu_clusters_Apr28/id", model_id)
dir.create(fig_dir, showWarnings = FALSE)

for(modyear in modyears){
  predyears <- (modyear+1):2009
  mseed <- modyear+2390

  rf_fit <- readRDS(paste0(gendata_dir, "/rf_fit_id", model_id, "_test", perctestset, "_seed", mseed, "_", modyear, ".rds"))

  for(predyear in c(predyears)){
    
    in_df <- read_csv(paste0(gendata_dir, "/modeldata_id", model_id, "_", predyear, ".csv")) %>%
      dplyr::mutate_if(is.character, as.factor)
    newdata_df <- in_df %>% dplyr::select(-fips, -year)  

    if(model_id == 16){ ## model 16 doesn't work because the factor levels are different across years
      newdata_df <- newdata_df %>%
        dplyr::mutate(fips_st = factor(fips_st, levels = levels(olddata_df$fips_st)),
                      regionID = factor(regionID, levels = levels(olddata_df$regionID)),
                      X_koep = factor(X_koep, levels = levels(olddata_df$X_koep)),
                      X_urban = factor(X_urban, levels = levels(olddata_df$X_urban)),
                      X_commut = factor(X_commut, levels = levels(olddata_df$X_commut)),
                      X_airtraf = factor(X_airtraf, levels = levels(olddata_df$X_airtraf)))
    }

    rf_pred <- tbl_df(as.data.frame(predict(rf_fit, newdata = newdata_df[,-which(names(newdata_df)=="cluster")], type = "prob"))) %>%
      dplyr::mutate(pred = predict(rf_fit, 
                            newdata = newdata_df[,-which(names(newdata_df)=="cluster")], 
                            type = "response"), 
                    fips = in_df$fips,
                    actual = in_df$cluster) %>%
      dplyr::select(fips, pred, actual)
    write_csv(rf_pred, paste0(gendata_dir, "/pred_clusters_id", model_id, "_test", perctestset, "_seed", mseed, "_m", modyear, "_p", predyear, ".csv"))
    print(summary(rf_pred))

    ## mark node pairs in the same group
    pred_groupings <- crossing(node1 = rf_pred$fips, node2 = rf_pred$fips) %>% ## careful -- node-node pairs are duplicated (node1-node2 & node2-node1)
      dplyr::left_join(rf_pred %>% dplyr::select(fips, pred) %>% dplyr::rename(node1 = fips, pred1 = pred)) %>%
      dplyr::left_join(rf_pred %>% dplyr::select(fips, pred) %>% dplyr::rename(node2 = fips, pred2 = pred)) %>%
      dplyr::mutate(samePredGroup = ifelse(pred1==pred2, 1, 0)) %>%
      dplyr::filter(as.numeric(node1) < as.numeric(node2)) %>% ## rm duplicated node pairs and self-edges
      dplyr::mutate(node_pairs = paste(node1, node2, sep = "_")) %>%
      dplyr::select(-node1, -node2)

    ## mark node pairs in different groups
    true_groupings <- crossing(node1 = rf_pred$fips, node2 = rf_pred$fips) %>% ## careful -- node-node pairs are duplicated (node1-node2 & node2-node1)
      dplyr::left_join(rf_pred %>% dplyr::select(fips, actual) %>% dplyr::rename(node1 = fips, actual1 = actual)) %>%
      dplyr::left_join(rf_pred %>% dplyr::select(fips, actual) %>% dplyr::rename(node2 = fips, actual2 = actual)) %>%
      dplyr::mutate(sameTrueGroup = ifelse(actual1==actual2, 1, 0)) %>%
      dplyr::filter(as.numeric(node1) < as.numeric(node2)) %>% ## rm duplicated node pairs and self-edges
      dplyr::mutate(node_pairs = paste(node1, node2, sep = "_")) %>%
      dplyr::select(-node1, -node2)

    ## do node pairs belong to the same true clusters and the same predicted clusters?
    compare_groupings <- full_join(pred_groupings, true_groupings, by = c("node_pairs")) %>%
      dplyr::select(node_pairs, samePredGroup, sameTrueGroup, pred1, pred2, actual1, actual2)
    
    write_csv(compare_groupings, paste0(gendata_dir, "/pred_clustermatch_id", model_id, "_test", perctestset, "_seed", mseed, "_m", modyear, "_p", predyear, ".csv"))
    # nmi <- NMI(compare_groupings %>% dplyr::select(node_pairs, sameTrueGroup), compare_groupings %>% dplyr::select(node_pairs, samePredGroup))

    rm(newdata_df, rf_pred, pred_groupings, true_groupings, compare_groupings)
    gc()

  }

}
