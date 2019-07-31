library(tidyverse)
library(randomForest)
library(randomForestExplainer)

# setwd("grant")
source("utils.R")

modyears <- 2003:2009
diag <- TRUE

for(modyear in modyears){
  print(paste(modyear, "analyses *********************"))
  seed <- modyear+2390
  set.seed(seed)

  model_id <- 15
  perctestset <- 30

  gendata_dir <- "generated_data"
  fig_dir <- paste0("figures/flu_clusters_Apr28/id", model_id)
  dir.create(fig_dir, showWarnings = FALSE)

  in_df <- read_csv(paste0(gendata_dir, "/modeldata_id", model_id, "_", modyear, ".csv")) %>%
    dplyr::mutate_if(is.character, as.factor)
  mod_df <- in_df %>% dplyr::select(-fips, -year)
  outcomes <- unique(mod_df$cluster)
  if(length(outcomes)==2){
    testsetFxn <- testset_2clusters
  } else if(length(outcomes)==3){
    testsetFxn  <- testset_3clusters
  } else if(length(outcomes)==4){
    testsetFxn <- testset_4clusters
  }
  
  testix <- testsetFxn(mod_df$cluster, perctestset/100)
  rf_fit <- doRandomForest(mod_df, testix)
  saveRDS(rf_fit, file = paste0(gendata_dir, "/rf_fit_id", model_id, "_test", perctestset, "_seed", seed, "_", modyear, ".rds"))

  if(diag){
    ## diagnostics
    min_depth_frame <- min_depth_distribution(rf_fit)
    saveRDS(min_depth_frame, file = paste0(gendata_dir, "/mod_rf_id", model_id, "_test", perctestset, "_seed", seed, "_", modyear, "_mindepth.rds"))

    importance_frame <- measure_importance(rf_fit)
    saveRDS(importance_frame, file = paste0(gendata_dir, "/mod_rf_id", model_id, "_test", perctestset, "_seed", seed, "_", modyear, "_importance.rds"))

    plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", min_no_of_trees = 50) +
      theme(legend.position = "bottom")
    ggsave(paste0(fig_dir, "/mindepth_numnodes_id", model_id, "_test", perctestset, "_seed", seed, "_", modyear, ".pdf"), width = 6, height = 4)
    plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes") +
      theme(legend.position = "bottom")
    ggsave(paste0(fig_dir, "/import_numnodes_id", model_id, "_test", perctestset, "_seed", seed, "_", modyear, ".pdf"), width = 6, height = 4)
    plot_multi_way_importance(importance_frame, x_measure = "accuracy_decrease", y_measure = "gini_decrease", size_measure = "p_value", no_of_labels = 5) +
      theme(legend.position = "bottom")
    ggsave(paste0(fig_dir, "/import_accGini_id", model_id, "_test", perctestset, "_seed", seed, "_", modyear, ".pdf"), width = 6, height = 4)
    plot_multi_way_importance(importance_frame, x_measure = "accuracy_decrease", y_measure = "mean_min_depth", size_measure = "p_value", no_of_labels = 5)  +
      theme(legend.position = "bottom")
    ggsave(paste0(fig_dir, "/import_accMnmindepth_id", model_id, "_test", perctestset, "_seed", seed, "_", modyear, ".pdf"), width = 6, height = 4)
  }
  

}