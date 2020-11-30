## MS supplement table on test and OOB error 11/29/2020
library(tidyverse)

model_id <- 15
perctestset <- 30

gendata_dir <- "generated_data"
modyears <- 2003:2009
fig_dir <- paste0("figures/flu_clusters_Apr28/id", model_id)
dir.create(fig_dir, showWarnings = FALSE)
fitfiles <- list.files(path = gendata_dir, pattern = paste0("rf_fit_id", model_id, "_test", perctestset))

error_tab <- data.frame()
for(fn in fitfiles){
  rf_fit <- readRDS(paste0(gendata_dir, "/", fn))
  modyear <- as.numeric(substring(fn, nchar(fn)-7, nchar(fn)-4))

  ## extract error rates
  ooberrors <- tbl_df(rf_fit$err.rate)
  testerrors <- tbl_df(rf_fit$test$err.rate)
  dummy <- data.frame(modyear = modyear,
                      type = c("OOB", "test"), 
                      error = c(ooberrors[nrow(ooberrors),]$OOB, testerrors[nrow(testerrors),]$Test))

  error_tab <- bind_rows(error_tab, dummy)
}


