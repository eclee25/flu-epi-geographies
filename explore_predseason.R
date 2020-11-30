## MS supplement table on one-season-ahead prediction error 11/29/2020
library(tidyverse)

model_id <- 15
perctestset <- 30

gendata_dir <- "generated_data"
modyears <- 2003:2008
predyears <- 2004:2009
fig_dir <- paste0("figures/flu_clusters_Apr28/id", model_id)
dir.create(fig_dir, showWarnings = FALSE)
cmfiles <- list.files(path = gendata_dir, pattern = paste0("pred_clustermatch_id", model_id, "_test", perctestset))
cfiles <- list.files(path = gendata_dir, pattern = paste0("pred_clusters_id", model_id, "_test", perctestset))

# node_pairs samePredGroup sameTrueGroup pred1 pred2 actual1 actual2

correct_pred_clustermatch <- function(ix){
  modyear <- yrcombos[ix,]$modyear
  predyear <- yrcombos[ix,]$predyear
  code <- paste0("m", modyear, "_p", predyear)
  fn <- cmfiles[which(grepl(code, cmfiles))]
  fn2 <- cfiles[which(grepl(code, cfiles))]
  df <- read_csv(paste0(gendata_dir, "/", fn))
  df2 <- read_csv(paste0(gendata_dir, "/", fn2))

  nmi <- NMI::NMI(dplyr::select(df2, fips, pred), dplyr::select(df2, fips, actual))

  rc <- dplyr::filter(df, sameTrueGroup == 1) %>%
  dplyr::summarise(matches = sum(samePredGroup), total = n()) %>%
  dplyr::mutate(percMatch = matches/total*100, myear = modyear, pyear = predyear, nmi = nmi$value) %>%
  dplyr::select(myear, pyear, percMatch, nmi, matches, total)

  return(rc)
}

yrcombos <- tidyr::crossing(modyear = modyears, predyear = predyears) %>%
  dplyr::filter(predyear > modyear)

df <- map_dfr(1:nrow(yrcombos), ~correct_pred_clustermatch(.x))
wide_df <- tidyr::pivot_wider(df %>% dplyr::mutate(percError = 100-percMatch) %>% dplyr::select(myear, pyear, percError), names_from = pyear, values_from = percError)
wide_df2 <- tidyr::pivot_wider(df %>% dplyr::select(myear, pyear, nmi), names_from = pyear, values_from = nmi)

# test2 <- correct_pred_clustermatch(testcm, 2003, 2005)