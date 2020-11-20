library(tidyverse)

model_id <- 15
perctestset <- 15

gendata_dir <- "generated_data"
modyears <- 2003:2009

fig_dir <- paste0("figures/flu_clusters_Apr28/id", model_id)
dir.create(fig_dir, showWarnings = FALSE)

md_ls <- list()
imp_ls <- list()


# fit <- readRDS(paste0(gendata_dir, "/rf_fit_id", model_id, "_test", perctestset, "_seed", mseed, "_", modyear, ".rds"))

for (i in 1:length(modyears)){

  modyear <- modyears[i]
  seed <- modyear+2390

  md_ls[[i]] <- readRDS(paste0(gendata_dir, "/mod_rf_id", model_id, "_test", perctestset, "_seed", seed, "_", modyear, "_mindepth.rds")) %>%
    dplyr::mutate(modyear = modyear)

  imp_ls[[i]] <- readRDS(paste0(gendata_dir, "/mod_rf_id", model_id, "_test", perctestset, "_seed", seed, "_", modyear, "_importance.rds")) %>%
    dplyr::mutate(modyear = modyear)
}

mindepth <- data.table::rbindlist(md_ls)
imp <- data.table::rbindlist(imp_ls)
imp_meanMD <- imp %>% group_by(variable) %>% summarise(mn_mean_min_depth = mean(mean_min_depth, na.rm = TRUE))

## initial exploratory analyses
mean_mindepth_summ <- ggplot(imp, aes(x = as.factor(modyear), y = mean_min_depth, group = variable)) +
  geom_col(position = "dodge") +
  geom_hline(data = imp_meanMD, aes(yintercept = mn_mean_min_depth), color = "red") +
  coord_flip() +
  scale_y_continuous("Mean Minimum Depth") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  facet_wrap(~variable)
ggsave(paste0(fig_dir, "/explore_rf_id", model_id, "_test", perctestset, "_mindepth1.png"), mean_mindepth_summ, width = 6, height = 6)

decrease_summ <- ggplot(imp, aes(x = accuracy_decrease, y = gini_decrease, group = variable)) +
  geom_point(aes(colour = variable)) +
  theme_bw() 
ggsave(paste0(fig_dir, "/explore_rf_id", model_id, "_test", perctestset, "_decrease1.png"), decrease_summ, width = 6, height = 6)

## summarise metrics across seasons
special_summ <- function(x){
  tibble(mean = mean(x, na.rm = TRUE), min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
}

recode_variable <- function(df){
  dplyr::mutate(df, plt_var = recode(variable,
                O_careseek = "careseek",
                O_imscoverage = "coverage",
                O_insured = "insured",
                X_adult = "adult",
                X_anomHumidity = "anomHumidity",
                X_B = "B",
                X_child = "child",
                X_H3A = "H3A",
                X_hospaccess = "hospaccess",
                X_housdensity = "housdensity",
                X_latitude = "latitude",
                X_logpopdensity = "logpopdensity",
                X_poverty = "poverty",
                X_priorImmunity = "priorImmunity",
                X_singlePersonHH = "singlePersonHH",
                X_srcLocDist = "srcLocDist",
                X_vaxcovE = "vaxcovE",
                X_vaxcovI = "vaxcovI"))
}

mean_min_depth <- imp %>%
  group_by(variable) %>%
  summarise(special_summ(mean_min_depth)) %>%
  dplyr::arrange(desc(mean)) %>%
  recode_variable %>%
  dplyr::mutate(plt_var = factor(plt_var, levels = plt_var))

accuracy_decrease <- imp %>%
  group_by(variable) %>%
  summarise(special_summ(accuracy_decrease)) %>%
  recode_variable %>%
  dplyr::mutate(plt_var = factor(plt_var, levels = mean_min_depth$plt_var))

ggplot(mean_min_depth, aes(x = mean, y = plt_var)) +
  geom_point() +
  geom_linerange(aes(xmin = min, xmax = max)) +
  theme_bw() +
  scale_x_continuous("Mean minimum tree depth\nacross influenza seasons") +
  theme(axis.title.y = element_blank())

ggplot(accuracy_decrease, aes(x = mean, y = plt_var)) +
  geom_point() +
  geom_linerange(aes(xmin = min, xmax = max)) +
  theme_bw() +
  scale_x_continuous("Mean decrease in accuracy\nacross influenza seasons") +
  theme(axis.title.y = element_blank())


# pred <- read_csv(paste0(gendata_dir, "/pred_clusters_id", model_id, "_test", perctestset, "_seed", mseed, "_m", modyear, "_p", predyear, ".csv"))

# dat15 <- read_csv(paste0(gendata_dir, "/modeldata_id", model_id, "_2008.csv"))