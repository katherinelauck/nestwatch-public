library("lme4"); library(tidyverse); nest <- read_rds('Data/active/success-cleaned.rds')

mod <- glmer(formula = at_least_one_success ~ Tmax_std_gridmet * NewLU1 * cavity_binary + 
               pcpbefore_raw_gridmet + laydate_scaled + (1 + Tmax_std_gridmet | species) + 
               (1 | year) + (1 | Region/UnCoor), data = nest, family = binomial(link = "logit"), 
             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))

write_rds(mod,"results/revisions/cavity_stdmaxlaydate3way_spslope_triple.rds")

