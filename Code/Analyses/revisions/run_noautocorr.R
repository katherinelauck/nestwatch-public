#!/usr/bin/env Rscript

#### Run models

library(INLA)
library(tidyverse)
library(magrittr)

TestHosts <- read_rds('results/revisions/inla_data.rds')
m <- read_rds("results/revisions/mainv1_withregion.rds")

resp <- "at_least_one_success"
fixed.effects <- formula(m) %>% as.character() %>% str_replace(pattern = " \\+ \\(.*\\)","")

mainv1_formula_withRegion <- as.formula(paste0(resp, " ~ ",
                                               fixed.effects[3],
                                               " + f(species, model = 'iid')",
                                               " + f(year, model = 'iid')",
                                               " + f(UnCoor, model = 'iid')",
                                               " + f(Region, model = 'iid')"))

inla_noautocorr <- inla(mainv1_formula_withRegion,
                        family = "binomial",
                        data = TestHosts,
                        control.compute = list(residuals = TRUE, dic = TRUE, waic = TRUE),
                        control.fixed = list(correlation.matrix=TRUE))
write_rds(inla_noautocorr,"results/revisions/inla_noautocorr.rds")

write_rds(inla_noautocorr$residuals$deviance.residuals,"results/revisions/resid_inla_noautocorr.rds")
  