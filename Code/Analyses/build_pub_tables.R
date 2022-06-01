#### Build tables for publication
#### Author: Katherine Lauck
#### Last updated: 17 May 2021

library(tidyverse)
library(gt)
library(boot) # Needed for inv.logit function
library(viridis)
library(lme4)

#### Predictor key

m.full <- read_rds("results/q12/success~stdmax2laydate2way.AK.RDS") # Read in full model
full <- summary(m.full)$coefficients
Predictor <- rownames(full)
full <- as.data.frame(full)
full <- bind_cols(Predictor = Predictor, full) %>% remove_rownames()
colnames(full) <- c("Predictor","Estimate","SE","Z","P-value")

m.full.min <- read_rds("results/q12/success~stdmin2laydate2way.AK.RDS") # Read in full model
full.min <- summary(m.full.min)$coefficients
Predictor <- rownames(full.min)
full.min <- as.data.frame(full.min)
full.min <- bind_cols(Predictor = Predictor, full.min) %>% remove_rownames()
colnames(full.min) <- c("Predictor","Estimate","SE","Z","P-value")

m.bbs <- read_rds("results/q4/success~BBSstdmax2laydate3way.AK.RDS") # Read in BBS model
m.ccs <- read_rds("results/q4/success~Conscore.stdmaxlaydate3way.Dk.NoLandscape.Full.rds") # Read in CCS model

bbs <- summary(m.bbs)$coefficients
Predictor <- rownames(bbs)
bbs <- as.data.frame(bbs)
bbs <- bind_cols(Predictor = Predictor, bbs) %>% remove_rownames()
colnames(bbs) <- c("Predictor","Estimate","SE","Z","P-value")

bbs <- bbs %>% mutate(type = "BBS population trend") # Add column type filled with BBS population trend to BBS

ccs <- summary(m.ccs)$coefficients
Predictor <- rownames(ccs)
ccs <- as.data.frame(ccs)
ccs <- bind_cols(Predictor = Predictor, ccs) %>% remove_rownames()
colnames(ccs) <- c("Predictor","Estimate","SE","Z","P-value")

ccs <- ccs %>% mutate(type = "NABCI conservation score")# Add column type filled with NABCI conservation score

m.cavity <- read_rds("results/q3/success~Cavity.stdmaxlaydate3way.Dk.NoLandscape.Full.rds") # Read in cavity model
m.substrate <- read_rds("results/q3/success~Nestbox.stdmaxlaydate3way.Dk.NoLandscape.Full.rds") # Read in substrate model

cavity <- summary(m.cavity)$coefficients
Predictor <- rownames(cavity)
cavity <- as.data.frame(cavity)
cavity <- bind_cols(Predictor = Predictor, cavity) %>% remove_rownames()
colnames(cavity) <- c("Predictor","Estimate","SE","Z","P-value")

cavity <- cavity %>% mutate(type = "Cavity nest or not") # Add column type filled with cavity population trend to cavity

substrate <- summary(m.substrate)$coefficients
Predictor <- rownames(substrate)
substrate <- as.data.frame(substrate)
substrate <- bind_cols(Predictor = Predictor, substrate) %>% remove_rownames()
colnames(substrate) <- c("Predictor","Estimate","SE","Z","P-value")

substrate <- substrate %>% mutate(type = "Nest in nestbox or not")# Add column type filled with NABCI conservation score

m.tavgnestpd_meanmax <- read_rds("results/spatial/success~tavgnestpd_meanmax_gridmet_3way.rds")
m.tmean_rel2sp <- read_rds("results/spatial/success~tmean_rel2sp_anom_3way.rds")
m.tnestpd_meanmax <- read_rds("results/spatial/success~tnestpd_meanmax_gridmet_3way.rds")
m.tnestpd_stdmaxsp <- read_rds("results/spatial/success~tnestpd_stdmaxsp_gridmet_3way.rds")

prep_predictor <- function(model) {
  tmp <- summary(model)$coefficients
  Predictor <- rownames(tmp)
  tmp <- as.data.frame(tmp)
  tmp <- bind_cols(Predictor = Predictor, tmp) %>% remove_rownames()
  colnames(tmp) <- c("Predictor","Estimate","SE","Z","P-value")
  return(tmp)
}

tavgnestpd_meanmax <- prep_predictor(m.tavgnestpd_meanmax)
tmean_rel2sp <- prep_predictor(m.tmean_rel2sp)
tnestpd_meanmax <- prep_predictor(m.tnestpd_meanmax)
tnestpd_stdmaxsp <- prep_predictor(m.tnestpd_stdmaxsp)

old <- select(full, Predictor) %>% 
  bind_rows(select(full.min,Predictor)) %>%
  bind_rows(select(bbs,Predictor)) %>%
  bind_rows(select(ccs,Predictor)) %>%
  bind_rows(select(substrate, Predictor)) %>%
  bind_rows(select(cavity, Predictor)) %>%
  bind_rows(select(tavgnestpd_meanmax, Predictor)) %>%
  bind_rows(select(tmean_rel2sp, Predictor)) %>%
  bind_rows(select(tnestpd_meanmax, Predictor)) %>%
  bind_rows(select(tnestpd_stdmaxsp, Predictor)) %>%
  filter(!Predictor=="(Intercept)") %>%
  distinct()

new <- c("Temperature anomaly (TA)",
         "Agriculture (Ag) land use (LU)",
         "Natural open (NO) LU",
         "Developed (D) LU",
         "TA, squared (TA^2)",
         "Precipitation",
         "% surrounding forest",
         "% surrounding developed LU",
         "% surrounding agriculture",
         "Nest in nestbox",
         "First egg date",
         "TA * Ag LU",
         "TA * NO LU",
         "TA * D LU",
         "TA^2 * Ag LU",
         "TA^2 * NO LU",
         "TA^2 * D LU",
         "Temperature anomaly (TA)",
         "TA, squared (TA^2)",
         "TA * Ag LU",
         "TA * NO LU",
         "TA * D LU",
         "TA^2 * Ag LU",
         "TA^2 * NO LU",
         "TA^2 * D LU",
         "Trait",
         "TA * trait",
         "Trait * Ag LU",
         "Trait * NO LU",
         "Trait * D LU",
         "TA^2 * trait",
         "TA * trait * Ag LU",
         "TA * trait * NO LU",
         "TA * trait * D LU",
         "TA^2 * trait * Ag LU",
         "TA^2 * trait * NO LU",
         "TA^2 * trait * D LU",
         "Trait",
         "TA * trait",
         "Trait * Ag LU",
         "Trait * NO LU",
         "Trait * D LU",
         "TA * trait * Ag LU",
         "TA * trait * NO LU",
         "TA * trait * D LU",
         "TA * trait",
         "Trait * Ag LU",
         "Trait * NO LU",
         "Trait * D LU",
         "TA * trait * Ag LU",
         "TA * trait * NO LU",
         "TA * trait * D LU",
         "Trait",
         "TA * trait",
         "Trait * Ag LU",
         "Trait * NO LU",
         "Trait * D LU",
         "TA * trait * Ag LU",
         "TA * trait * NO LU",
         "TA * trait * D LU",
         "Historical temp (HT)",
         "TA * HT",
         "HT * Ag LU",
         "HT * NO LU",
         "HT * D LU",
         "TA * HT * Ag LU",
         "TA * HT * NO LU",
         "TA * HT * D LU",
         "Historical temp (HT)",
         "TA * HT",
         "HT * Ag LU",
         "HT * NO LU",
         "HT * D LU",
         "TA * HT * Ag LU",
         "TA * HT * NO LU",
         "TA * HT * D LU",
         "Historical temp (HT)",
         "TA * HT",
         "HT * Ag LU",
         "HT * NO LU",
         "HT * D LU",
         "TA * HT * Ag LU",
         "TA * HT * NO LU",
         "TA * HT * D LU",
         "Historical temp (HT)",
         "TA * HT",
         "HT * Ag LU",
         "HT * NO LU",
         "HT * D LU",
         "TA * HT * Ag LU",
         "TA * HT * NO LU",
         "TA * HT * D LU"
         )

pred_key <- bind_cols(new = new, old = old$Predictor)


#### Max and min model results tables (Table S1)

m.max <- read_rds("results/q12/success~stdmax2laydate2way.AK.RDS")
m.min <- read_rds("results/q12/success~stdmin2laydate2way.AK.RDS")

max <- summary(m.max)$coefficients
Predictor <- rownames(max)
max <- as.data.frame(max)
max <- bind_cols(Predictor = Predictor, max) %>% remove_rownames() %>% mutate(type = "Maximum TA")
colnames(max) <- c("Predictor","Estimate","SE","Z","P-value","type")

min <- summary(m.min)$coefficients
Predictor <- rownames(min)
min <- as.data.frame(min)
min <- bind_cols(Predictor = Predictor, min) %>% remove_rownames() %>% mutate(type = "Minimum TA")
colnames(min) <- c("Predictor","Estimate","SE","Z","P-value","type")


full_table <- bind_rows(max,min) %>%
  mutate_at(c("Estimate", "SE"), round, digits = 2) %>%
  mutate_at("P-value",round, digits = 3) %>%
  select(!Z) %>%
  filter(!Predictor=="(Intercept)") %>%
  mutate(Predictor = pred_key$new[match(Predictor, pred_key$old)]) %>%
  group_by(type) %>%
  mutate(row=row_number()) %>% 
  pivot_longer(-c(type, row, Predictor)) %>%
  pivot_wider(names_from=c(type, name), values_from=value) %>%
  select(-row) %>%
  mutate(`Maximum TA_P-value` = if_else(`Maximum TA_P-value` == 0.000,"<0.001",as.character(`Maximum TA_P-value`))) %>%
  mutate(`Minimum TA_P-value` = if_else(`Minimum TA_P-value` == 0.000,"<0.001",as.character(`Minimum TA_P-value`))) %>%
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(
    delim="_"
  )

setwd("figures")
gtsave(data = full_table,filename = "TS1_full_table.html")
setwd("..")

#### Max TA * LU validation table (Table S2)

m.max <- read_rds("results/q12/success~stdmax2laydate2way.AK.RDS")
max.txlu <- read_rds("results/q12/success~stdmax2laydateLU_LRT2_AK.rds")
max.linear <- read_rds("results/q12/success~stdmax2laydateLU_LRT_AK.rds")

m.min <- read_rds("results/q12/success~stdmin2laydate2way.AK.RDS")
min.txlu <- read_rds("results/q12/success~stdmin2laydateLU_LRT2_AK.rds")
min.linear <- read_rds("results/q12/success~stdmin2laydateLU_LRT_AK.rds")

max_sq <- anova(max.txlu,m.max)[2,]
max_linear <- bind_rows(anova(max.linear,max.txlu)[2,],anova(max.linear,max.txlu)[1,])

min_sq <- anova(min.txlu,m.min)[2,]
min_linear <- bind_rows(anova(min.linear,min.txlu)[2,],anova(min.linear,min.txlu)[1,])

tab_val <- bind_rows(max_sq,max_linear,min_sq,min_linear) %>%
  round(., digits = 3) %>%
  tibble() %>%
  select(AIC,Chisq,`Pr(>Chisq)`) %>%
  mutate(Model = rep(c("TA2 * LU + TA * LU", "TA2 + TA * LU", "TA2 + TA + LU"),2), type = c(rep("Maximum TA", 3),rep("Minimum TA",3)), .before = AIC) %>%
  rename(P = `Pr(>Chisq)`) %>%
  group_by(type) %>%
  mutate(row=row_number()) %>% 
  pivot_longer(-c(type, row, Model)) %>%
  pivot_wider(names_from=c(type, name), values_from=value) %>%
  select(-row) %>%
  mutate(`Maximum TA_P` = if_else(`Maximum TA_P` == 0.000,"<0.001",as.character(`Maximum TA_P`))) %>%
  mutate(`Minimum TA_P` = if_else(`Minimum TA_P` == 0.000,"<0.001",as.character(`Minimum TA_P`))) %>%
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(
    delim="_"
  )

setwd("figures")
gtsave(filename = "TS2_luxta2_validation.html", data = tab_val)
setwd("..")

#### Squared temp validation table - per land use (Table S3)

notemp <- read_rds("results/q12/success~notemp_ag.rds")
linear_temp <- read_rds("results/q12/success~stdmax_ag.rds")
temp_sq <- read_rds("results/q12/success~stdmax2_ag.rds")

ag_sq <- anova(linear_temp, temp_sq)[2,]
ag_linear <- bind_rows(anova(notemp,linear_temp)[2,],anova(notemp,linear_temp)[1,])

notemp <- read_rds("results/q12/success~notemp_forest.rds")
linear_temp <- read_rds("results/q12/success~stdmax_forest.rds")
temp_sq <- read_rds("results/q12/success~stdmax2_forest.rds")

forest_sq <- anova(linear_temp, temp_sq)[2,]
forest_linear <- bind_rows(anova(notemp,linear_temp)[2,],anova(notemp,linear_temp)[1,])

notemp <- read_rds("results/q12/success~notemp_human.rds")
linear_temp <- read_rds("results/q12/success~stdmax_human.rds")
temp_sq <- read_rds("results/q12/success~stdmax2_human.rds")

human_sq <- anova(linear_temp, temp_sq)[2,]
human_linear <- bind_rows(anova(notemp,linear_temp)[2,],anova(notemp,linear_temp)[1,])

notemp <- read_rds("results/q12/success~notemp_open.rds")
linear_temp <- read_rds("results/q12/success~stdmax_open.rds")
temp_sq <- read_rds("results/q12/success~stdmax2_open.rds")

open_sq <- anova(linear_temp, temp_sq)[2,]
open_linear <- bind_rows(anova(notemp,linear_temp)[2,],anova(notemp,linear_temp)[1,])

tab_sq <- bind_rows(ag_sq, ag_linear, forest_sq, forest_linear, human_sq, human_linear, open_sq, open_linear) %>%
  mutate_at(c("AIC", "Chisq"), round, digits = 2) %>%
  mutate_at("Pr(>Chisq)",round, digits = 3) %>%
  tibble() %>%
  select(AIC,Chisq,`Pr(>Chisq)`) %>%
  mutate(lu = rep(c("Agriculture","Forest","Developed","Natural open"), each = 3), Model = rep(c("Squared temp", "Linear temp", "No temp term"), times = 4), .before = AIC) %>%
  rename(P = `Pr(>Chisq)`) %>%
  group_by(lu) %>%
  mutate(row=row_number()) %>% 
  pivot_longer(-c(lu, row, Model)) %>%
  pivot_wider(names_from=c(lu, name), values_from=value) %>%
  select(-row) %>%
  mutate(`Agriculture_P` = if_else(`Agriculture_P` == 0.000,"<0.001",as.character(`Agriculture_P`))) %>%
  mutate(`Forest_P` = if_else(`Forest_P` == 0.000,"<0.001",as.character(`Forest_P`))) %>%
  mutate(`Developed_P` = if_else(`Developed_P` == 0.000,"<0.001",as.character(`Developed_P`))) %>%
  mutate(`Natural open_P` = if_else(`Natural open_P` == 0.000,"<0.001",as.character(`Natural open_P`))) %>%
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(
    delim="_"
  )

setwd("figures")
gtsave(filename = "TS3_squared-validation.html", data = tab_sq)
setwd("..")

###### Full output tables for traits (Table S5)
# m.bbs <- read_rds("results/q4/success~BBSstdmaxlaydate3way.LRT.AK.rds") # Read in BBS model
m.ccs <- read_rds("results/q4/success~Conscore.stdmaxlaydate3way.Dk.NoLandscape.Full.rds") # Read in CCS model

# bbs <- summary(m.bbs)$coefficients
# Predictor <- rownames(bbs)
# bbs <- as.data.frame(bbs)
# bbs <- bind_cols(Predictor = Predictor, bbs) %>% remove_rownames()
# colnames(bbs) <- c("Predictor","Estimate","SE","Z","P-value")
# 
# bbs <- bbs %>% mutate(type = "BBS population trend") # Add column type filled with BBS population trend to BBS

ccs <- summary(m.ccs)$coefficients
Predictor <- rownames(ccs)
ccs <- as.data.frame(ccs)
ccs <- bind_cols(Predictor = Predictor, ccs) %>% remove_rownames()
colnames(ccs) <- c("Predictor","Estimate","SE","Z","P-value")

ccs <- ccs %>% mutate(type = "NABCI conservation score") # Add column type filled with NABCI conservation score

m.cavity <- read_rds("results/q3/success~Cavity.stdmaxlaydate3way.Dk.NoLandscape.Full.rds") # Read in cavity model
m.substrate <- read_rds("results/q3/success~Nestbox.stdmaxlaydate3way.Dk.NoLandscape.Full.rds") # Read in substrate model

cavity <- summary(m.cavity)$coefficients
Predictor <- rownames(cavity)
cavity <- as.data.frame(cavity)
cavity <- bind_cols(Predictor = Predictor, cavity) %>% remove_rownames()
colnames(cavity) <- c("Predictor","Estimate","SE","Z","P-value")

cavity <- cavity %>% mutate(type = "Cavity nest or not") 

substrate <- summary(m.substrate)$coefficients
Predictor <- rownames(substrate)
substrate <- as.data.frame(substrate)
substrate <- bind_cols(Predictor = Predictor, substrate) %>% remove_rownames()
colnames(substrate) <- c("Predictor","Estimate","SE","Z","P-value")

substrate <- substrate %>% mutate(type = "Nest in nestbox or not") # Add column type filled with NABCI conservation score

# Combine into table

trend_table <- bind_rows(ccs,cavity, substrate) %>%
  mutate_at(c("Estimate", "SE"), round, digits = 2) %>%
  mutate_at("P-value",round, digits = 3) %>%
  select(!Z) %>%
  filter(!Predictor=="(Intercept)") %>%
  group_by(type,Predictor) %>%
  mutate(row=row_number()) %>% 
  mutate(Predictor = pred_key$new[match(Predictor, pred_key$old)]) %>%
  pivot_longer(-c(type, row, Predictor)) %>%
  pivot_wider(names_from=c(type, name), values_from=value) %>%
  select(-row) %>%
  mutate(`NABCI conservation score_P-value` = if_else(`NABCI conservation score_P-value` == 0.000,"<0.001",as.character(`NABCI conservation score_P-value`))) %>%
  mutate(`Cavity nest or not_P-value` = if_else(`Cavity nest or not_P-value` == 0.000,"<0.001",as.character(`Cavity nest or not_P-value`))) %>%
  mutate(`Nest in nestbox or not_P-value` = if_else(`Nest in nestbox or not_P-value` == 0.000,"<0.001",as.character(`Nest in nestbox or not_P-value`))) %>%
  ungroup(Predictor) %>%
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(
    delim="_"
  )

gtsave(filename = "figures/TS5_trait_table.html", data = trend_table)

#### Triple interaction validation tables (Table S6)

# triple <- read_rds("results/q4/success~BBSstdmaxlaydate3way.LRT.AK.RDS")
# double <- read_rds("results/q4/success~BBSstdmaxlaydate_LRT2.AK.RDS")
# 
# bbs <- anova(triple, double) %>% map_df(rev)

triple <- read_rds("results/q4/success~Conscore.stdmaxlaydate3way.Dk.NoLandscape.Full.rds")
double <- read_rds("results/q4/success~Conscore.stdmaxlaydate3way.Dk.NoLandscape.No3Way.rds")

ccs <- anova(triple, double) %>% map_df(rev)

triple <- read_rds("results/q3/success~Nestbox.stdmaxlaydate3way.Dk.NoLandscape.Full.rds")
double <- read_rds("results/q3/success~Nestbox.stdmaxlaydate3way.Dk.NoLandscape.No3Way.rds")

substrate <- anova(triple, double) %>% map_df(rev)

triple <- read_rds("results/q3/success~Cavity.stdmaxlaydate3way.Dk.NoLandscape.Full.rds")
double <- read_rds("results/q3/success~Cavity.stdmaxlaydate3way.Dk.NoLandscape.No3Way.rds")

cavity <- anova(triple, double) %>% map_df(rev)

tab_triple <- bind_rows(ccs, cavity,substrate) %>%
  mutate_at(c("AIC", "Chisq"), round, digits = 2) %>%
  mutate_at("Pr(>Chisq)",round, digits = 3) %>%
  tibble() %>%
  select(AIC,Chisq,`Pr(>Chisq)`) %>%
  mutate(interaction = rep(c("Conservation score","Cavity vs not","Nestbox vs not"), each = 2), Model = rep(c("Triple interaction", "No triple interaction"), times = 3), .before = AIC) %>%
  rename(P = `Pr(>Chisq)`) %>%
  group_by(interaction) %>%
  mutate(row=row_number()) %>% 
  pivot_longer(-c(interaction, row, Model)) %>%
  pivot_wider(names_from=c(interaction, name), values_from=value) %>%
  select(-row) %>%
  mutate(`Conservation score_P` = if_else(`Conservation score_P` == 0.000,"<0.001",as.character(`Conservation score_P`))) %>%
  mutate(`Nestbox vs not_P` = if_else(`Nestbox vs not_P` == 0.000,"<0.001",as.character(`Nestbox vs not_P`))) %>%
  mutate(`Cavity vs not_P` = if_else(`Cavity vs not_P` == 0.000,"<0.001",as.character(`Cavity vs not_P`))) %>%
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(
    delim="_"
  )

gtsave(filename = "figures/TS6_triple_validation.html", data = tab_triple)

# Full output for spatial models (Table S7)

models <- list.files(path = "results/spatial",pattern = "^.+_3way\\.rds$",full.names = TRUE)
toremove <- str_which(models,"(Ag)|(Forest)|(Human)|(Natural_open)|(tnestpd_rel2sheard_z)|(tnestpd_rel2sheard_anom)|(tmean_rel2sp_z)")
models <- models[-toremove]
type_key <- tibble(old = c("tavgnestpd_meanmax_gridmet","tmean_rel2sp_anom","tnestpd_meanmax_gridmet","tnestpd_stdmaxsp_gridmet"),
                   new = c("Mean historical max spring temp","Mean annual temp relative to species range average","Mean max historical nest period temp","Mean max historical nest period temp relative to same per species in Nestwatch database"))

process_models <- function(path) {
  m <- read_rds(path)
  coef <- summary(m)$coefficients
  Predictor <- rownames(coef)
  coef <- as.data.frame(coef)
  out <- bind_cols(Predictor = Predictor, coef) %>% remove_rownames()
  colnames(out) <- c("Predictor","Estimate","SE","Z","P-value")
  out <- out %>% mutate(type = str_extract(path,pattern = "(?<=~)(.)*(?=_3way)")) %>%
    mutate(type = type_key$new[match(type, type_key$old)])
}

# Combine into table, grouped by type

spatial_table <- map_dfr(models, process_models) %>%
  mutate_at(c("Estimate", "SE"), round, digits = 2) %>%
  mutate_at("P-value",round, digits = 3) %>%
  group_by(type) %>%
  select(!Z) %>%
  filter(!Predictor=="(Intercept)") %>%
  mutate(row=row_number()) %>% 
  mutate(Predictor = pred_key$new[match(Predictor, pred_key$old)]) %>%
  pivot_longer(-c(type, row, Predictor)) %>%
  pivot_wider(names_from=c(type, name), values_from=value,names_sep = "~") %>%
  select(-row) %>%
  mutate(`Mean historical max spring temp~P-value` = if_else(`Mean historical max spring temp~P-value` == 0.000,"<0.001",as.character(`Mean historical max spring temp~P-value`))) %>%
  mutate(`Mean annual temp relative to species range average~P-value` = if_else(`Mean annual temp relative to species range average~P-value` == 0.000,"<0.001",as.character(`Mean annual temp relative to species range average~P-value`))) %>%
  mutate(`Mean max historical nest period temp~P-value` = if_else(`Mean max historical nest period temp~P-value` == 0.000,"<0.001",as.character(`Mean max historical nest period temp~P-value`))) %>%
  mutate(`Mean max historical nest period temp relative to same per species in Nestwatch database~P-value` = if_else(`Mean max historical nest period temp relative to same per species in Nestwatch database~P-value` == 0.000,"<0.001",as.character(`Mean max historical nest period temp relative to same per species in Nestwatch database~P-value`))) %>%
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(
    delim="~"
  )

setwd("figures")
gtsave(filename = "TS7_spatial_table.html", data = spatial_table)
setwd("..")

#### Does the three-way interaction of temp anomaly, site hotness, land use help the model? (Table S8)

#### Spatial triple interaction validation tables

triple <- read_rds("results/spatial/success~tavgnestpd_meanmax_gridmet_3way.rds")
double <- read_rds("results/spatial/success~tavgnestpd_meanmax_gridmet_2way.rds")

tavgnestpd <- anova(triple, double)

triple <- read_rds("results/spatial/success~tmean_rel2sp_anom_3way.rds")
double <- read_rds("results/spatial/success~tmean_rel2sp_anom_2way.rds")

tmean_rel2sp_anom <- anova(triple, double)

# triple <- read_rds("results/spatial/success~tmean_rel2sp_z_3way.rds")
# double <- read_rds("results/spatial/success~tmean_rel2sp_z_2way.rds")
# 
# tmean_rel2sp_z <- anova(triple, double)

triple <- read_rds("results/spatial/success~tnestpd_meanmax_gridmet_3way.rds")
double <- read_rds("results/spatial/success~tnestpd_meanmax_gridmet_2way.rds")

tnestpd <- anova(triple, double)

triple <- read_rds("results/spatial/success~tnestpd_stdmaxsp_gridmet_3way.rds")
double <- read_rds("results/spatial/success~tnestpd_stdmaxsp_gridmet_2way.rds")

tnestpd_stdsp <- anova(triple, double)

# triple <- read_rds("results/spatial/success~tnestpd_rel2sheard_anom_3way.rds")
# double <- read_rds("results/spatial/success~tnestpd_rel2sheard_anom_2way.rds")
# 
# tnestpd_rel2sheard_anom <- anova(triple, double)
# 
# triple <- read_rds("results/spatial/success~tnestpd_rel2sheard_z_3way.rds")
# double <- read_rds("results/spatial/success~tnestpd_rel2sheard_z_2way.rds")
# 
# tnestpd_rel2sheard_z <- anova(triple, double) # still running on cluster

tab_triple_spatial <- bind_rows(tnestpd,tavgnestpd,tmean_rel2sp_anom,tnestpd_stdsp) %>%
  round(., digits = 2) %>%
  tibble() %>%
  select(AIC,Chisq,`Pr(>Chisq)`) %>%
  mutate(interaction = rep(c("tnestpd_meanmax_gridmet","tavgnestpd_meanmax_gridmet","tmean_rel2sp_anom","tnestpd_stdmaxsp_gridmet"), each = 2), Model = rep(c("No triple interaction", "Triple interaction"), times = 4), .before = AIC) %>%
  #mutate(AIC = as.character(AIC),Chisq = as.character(Chisq)) %>%
  rename(P = `Pr(>Chisq)`) %>%
  mutate(interaction = type_key$new[match(interaction, type_key$old)]) %>%
  group_by(interaction) %>%
  mutate(row=row_number()) %>% 
  pivot_longer(-c(interaction, row, Model)) %>%
  pivot_wider(names_from=c(interaction, name), values_from=value,names_sep = "~")

tab_triple_spatial <- bind_rows(tab_triple_spatial[2,],tab_triple_spatial[1,]) %>%
  select(-row) %>%
  mutate(`Mean historical max spring temp~P` = if_else(`Mean historical max spring temp~P` == 0.000,"<0.001",as.character(`Mean historical max spring temp~P`))) %>%
  mutate(`Mean annual temp relative to species range average~P` = if_else(`Mean annual temp relative to species range average~P` == 0.000,"<0.001",as.character(`Mean annual temp relative to species range average~P`))) %>%
  mutate(`Mean max historical nest period temp~P` = if_else(`Mean max historical nest period temp~P` == 0.000,"<0.001",as.character(`Mean max historical nest period temp~P`))) %>%
  mutate(`Mean max historical nest period temp relative to same per species in Nestwatch database~P` = if_else(`Mean max historical nest period temp relative to same per species in Nestwatch database~P` == 0.000,"<0.001",as.character(`Mean max historical nest period temp relative to same per species in Nestwatch database~P`))) %>%
  gt() %>%
  tab_spanner_delim(
    delim="~"
  )

gtsave(filename = "figures/TS8_triple_validation_spatial.html", data = tab_triple_spatial)

measures <- c(
  #"tnestpd_rel2sheard_z",# missing tmax & tmaxsq? need to rerun on cluster? Maybe they didn't converge - but also we are dropping this measure
  #"tnestpd_rel2sheard_anom",
  "tnestpd_meanmax_gridmet","tavgnestpd_meanmax_gridmet","tmean_rel2sp_anom",
  #"tmean_rel2sp_z", # dropping this measure
  "tnestpd_stdmaxsp_gridmet")

get_anova <- function(name) {
  files <- list.files("results/spatial",full.names = TRUE)
  models <- files[str_which(files, pattern = paste0("^(.)+",name,"_((tmax)|(nohot)|(tmaxsq))\\.rds$"))]
  double <- read_rds(models[str_which(models,pattern = "^(.)+tmax\\.rds$")])
  nohot <- read_rds(models[str_which(models,pattern = "^(.)+nohot\\.rds$")])
  tmaxsq <- read_rds(models[str_which(models,pattern = "^(.)+tmaxsq\\.rds$")])
  one <- anova(double,nohot) %>% round(., digits = 2) %>% tibble() %>%
    mutate(Model = c("No interaction","Historical temp * max temp anomaly"), measure = name, .before = AIC)
  two <- anova(tmaxsq,double) %>% round(., digits = 2) %>% tibble() %>%
    mutate(Model = c("Historical temp * max temp anomaly","Historical temp * max temp anomaly^2"), measure = name, .before = AIC)
  out <- bind_rows(two[2,],one[2,],one[1,])
  return(out)
}

# use names to pair up model objects and produce anovas named by model and land use


tab_cavity <- map(measures,get_anova) %>%
  bind_rows() %>%
  select(AIC,Chisq,`Pr(>Chisq)`,Model,measure) %>%
  rename(P = `Pr(>Chisq)`) %>%
  #mutate(P = ifelse(P <= .05,paste0(P,"*"),P),AIC = as.character(AIC),Chisq = as.character(Chisq)) %>%
  mutate(measure = type_key$new[match(measure, type_key$old)]) %>%
  group_by(measure) %>%
  mutate(row=row_number()) %>%
  pivot_longer(-c(row, Model, measure)) %>%
  pivot_wider(names_from=c(measure, name), values_from=value,names_sep = "~") %>%
  select(-row) %>%
  filter(Model != "Historical temp * max temp anomaly^2") %>%
  mutate(`Mean historical max spring temp~P` = if_else(`Mean historical max spring temp~P` == 0.000,"<0.001",as.character(`Mean historical max spring temp~P`))) %>%
  mutate(`Mean annual temp relative to species range average~P` = if_else(`Mean annual temp relative to species range average~P` == 0.000,"<0.001",as.character(`Mean annual temp relative to species range average~P`))) %>%
  mutate(`Mean max historical nest period temp~P` = if_else(`Mean max historical nest period temp~P` == 0.000,"<0.001",as.character(`Mean max historical nest period temp~P`))) %>%
  mutate(`Mean max historical nest period temp relative to same per species in Nestwatch database~P` = if_else(`Mean max historical nest period temp relative to same per species in Nestwatch database~P` == 0.000,"<0.001",as.character(`Mean max historical nest period temp relative to same per species in Nestwatch database~P`))) %>%
  gt() %>%
  tab_spanner_delim(
    delim="~"
  )

gtsave(filename = "figures/TS9_spatial_hotnessxanomaly_validation.html", data = tab_cavity)

# big_spatial_LRT <- bind_rows(tab_triple_spatial,tab_cavity) %>%
#   mutate(`Mean historical max spring temp~P` = if_else(`Mean historical max spring temp~P` == 0.000,"<0.001",as.character(`Mean historical max spring temp~P`))) %>%
#   mutate(`Mean annual temp relative to species range average~P` = if_else(`Mean annual temp relative to species range average~P` == 0.000,"<0.001",as.character(`Mean annual temp relative to species range average~P`))) %>%
#   mutate(`Mean max historical nest period temp~P` = if_else(`Mean max historical nest period temp~P` == 0.000,"<0.001",as.character(`Mean max historical nest period temp~P`))) %>%
#   mutate(`Mean max historical nest period temp relative to same per species in Nestwatch database~P` = if_else(`Mean max historical nest period temp relative to same per species in Nestwatch database~P` == 0.000,"<0.001",as.character(`Mean max historical nest period temp relative to same per species in Nestwatch database~P`))) %>%
#   gt() %>% tab_options(data_row.padding = px(1)) %>%
#   tab_spanner_delim(
#     delim = "~"
#   )
# 
# setwd("figures")
# gtsave(filename = "TS8_spatial_LRT_supplement.html", data = big_spatial_LRT)
# setwd("..")

# #### trait LRT squared term (Table S10) NEED TO REPLACE MODEL NAMES WITH NAMES FROM ALISON
# 
# triple <- read_rds("results/q4/success~BBSstdmaxlaydate3way.LRT.AK.RDS")
# double <- read_rds("results/q4/success~BBSstdmaxlaydate_LRT2.AK.RDS")
# 
# bbs <- anova(triple, double)
# 
# triple <- read_rds("results/q4/success~Conscore.stdmaxlaydate3way.AK.LRT.RDS")
# double <- read_rds("results/q4/success~ConScoreStdmaxlaydate_LRT2.AK.RDS")
# 
# ccs <- anova(triple, double)
# 
# triple <- read_rds("results/q3/success~stdmaxsubstratelaydate.AK.RDS")
# double <- read_rds("results/q3/success~substratelaydate.tmax.LRT.AK.RDS")
# 
# substrate <- anova(triple, double)
# 
# triple <- read_rds("results/q3/success~stdmaxcavitylaydate.AK.RDS")
# double <- read_rds("results/q3/success~cavitylaydate.tmax.LRT.AK.RDS")
# 
# cavity <- anova(triple, double)
# 
# tab_traitlrt <- bind_rows(bbs,substrate, cavity) %>%
#   mutate_at(c("AIC", "Chisq"), round, digits = 2) %>%
#   mutate_at("Pr(>Chisq)",round, digits = 3) %>%
#   tibble() %>%
#   select(AIC,Chisq,`Pr(>Chisq)`) %>%
#   mutate(interaction = rep(c("BBS trend","Nestbox vs not","Cavity vs not"), each = 2), Model = rep(c("Triple interaction", "Double interaction"), times = 3), .before = AIC) %>%
#   rename(P = `Pr(>Chisq)`) %>%
#   group_by(interaction) %>%
#   mutate(row=row_number()) %>% 
#   pivot_longer(-c(interaction, row, Model)) %>%
#   pivot_wider(names_from=c(interaction, name), values_from=value) %>%
#   select(-row) %>%
#   mutate(`BBS trend_P` = if_else(`BBS trend_P` == 0.000,"<0.001",as.character(`BBS trend_P`))) %>%
#   mutate(`Nestbox vs not_P` = if_else(`Nestbox vs not_P` == 0.000,"<0.001",as.character(`Nestbox vs not_P`))) %>%
#   mutate(`Cavity vs not_P` = if_else(`Cavity vs not_P` == 0.000,"<0.001",as.character(`Cavity vs not_P`))) %>%
#   gt() %>% tab_options(data_row.padding = px(1)) %>%
#   tab_spanner_delim(
#     delim="_"
#   )
# 
# gtsave(filename = "figures/TS10_squaredLRTtraits.html", data = tab_traitlrt)

# #### Substrate per land use validation
# 
# triple <- read_rds("results/q3/success~stdmaxsubstrate.laydate.ag.AK.RDS")
# double <- read_rds("results/q3/success~stdmaxsubstrate.laydate.ag.LRT.AK.RDS")
# 
# ag <- anova(triple, double)
# 
# triple <- read_rds("results/q3/success~stdmaxsubstrate.laydate.forest.AK.RDS")
# double <- read_rds("results/q3/success~stdmaxsubstrate.laydate.for.LRT.AK.RDS")
# 
# forest <- anova(triple, double)
# 
# triple <- read_rds("results/q3/success~stdmaxsubstrate.laydate.human.AK.RDS")
# double <- read_rds("results/q3/success~stdmaxsubstrate.laydate.human.LRT.AK.RDS")
# 
# human <- anova(triple, double)
# 
# triple <- read_rds("results/q3/success~stdmaxsubstrate.laydate.natop.AK.RDS")
# double <- read_rds("results/q3/success~stdmaxsubstrate.laydate.natop.LRT.AK.RDS")
# 
# open <- anova(triple, double)
# 
# tab_substrate <- bind_rows(forest, ag, human, open) %>%
#   round(., digits = 2) %>%
#   tibble() %>%
#   select(AIC,Chisq,`Pr(>Chisq)`) %>%
#   mutate(lu = rep(c("Forest","Agriculture","Human","Open"), each = 2), Model = rep(c("Double interaction","Triple interaction"), times = 4), .before = AIC) %>%
#   rename(P = `Pr(>Chisq)`) %>%
#   group_by(lu) %>%
#   mutate(row=row_number()) %>% 
#   pivot_longer(-c(lu, row, Model)) %>%
#   pivot_wider(names_from=c(lu, name), values_from=value) %>%
#   select(-row) %>%
#   gt() %>% tab_options(data_row.padding = px(1)) %>%
#   tab_spanner_delim(
#     delim="_"
#   )
# 
# gtsave(filename = "figures/substrate_perlu.html", data = tab_substrate)
# 
# #### Cavity per land use validation
# 
# triple <- read_rds("results/q3/success~stdmaxcavity.laydate.ag.AK.RDS")
# double <- read_rds("results/q3/success~stdmaxcavity.laydate.ag.LRT.AK.RDS")
# 
# ag <- anova(triple, double)
# 
# triple <- read_rds("results/q3/success~stdmaxcavity.laydate.forest.AK.RDS")
# double <- read_rds("results/q3/success~stdmaxcavity.laydate.for.LRT.AK.RDS")
# 
# forest <- anova(triple, double)
# 
# triple <- read_rds("results/q3/success~stdmaxcavity.laydate.human.AK.RDS")
# double <- read_rds("results/q3/success~stdmaxcavity.laydate.human.LRT.AK.RDS")
# 
# human <- anova(triple, double)
# 
# triple <- read_rds("results/q3/success~stdmaxcavity.laydate.natop.AK.RDS")
# double <- read_rds("results/q3/success~stdmaxcavity.laydate.natop.LRT.AK.RDS")
# 
# open <- anova(triple, double)
# 
# tab_cavity <- bind_rows(forest,ag, human, open) %>%
#   round(., digits = 2) %>%
#   tibble() %>%
#   select(AIC,Chisq,`Pr(>Chisq)`) %>%
#   mutate(lu = rep(c("Forest","Agriculture","Human","Open"), each = 2), Model = rep(c("Double interaction","Triple interaction"), times = 4), .before = AIC) %>%
#   rename(P = `Pr(>Chisq)`) %>%
#   group_by(lu) %>%
#   mutate(row=row_number()) %>% 
#   pivot_longer(-c(lu, row, Model)) %>%
#   pivot_wider(names_from=c(lu, name), values_from=value) %>%
#   select(-row) %>%
#   gt() %>% 
#   tab_spanner_delim(
#     delim="_"
#   )
# 
# gtsave(filename = "figures/cavity_perlu.png", data = tab_cavity)
# 
# 
# 
# #### Spatial per land use validation
# 
# models <- tibble(var = c(rep(c("tnestpd_meanmax_gridmet","tmeanmax_avgnestpd_gridmet_scaled","tmean_rel2sp_anom","tnestpd_stdmaxsp_gridmet"),each = 4)),
#                  filename = c(rep(c("tnestpd_meanmax_gridmet","tavgnestpd_meanmax_gridmet","tmean_rel2sp_anom","tnestpd_stdmaxsp_gridmet"),each = 4)),
#                  land_use = c(rep(c("Ag","Human","Forest","Natural_open"), times = 4)))
# 
# names <- paste0(models$filename,"_",models$land_use)
# 
# get_anova <- function(name) {
#   files <- list.files("results/spatial",full.names = TRUE)
#   models <- files[str_which(files, pattern = paste0("^(.)+",name,"(.)+$"))]
#   double <- read_rds(models[str_which(models,pattern = "^(.)+2way\\.rds$")])
#   triple <- read_rds(models[str_which(models,pattern = "^(.)+3way\\.rds$")])
#   out <- anova(triple,double) %>% round(., digits = 2) %>% tibble() %>%
#     mutate(lu = str_extract(name,"(Ag)|(Human)|(Forest)|(Natural_open)"), Model = c("Double interaction","Triple interaction"), measure = str_extract(name,"(.)+(?=(_Ag)|(_Human)|(_Forest)|(_Natural_open))"), .before = AIC)
#   return(out)
# }
# 
# # use names to pair up model objects and produce anovas named by model and land use
# 
# 
# tab_cavity <- map(names,get_anova) %>%
#   bind_rows() %>%
#   select(AIC,Chisq,`Pr(>Chisq)`,lu,Model,measure) %>%
#   rename(P = `Pr(>Chisq)`) %>%
#   mutate(P = ifelse(P <= .05,paste0(P,"*"),P),AIC = as.character(AIC),Chisq = as.character(Chisq)) %>%
#   group_by(measure) %>%
#   mutate(row=row_number()) %>%
#   pivot_longer(-c(lu, row, Model, measure)) %>%
#   pivot_wider(names_from=c(measure, name), values_from=value,names_sep = "-") %>%
#   group_by(lu) %>%
#   select(-row) %>%
#   gt() %>%
#   tab_spanner_delim(
#     delim="-"
#   )
# 
# gtsave(filename = "figures/spatial_landuse_validation.png", data = tab_cavity,vwidth = 1600)
# # 
# 
# 



# #### Tmin LRT
# 
# notemp <- read_rds("results/q12/success~notemp_ag.rds")
# linear_temp <- read_rds("results/q12/success~stdmax_ag.rds")
# temp_sq <- read_rds("results/q12/success~stdmax2_ag.rds")
# 
# ag_sq <- anova(linear_temp, temp_sq)[2,]
# ag_linear <- bind_rows(anova(notemp,linear_temp)[2,],anova(notemp,linear_temp)[1,])
# 
# notemp <- read_rds("results/q12/success~notemp_forest.rds")
# linear_temp <- read_rds("results/q12/success~stdmax_forest.rds")
# temp_sq <- read_rds("results/q12/success~stdmax2_forest.rds")
# 
# forest_sq <- anova(linear_temp, temp_sq)[2,]
# forest_linear <- bind_rows(anova(notemp,linear_temp)[2,],anova(notemp,linear_temp)[1,])
# 
# notemp <- read_rds("results/q12/success~notemp_human.rds")
# linear_temp <- read_rds("results/q12/success~stdmax_human.rds")
# temp_sq <- read_rds("results/q12/success~stdmax2_human.rds")
# 
# human_sq <- anova(linear_temp, temp_sq)[2,]
# human_linear <- bind_rows(anova(notemp,linear_temp)[2,],anova(notemp,linear_temp)[1,])
# 
# notemp <- read_rds("results/q12/success~notemp_open.rds")
# linear_temp <- read_rds("results/q12/success~stdmax_open.rds")
# temp_sq <- read_rds("results/q12/success~stdmax2_open.rds")
# 
# open_sq <- anova(linear_temp, temp_sq)[2,]
# open_linear <- bind_rows(anova(notemp,linear_temp)[2,],anova(notemp,linear_temp)[1,])
# 
# tab_sq <- bind_rows(ag_sq, ag_linear, forest_sq, forest_linear, human_sq, human_linear, open_sq, open_linear) %>%
#   round(., digits = 2) %>%
#   tibble() %>%
#   select(AIC,Chisq,`Pr(>Chisq)`) %>%
#   mutate(lu = rep(c("Agriculture","Forest","Human","Open"), each = 3), Model = rep(c("Squared temp", "Linear temp", "No temp term"), times = 4), .before = AIC) %>%
#   rename(P = `Pr(>Chisq)`) %>%
#   group_by(lu) %>%
#   mutate(row=row_number()) %>% 
#   pivot_longer(-c(lu, row, Model)) %>%
#   pivot_wider(names_from=c(lu, name), values_from=value) %>%
#   select(-row) %>%
#   gt() %>% 
#   tab_spanner_delim(
#     delim="_"
#   )
# 
# gtsave(filename = "figures/squared-validation.png", data = tab_sq)
