#### Build tables for publication
#### Author: Katherine Lauck
#### Last updated: 17 May 2021

library(tidyverse)
library(gt)
library(boot) # Needed for inv.logit function
library(viridis)
library(lme4)

#### Predictor key

m.full <- read_rds("results/revisions/mainv1_withregion.rds") # Read in full model
full <- summary(m.full)$coefficients
Predictor <- rownames(full)
full <- as.data.frame(full)
full <- bind_cols(Predictor = Predictor, full) %>% remove_rownames()
colnames(full) <- c("Predictor","Estimate","SE","Z","P-value")

m.full.min <- read_rds("results/q12/success~stdmin2laydate2way.AK_quad.RDS") # Read in full model
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
eletemp <- read_rds("results/revisions/mainv1_eletemp_quad.rds")
lat <- read_rds("results/revisions/mainv1_lat.rds")
m.luxpafter <- read_rds("results/revisions/mainv1_luxpafter_quad.rds")
m.luxpbefore <- read_rds("results/revisions/mainv1_luxpbefore_quad.rds")

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
eletemp <- prep_predictor(eletemp)
lat <- prep_predictor(lat)
luxpafter <- prep_predictor(m.luxpafter)
luxpbefore <- prep_predictor(m.luxpbefore)

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
  bind_rows(select(eletemp, Predictor)) %>%
  bind_rows(select(lat, Predictor)) %>%
  bind_rows(select(luxpafter, Predictor)) %>%
  bind_rows(select(luxpbefore, Predictor)) %>%
  filter(!Predictor=="(Intercept)") %>%
  distinct()

new <- c("Temperature anomaly (TA)",
         "Agriculture (Ag) land use (LU)",
         "Natural open (NO) LU",
         "Developed (D) LU",
         "TA, squared (TA^2)",
         "Precipitation 365 days before (PB)",
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
         "TA * HT * D LU",
         "Elevation",
         "Elevation * TA",
         "Latitude",
         "Total precip 45 days after (PA)",
         "PA^2",
         "PA * Ag LU",
         "PA * NO LU",
         "PA * D LU",
         "PA^2 * Ag LU",
         "PA^2 * NO LU",
         "PA^2 * D LU",
         "PB^2",
         "PB * Ag LU",
         "PB * NO LU",
         "PB * D LU",
         "PB^2 * Ag LU",
         "PB^2 * NO LU",
         "PB^2 * D LU")

pred_key <- bind_cols(new = new, old = old$Predictor)

#### Sample size table (Table S1)

samp_tab <- read_csv("figures/samplesizeTABLE.csv") %>%
  select(-1) %>%
  gt() %>% tab_options(data_row.padding = px(1))

setwd("figures")
gtsave(data = samp_tab,filename = "TS1_samplesize.html")
setwd("..")


#### Max and min model results tables (Table S2) - needs to be updated with precip models when they are done running

m.max <- read_rds("results/revisions/mainv1_withregion.rds")
m.min <- read_rds("results/q12/success~stdmin2laydate2way.AK_quad.RDS")
m.p.after <- read_rds("results/revisions/mainv1_luxpafter_quad.rds")
m.p.before <- read_rds("results/revisions/mainv1_luxpafter_quad.rds")

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

p.after <- summary(m.p.after)$coefficients
Predictor <- rownames(p.after)
p.after <- as.data.frame(p.after)
p.after <- bind_cols(Predictor = Predictor, p.after) %>% remove_rownames() %>% mutate(type = "Precip anomaly 45 days after lay date")
colnames(p.after) <- c("Predictor","Estimate","SE","Z","P-value","type")

p.before <- summary(m.p.before)$coefficients
Predictor <- rownames(p.before)
p.before <- as.data.frame(p.before)
p.before <- bind_cols(Predictor = Predictor, p.before) %>% remove_rownames() %>% mutate(type = "Total precip 365 days before lay date")
colnames(p.before) <- c("Predictor","Estimate","SE","Z","P-value","type")


full_table_temp <- bind_rows(max,min) %>%
  mutate_at(c("Estimate", "SE"), round, digits = 2) %>%
  mutate_at("P-value",round, digits = 3) %>%
  select(!Z) %>%
  filter(!Predictor=="(Intercept)") %>%
  mutate(Predictor = pred_key$new[match(Predictor, pred_key$old)]) %>%
  group_by(type) %>%
  # mutate(row=row_number()) %>% 
  pivot_longer(-c(type, Predictor)) %>%
  pivot_wider(names_from=c(type, name), values_from=value) %>%
  # select(-row) %>%
  mutate(`Maximum TA_P-value` = if_else(`Maximum TA_P-value` == 0.000,"<0.001",as.character(`Maximum TA_P-value`))) %>%
  mutate(`Minimum TA_P-value` = if_else(`Minimum TA_P-value` == 0.000,"<0.001",as.character(`Minimum TA_P-value`))) %>%
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(
    delim="_"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = `Maximum TA_P-value`,
    )
  )

setwd("figures")
gtsave(data = full_table_temp,filename = "TS2_full_table_temp.html")
setwd("..")

full_table_pcp <- bind_rows(p.after,p.before) %>%
  mutate_at(c("Estimate", "SE"), round, digits = 2) %>%
  mutate_at("P-value",round, digits = 3) %>%
  select(!Z) %>%
  filter(!Predictor=="(Intercept)") %>%
  mutate(Predictor = pred_key$new[match(Predictor, pred_key$old)]) %>%
  group_by(type) %>%
  # mutate(row=row_number()) %>% 
  pivot_longer(-c(type, Predictor)) %>%
  pivot_wider(names_from=c(type, name), values_from=value) %>%
  # select(-row) %>%
  mutate(`Precip anomaly 45 days after lay date_P-value` = if_else(`Precip anomaly 45 days after lay date_P-value` == 0.000,"<0.001",as.character(`Precip anomaly 45 days after lay date_P-value`))) %>%
  mutate(`Total precip 365 days before lay date_P-value` = if_else(`Total precip 365 days before lay date_P-value` == 0.000,"<0.001",as.character(`Total precip 365 days before lay date_P-value`))) %>%
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(
    delim="_"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = `Precip anomaly 45 days after lay date_P-value`,
    )
  )

setwd("figures")
gtsave(data = full_table_pcp,filename = "TS8_full_table_pcp.html")
setwd("..")

#### Max TA * LU validation table (Table S3)

squared_validation_table_data <- function(groups,models) {
  
  models <- filter(models,group == groups) %>% pull(file)
  
  get_lu_coef <- function(model) {
    coef <- fixef(model)
    if("pcpbefore_raw_gridmet_sq" %in% names(coef)) {assign("var","pcpbefore_raw_gridmet")
    } else if("pcpafter_std_gridmet_sq" %in% names(coef)) {assign("var","pcpafter_std_gridmet")
    } else if("Tmax_std_gridmet_sq" %in% names(coef)) {assign("var","Tmax_std_gridmet")
    } else if("Tmin_std_gridmet_sq" %in% names(coef)) {assign("var","Tmin_std_gridmet")
            }
    if(deparse(substitute(model)) %>% str_detect("quad")) {
      out <- tibble(lu = rep(c("Agriculture","Forest","Natural_open","Human")),
                    Var = c(coef[paste0(var)] + coef[paste0(var,":NewLU1Ag")],
                                      coef[paste0(var)],
                                      coef[paste0(var)] + coef[paste0(var,":NewLU1Natural_open")],
                                      coef[paste0(var)] + coef[paste0(var,":NewLU1Human")]),
                    `Var^2` = c(coef[paste0(var,"_sq")] + coef[paste0("NewLU1Ag:",var,"_sq")],
                                         coef[paste0(var,"_sq")],
                                         coef[paste0(var,"_sq")] + coef[paste0("NewLU1Natural_open:",var,"_sq")],
                                         coef[paste0(var,"_sq")] + coef[paste0("NewLU1Human:",var,"_sq")]))
    } else if(deparse(substitute(model)) %>% str_detect("linear")) {
      out <- tibble(lu = rep(c("Agriculture","Forest","Natural_open","Human")),
                    Var = c(coef[paste0(var)] + coef[paste0(var,":NewLU1Ag")],
                                      coef[paste0(var)],
                                      coef[paste0(var)] + coef[paste0(var,":NewLU1Natural_open")],
                                      coef[paste0(var)] + coef[paste0(var,":NewLU1Human")]),
                    `Var^2` = rep(NA,4))
    } else if(deparse(substitute(model)) %>% str_detect("noint")) {
      out <- tibble(lu = rep(c("Agriculture","Forest","Natural_open","Human")),
                    Var = rep(NA,4),
                    `Var^2` = rep(NA,4))
    } else {stop("invalid model type - should be quad, linear, or noint")}
    return(out)
  }
  
  noint <- read_rds(str_subset(models,"(notemp)|(noint)|(LRT(?!2))"))
  coef_noint <- get_lu_coef(noint)
  linear <- read_rds(str_subset(models,"(stdmax(?!2))|(linear)|(LRT2)"))
  coef_linear <- get_lu_coef(linear)
  quad <- read_rds(str_subset(models,"(stdmax2)|(temp_sq)|(quad)"))
  coef_quad <- get_lu_coef(quad)
  
  tmp <- anova(linear, quad)[2,] %>% 
    slice(rep(1:n(),times = 4)) %>% 
    tibble() %>%
    bind_cols(coef_quad, Model = "Squared temp/precip * LU",group = groups)
    
  tmp2 <- bind_rows(anova(noint,linear)[2,],anova(noint,linear)[1,]) %>% 
    slice(rep(1:n(),each = 4)) %>% 
    tibble() %>% 
    bind_cols(bind_rows(coef_linear,coef_noint),
              Model = rep(c("Linear temp/precip * LU","Additive temp/precip"),each = 4),
              group = rep(groups,times = 8))
  out <- bind_rows(tmp,tmp2)
  
}

models <- tibble(file = c("results/revisions/mainv1_withregion_quad.rds",
                          "results/revisions/mainv1_withregion_linear.rds",
                          "results/revisions/mainv1_withregion_noint.rds",
                          "results/q12/success~stdmin2laydate2way.AK_quad.RDS",
                          "results/q12/success~stdmin2laydateLU_LRT2_AK.rds",
                          "results/q12/success~stdmin2laydateLU_LRT_AK.rds",
                          "results/revisions/mainv1_lat_quad.rds",
                          "results/revisions/mainv1_lat_linear.rds",
                          "results/revisions/mainv1_lat_noint.rds",
                          "results/revisions/mainv1_eletemp_quad.rds",
                          "results/revisions/mainv1_eletemp_linear.rds",
                          "results/revisions/mainv1_eletemp_noint.rds",
                          "results/revisions/mainv1_spslope_quad.rds",
                          "results/revisions/mainv1_spslope_linear.rds",
                          "results/revisions/mainv1_spslope_noint.rds",
                          "results/revisions/mainv1_nopred_quad.rds",
                          "results/revisions/mainv1_nopred_linear.rds",
                          "results/revisions/mainv1_nopred_noint.rds",
                          "results/revisions/mainv1_noextreme_quad.rds",
                          "results/revisions/mainv1_noextreme_linear.rds",
                          "results/revisions/mainv1_noextreme_noint.rds",
                          "results/revisions/mainv1_500m_quad.rds",
                          "results/revisions/mainv1_500m_linear.rds",
                          "results/revisions/mainv1_500m_noint.rds",
                          "results/revisions/mainv1_1km_quad.rds",
                          "results/revisions/mainv1_1km_linear.rds",
                          "results/revisions/mainv1_1km_noint.rds",
                          "results/revisions/mainv1_res16_quad.rds",
                          "results/revisions/mainv1_res16_linear.rds",
                          "results/revisions/mainv1_res16_noint.rds",
                          "results/revisions/mainv1_luxpbefore_quad.rds",
                          "results/revisions/mainv1_luxpbefore_linear.rds",
                          "results/revisions/mainv1_luxpbefore_noint.rds",
                          "results/revisions/mainv1_luxpafter_quad.rds",
                          "results/revisions/mainv1_luxpafter_linear.rds",
                          "results/revisions/mainv1_luxpafter_noint.rds"),
                 group = rep(c("Base model: TA = Maximum temperature anomaly","Minimum temperature anomaly","Latitude","Elevation * temp","Temp * species random slope","Predation observations removed","Extreme temp observations removed","Landscape 500m buffer","Landscape 1km buffer","Thinned","Total precip 365 days before laydate","Precip anomaly 45 days after laydate"),each = 3))

fig_data <- map_dfr(models$group %>% unique(),squared_validation_table_data,models)

# m.max <- read_rds("results/revisions/mainv1_withregion.rds")
# max.txlu <- read_rds("results/revisions/mainv1_withregion_linear.rds")
# max.linear <- read_rds("results/revisions/mainv1_withregion_linear.rds")
# 
# m.min <- read_rds("results/q12/success~stdmin2laydate2way.AK.RDS")
# min.txlu <- read_rds("results/q12/success~stdmin2laydateLU_LRT2_AK.rds")
# min.linear <- read_rds("results/q12/success~stdmin2laydateLU_LRT_AK.rds")
# 
# max_sq <- anova(max.txlu,m.max)[2,]
# max_linear <- bind_rows(anova(max.linear,max.txlu)[2,],anova(max.linear,max.txlu)[1,])
# 
# min_sq <- anova(min.txlu,m.min)[2,]
# min_linear <- bind_rows(anova(min.linear,min.txlu)[2,],anova(min.linear,min.txlu)[1,])

tab_mainresult <- fig_data %>% filter(group %in% c("Base model: TA = Maximum temperature anomaly","Minimum temperature anomaly","Total precip 365 days before laydate","Precip anomaly 45 days after laydate")) %>%
  mutate_at(c("Chisq"), round, digits = 2) %>%
  mutate_at(c("AIC"), round, digits = 0) %>%
  mutate_at(c("Pr(>Chisq)","Var","Var^2"),round, digits = 3) %>%
  rename(TA = Var,`TA^2` = `Var^2`) %>%
  tibble() %>%
  select(AIC,Chisq,`Pr(>Chisq)`,lu,group,TA,`TA^2`) %>%
  mutate(lu = replace(lu,lu == "Human","Developed"),lu = replace(lu,lu == "Natural_open","Natural open")) %>%
  mutate(lu = factor(lu,levels = c("Forest","Agriculture","Natural open","Developed"))) %>%
  mutate(Model = rep(c("TA^2 * LU + TA * LU", "TA^2 + TA * LU", "TA^2 + TA + LU"),each = 4, times = length(unique(group))), .before = AIC) %>%
  rename(P = `Pr(>Chisq)`) %>%
  group_by(group) %>%
  mutate(row=row_number(),
         P = if_else(P == 0.000,"<0.001",as.character(P)),
         TA = as.character(TA),
         `TA^2` = as.character(`TA^2`)) %>%
  pivot_longer(-c(group, row, Model,lu,AIC,Chisq,P)) %>%
  select(-row) %>%
  pivot_wider(names_from=c(lu, name), values_from=value,names_sep = "~",values_fill = "m") %>%
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(
    delim="~"
  ) %>%
  tab_style(cell_text(style = "italic"),cells_row_groups())  %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = c(P,`Agriculture~TA^2`,`Forest~TA^2`,`Natural open~TA^2`,`Developed~TA^2`),
    )
  )

setwd("figures")
gtsave(filename = "TS3_mainresult.html", data = tab_mainresult)
setwd("..")

tab_val <- fig_data %>% filter(group %in% c("Base model: TA = Maximum temperature anomaly","Latitude","Elevation * temp","Temp * species random slope","Predation observations removed","Extreme temp observations removed","Landscape 500m buffer","Landscape 1km buffer","Thinned")) %>%
  mutate_at(c("Chisq"), round, digits = 2) %>%
  mutate_at(c("AIC"), round, digits = 0) %>%
  mutate_at(c("Pr(>Chisq)","Var","Var^2"),round, digits = 3) %>%
  rename(TA = Var,`TA^2` = `Var^2`) %>%
  tibble() %>%
  select(AIC,Chisq,`Pr(>Chisq)`,lu,group,TA,`TA^2`) %>%
  mutate(lu = replace(lu,lu == "Human","Developed"),lu = replace(lu,lu == "Natural_open","Natural open")) %>%
  mutate(lu = factor(lu,levels = c("Forest","Agriculture","Natural open","Developed"))) %>%
  mutate(Model = rep(c("TA^2 * LU + TA * LU", "TA^2 + TA * LU", "TA^2 + TA + LU"),each = 4, times = length(unique(group))), .before = AIC) %>%
  rename(P = `Pr(>Chisq)`) %>%
  group_by(group) %>%
  mutate(row=row_number(),
         P = if_else(P == 0.000,"<0.001",as.character(P)),
         TA = as.character(TA),
         `TA^2` = as.character(`TA^2`)) %>%
  pivot_longer(-c(group, row, Model,lu,AIC,Chisq,P)) %>%
  select(-row) %>%
  pivot_wider(names_from=c(lu, name), values_from=value,names_sep = "~",values_fill = "m") %>%
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(
    delim="~"
  ) %>%
  tab_style(cell_text(style = "italic"),cells_row_groups())  %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = c(P,`Agriculture~TA^2`,`Forest~TA^2`,`Natural open~TA^2`,`Developed~TA^2`),
    )
  )

setwd("figures")
gtsave(filename = "TS6_sensitivity.html", data = tab_val)
setwd("..")

#### Squared temp validation table - per land use (Table S4)

models <- tibble(file = c("results/q12/success~notemp_ag.rds",
           "results/q12/success~stdmax_ag.rds",
           "results/q12/success~stdmax2_ag.rds",
           "results/q12/success~notemp_forest.rds",
           "results/q12/success~stdmax_forest.rds",
           "results/q12/success~stdmax2_forest.rds",
           "results/q12/success~notemp_open.rds",
           "results/q12/success~stdmax_open.rds",
           "results/q12/success~stdmax2_open.rds",
           "results/q12/success~notemp_human.rds",
           "results/q12/success~stdmax_human.rds",
           "results/q12/success~stdmax2_human.rds",
           "results/revisions/mainv1_luxpafter_quad_ag.rds",
           "results/revisions/mainv1_luxpafter_quad_forest.rds",
           "results/revisions/mainv1_luxpafter_quad_open.rds",
           "results/revisions/mainv1_luxpafter_quad_human.rds",
           "results/revisions/mainv1_luxpafter_linear_ag.rds",
           "results/revisions/mainv1_luxpafter_linear_forest.rds",
           "results/revisions/mainv1_luxpafter_linear_open.rds",
           "results/revisions/mainv1_luxpafter_linear_human.rds",
           "results/revisions/mainv1_luxpafter_noint_ag.rds",
           "results/revisions/mainv1_luxpafter_noint_forest.rds",
           "results/revisions/mainv1_luxpafter_noint_open.rds",
           "results/revisions/mainv1_luxpafter_noint_human.rds"),
           group = rep(c("Var = Maximum temperature anomaly","Var = Total precipitation 45 days after laydate"),each = 12)
)

squared_validation_table_data <- function(lu,groups,models) {
  
  models <- filter(models,group == groups) %>% pull(file) %>% str_subset(lu)
  notemp <- read_rds(str_subset(models,"(notemp)|(noint)"))
  linear_temp <- read_rds(str_subset(models,"(stdmax(?!2))|(linear)"))
  temp_sq <- read_rds(str_subset(models,"(stdmax2)|(temp_sq)|(quad)"))
  
  if(lu == "ag") {
    lu <- "Agriculture"
  } else if(lu == "forest") {
    lu <- "Forest"
  } else if(lu == "open") {
    lu <- "Natural open"
  } else if(lu == "human") {
    lu <- "Developed"
  }
  
  coef <- fixef(temp_sq)
  
  if("pcpbefore_raw_gridmet_sq" %in% names(coef)) {assign("var","pcpbefore_raw_gridmet")
  } else if("pcpafter_std_gridmet_sq" %in% names(coef)) {assign("var","pcpafter_std_gridmet")
  } else if("Tmax_std_gridmet_sq" %in% names(coef)) {assign("var","Tmax_std_gridmet")
  } else if("Tmin_std_gridmet_sq" %in% names(coef)) {assign("var","Tmin_std_gridmet")
  }
  
  out <- anova(linear_temp, temp_sq)[2,] %>% tibble() %>%
    bind_cols(`Temp Coef` = fixef(temp_sq)[var],`Temp^2 Coef` = fixef(temp_sq)[paste0(var,"_sq")],lu = lu, Model = "Var + Var^2", Group = groups) %>%
    bind_rows(bind_rows(anova(notemp,linear_temp)[2,],anova(notemp,linear_temp)[1,]) %>% tibble() %>% 
                bind_cols(`Temp Coef` = c(fixef(linear_temp)[var],
                                       NA),
                          `Temp^2 Coef` = c(NA,NA),
                          lu = c(lu,lu),
                          Model = c("Var","No Var term"),
                          Group = c(groups,groups)))
  
}

fig_data <- map2_dfr(.x = rep(c("ag","forest","open","human"),
                              times = length(select(models,group) %>% pull() %>% unique())),
                     .y = rep(select(models,group) %>% pull() %>% unique(),
                              each = 4),
                     squared_validation_table_data,
                     models = models)

tab_sq_ta <- fig_data %>% filter(Group == "Var = Maximum temperature anomaly") %>%
  mutate(Chisq = Chisq %>% round(digits = 2), AIC = AIC %>% round(digits = 0)) %>%
  mutate(across(c("Pr(>Chisq)","Temp Coef","Temp^2 Coef"),round, digits = 3)) %>%
  select(AIC,Chisq,`Pr(>Chisq)`,`Temp Coef`,`Temp^2 Coef`,lu,Model) %>%
  rename(P = `Pr(>Chisq)`) %>%
  mutate(lu = factor(lu,levels = c("Forest","Agriculture","Natural open","Developed"))) %>%
  mutate(across(c("AIC","Chisq","Temp Coef","Temp^2 Coef"),as.character)) %>%
  rename(`TA coef` = "Temp Coef",`TA^2 coef` = "Temp^2 Coef") %>%
  mutate(P = if_else(P == 0.000, "<0.001",as.character(P))) %>%
  mutate(Model = fct_recode(Model,`TA + TA^2` = "Var + Var^2",TA = "Var",`No temp term` = "No Var term")) %>%
  # group_by(lu) %>%
  pivot_longer(-c(lu, Model)) %>%
  pivot_wider(names_from=c(lu, name), values_from=value) %>%
  # rename(Statistic = name) %>%
  # select(-row) %>%
  # mutate(`Agriculture_P` = if_else(`Agriculture_P` == 0.000,"<0.001",as.character(`Agriculture_P`))) %>%
  # mutate(`Forest_P` = if_else(`Forest_P` == 0.000,"<0.001",as.character(`Forest_P`))) %>%
  # mutate(`Developed_P` = if_else(`Developed_P` == 0.000,"<0.001",as.character(`Developed_P`))) %>%
  # mutate(`Natural open_P` = if_else(`Natural open_P` == 0.000,"<0.001",as.character(`Natural open_P`))) %>%
  # group_by(lu) %>%
  gt() %>% 
  tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(delim="_") %>%
  tab_style(cell_text(style = "italic"),cells_row_groups())  %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = c(`Agriculture_TA^2 coef`,`Forest_TA^2 coef`,`Natural open_TA^2 coef`),
    )
  )

setwd("figures")
gtsave(filename = "TS4_squared-validation_ta.html", data = tab_sq_ta)
setwd("..")

tab_sq_pcp <- fig_data %>% filter(Group == "Var = Total precipitation 45 days after laydate") %>%
  mutate(Chisq = Chisq %>% round(digits = 2), AIC = AIC %>% round(digits = 0)) %>%
  mutate(across(c("Pr(>Chisq)","Temp Coef","Temp^2 Coef"),round, digits = 3)) %>%
  select(AIC,Chisq,`Pr(>Chisq)`,`Temp Coef`,`Temp^2 Coef`,lu,Model) %>%
  rename(P = `Pr(>Chisq)`) %>%
  mutate(lu = factor(lu,levels = c("Forest","Agriculture","Natural open","Developed"))) %>%
  mutate(across(c("AIC","Chisq","Temp Coef","Temp^2 Coef"),as.character)) %>%
  rename(`Precip coef` = "Temp Coef",`Precip^2 coef` = "Temp^2 Coef") %>%
  mutate(P = if_else(P == 0.000, "<0.001",as.character(P))) %>%
  mutate(Model = fct_recode(Model,`PA + PA^2` = "Var + Var^2",PA = "Var",`No precip term` = "No Var term")) %>%
  # group_by(lu) %>%
  pivot_longer(-c(lu, Model)) %>%
  pivot_wider(names_from=c(lu, name), values_from=value) %>%
  # rename(Statistic = name) %>%
  # select(-row) %>%
  # mutate(`Agriculture_P` = if_else(`Agriculture_P` == 0.000,"<0.001",as.character(`Agriculture_P`))) %>%
  # mutate(`Forest_P` = if_else(`Forest_P` == 0.000,"<0.001",as.character(`Forest_P`))) %>%
  # mutate(`Developed_P` = if_else(`Developed_P` == 0.000,"<0.001",as.character(`Developed_P`))) %>%
  # mutate(`Natural open_P` = if_else(`Natural open_P` == 0.000,"<0.001",as.character(`Natural open_P`))) %>%
  # group_by(lu) %>%
  gt() %>% 
  tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(delim="_") %>%
  tab_style(cell_text(style = "italic"),cells_row_groups())  %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = c(`Agriculture_Precip^2 coef`,`Forest_Precip^2 coef`,`Natural open_Precip^2 coef`),
    )
  )

setwd("figures")
gtsave(filename = "TS9_squared-validation_pcp.html", data = tab_sq_pcp)
setwd("..")

##### Table S5: nests in each land cover subcategory.

tab <- read_csv("figures/HabitatTable.csv") %>% 
  rename(`Number of attempts` = NumberOfAttempts, `Fraction of total attempts` = FractionOfTotalAttempts,`Fraction of category attempts` = FractionOfCategoryAttempts) %>%
  gt() %>%
  tab_options(data_row.padding = px(1))

setwd("figures")
gtsave(filename = "TS5_habitatTable.html", data = tab)
setwd("..")

##### Table 7: Bayesian table

bayes <- read_csv("figures/jags_table_103122.csv") %>%
  #filter(if_any(Agriculture:`Natural Open`, ~ str_count(.x,"-") == 0 | str_count(.x,"-") == 3)) %>%
  gt() %>%
  tab_options(data_row.padding = px(1)) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightgreen"),
      cell_text(weight = "bold")
    ),
    locations = list(
      cells_body(
        columns = Agriculture,
        rows = str_count(Agriculture,"-") == 0 | str_count(Agriculture,"-") == 3),
      cells_body(
        columns = Forest,
        rows = str_count(Forest,"-") == 0 | str_count(Forest,"-") == 3),
      cells_body(
        columns = Developed,
        rows = str_count(Developed,"-") == 0 | str_count(Developed,"-") == 3),
      cells_body(
        columns = `Natural Open`,
        rows = str_count(`Natural Open`,"-") == 0 | str_count(`Natural Open`,"-") == 3))) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = c(Agriculture, Forest, Developed),
    )
  )

setwd("figures")
gtsave(filename = "TS7_bayes.html", data = bayes)
setwd("..")

###### Full output tables for traits (Table S10)
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
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = c(`NABCI conservation score_P-value`,`Cavity nest or not_P-value`),
    )
  )

gtsave(filename = "figures/TS10_trait_table.html", data = trend_table)

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
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = c(`Conservation score_P`,`Cavity vs not_P`),
    )
  )

gtsave(filename = "figures/TS11_triple_validation.html", data = tab_triple)

# Full output for spatial models (Table S12)

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
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = c(`Mean historical max spring temp~P-value`,`Mean annual temp relative to species range average~P-value`,`Mean max historical nest period temp~P-value`),
    )
  )

setwd("figures")
gtsave(filename = "TS12_spatial_table.html", data = spatial_table)
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
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(
    delim="~"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = c(`Mean max historical nest period temp~P`,`Mean historical max spring temp~P`,`Mean annual temp relative to species range average~P`),
    )
  )

gtsave(filename = "figures/TS14_triple_validation_spatial.html", data = tab_triple_spatial)

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
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_spanner_delim(
    delim="~"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = c(`Mean max historical nest period temp~P`,`Mean historical max spring temp~P`,`Mean annual temp relative to species range average~P`),
    )
  )

gtsave(filename = "figures/TS13_spatial_hotnessxanomaly_validation.html", data = tab_cavity)

#### Table S15: predicted mean change in nest success

require(rio)

delta <- import("figures/TS12_predicted average change in nest success between current and future predictions.xlsx") %>% tibble() %>%
  mutate(across(`Mid-century RCP 4.5`:`End-century RCP 8.5`, ~ round(.x,digits = 4))) %>%
  gt() %>% tab_options(data_row.padding = px(1)) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1.5),
      color = "lightgrey"
    ),
    locations = cells_body(
      columns = c(`Mid-century RCP 4.5`,`End-century RCP 4.5`,`Mid-century RCP 8.5`),
    )
  )

gtsave(filename = "figures/TS15_predictedchange.html", data = delta)

#### Random checks

ls500m <- read_rds("results/revisions/mainv1_500m_quad.rds")
ls1km <- read_rds("results/revisions/mainv1_1km_quad.rds")
ls2km <- read_rds("results/revisions/mainv1_withregion_quad.rds")

anova(ls2km,ls1km,ls500m)

library(car)

read_rds("results/revisions/mainv1_withregion.rds") %>% vif()

forest.lat <- aov(lat~NewLU1,data = nest)
summary(forest.lat)
TukeyHSD(forest.lat)

boxplot(lat~NewLU1, data = nest)

forest.ele <- aov(elevation~NewLU1,data = nest)
summary(forest.ele)
TukeyHSD(forest.ele)

boxplot(elevation~NewLU1, data = nest)

##### Moran's I output table (not yet numbered, but will go in supplement)

moran_tab <- read_csv("Manuscript/Revisions/autocorr_model_tracking.csv") %>%
  filter(model %in% c("mainv1_withregion","mainv1_noregion","mainv1_res15quad","mainv1_res16quad","mainv1_res17quad")) %>%
  select(model,autocorrelation,p.value) %>%
  mutate(model = factor(.$model,levels = c("mainv1_withregion","mainv1_noregion","mainv1_res15quad","mainv1_res16quad","mainv1_res17quad"),ordered = TRUE)) %>%
  arrange(model) %>%
  mutate(autocorrelation = c("UnCoor nested in Region","None","Region with data thinned to resolution 15","Region with data thinned to resolution 16","Region with data thinned to resolution 17")) %>%
  mutate_at(c("p.value"),signif, digits = 3) %>%
  mutate(p.value = format(p.value,scientific = FALSE,drop0trailing = TRUE)) %>%
  rename(Model = model,Autocorrelation = autocorrelation,`P-value` = p.value) %>%
  # mutate(p.value = if_else(p.value == 0.000,"<0.001",as.character(p.value))) %>%
  gt() %>%
  tab_options(data_row.padding = px(1))

setwd("figures")
gtsave(filename = "TSX_morani.html", data = moran_tab)
setwd("..")

full <- read_rds("results/revisions/mainv1_withregion.rds")
thin <- read_rds("results/revisions/mainv1_res16quad.rds")

nest %>% group_by(at_least_one_success) %>% summarize(n())

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
