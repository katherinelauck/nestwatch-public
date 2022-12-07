require(furrr)
require(lme4)

models <- c("Code/Analyses/revisions/mainv1_region150eletemplonlat.R",
            "Code/Analyses/revisions/mainv1_region150eletemplat.R",
            "Code/Analyses/revisions/mainv1_region150eletemp.R",
            "Code/Analyses/revisions/mainv1_region150lonlat.R",
            "Code/Analyses/revisions/mainv1_region150lat.R",
            "Code/Analyses/revisions/mainv1_eletemplonlat.R",
            "Code/Analyses/revisions/mainv1_eletemplat.R",
            "Code/Analyses/revisions/mainv1_eletemp.R",
            "Code/Analyses/revisions/mainv1_lonlat.R",
            "Code/Analyses/revisions/mainv1_lat.R")

plan(multisession)

future_walk(models,source)
