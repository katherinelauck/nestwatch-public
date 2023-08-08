source("Code/helper-functions.R")
# run_moran_i(list.files(path = "results/revisions",pattern = "(mainv2\\.rds$)|(mainv2_noregion\\.rds$)",full.names = TRUE),
#             prop = .1,
#             n = 500,
#             plan = list(sequential,tweak(multisession,workers = 7)),
#             outfile = "results/revisions/moran.i_mainv2.rds")


run_moran_i(list.files(path = "results/revisions",pattern = "mainv1_5perhexres[[:digit:]]+quad",full.names = TRUE),
            prop = 1,
            n = 1,
            plan = list(multisession, sequential),
            outfile = "results/revisions/moran.i_mainv1_5perhex.rds")
