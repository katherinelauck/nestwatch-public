##### Helper function for writing models
##### Author: Katherine Lauck
##### Last updated: 14 December 2021

write_model <- function(model_file){
  require(tidyverse)
  if(!(grepl(".*failure.*",model_file))){
    response <- "success"
  } else if(grepl(".*failure.*",model_file)){
    response <- "failure"
  }
  name <- gsub("\\.RDS|\\.rds","",str_extract(model_file,"((success~)|(failure~)).*$"))
  model <- read_rds(model_file)
  question <- str_extract(model_file,"(q12)|(q3)|(q4)|(revisions)")
  depend <- paste0("library(\"lme4\"); library(tidyverse); nest <- read_rds('Data/active/",response,"-cleaned.rds')")
  model_code <- paste0("mod<-",paste(trimws(deparse(model@call)),collapse = " "))
  filename <- paste0("write_rds(mod,\"results/",question,"/",name,".rds\")")
  cat(depend,model_code,filename,file = paste0("Code/Analyses/",question,"/",name,".R"),sep = "\n")
  
  sh.name <- gsub("success~","",name)
  cat("#!/bin/bash -l\n\n# setting name of job",
      paste0("#SBATCH --job-name=",sh.name),
      "\n# setting home directory\n#SBATCH -D /home/kslauck/projects/nestwatch\n\n# setting standard error output\n#SBATCH -e /home/kslauck/projects/nestwatch/slurm_log/sterror_%j.txt\n\n# setting standard output\n#SBATCH -o /home/kslauck/projects/nestwatch/slurm_log/stdoutput_%j.txt\n\n# setting medium priority\n#SBATCH -p med\n\n# setting the max time\n#SBATCH -t 18:00:00\n\n# mail alerts at beginning and end of job\n#SBATCH --mail-type=BEGIN\n#SBATCH --mail-type=END\n\n# send mail here\n#SBATCH --mail-user=kslauck@ucdavis.edu\n\n# now we'll print out the contents of the R script to the standard output file",
      paste0("cat ",paste0("Code/Analyses/",question,"/",name,".R")),
      "echo \"ok now for the actual standard output\"\n\n# now running the actual script!\n\n\n# load R\nmodule load R\n",
      paste0("Rscript ",paste0("Code/Analyses/",question,"/",name,".R")),
      file = paste0("Code/Analyses/",question,"/",name,".sh"), sep = "\n")
}

run_all_models <- function(dir = "Code/Analyses/revisions",pattern = "\\.sh$",loc = "cluster"){
  require(tidyverse)
  files <- list.files(dir,full.names = TRUE) %>% str_subset(pattern)
  files <- files[!(str_detect(files,"(run_all_models)|(morani_cluster)"))]
  if(loc == "cluster") {
  cat("#!/bin/bash",
      paste0("sbatch /home/kslauck/projects/nestwatch/",files),
      file = paste0(dir,"/","run_all_models.sh"),
      sep = "\n")
  } else if(loc == "lab") {
    cat("require(furrr)",
        paste0("files <- c(\"",
        paste(files,collapse = "\",\""),
        "\")",collapse = ""),
        "plan(multisession)",
        "future_map(files,source)",
        file = paste0(dir,"/","run_all_models.R"),
        sep = "\n")
  }
}


write_bash <- function(dir) {
  require(tidyverse)
  write <- function(model_code_filename) {
    name <- gsub("\\.R|\\.r","",str_extract(model_code_filename,"(?<=revisions/).*$"))
    question <- str_extract(model_code_filename,"(q12)|(q3)|(q4)|(revisions)")
    sh.name <- gsub("success~","",name)
    cat("#!/bin/bash -l\n\n# setting name of job",
        paste0("#SBATCH --job-name=",sh.name),
        "\n# setting home directory\n#SBATCH -D /home/kslauck/projects/nestwatch\n\n# setting standard error output\n#SBATCH -e /home/kslauck/projects/nestwatch/slurm_log/sterror_%j.txt\n\n# setting standard output\n#SBATCH -o /home/kslauck/projects/nestwatch/slurm_log/stdoutput_%j.txt\n\n# setting medium priority\n#SBATCH -p med\n\n# setting the max time\n#SBATCH -t 18:00:00\n\n# mail alerts at beginning and end of job\n#SBATCH --mail-type=BEGIN\n#SBATCH --mail-type=END\n\n# send mail here\n#SBATCH --mail-user=kslauck@ucdavis.edu\n\n# now we'll print out the contents of the R script to the standard output file",
        paste0("cat ",paste0("Code/Analyses/",question,"/",name,".R")),
        "echo \"ok now for the actual standard output\"\n\n# now running the actual script!\n\n\n# load R\nmodule load R\n",
        paste0("Rscript ",paste0("Code/Analyses/",question,"/",name,".R")),
        file = paste0("Code/Analyses/",question,"/",name,".sh"), sep = "\n")
    }
  files <- list.files(dir,full.names = TRUE) %>% str_subset("\\.R$")
  walk(files,write)
  }

run_moran_i <- function(models,prop = 1,n = 1,plan = list(sequential),outfile) {
  # models: character vector of model files (full names needed for compatibility with Linux/cluster) that you want to run Moran's I test on
  # prop: proportion of rows to sample for bootstrapping, if needed
  # n: number of replicate samples per model for bootstrapping, if needed
  # plan: parallel processing plan, see package furrr for more details. Must be a list. Parallel processing may operate on one or both levels, the top level being per model and the second level being per sample. So far I've had the best results just parallel processing at the model level.
  # outfile: string specifying save location of output
  require(tidyverse)
  require(ape)
  require(furrr)
  require(purrr)
  require(lme4)
  require(future.batchtools)
  require(future.apply)
  
  bootstrap.moran.i <- function(model,prop){
    m <- read_rds(model)
    if(class(m) == 'inla') {
      UnCoor <- read_rds('results/revisions/autocorr_data.rds') %>% pull(UnCoor)
      res <- m$residuals$deviance.residuals %>% tibble()
    } else if("UnCoor" %in% names(m@frame)) {
      UnCoor <- m@frame %>% pull(UnCoor)
      res <- resid(m) %>% tibble()
    } else if(str_replace(model,"(quad)?(\\.rds)","data.rds") %in% list.files(path = "results/revisions",full.names = TRUE)) {
      UnCoor <- str_replace(model,"(quad)?(\\.rds)","data.rds") %>%
        read_rds() %>%
        pull(UnCoor)
      res <- resid(m) %>% tibble()
    } else {
      stop("Provide UnCoor source for distance matrix construction")
    }
    loc <- str_split_fixed(UnCoor,"_",n=2) %>%
      as_tibble(.name_repair = "universal") %>% 
      rename(lon = "...1",lat = "...2") %>%
      mutate(row_num = row_number())
    loc_sample <- slice_sample(loc, prop = prop)
    nest.dists.inv <- 1/as.matrix(dist(cbind(as.numeric(loc_sample$lon),as.numeric(loc_sample$lat))))
    nest.dists.inv[is.infinite(nest.dists.inv)] <- 0
    p <- res %>% slice(c(pull(loc_sample,row_num))) %>% pull() %>% Moran.I(nest.dists.inv)
  }
  
  rep_moran <- function(model,prop,n){
    gc()
    print(paste0("starting ",model))
    future_replicate(n,{
      bootstrap.moran.i(model,prop)
    }, .options = furrr_options(seed = TRUE)) %>% t() %>% 
      as_tibble() %>% 
      unnest(cols = c(observed,expected,sd,p.value)) %>%
      mutate(model = model %>% str_replace("results/revisions/",""))
  }
  
  plan(plan)
  
  out <- future_map_dfr(models,rep_moran, prop = prop,n = n,.options = furrr_options(seed = TRUE))
  write_rds(out,outfile)
}

track_region_models <- function() {
  require(tidyverse)
  require(ape)
  require(furrr)
  require(purrr)
  require(lme4)
  require(future.batchtools)
  require(future.apply)
  
  # combine moran's I results files
  morani_results <- list.files(path = "results/revisions", full.names = TRUE,include.dirs = FALSE) %>%
    str_subset("(moran\\.i|morani)(?!(.*(small)?\\.csv))") %>%
    str_subset("(moran\\.i|morani)(?!(.*small))") %>%
    map(read_rds) %>%
    map_dfr(as_tibble) %>%
    group_by(model) %>%
    summarize(p.value = mean(p.value),observed = mean(observed))
  
  # create list of all attempted models
    # extract list of models that managed to run from combined moran's I object, add files that did not run
  models_all <- morani_results %>% 
    pull(model) %>%
    c(c("mainv1_region150elevationlatlon.R","mainv1_regionfixed.R","mainv1_regionfixedlatelevation.R"))
  
  extract_row <- function(mod,morani_results) {
  # For each attempted model, extract:
    # model file name
    name <- mod %>%
      str_replace("(\\.rds|\\.R)","")
    print(name)
    if(str_detect(mod,"\\.R") || !file.exists(paste0("results/revisions/",mod))) {
      run <- "N"
      converge <- NA
      p_value <- NA
      observed <- NA
      singular <- NA
      beta_ag <- NA
      beta_ag_p <- NA
      beta_forest <- NA
      beta_forest_p <- NA
      beta_open <- NA
      beta_open_p <- NA
      beta_human <- NA
      beta_human_p <- NA
      N <- NA
      if(str_detect(mod,"\\.R")) {
      autocorr <- paste0("Code/Analyses/revisions/",mod) %>%
        read_lines() %>%
        as_tibble() %>%
        slice(2) %>%
        pull(value) %>%
        str_extract_all("(lat \\* lon)|(lat_sq \\+ lon_sq)|(lat(?!( \\* lon)))|(elevation \\* Tmax_std_gridmet)|(elevation(?!( \\* Tmax_std_gridmet)))|(Region/UnCoor)|((Region[:digit:]{2,})/UnCoor)|(Region(?!(/UnCoor)))|(sollman)") %>%
        unlist() %>%
        str_c(collapse = " + ") }
      else {autocorr = "missing model file. Look at model name"}
    } else if(str_detect(mod,"inla")) {
      run <- "Y"
      tmp <- paste0("results/revisions/",mod) %>%
        read_rds()
      m <- tmp %>%
        summary()
      converge <- NA
      singular <- NA
      autocorr <- NA
      p_value <- morani_results %>%
        filter(model == mod) %>%
        pull(p.value)
      observed <- morani_results %>%
        filter(model == mod) %>%
        pull(observed)
      fix <- tmp$summary.fixed
      beta_ag <- fix["Tmax_std_gridmet.NewLU1Ag","mean"] %>%
        as.numeric()
      beta_ag_p <- NA
      beta_forest <- fix["Tmax_std_gridmet","mean"] %>%
        as.numeric()
      beta_forest_p <- NA
      beta_open <- fix["Tmax_std_gridmet.NewLU1Natural_open","mean"] %>%
        as.numeric()
      beta_open_p <- NA
      beta_human <- fix["Tmax_std_gridmet.NewLU1Human","mean"] %>%
        as.numeric()
      beta_human_p <- NA
      N <- NA
    } else if(str_detect(mod,"\\.rds")) {
      run <- "Y"
      tmp <- paste0("results/revisions/",mod) %>%
        read_rds()
      m <- tmp %>%
        summary()
      converge <- m$optinfo$conv$lme4$messages %>%
        str_detect("failed to converge") %>%
        any() %>%
        if_else("N","Y")
      singular <- m$optinfo$conv$lme4$messages %>%
        str_detect("singular") %>%
        any() %>%
        if_else("Y","N")
      autocorr <- formula(m) %>%
        as.character() %>%
        str_remove("\\n") %>%
        as_tibble() %>%
        slice(3) %>%
        pull(value) %>%
        str_extract_all("(lat \\* lon)|(lat_sq \\+ lon_sq)|(lat(?!( \\* lon)))|(elevation \\* Tmax_std_gridmet)|(elevation(?!( \\* Tmax_std_gridmet)))|(Region/UnCoor)|((Region[:digit:]{2,})/UnCoor)|(Region(?!(/UnCoor)))|(sollman)") %>%
        unlist() %>%
        str_c(collapse = " + ")
      if(str_detect(mod,"res[:digit:]{2}")){
        autocorr <- paste0(autocorr,
                           " + thinned to ",
                           str_extract(mod,"res[:digit:]{2}"))
      }
      p_value <- morani_results %>%
        filter(model == mod) %>%
        pull(p.value)
      observed <- morani_results %>%
        filter(model == mod) %>%
        pull(observed)
      fix <- tmp %>%
        fixef()
      cof <- tmp %>%
        summary() %>%
        coef()
      beta_ag <- fix["Tmax_std_gridmet:NewLU1Ag"] %>%
        as.numeric()
      beta_ag_p <- cof["Tmax_std_gridmet:NewLU1Ag",4] %>%
        as.numeric()
      beta_forest <- fix["Tmax_std_gridmet"] %>%
        as.numeric()
      beta_forest_p <- cof["Tmax_std_gridmet",4] %>%
        as.numeric()
      beta_open <- fix["Tmax_std_gridmet:NewLU1Natural_open"] %>%
        as.numeric()
      beta_open_p <- cof["Tmax_std_gridmet:NewLU1Natural_open",4] %>%
        as.numeric()
      beta_human <- fix["Tmax_std_gridmet:NewLU1Human"] %>%
        as.numeric()
      beta_human_p <- cof["Tmax_std_gridmet:NewLU1Human",4] %>%
        as.numeric()
      N <- tmp %>%
        nobs()
    }
    tibble(model = name,
           autocorrelation = autocorr,
           succeed = run,
           convergence = converge,
           singular = singular,
           beta_ag = beta_ag,
           beta_ag_p = beta_ag_p,
           beta_forest = beta_forest,
           beta_forest_p = beta_forest_p,
           beta_open = beta_open,
           beta_open_p = beta_open_p,
           beta_human = beta_human,
           beta_human_p = beta_human_p,
           N = N,
           p.value = p_value,
           observed = observed) %>% return()
  }
  # call extract_row on list of files, return object
  out <- map_dfr(models_all,extract_row,morani_results = morani_results)
  write_csv(out,"Manuscript/revision3/autocorr_model_tracking.csv")
  return(out)
}

replace_model_code <- function(dir,model_search_pattern,pattern,replacement) {
  require(readr)
  files <- list.files(path = paste0("Code/Analyses/",dir),full.names = TRUE) %>% str_subset(model_search_pattern)
  tmp <- function(file) {
  name <- file
  contents <- read_file(file) %>% str_replace_all(pattern,replacement)
  write_file(contents,file = name)
  }
  walk(files,tmp)
}
