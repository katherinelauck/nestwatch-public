##### Helper function for writing models
##### Author: Katherine Lauck
##### Last updated: 14 December 2021

write_model <- function(model_file){
  require(tidyverse)
  if(grepl(".*success.*",model_file)){
    response <- "success"
  } else if(grepl(".*failure.*",model_file)){
    response <- "failure"
  }
  name <- gsub("\\.RDS|\\.rds","",str_extract(model_file,"((success~)|(failure~)).*$"))
  model <- read_rds(model_file)
  question <- str_extract(model_file,"(q12)|(q3)|(q4)")
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

run_all_models <- function(model_scripts){
  q12 <- list.files("Code/Analyses/q12",pattern = "sh$",full.names = TRUE)
  q3 <- list.files("Code/Analyses/q3",pattern = "sh$",full.names = TRUE)
  q4 <- list.files("Code/Analyses/q4",pattern = "sh$",full.names = TRUE)
  all <- c(q12,q3,q4)
  cat("#!/bin/bash",paste0("sbatch /home/kslauck/projects/nestwatch/",all),
      file = "Code/Analyses/run_all_models.sh",
      sep = "\n")
}
