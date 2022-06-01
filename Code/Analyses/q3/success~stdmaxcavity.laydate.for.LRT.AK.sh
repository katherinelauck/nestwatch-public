#!/bin/bash -l

# setting name of job
#SBATCH --job-name=stdmaxcavity.laydate.for.LRT.AK

# setting home directory
#SBATCH -D /home/kslauck/projects/nestwatch

# setting standard error output
#SBATCH -e /home/kslauck/projects/nestwatch/slurm_log/sterror_%j.txt

# setting standard output
#SBATCH -o /home/kslauck/projects/nestwatch/slurm_log/stdoutput_%j.txt

# setting medium priority
#SBATCH -p med

# setting the max time
#SBATCH -t 18:00:00

# mail alerts at beginning and end of job
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

# send mail here
#SBATCH --mail-user=kslauck@ucdavis.edu

# now we'll print out the contents of the R script to the standard output file
cat Code/Analyses/q3/success~stdmaxcavity.laydate.for.LRT.AK.R
echo "ok now for the actual standard output"

# now running the actual script!


# load R
module load R

Rscript Code/Analyses/q3/success~stdmaxcavity.laydate.for.LRT.AK.R
