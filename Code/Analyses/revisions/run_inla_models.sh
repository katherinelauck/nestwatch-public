#!/bin/bash -l

#SBATCH -p med                                                                  # setting medium priority
#SBATCH --job-name=run_inla_models                                               # setting name of job
#SBATCH -c 1                                                                    # number of cores
#SBATCH --mem=10G                                                              # setting the memory per cpu
#SBATCH -t 10-00:00:00                                                            # setting the max time
#SBATCH -D /home/kslauck/projects/nestwatch                                     # setting home directory
#SBATCH -e /home/kslauck/projects/nestwatch/slurm_log/sterror_%j.txt            # setting standard error output
#SBATCH -o /home/kslauck/projects/nestwatch/slurm_log/stdoutput_%j.txt          # setting standard output
#SBATCH --mail-type=BEGIN                                                       # mail alerts at beginning and end of job
#SBATCH --mail-type=END
#SBATCH --mail-user=kslauck@ucdavis.edu                                         # send mail here

# Standard output file contents
echo "## Script:"
cat Code/Analyses/revisions/run_inla_models.R
echo "## Mesh:" $1
echo "## Type:" $2
echo "## Output:"

# initialize mamba
. ~/mambaforge/etc/profile.d/mamba.sh

# activate mamba environment
mamba activate nestwatch

# fail on weird errors
set -e
set -x

# Run script
Rscript Code/Analyses/revisions/run_inla_models.R $1 $2

# Print out values of the current jobs SLURM environment variables
env | grep SLURM

# Print out final statistics about resource use before job exits
scontrol show job ${SLURM_JOB_ID}

sstat --format 'JobID,MaxRSS,AveCPU' -P ${SLURM_JOB_ID}.batch
