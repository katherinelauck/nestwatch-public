#!/bin/bash
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/annualmeantemp3way.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax3way_daymet.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/stdmax3way_daymet.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax3way_gridmet.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/stdmax3way_gridmet.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax3way_gridmet_exp.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/stdmax3way_gridmet_exp.sh
