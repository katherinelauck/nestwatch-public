#!/bin/bash
#
# These models use gridmet and do not include min anomaly.
# Substrate is either a fixed effect or a binary fixed effect - one set of iterations for each.

sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax3way_gridmet_nomin_subbin.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax2way_gridmet_nomin_subbin.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax3way_gridmet_nosubstrate.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax2way_gridmet_nosubstrate.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax3way_gridmet_nomin.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax2way_gridmet_nomin.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax3way_gridmet_nomin_nosubstrate.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax2way_gridmet_nomin_nosubstrate.sh
