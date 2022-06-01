#!/bin/bash
#
# These models use gridmet.
# Substrate is a binary fixed effect.
# Minimum temp anomaly is included in half of models.

sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax3way_gridmet_nomin_subbin.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax2way_gridmet_nomin_subbin.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax3way_gridmet_subbin.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/meanmax2way_gridmet_subbin.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/stdmax3way_gridmet_nomin_subbin.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/stdmax2way_gridmet_nomin_subbin.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/stdmax3way_gridmet_subbin.sh
sbatch /home/kslauck/projects/nestwatch/Code/Analyses/stdmax2way_gridmet_subbin.sh