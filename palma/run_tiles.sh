#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --partition=zen4
#SBATCH --time=48:00:00
#SBATCH --array=1-20
#SBATCH --job-name=emd_AEF_tiles
#SBATCH --output=/home/o/obinzami/emb_logs/%x_part_%a.txt
#SBATCH --mail-type=ALL
#SBATCH --mail-user=o.bin.zamir@uni-muenster.de


cd $WORK/Embedding_BWI_pro_download
module purge
source palma/modules.sh
export GDAL_CACHEMAX=1024
export EMB_OUT=$WORK/embed_bwi/output

while true; do
  Rscript main.r $SLURM_ARRAY_TASK_ID ${N_PARTS:-20} ${MAX_NEW:-30} ${EMB:-alphaEarth}
  [ $? -eq 10 ] || break
done