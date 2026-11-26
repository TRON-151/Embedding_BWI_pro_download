#!/bin/bash

# dequantizes the AlphaEarth and OlmoEarth tiles (int8 -> float32) with dequantize.r
# 20 parts, each part takes every 20th file. files that are done are skipped,
# so the same command can be sent again after a time limit or a failed part.
# test first with testing <- TRUE in dequantize.r, as one part that does all 10 test files:
#   sbatch --array=1 --export=ALL,N_PARTS=1 palma/run_dequant.sh

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --partition=zen4
#SBATCH --time=24:00:00
#SBATCH --array=1-40
#SBATCH --job-name=dequant_tiles
#SBATCH --output=/home/o/obinzami/emb_logs/%x_part_%a.txt
#SBATCH --mail-type=ALL
#SBATCH --mail-user=o.bin.zamir@uni-muenster.de


cd $WORK/Embedding_BWI_pro_download
module purge
source palma/modules.sh
export GDAL_CACHEMAX=1024
export EMB_OUT=$WORK/embed_bwi/output

# N_PARTS stays 20 even if you send only some parts again (sbatch --array=7 ...),
# so part 7 is still the same files. change it only together with --array
Rscript dequantize.r tiles $SLURM_ARRAY_TASK_ID ${N_PARTS:-40}