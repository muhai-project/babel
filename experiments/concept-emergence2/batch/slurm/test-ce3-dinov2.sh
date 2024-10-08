#!/bin/bash
#SBATCH --job-name=test-ce3-dinov2
#SBATCH --error=/user/brussel/101/vsc10156/concept-emergence2/batch/slurm/logs/test-ce3-dinov2_%a_e.txt
#SBATCH --output=/user/brussel/101/vsc10156/concept-emergence2/batch/slurm/logs/test-ce3-dinov2_%a_o.txt
#SBATCH --time=24:00:00
#SBATCH	--ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --array=1

# load atools
module purge
module load atools/1.5.1-GCCcore-12.3.0

# read input data from csv
source <(aenv --data $VSC_HOME/concept-emergence2/batch/data-test/test-ce3-dinov2.csv --sniff 4096)

# load sbcl
module purge
module load SBCL/2.4.1-GCCcore-12.3.0

# run script
sbcl --dynamic-space-size 70000 --load $VSC_HOME/concept-emergence2/batch/test.lisp \
    exp-name $exp_name \
    nr-of-interactions $nr_of_interactions \
    dataset $dataset \
    dataset-split $dataset_split \
    available-channels "$available_channels" \
    scene-sampling $scene_sampling \
    topic-sampling $topic_sampling