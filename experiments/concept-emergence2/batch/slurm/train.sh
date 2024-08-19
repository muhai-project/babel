#!/bin/bash

# load atools
module purge
module load atools/1.5.1-GCCcore-12.3.0

# read input data from csv
source <(aenv --data $VSC_HOME/concept-emergence2/batch/data-train/$name.csv --sniff 4096)

# load sbcl
module purge
module load SBCL/2.4.1-GCCcore-12.3.0

# run script
sbcl --dynamic-space-size $space --load $VSC_HOME/concept-emergence2/batch/run.lisp \
    exp-name $exp_name \
    nr-of-series $nr_of_series \
    nr-of-interactions $nr_of_interactions \
    population-size $population_size \
    dataset $dataset \
    dataset-split $dataset_split \
    available-channels "$available_channels" \
    disable-channels $disable_channels \
    amount-disabled-channels $amount_disabled_channels \
    sensor-noise $sensor_noise \
    sensor-std $sensor_std \
    observation-noise $observation_noise \
    observation-std $observation_std \
    scene-sampling $scene_sampling \
    topic-sampling $topic_sampling \
    similarity-threshold $similarity_threshold \
    align $align \
    entrenchment-incf $entrenchment_incf \
    entrenchment-decf $entrenchment_decf \
    entrenchment-li $entrenchment_li \
    trash-concepts $trash_concepts \
    weight-update-strategy $weight_update_strategy \
    initial-weight $initial_weight \
    weight-incf $weight_incf \
    weight-decf $weight_decf \
    switch-condition $switch_condition \
    switch-conditions-after-n-interactions $switch_conditions_after_n_interactions \
    stage-parameters "$stage_parameters" \
    seed $seed \
    exp-top-dir $exp_top_dir