import argparse
import csv
import os

parser = argparse.ArgumentParser()
parser.add_argument("--csv", type=str)

# assumes folder structure:
# batch/
#   bash/
#       create.py
#       run.lisp
#       scripts/
#           script_1.sh
#           ...
#           script_x.sh
template = """\
#!/bin/bash

sbcl --dynamic-space-size 16000 --non-interactive --load test.lisp \\
    exp-name {exp_name} \\
    nr-of-interactions {nr_of_interactions} \\
    dataset-loader {dataset_loader} \\
    min-context-size {min_context_size} \\
    max-context-size {max_context_size} \\
    dataset "({dataset})" \\
    dataset-split {dataset_split} \\
    feature-set "({feature_set})" \\
    scene-sampling {scene_sampling} \\
    topic-sampling {topic_sampling} \\
    seed ${{1}} \\
    exp-top-dir ${{2}}
"""


def create_bash_script(row):
    return template.format(**row)


def main(input_file, output_dir, exp_fname):
    os.makedirs(output_dir, exist_ok=True)
    with open(input_file, "r") as csv_file:
        csv_reader = csv.DictReader(csv_file)

        for idx, row in enumerate(csv_reader, start=1):
            script_content = create_bash_script(row)
            with open(f"{output_dir}/{exp_fname}_{idx}.sh", "w") as script_file:
                script_file.write(script_content)


if __name__ == "__main__":
    args = parser.parse_args()
    exp_fname = args.csv
    input_csv_file = f"config/test/{exp_fname}.csv"
    output_directory = "bash/scripts"

    main(input_csv_file, output_directory, exp_fname)
