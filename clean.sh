#!/usr/bin/env bash

module load Anaconda3/2022.05
# module load NCO/5.0.1-foss-2021a
source activate $DATA/envs/snakemake

# PYTHONPATH sometimes causes issues
export PYTHONPATH=

echo "Any cleaning?"
select yn in "None" "Logs and snakemake lock" "Logs only" "Lock only"; do
    case $yn in
        "None" ) break;;
        "Logs and snakemake lock" ) snakemake --profile $HOME/.config/snakemake/slurm.arc --cores 1 --use-conda --conda-base-path $DATA --unlock; rm -f log/*.log; break;;
        "Logs only" ) rm -f log/*.log; break;;
        "Lock only" ) snakemake --profile $HOME/.config/snakemake/slurm.arc --cores 1 --use-conda --conda-base-path $DATA --unlock; break;;
    esac
done

