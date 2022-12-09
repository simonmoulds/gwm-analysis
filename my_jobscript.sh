#!/usr/bin/env bash
#SBATCH --partition=short
#SBATCH -o slurm-%j.out
#SBATCH -e slurm-%j.out
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=simon.moulds@ouce.ox.ac.uk
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=12
#SBATCH --job-name=esmvaltool
#SBATCH --time=12:00:00

# cd $SCRATCH || exit 1

module load Anaconda3/2020.11
module load NCO/5.0.1-foss-2021a
source activate $DATA/envs/esmvaltool

# PYTHONPATH sometimes causes issues
export PYTHONPATH=

snakemake --profile $HOME/.config/snakemake/slurm.arc --cores 1 --use-conda --conda-basepath $DATA --rerun-incomplete

# smcmd="snakemake --profile $HOME/.config/snakemake/slurm.arc $@"
# cpu=1

# echo "Any cleaning?"
# select yn in "None" "Logs and snakemake lock" "Logs only" "Lock only"; do
#     case $yn in
#         "None" ) break;;
#         "Logs and snakemake lock" ) snakemake --use-conda --unlock; rm -f log/*.log; break;;
#         "Logs only" ) rm -f log/*.log; break;;
#         "Lock only" ) snakemake --use-conda --unlock; break;;
#     esac
# done

# echo "Time of job (hours, 0 to not submit master, negative to submit as single job)? (default: 12)"
# read hrs
# hrs="${hrs:=12}"
# if (( $hrs > 12 )); then
#     p=medium
# else
#     p=short
# fi

# if (( $hrs < 0 )); then
#     echo "N CPUs for single job? (default 1)"
#     read cpu
#     cpu="${cpu:=1}"
#     smcmd="snakemake --use-conda -j $cpu  $@"
#     hrs=$(($hrs*-1))
# fi

# if (( $hrs > 0 )); then
#     sbatch -p $p -c $cpu --time="$hrs:00:00" --wrap="$smcmd" -J sm_master_$(basename $(pwd))
# else
#     $smcmd
# fi
