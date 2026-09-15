#!/bin/bash
# gcamsdg cluster job template, used by run(cluster = TRUE).
#
# Edit the #SBATCH directives below ONCE for your own cluster account,
# partition, and resource needs. run(cluster = TRUE, sbatch_args = list(...))
# can override any of these per-call without touching this file (e.g.
# sbatch_args = list(time = "48:00:00")).
#
#SBATCH --job-name=__JOB_NAME__
#SBATCH --output=__LOG_PATH__
#SBATCH --partition=general       # <- set to your cluster's partition/queue
#SBATCH --account=your_account    # <- set to your cluster account/allocation
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=24:00:00

# Load whatever modules your cluster needs for R (and, if the "land"
# indicator is requested, Demeter's conda environment - gcamsdg activates
# that internally via reticulate, using the conda_env passed to run()).
module load R

# The Rscript invocation below is appended automatically by
# .submit_gcamsdg_cluster_job(); do not add your own "Rscript run_from_args.R"
# line here.
