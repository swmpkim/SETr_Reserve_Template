# Use this script as a wrapper to run the analysis script, but put output where we want it
library(here)
library(rmarkdown)

# this uses render BUT
# IT USES THE CURRENT GLOBAL ENVIRONMENT
# SO SHOULD ONLY BE RUN IN A CLEAN R SESSION

infile <- here::here("R_scripts", "analysis", "005_rate_calculations.Rmd")
outdir <- here::here("R_output", "analysis")
rmarkdown::render(infile, output_dir = outdir)