# Use this script as a wrapper to run the analysis script, but put output where we want it
# and choose its name

library(here)
library(rmarkdown)

# this uses render BUT
# IT USES THE CURRENT GLOBAL ENVIRONMENT
# SO SHOULD ONLY BE RUN IN A CLEAN R SESSION

# some insurance that there are no items in the environment:
rm(list = ls())


infile <- here::here("R_scripts", "sourced", "005_rate_calculations.Rmd")
outdir <- here::here("R_output", "analysis")
outfile <- paste0("SET_Analyses_", Sys.Date(), ".docx")
rmarkdown::render(infile, output_dir = outdir, output_file = outfile)

