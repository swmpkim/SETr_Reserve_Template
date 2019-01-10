# Use this script as a wrapper to run the analysis script, but put output where we want it
library(here)
library(rmarkdown)

# this uses render BUT
# IT USES THE CURRENT GLOBAL ENVIRONMENT
# SO SHOULD ONLY BE RUN IN A CLEAN R SESSION

infile <- here::here("R_scripts", "analysis", "005_rate_calculations.Rmd")
outdir <- here::here("R_output", "analysis")
render(infile, output_dir = outdir)



# this generates html into a different directory, but not other file types

# library(ezknitr)
# topdir <- here::here()
# https://github.com/ropensci/ezknitr#readme
# ezknit(file = "R_scripts/analysis/005_rate_calculations.Rmd",
# out_dir = "R_output/analysis",
# fig_dir = "figs",
# wd = topdir)