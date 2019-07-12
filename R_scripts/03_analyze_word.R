# Generate Microsoft Word report of basic SET data analyses

#### INSTRUCTIONS ############################################################

# 1:
# Re-start your R session to make sure there's no interference:
# From the menu bar, select 'Session', then 'Restart R'
# Windows keyboard shortcut is Ctrl + Shift + F10

# 2:

# Select this entire script. 
# Keyboard shortcut is Ctrl + a on windows or Cmd + a on Mac
# Run it: either using the "Run" button in the upper right corner
# or the keyboard shortcut Ctrl/Cmd + Enter

##############################################################################


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

if(file.exists(paste0(outdir, "/", outfile))){
    message(paste0("\n \nYour report has been generated. Navigate to R_output/analysis and you will find '", outfile, "'. \n \n"))
} else {
    message("\n \nVery sorry; something has gone wrong. Contact Kim Cressman so she can figure it out and fix it. \n \n")
}


