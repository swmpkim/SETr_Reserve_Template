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


## analytic report
infile <- here::here("R_scripts", "sourced", "005_rate_calculations.Rmd")
outdir <- here::here("R_output", "analysis")
outfile <- paste0("SET_Analyses_", Sys.Date(), ".docx")
rmarkdown::render(infile, output_dir = outdir, output_file = outfile)

# look for the desired file and generate a message (to be printed at the end) based on the result
msg1 <- ifelse(file.exists(paste0(outdir, "/", outfile)), 
               paste0("\n \nYour analytical report has been generated. Navigate to R_output/analysis and you will find '", outfile, "'. \n"),
               "\n \nSomething has gone wrong. Please scroll up and see if there's a line that starts with 'Quit from line ___'; copy that and email it to Kim Cressman for help. \n")



## outreach report
infile <- here::here("R_scripts", "sourced", "0055_outreach_graphics.Rmd")
outdir <- here::here("R_output", "outreach_doc")
outfile <- paste0("SET_Outreach_", Sys.Date(), ".docx")
rmarkdown::render(infile, output_dir = outdir, output_file = outfile)

# look for the desired file and generate a message based on the result
msg2 <- ifelse(
    file.exists(paste0(outdir, "/", outfile)), 
    paste0("\n \nYour outreach summary report has been generated. Navigate to R_output/outreach_doc and you will find '", outfile, "'. \n \n"),
    "\n \nSomething has gone wrong. Please scroll up and see if there's a line that starts with 'Quit from line ___'; copy that and email it to Kim Cressman for help. \n \n"
)

# print both messages
message(c(msg1, msg2))
