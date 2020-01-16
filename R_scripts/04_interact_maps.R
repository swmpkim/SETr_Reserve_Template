## map-making


#### INSTRUCTIONS ############################################################
## This can only be run AFTER running '03_analyze_word.R'
## because it uses data generated during that analysis

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


library(leaflet)
library(here)
library(dplyr)

# where to look for the input file
file_in <- here::here("data", "intermediate", "rate_summary.csv")

# throw an error if that file doesn't exist, and otherwise
# run the script that actually makes the map
if(!file.exists(file_in)){
    stop("The necessary input file was not found. Please run 'R_scripts/03_analyze_word.R' and then try again.")
} else {
    source(here::here("R_scripts", "sourced", "007_map_making.R"))
    message("If you do not see a map, click on the 'Viewer' tab that is near the 'Files' pane in RStudio. \n \nThe map can be saved by clicking on 'Export' above it.")
    m
}
