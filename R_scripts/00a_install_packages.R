# Install packages
# THIS ONLY NEEDS TO BE RUN ONCE

################ INSTRUCTIONS ##################################
### Use Ctrl+A (Windows) / Cmd+A (Mac)   to select this entire script
### Then Ctrl+Enter / Cmd+Enter  to run it

### After this completes, run the script '00b_check_packages.R'
### to make sure everything installed properly
################################################################


# identify needed packages
pkg_wrangle <- c("dplyr", "here", "janitor", "lubridate", "purrr", "forcats", "readr", "readxl", "stringr", "tidyr")
pkg_interact <- c("DT", "ggplot2", "plotly", "shiny", "leaflet")
pkg_analyze <- c("rmarkdown", "broom", "flextable", "mapview")

# install packages
install.packages(c(pkg_wrangle, pkg_interact, pkg_analyze))

# webshot separately because it seems to cause problems when bundled in
install.packages("webshot")

# need phantomjs for static maps in the analysis output
# as of november 2019, is_phantomjs_installed() is in the CRAN version!!!
# so first check to see if it's already installed 
# (from, e.g., SWMP status reports)
# and if it's not, then install it

library(webshot)
if(!is_phantomjs_installed()){
    install_phantomjs()
}