# Install packages
# THIS ONLY NEEDS TO BE RUN ONCE

################ INSTRUCTIONS ##################################
### Use Ctrl+A (Windows) / Cmd+A (Mac)   to select this entire script
### Then Ctrl+Enter / Cmd+Enter  to run it

### After this completes, run the script '00b_check_packages.R'
### to make sure everything installed properly
################################################################


# identify needed packages
pkg_wrangle <- c("tidyr", "dplyr", "here", "janitor", "lubridate", "purrr", "forcats", "readr", "readxl", "stringr")
pkg_interact <- c("DT", "ggplot2", "plotly", "shiny", "leaflet")
pkg_analyze <- c("rmarkdown", "broom", "flextable", "mapview")

# install packages
install.packages(c(pkg_wrangle, pkg_interact, pkg_analyze))

# also need dev version of webshot
# somewhere in here though rtools is being installed
install.packages("devtools")
devtools::install_github("wch/webshot")

# and phantomjs for static maps in the analysis output
# install through webshot package;
# first can check to see if it's already installed (from, e.g., SWMP status reports)
# and if it's not, then install it
library(webshot)
if(!is_phantomjs_installed()){
    install_phantomjs()
}