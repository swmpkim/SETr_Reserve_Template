# Install packages
# THIS ONLY NEEDS TO BE RUN ONCE

################ INSTRUCTIONS ##################################
### Use Ctrl+A (Windows) / Cmd+A (Mac)   to select this entire script
### Then Ctrl+Enter / Cmd+Enter  to run it

### After this completes, run the script '00b_check_packages.R'
### to make sure everything installed properly
################################################################


# identify needed packages
pkg_wrangle <- c("dplyr", "here", "janitor", "lubridate", "purrr", "forcats", "readr", "readxl", "stringr")
pkg_interact <- c("DT", "ggplot2", "plotly", "shiny", "leaflet")
pkg_analyze <- c("rmarkdown", "broom", "flextable")

# install packages
install.packages(c(pkg_wrangle, pkg_interact, pkg_analyze))

# also need dev version of tidyr
install.packages("devtools")
devtools::install_github("tidyverse/tidyr")