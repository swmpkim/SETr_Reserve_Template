################ INSTRUCTIONS ##################################
### Use Ctrl+A (Windows) / Cmd+A (Mac)   to select this entire script
### Then Ctrl+Enter / Cmd+Enter  to run it
### Then check the output in your console and see if you 
### need to re-install any packages

### If you do, I recommend trying one at a time, using the command
### install.packages("package_name_here")

### Then re-run this script to make sure everything worked

### If you have any problems, contact Kim

################################################################


# list of packages we need to have installed for our workflow
pkg_wrangle <- c("dplyr", "here", "janitor", "lubridate", "purrr", "forcats", "readr", "readxl", "stringr", "tidyr")
pkg_interact <- c("DT", "ggplot2", "plotly", "shiny", "leaflet")
pkg_analyze <- c("rmarkdown", "broom", "flextable", "webshot", "mapview")


# glue the sub-categories together into one vector of packages we need
pkgs_needed <- c(pkg_wrangle, pkg_interact, pkg_analyze)



## now try loading each one and see if it works
# set up an output vector
pkg_result <- vector("logical", length(pkgs_needed))
# loop through the needed packages
for(i in seq_along(pkgs_needed)){
    # try to load the package and report whether it works
    # record that TRUE or FALSE in the pkg_result vector
    pkg_result[i] <- library(pkgs_needed[i], character.only = TRUE, quietly = TRUE, logical.return = TRUE)
}


# make a vector of missing packages:
# pkgs_needed that failed to load
pkgs_missing <- pkgs_needed[!pkg_result]


# find out if phantomjs is missing
# only run if webshot isn't missing, 
# and if it's a version that has this function
# otherwise return true for phantomjs missing
if(!("webshot" %in% pkgs_missing) && packageVersion("webshot") >= "0.5.1.9000"){
    phantomjs_missing <- !webshot::is_phantomjs_installed()
} else {
    phantomjs_missing <- TRUE
}



# set up the pieces of messages to print to the console
msg_pkgs_good <- "\n \nAll required packages are installed and loading properly! \n \n"
msg_some_pkgs_missing <- "\n \nYou need to install the following packages. You can try again now by running: \ninstall.packages(pkgs_missing)"
msg_phantomjs_missing <- "\nA software component called 'phantomjs' is missing. \n---If the 'webshot' package is in your list of missing packages, install it using devtools::install_github('wch/webshot')  \n---Once 'webshot' is installed, run the line webshot::install_phantomjs()  \n---If it is still not installed, you may need to download it manually from http://phantomjs.org/download.html"



# print messages to the console based on exactly what's working / missing
if(length(pkgs_missing) == 0 && !phantomjs_missing){
    message(msg_pkgs_good)
} else if(length(pkgs_missing) == 0 && phantomjs_missing){
    message(paste0(msg_pkgs_good, "BUT \n", msg_phantomjs_missing))
} else if(length(pkgs_missing) != 0 && phantomjs_missing){
    message(msg_some_pkgs_missing); cat(pkgs_missing, sep = "\n"); message(paste("\nAND \n", msg_phantomjs_missing))
} else { message(msg_some_pkgs_missing); cat(pkgs_missing, sep="\n") }


