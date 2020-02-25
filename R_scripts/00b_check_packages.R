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
pkg_analyze <- c("rmarkdown", "broom", "flextable", "officer", "webshot", "mapview")


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



# check tidyr version
# only if it was there
tidyr_okay <- FALSE
if(pkg_result[which(pkgs_needed == "tidyr")]){
    tidyr_okay <- packageVersion("tidyr") >= 1.0
}



# find out if phantomjs is okay
# only run if webshot isn't missing, 
# and if it's a version that has this function
# otherwise return false for phantomjs okay
if(!("webshot" %in% pkgs_missing) && packageVersion("webshot") >= "0.5.1.9000"){
    phantomjs_okay <- webshot::is_phantomjs_installed()
} else {
    phantomjs_okay <- FALSE
}


# set up the conditions for when things should be printed

# all packages okay
pkgs_okay <- length(pkgs_missing) == 0 && tidyr_okay

# all packages load, but tidyr is outdated
tidyr_only_pkg_problem <- length(pkgs_missing) == 0 && !tidyr_okay

# some packages don't load, and tidyr is one of them
# no problem, it'll show up in "msg_some_pkgs_missing"

# some packages don't load; tidyr loads but is outdated
# will want to combine mgs_tidyr_outdated with msg_some_pkgs_missing
tidyr_and_other_pkg_problems <- length(pkgs_missing) != 0 && !("tidyr" %in% pkgs_missing) && !tidyr_okay



# set up the pieces of messages to print to the console
msg_pkgs_good <- "\n \nAll required packages are installed and loading properly! \n \n"

msg_some_pkgs_missing <- "\nYou need to install the following packages. You can try again now by running: \ninstall.packages(pkgs_missing)"

msg_phantomjs_missing <- "\nA software component called 'phantomjs' is missing. \n---If the 'webshot' package is in your list of missing packages, install it using install.packages('webshot')  \n---Once 'webshot' is installed, run the line webshot::install_phantomjs()  \n---If it is still not installed, you may need to download it manually from http://phantomjs.org/download.html"

msg_tidyr_outdated <- "\n \nYour version of tidyr needs to be updated to at least 1.0.0; \ntry running install.packages(\"tidyr\")"



# print messages to the console based on exactly what's working / missing
### if everything is good, print that
# start with package cases only; we'll deal with phantomjs at the end


# nest within phantomjs being okay or not
if(phantomjs_okay){
    # then run through package scenarios
    if(pkgs_okay){
        message(msg_pkgs_good)
    } else if(tidyr_only_pkg_problem){
        message(paste0(msg_tidyr_outdated, 
                       "\notherwise, everything looks okay \n \n"))
    } else if(tidyr_and_other_pkg_problems){
        message(paste(msg_tidyr_outdated, "\nAND", msg_some_pkgs_missing, sep = "\n")); cat(pkgs_missing, sep = "\n")
    } else {
        message(msg_some_pkgs_missing); cat(pkgs_missing, sep = "\n")
    }
} else {
    # run through package scenarios if phantomjs isn't okay
    if(pkgs_okay){
        message(paste0(msg_pkgs_good, "BUT \n", msg_phantomjs_missing))
    } else if(tidyr_only_pkg_problem){
        message(paste0(msg_tidyr_outdated, 
                       "\n \nAND \n",
                       msg_phantomjs_missing))
    } else if(tidyr_and_other_pkg_problems){
        message(paste(msg_tidyr_outdated, "\nAND", msg_some_pkgs_missing, sep = "\n")); cat(pkgs_missing, sep = "\n"); message(paste0("\n \nAND \n", msg_phantomjs_missing))
    } else {
        message(msg_some_pkgs_missing); cat(pkgs_missing, sep = "\n"); message(paste0("\n \nAND \n", msg_phantomjs_missing))
    }
}
