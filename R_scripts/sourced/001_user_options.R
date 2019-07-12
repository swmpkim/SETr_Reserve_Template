# Read in the user-defined input specifications and format it 
# so the options can be fed into function arguments

library(here)
library(dplyr)
library(tidyr)
library(readxl)

in_path <- here::here("metadata", "user_defined_inputs.xlsx")

# read in qaqc options
opts_qaqc <- read_excel(in_path, sheet = "qaqc_codes")

# create vector of codes chosen for exclusion
codes_to_exclude <- opts_qaqc %>% 
    rename(to_exclude = 1,
           full_code = "Full Code in Data") %>% 
    select(to_exclude, full_code) %>% 
    filter(!is.na(full_code),
           to_exclude == "-3") %>% 
    select(full_code) %>% 
    unlist()
# create a true/false conditon for use in other scripts
# so another script can decide whether to try to turn any data values into NAs
excl_exist <- length(codes_to_exclude) > 0


# read in and reformat general options
# make it a wide data frame so the options can just be called out like a normal data frame column
opts_general <- read_excel(in_path, sheet = "general")
opts_long <- opts_general %>% 
    filter(!is.na(R_param)) %>% 
    select(-1) %>% 
    spread(key = R_param, value = Value)


# clean up so only the objects we need are left in the working environment
rm(list = c("opts_general", "opts_qaqc", "in_path"))
