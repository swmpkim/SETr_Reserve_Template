# Script to read in excel sheet of raw data, which has 
# and turn it into a _processed.csv file, with
# all SETs in one table


#### INSTRUCTIONS #############################################################

# 1:
# Re-start your R session to make sure there's no interference:
# From the menu bar, select 'Session', then 'Restart R'
# Windows keyboard shortcut is Ctrl + Shift + F10

# 2:

# Select this entire script. 
# Keyboard shortcut is Ctrl + a on windows or Cmd + a on Mac
# Run it: either using the "Run" button in the upper right corner
# or the keyboard shortcut Ctrl/Cmd + Enter

###############################################################################


# this script is modified from SETr_data_transformations/R/01_NEW_wide_to_long.R
# it has been made to automatically select the excel file that ends with set.xlsx
# and transform it into the longer format that can be used in the downstream scripts

## as of 2019-09-16, the pivoting specs have changed to match tidyr v. 1.0.0:
# set up specs with build_longer_spec  rather than pivot_longer_spec
# perform pivot with pivot_longer_spec rather than pivot_longer

# this script has also been moved to the top R level because it has to be run before anything else can be done.


library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)
library(lubridate)
library(here)

# interactively choose file to work with
# file_path <- choose.files()

in_path <- here::here('data', 'raw')
# in that folder, find the name of the file(s) that includes 'set.xls or set.xlsx'
filelist <- grep('set.xls', dir(in_path), value = TRUE)
file_path <- paste0(in_path, "/", filelist)


# read in all sheets and append them to each other
# in so doing, force all columns to be seen as text
# so there are no issues joining them together
dat <- file_path %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map_df( ~ read_excel(path = file_path, 
                             sheet = .x, 
                             col_types = "text"), 
                .id = "sheet")

# check to make sure the SET ID that was entered matches the name of each sheet
mismatches <- dat$set_id != dat$sheet

# make this whole script stop if something doesn't match
if(sum(mismatches) > 0){
        print(dat[mismatches, c("sheet", "set_id", "year", "month", "arm_position")])
        stop("There are SET IDs that do not match the sheet name. Please check and correct the rows printed above before proceeding.")
        }else{
# if no problem, do everything else

# first, format the data:
# get rid of the "sheet" column
# make sure date is date format; 
# several columns should be character: set_id, arm_position, 
# and anything that ends in qaqc_code
dat_formatted <- dat %>% 
        select(-sheet) %>% 
        mutate_at(c("set_id", "arm_position"), as.character) %>% 
        mutate_at(vars(ends_with("qaqc_code")), as.character)


# have to change column names first because there are too many underscores
names(dat_formatted) <- gsub("qaqc_code", "qaqccode", names(dat_formatted))
names(dat_formatted) <- gsub("pin_", "pin", names(dat_formatted))
names(dat_formatted) <- gsub("height_", "height", names(dat_formatted))

# set up to pivot
spec <- dat_formatted %>% 
        build_longer_spec(
                cols = starts_with("pin1_height"):"pin9_qaqccode",
                names_to = c("pinnumber", ".value"),
                names_sep = "_"
        )

# pivot to longer
dat_long <- dat_formatted %>% 
        pivot_longer_spec(spec = spec)

# put underscores back in the names
names(dat_long) <- gsub("pin", "pin_", names(dat_long))
dat_long$pin_number <- gsub("pin", "pin_", dat_long$pin_number)
names(dat_long) <- gsub("qaqccode", "qaqc_code", names(dat_long))
names(dat_long) <- gsub("height", "height_", names(dat_long))

# format and arrange before output
dat_long <- dat_long %>% 
        select(set_id, year, month, day, arm_position, arm_qaqc_code, 
               pin_number, starts_with("height"), qaqc_code, everything()) %>% 
        arrange(set_id, year, month, day, arm_position, pin_number)

# generate output and write out file
file_in <- str_extract(file_path, "[:alpha:]+\\.xls")
file_out <- paste0(substr(file_in, 1, nchar(file_in)-4), "_processed.csv")
out_path <- here::here("data", "processed")
file_out <- paste0(out_path, "/", file_out)
write.csv(dat_long, file_out, row.names = FALSE)

message("\n \nDone! Move on to other scripts. \n \n")
        }

