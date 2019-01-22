library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(here)

###########################################################################

# set up the file path structure
in_path <- here::here('data', 'raw')
out_path <- here::here('data', 'processed')

# in that folder, find the name of the file(s) that includes 'set.xls or set.xlsx'
filelist <- grep('set.xls', dir(in_path), value = TRUE)
file_to_read <- paste0(in_path, "/", filelist)

# find out how many sheets there are
sheets_in_file <- excel_sheets(file_to_read)

###########################################################################

# set up a list for reading in sheets
dat_in <- list()

# read in all the sheets
for(i in seq_along(sheets_in_file)){
    dat_in[[i]] <- read_excel(file_to_read, sheet = i)
}

# glue them together into one big data frame
dat <- reshape::merge_recurse(dat_in)

###########################################################################


# format the data file

# get date into the right format
dat$date <- lubridate::ymd(dat$date)

# find the columns that contain mm or cm in their names
pos_of_hts <- grep("mm|cm", names(dat))
# find the columns with 'flag' in their names
pos_of_flags <- grep("flag", names(dat))


# pull all pin heights and flags into long format,
# then separate column names to identify pin numbers and type of information (flag or height)
# and spread back out so there's one row per pin, and a column each for height and flag
dat_long <- dat %>%
    gather(key = "param", value = "value", c(pos_of_flags, pos_of_hts)) %>%
    separate(param, into = c("pin", "num", "type")) %>%
    mutate(pin_number = paste(pin, num, sep = "_")) %>%
    select(-pin, -num) %>%
    mutate(type = case_when(type == "flag" ~ "flag",
                            TRUE ~ paste0("pin_height_", type))) %>%
    spread(key = type, value = value)

out_name <- paste0(out_path, "/", tolower(unique(dat$reserve)), "set_QC.csv")
write_csv(dat_long, out_name)
