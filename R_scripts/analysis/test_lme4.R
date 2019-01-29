library(knitr)
library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)
library(broom)
library(here)
library(lme4)
source(here::here('R_scripts', '000_functions.R'))


# find the folder with processed data
path <- here::here('data', 'processed')

# in that folder, find the name of the file(s) that ends with 'set.csv'
filelist <- grep('set_QC.csv$', dir(path), value = TRUE)



# generate warnings if the folder is empty; if there are multiple matching files, select the first one
if (length(filelist) == 0) stop("There are no files of the correct name/format (---set_QC.csv) in your processed data folder.")

if (length(filelist) > 1) {
    warning("There is more than one file of the correct format (---set_QC.csv) in your data folder. The first file alphabetically will be used.")
    filelist <- filelist[1]
}



# generate the full path to the file; read it in and get pin heights to mm
filename <- paste0(path, "/", filelist)
dat <- read_csv(filename)
dat <- height_to_mm(dat)



# if the date column is datetime, posixct, or posixlt, change it to Date
if (sum(class(dat$date) %in% c("datetime", "POSIXct", "POSIXlt")) > 0)
    dat$date <- as.Date(dat$date)

###############################################################################

# get rid of any placeholders; make sure set_id is a factor  
dat <- dat %>%
    filter(!is.na(date),
           !is.na(pin_number)) %>%
    mutate(set_id = as.factor(set_id),
           arm_position = as.character(arm_position))



################################################################################
# get the metadata in
################################################################################

# find the folder with metadata
path <- here::here('metadata')

# in that folder, find the name of the file(s) that ends with 'setm.csv'
filelist <- grep('set_metadata.xls', dir(path), value = TRUE)

# generate warnings if the folder is empty; if there are multiple matching files, select the first one
if (length(filelist) == 0) stop("There are no files of the correct name/format (---set_metadata.xls or .xlsx) in your metadata folder.")

if (length(filelist) > 1) {
    warning("There is more than one file of the correct format (---set_metadata.xls or .xlsx) in your metadata folder. The first file alphabetically will be used.")
    filelist <- filelist[1]
}

# generate the full path to the file; read it in, clean the names, get rid of empty rows and columns
filename <- paste0(path, "/", filelist)
mdat <- read_excel(filename) %>%
    clean_names() %>%
    janitor::remove_empty(which = c("rows", "cols"))

###############################################################################

# first pull out set_id from both data frames
data_setid <- unique(as.character(dat$set_id))
metadata_setid <- unique(mdat$unique_set_id)

# find set_ids that are in the data, but not in the metadata
dat_not_m <- setdiff(data_setid, metadata_setid)
# find set_ids that are in the metadata, but not in the data
m_not_dat <- setdiff(metadata_setid, data_setid)

if (length(dat_not_m) > 0) {
    toprint <- paste(dat_not_m, collapse = ", ")
    warning(paste0("The following SET IDs exist in your data, but not in your metadata: ", toprint))
}

if (length(m_not_dat) > 0) {
    toprint <- paste(m_not_dat, collapse = ", ")
    warning(paste0("The following SET IDs exist in your metadata, but not in your data: ", toprint))
}

if (length(dat_not_m) + length(m_not_dat) == 0) {
    print("SET IDs match in your data and metadata files.")
}

# cleanup
rm(dat_not_m, m_not_dat, metadata_setid, data_setid, filelist, filename, path)


slr_path <- here::here('metadata')
slr_file <- paste0(slr_path, "/slr_rates.csv")
slr_rates <- read_csv(slr_file) %>%
    clean_names() %>%
    janitor::remove_empty(which = c("rows", "cols"))


res_to_match <- unique(dat$reserve)

if (res_to_match %in% unique(slr_rates$reserve)) {
    slr_res <- slr_rates %>%
        filter(reserve == res_to_match) %>%
        select(-link)
    slr <- slr_res$slr_rate_mm_yr
    slr_ci <- slr_res$x95_percent_ci
} else {warning("This reserve does not have an entry in the sea level rise rates file. Please check metadata/slr_rates.csv and make sure your reserve is present.")}


mdat %>%
    select(unique_set_id, set_type, latitude_dec_deg, longitude_dec_deg, co_dominant_species1, co_dominant_species2, co_dominant_species3) %>%
    knitr::kable(align = "c", col.names = c("SET ID", "Type", "Lat", "Long", "Main Veg 1", "Main Veg 2", "Main Veg 3"))


############################### nlme way ##################################
# 
# models2 <- dat %>%
#     group_by(reserve, set_id) %>%
#     do(mod = lme(pin_height ~ date, data = ., random = ~1|arm_position/pin_number, na.action = na.omit))
# 
# modelcoef2 <- tidy(models2, mod, effects = "fixed") %>%
#     filter(term == "date") %>%
#     mutate(rate_mm.yr = estimate*365.25,
#            se_mm.yr = std.error*365.25,
#            CIlow_mm.yr = rate_mm.yr - 1.96*se_mm.yr,
#            CIhigh_mm.yr = rate_mm.yr + 1.96*se_mm.yr,
#            p_value = round(p.value, 4),
#            statistic = round(statistic, 3)) %>%
#     select(reserve, set_id, rate_mm.yr, se_mm.yr, CIlow_mm.yr, CIhigh_mm.yr, statistic, p_value)
# 
# kable(modelcoef2, caption = "lme estimates of change by SET", 
#       digits = 3,
#       align = 'c')

################################################################################


##############################################################################
########### lme4 way 
##############################################################################
models3 <- dat %>%
    group_by(reserve, set_id) %>%
    do(mod = lmer(pin_height ~ date + (1|arm_position/pin_number), data = ., na.action = na.omit))

modelcoef3 <- tidy(models3, mod, effects = "fixed") %>%
    filter(term == "date") %>%
    mutate(rate_mm.yr = estimate*365.25,
           se_mm.yr = std.error*365.25,
           CIlow_mm.yr = rate_mm.yr - 1.96*se_mm.yr,
           CIhigh_mm.yr = rate_mm.yr + 1.96*se_mm.yr,
           statistic = round(statistic, 3)) %>%
    select(reserve, set_id, rate_mm.yr, se_mm.yr, CIlow_mm.yr, CIhigh_mm.yr, statistic)

# other diagnostics from model  
glance(modelcoef3)
