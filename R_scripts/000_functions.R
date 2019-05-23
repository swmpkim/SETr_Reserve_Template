########################################
# turn any pin heights into mm
# and name the column pin_height
########################################
#' Pin Height to mm
#'
#' @param data 
#'
#' @return the original data frame, but if there was a "height_cm" or "height_mm" column, it is now named "pin_height". If original readings were in cm, they have been transformed into mm.
#' @export
#'
#' @examples
height_to_mm <- function(data){
    if(exists('height_cm', data)) {
        data <- data %>%
            mutate(pin_height = height_cm * 10) %>%
            select(-height_cm)
    }
    if(exists('height_mm', data)){
        data <- data %>%
            mutate(pin_height = height_mm) %>%
            select(-height_mm)
    }
    return(data)
}




######################################################
######################################################
#### Cumulative change (change since first reading)
######################################################


#' Calculate Cumulative Change at a SET
#'
#' @param dat a data frame with one row per pin reading, and the following columns, named exactly: date, set_id, arm_position, pin_number, pin_height
#'
#' @return a list of three tibbles: one each for pin, arm, and set level calculations. Pin level change is calculated first, as the difference between a pin reading and the reading from the earliest date at that set_id--arm--pin. If the first reading was NA, the entire pin's cumulative readings will be NA. The column name in the $pin tibble is "cumu". For every date of a pin reading, this calculated value will exist. On the first date, it is 0. Cumulative pin changes are then averaged to the arm position level on each date, excluding NAs. St Deviation and St Error are also calculated. There is one calculated value for every arm on every reading date. These columns in the $arm tibble are "mean_cumu", "sd_cumu", and "se_cumu". The cumulative arm changes are then averaged to the SET level, also with st dev and st err. There is one calculated value for every SET on every reading date. The columns in the $set tibble are again "mean_cumu", "sd_cumu", and "se_cumu".
#' @export
#'
#' @examples
#' 
calc_change_cumu <- function(dat) {
    
    ## conditions: have correct columns in data frame
    ## stop and give an informative message if this isn't met
    req_clms <- c("set_id", "arm_position", "pin_number", "pin_height", "date")
    
    if(sum(req_clms %in% names(dat)) != length(req_clms)){
        stop(paste("Your data frame must have the following columns, with these names, but is missing at least one:", paste(req_clms, collapse = ", ")))
    }
    
    
    ## calculations
    
    # by pin
    change_cumu_pin <- dat %>%
        group_by(set_id, arm_position, pin_number) %>%
        mutate(cumu = pin_height - pin_height[1]) %>% 
        # mutate(cumu = pin_height - pin_height[min(which(!is.na(pin_height)))]) %>% ##### subtract off the first pin reading that's not NA
        select(-pin_height) %>%
        ungroup()
    
    # pins averaged up to arms
    change_cumu_arm <- change_cumu_pin %>%
        group_by(set_id, arm_position, date) %>%
        select(-pin_number) %>%
        summarize(mean_cumu = mean(cumu, na.rm = TRUE),
                  sd_cumu = sd(cumu, na.rm = TRUE),
                  se_cumu = sd(cumu, na.rm = TRUE)/sqrt(length(!is.na(cumu)))) %>%
        ungroup()
    
    # arms averaged up to SETs
    change_cumu_set <- change_cumu_arm %>%
        group_by(set_id, date) %>%
        select(-arm_position, mean_value = mean_cumu) %>%
        summarize(mean_cumu = mean(mean_value, na.rm = TRUE),
                  sd_cumu = sd(mean_value, na.rm = TRUE),
                  se_cumu = sd(mean_value, na.rm = TRUE)/sqrt(length(!is.na(mean_value)))) %>%
        ungroup()
    
    return(list(pin = change_cumu_pin, arm = change_cumu_arm, set = change_cumu_set))
}




######################################################
######################################################
#### Incremental Change (change since last reading)
######################################################

#' Incremental Change Calculations
#'
#' @param dat a data frame with one row per pin reading, and the following columns, named exactly: date, set_id, arm_position, pin_number, pin_height
#'
#' @return a list of three tibbles: one each for pin, arm, and set level calculations. Pin level change is calculated first, as the difference between a pin reading and the prior pin reading from that set_id--arm--pin. The column name in the $pin tibble is "incr". For every date of a pin reading, this calculated value will exist or be NA. On the first date, it is NA. Incremental pin changes are then averaged to the arm position level on each date, excluding NAs. St Deviation and St Error are also calculated. There is one calculated value for every arm on every reading date. These columns in the $arm tibble are "mean_incr", "sd_incr", and "se_incr". The cumulative arm changes are then averaged to the SET level, also with st dev and st err. There is one calculated value for every SET on every reading date. The columns in the $set tibble are again "mean_incr", "sd_incr", and "se_incr". Pin level calculations are the most helpful for qa/qc, as it is possible to check for and follow-up on readings that have changed more than a certain amount (e.g. 25 mm) between readings.
#' @export
#'
#' @examples
calc_change_incr <- function(dat){
    
    ## conditions: have correct columns in data frame
    ## stop and give an informative message if this isn't met
    req_clms <- c("set_id", "arm_position", "pin_number", "pin_height", "date")
    
    if(sum(req_clms %in% names(dat)) != length(req_clms)){
        stop(paste("Your data frame must have the following columns, with these names, but is missing at least one:", paste(req_clms, collapse = ", ")))
    }
    
    
    ## calculations
    
    
    # by pin
    change_incr_pin <- dat %>%
        arrange(set_id, arm_position, pin_number, date) %>%
        group_by(set_id, arm_position, pin_number) %>%
        mutate(incr = pin_height - lag(pin_height, 1)) %>%
        ungroup()
    
    # pins averaged up to arms
    change_incr_arm <- change_incr_pin %>%
        group_by(set_id, arm_position, date) %>%
        select(-pin_number) %>%
        summarize(mean_incr = mean(incr, na.rm = TRUE),
                  sd_incr = sd(incr, na.rm = TRUE),
                  se_incr = sd(incr, na.rm = TRUE)/sqrt(length(!is.na(incr)))) %>%
        ungroup()
    
    # arms averaged up to SETs
    change_incr_set <- change_incr_arm %>%
        group_by(set_id, date) %>%
        select(-arm_position, mean_value = mean_incr) %>%
        summarize(mean_incr = mean(mean_value, na.rm = TRUE),
                  sd_incr = sd(mean_value, na.rm = TRUE),
                  se_incr = sd(mean_value, na.rm = TRUE)/sqrt(length(!is.na(mean_value)))) %>%
        ungroup()
    
    return(list(pin = change_incr_pin, arm = change_incr_arm, set = change_incr_set))
}


#######################################
### Graphs
#######################################

# maybe figure out how to make free y scales an option in the function call

## histogram, colored by arm
hist_by_arm <- function(data, columns = 4, scales = "free_y"){
    ggplot(data) +
        geom_histogram(aes(pin_height, fill = as.factor(arm_position)), color = 'black') +
        facet_wrap(~set_id, ncol = columns, scales = scales) +
        labs(title = 'Histogram of raw pin heights by SET', 
             subtitle = 'colored by arm position; stacked',
             x = 'Pin Height (mm)',
             fill = 'Arm Position') +
        theme_bw() +
        theme(legend.position = 'bottom')
}


#### raw pin readings

# by arm
plot_raw_arm <- function(data, columns = 4, pointsize = 2, sdline = TRUE, scales = "free_y"){
    data %>%
        group_by(set_id, arm_position, date) %>%
        summarize(mean = mean(pin_height, na.rm = TRUE),
                  sd = sd(pin_height, na.rm = TRUE)) %>%
        ggplot(aes(x = date, y = mean, col = as.factor(arm_position))) +
        geom_point(size = pointsize) +
        geom_line(alpha = 0.6) +
        {if(sdline) geom_errorbar(aes(x = date, ymin = mean - sd, ymax = mean + sd, col = as.factor(arm_position)))} +
        facet_wrap(~set_id, ncol = columns, scales = scales) +
        labs(title = 'Pin Height (raw measurement; averaged to arm level)',
             x = 'Date',
             y = 'Mean pin height (mm)',
             color = 'Arm Position') +
        theme_bw() +
        theme(legend.position = 'bottom')
}


# individual pins; choose a SET (put in quotes in function call)
plot_raw_pin <- function(data, set, columns = 2, pointsize = 2, scales = "fixed"){
    data %>%
        filter(set_id == !!set) %>%
        group_by(set_id, arm_position, pin_number, date) %>%
        ggplot(aes(x = date, y = pin_height, col = as.factor(pin_number))) +
        geom_point(size = pointsize) +
        geom_line(alpha = 0.6) +
        facet_wrap(~arm_position, ncol = columns, scales = scales) +
        labs(title = 'Pin Height (raw measurement)',
             subtitle = sym(set),
             x = 'Date',
             y = 'Measured pin height (mm)',
             color = 'Pin') +
        theme_bw() +
        theme(legend.position = 'bottom')
}


##### cumulative change

## by arm
plot_cumu_arm <- function(data, columns = 4, pointsize = 2, scales = "fixed") {
    # data needs to be the $arm piece of the output from calc_change_cumu
    ggplot(data, aes(x = date, y = mean_cumu, col = as.factor(arm_position))) +
        geom_point(size = pointsize) +
        geom_line() +
        facet_wrap(~set_id, ncol = columns, scales = scales) +
        labs(title = 'Cumulative Change',
             x = 'Date',
             y = 'Change since first reading (mm)',
             color = "Arm Position") +
        theme_bw() +
        theme(legend.position = 'bottom')
}


## by set
plot_cumu_set <- function(data, columns = 4, pointsize = 3.5, scales = "fixed", smooth = TRUE, lty_smooth = 5){
    # data needs to be the $set piece of the output from calc_change_cumu
    ggplot(data, aes(x = date, y = mean_cumu)) +
        geom_line(col = 'lightsteelblue4') +
        {if(smooth) geom_smooth(se = FALSE, method = 'lm', 
                    col = 'steelblue4', lty = lty_smooth, size = 1)} +
        geom_point(shape = 21, 
                   fill = 'lightsteelblue1', col = 'steelblue3', 
                   size = pointsize, alpha = 0.9) +
        facet_wrap(~set_id, ncol = columns, scales = scales) +
        labs(title = 'Cumulative Change since first reading', 
             subtitle = 'dashed line is linear regression',
             x = 'Date',
             y = 'Change since first reading (mm)') +
        theme_classic()
}


###### incremental change
plot_incr_arm <- function(data, threshold = 25, columns = 4, set = NULL, 
                          pointsize = 2, scales = "fixed"){
    # data needs to be the $arm piece of the output from calc_change_inc
    if(is.null(set)){
        to_plot <- data
        plot_title <- 'Incremental Change by arm'
    }
    else{
        to_plot <- data %>%
            filter(set_id == !!set) 
        plot_title <- paste('Incremental Change by arm at', set)
    }
    
    ggplot(to_plot, aes(x = date, y = mean_incr, col = as.factor(arm_position))) +
        geom_point(size = pointsize) +
        geom_hline(yintercept = threshold, col = "red", size = 1) +
        geom_hline(yintercept = -1*threshold, col = "red", size = 1) +
        facet_wrap(~set_id, ncol = columns, scales = scales) +
        labs(title = plot_title, 
             subtitle = paste('red lines at +/-', threshold, 'mm'),
             x = 'Date',
             y = 'Change since previous reading (mm)',
             color = 'Arm Position') +
        theme_bw() +
        theme(legend.position = 'bottom')
}





# by pin
plot_incr_pin <- function(data, set, threshold = 25, columns = 2, pointsize = 2, scales = "fixed"){
    # data needs to be the $pin piece of the output from calc_change_inc
        ggplot(data = filter(data, set_id == !!set), 
               aes(x = date, y = incr, 
                   col = as.factor(pin_number))) +
            geom_point(size = pointsize) +
            geom_hline(yintercept = threshold, col = "red", size = 1) +
            geom_hline(yintercept = -1*threshold, col = "red", size = 1) +
            facet_wrap(~arm_position, ncol = columns, scales = scales) +
            labs(title = paste('Incremental Change by pin at', set), 
                 subtitle = paste('red lines at +/-', threshold, 'mm'),
                 x = 'Date',
                 y = 'Change since previous reading (mm)',
                 color = 'Pin') +
            theme_bw() +
            theme(legend.position = 'bottom')
}