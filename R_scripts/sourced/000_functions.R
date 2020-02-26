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
#### Generate rate comparison tables
######################################################
rate_comp <- function(df, comp_rate, comp_condition){
    # THESE ARE ALL VERY SPECIFIC TO NERR DATA FRAMES
    df %>%
        filter({{comp_rate}} %in% comp_condition) %>%
        select(reserve, set_id, rate, CI_low, CI_high) %>% 
        flextable() %>% 
        theme_booktabs() %>% 
        autofit()
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
plot_raw_arm <- function(data, columns = 4, pointsize = 2, sdline = TRUE, sdlinesize = 1, scales = "free_y"){
    data %>%
        group_by(set_id, arm_position, date) %>%
        summarize(mean = mean(pin_height, na.rm = TRUE),
                  sd = sd(pin_height, na.rm = TRUE)) %>%
        ggplot(aes(x = date, y = mean, col = as.factor(arm_position))) +
        geom_point(size = pointsize) +
        geom_line(alpha = 0.6) +
        {if(sdline) geom_errorbar(aes(x = date, 
                                      ymin = mean - sd, 
                                      ymax = mean + sd, 
                                      col = as.factor(arm_position)
                                      ),
                                  size = sdlinesize
                                  )} +
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
        {if(smooth) labs(title = 'Cumulative Change since first reading', 
                         subtitle = 'dashed line is linear regression',
                         x = 'Date',
                         y = 'Change since first reading (mm)')} +
        {if(!smooth) labs(title = 'Cumulative Change since first reading', 
                          x = 'Date',
                          y = 'Change since first reading (mm)')} +
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





# "star wars graph" - comparison of SET change rates to SLR
# with various options

plot_rate_comps <- function(data, plot_type = 3, color_by_veg = FALSE, 
                            set_ids, set_ci_low, set_ci_high,
                            rates, 
                            comp1, comp1_ci_low, comp1_ci_high,
                            comp2 = NULL, comp2_ci_low = NULL, comp2_ci_high = NULL,
                            veg){
    
    # plot_type: 1 = basic; points only; no confidence intervals
    #            2 = CIs for SET rates, but not sea level rise (SLR)
    #            3 = CIs for both SETs and SLR
    #            4 = all of the above, plus a second comparison point and CIs
    # default is the full plot with CIs, and with points all the same color
    
    
    # updates to this function, 12/13/19:
    # ------------
    # changed geom_errorbar to geom_errorbarh for simplicity
    # and to enable possible use of NAVD 88 on y-axis
    # slr changed to 'comp1'
    # slr_ci split into comp1_ci _low and _high to be consistent with SET CIs
    # comp2 added, to enable addition of either 19-year slr or short-term slr 
    
    # intent is that comp1 = long-term sea level rise
    # and comp2 = 19-year sea level rise
    # but with these names they can be flexible
    
    
    # calculate CI half-widths for plot labeling
    comp1_ci_halfwidth <- (comp1_ci_high - comp1_ci_low) / 2
    comp2_ci_halfwidth <- (comp2_ci_high - comp2_ci_low) / 2
    
    
    # assemble the base plot, with axes and lines for 0 and SLR
    #####################################################################
    p <- ggplot() +
        geom_blank(data = data, 
                   aes(x = {{rates}},
                       y = {{set_ids}})) +
        geom_vline(aes(xintercept = {{comp1}}), 
                   col = "navyblue", 
                   size = 1, 
                   alpha = 0.9) +
        geom_vline(aes(xintercept = 0), 
                   col = "gray70") +
        theme_classic()
    
    
    # assemble each piece
    #####################################################################
    
    # points, not colored by veg
    points_same <- geom_point(data = data, 
                              aes(x = {{rates}}, 
                                  y = {{set_ids}}), 
                              size = 3, 
                              col = "red3")
    
    # labels, when CIs are included for both SETs and SLR
    labels_set_slr <- labs(title = "Elevation Change with 95% Confidence Intervals", 
                        subtitle = paste0("Local, long-term SLR in blue: ", 
                                          {{comp1}}, " +/- ", 
                                          comp1_ci_halfwidth, " mm/yr"), 
                        x = "Rate of change (mm/yr)", 
                        y = "SET")
    
    # labels when SETs, SLR, and 19-year change are included
    labels_all <- labs(title = "Elevation Change with 95% Confidence Intervals", 
                       subtitle = paste0("Long-term SLR, solid line & dark shading: ", 
                                         {{comp1}}, " +/- ", 
                                         comp1_ci_halfwidth, " mm/yr",
                                         "\n19-yr water level change, dashed line & light shading: ", 
                                         {{comp2}}, " +/- ", 
                                         comp2_ci_halfwidth, " mm/yr"), 
                       x = "Rate of change (mm/yr)", 
                       y = "SET")
    
    # labels, when no CIs are included
    labels_minimal <- labs(title = "Elevation Change", 
                           subtitle = paste0("Local SLR in blue: ", {{comp1}}, " mm/yr"), 
                           x = "Rate of change (mm/yr)",
                           y = "SET")
    
    # labels, when CIs are included for SETs but not SLR
    labels_partial_setci <- labs(title = "Elevation Change with 95% Confidence Intervals", 
                                 subtitle = paste0("Local SLR in blue: ", {{comp1}}, " mm/yr"), 
                                 x = "Rate of change (mm/yr)",
                                 y = "SET")
    
    # geom to include when CIs are included for SETs
    set_cis <- geom_errorbarh(data = data, 
                              aes(y = {{set_ids}}, 
                                  xmin = {{set_ci_low}}, 
                                  xmax = {{set_ci_high}}), 
                              col = "gray55", 
                              size = 1) 
    
    # geom to include when CI is included for SLR
    slr_cis <- geom_rect(aes(ymin = -Inf,
                             ymax = Inf, 
                             xmin = {{comp1_ci_low}}, 
                             xmax = {{comp1_ci_high}}), 
                         fill = "#08519c", # formerly navyblue. #08519c is a contender; 0.2 here and 0.1 in comp2; #08306b is another one i like a lot
                         alpha = 0.2) 
    
    # geom to include with point estimate for comp2
    comp2_line <- geom_vline(aes(xintercept = {{comp2}}), 
                             col = "navyblue", 
                             size = 1,
                             linetype = "dashed",
                             alpha = 0.9)
    
    # geom to include when point and CI included for comp2
    comp2_cis <- geom_rect(aes(ymin = -Inf,
                               ymax = Inf, 
                               xmin = {{comp2_ci_low}}, 
                               xmax = {{comp2_ci_high}}), 
                           fill = "#08519c",     #7bccc4, 0.2
                           alpha = 0.1) 
    
    
    # geom and labels if points will be colored by dominant vegetation type
    if(color_by_veg){
        # the veg column has to be defined
        # this doesn't work though: stopifnot(exists({{veg}}, data))
        points_veg <- geom_point(data = data, 
                                 aes(x = {{rates}}, 
                                     y = {{set_ids}},
                                     col = {{veg}}), 
                                 size = 3) 
        colors_veg <- scale_color_brewer(type = "qual", palette = "Dark2") 
        labels_veg <- labs(color = "Dominant Vegetation")
    }
    
    
    
    ##### Assemble in different ways
    #####################################################################    
    
    
    ####################################################################
    ### minimal plot: points only; no confidence intervals
    ####################################################################
    
    # don't color by veg
    if(plot_type == 1 && !color_by_veg){
        p <- p +
            points_same +
            labels_minimal
    }
    
    # do color by veg
    if(plot_type == 1 && color_by_veg){
        p <- p +
            points_veg +
            colors_veg +
            labels_minimal +
            labels_veg
    }
    
    
    
    
    ####################################################################
    # Add in CIs for SETs
    ####################################################################
    # don't color by veg
    if(plot_type == 2 && !color_by_veg){
        p <- p +
            set_cis +
            points_same +
            labels_partial_setci
    }
    
    # do color by veg
    if(plot_type == 2 && color_by_veg){
        p <- p +
            set_cis +
            points_veg +
            colors_veg +
            labels_partial_setci +
            labels_veg
    }
    
    
    
    ####################################################################
    # CIs for both SETs and SLR
    ####################################################################
    # don't color by veg
    if(plot_type == 3 && !color_by_veg){
        p <- p +
            set_cis +
            slr_cis +
            points_same +
            labels_set_slr
    }
    
    
    # do color by veg
    if(plot_type == 3 && color_by_veg){
        p <- p +
            set_cis +
            slr_cis +
            points_veg +
            colors_veg +
            labels_set_slr +
            labels_veg
    }
    
    
    ####################################################################
    # Everything including comp2
    ####################################################################
    # don't color by veg
    if(plot_type == 4 && !color_by_veg){
        p <- p +
            set_cis +
            slr_cis +
            comp2_line +
            comp2_cis +
            points_same +
            labels_all
    }
    
    
    # do color by veg
    if(plot_type == 4 && color_by_veg){
        p <- p +
            set_cis +
            slr_cis +
            comp2_line +
            comp2_cis +
            points_veg +
            colors_veg +
            labels_all +
            labels_veg
    }
    
    
    return(p)
    
} 