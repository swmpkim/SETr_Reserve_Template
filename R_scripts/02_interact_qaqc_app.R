# Interactive app to help with QA/QC of NERRS SET data


#### INSTRUCTIONS ############################################################

# 1:
# Re-start your R session to make sure there's no interference:
# From the menu bar, select 'Session', then 'Restart R'
# Windows keyboard shortcut is Ctrl + Shift + F10

# 2:
# In the upper right-hand corner of this (source) window,
# there is a button that says "Run App".
# Push it.
# Make sure pop-ups are enabled; the app comes up in a browser window.

##############################################################################


# load packages and function script
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(here)
library(shiny)
library(plotly)
library(DT)
library(janitor)
source(here::here('R_scripts', 'sourced', '000_functions.R'))



################################################################################
# get the data in
################################################################################

# find the folder with processed data
path <- here::here('data', 'processed')

# in that folder, find the name of the file(s) that ends with 'set_QC.csv'
filelist <- grep('set_processed.csv$', dir(path), value = TRUE)

# generate warnings if the folder is empty; if there are multiple matching files, select the first one
if (length(filelist) == 0) stop("There are no files of the correct name/format (---set_processed.csv) in your processed data folder.")

if (length(filelist) > 1) {
    warning("There is more than one file of the correct format (---set_processed.csv) in your data folder. The first file alphabetically will be used.")
    filelist <- filelist[1]
}

# generate the full path to the file; read it in, paste dates together, and get pin heights to mm
filename <- paste0(path, "/", filelist)
dat <- read_csv(filename) %>% 
    mutate(date = lubridate::ymd(paste(year, month, day)))
dat <- height_to_mm(dat)


# if the date column is datetime, posixct, or posixlt, change it to Date
if (sum(class(dat$date) %in% c("datetime", "POSIXct", "POSIXlt")) > 0)
    dat$date <- as.Date(dat$date)



###############################################################################
# calculate cumulative and incremental change
###############################################################################

# cumulative change
cumu_out <- calc_change_cumu(dat)
cumu_out_set <- cumu_out$set
# incremental change
incr_out <- calc_change_incr(dat)


###############################################################################
# reshape incremental change so it looks more like the NPS spreadsheet,
# with date as column headers and one row per pin - 36 rows per set
# would be great to conditionally format cells outside the threshold, dynamically
###############################################################################
incr_wide <- incr_out$pin %>% 
    select(set_id, arm_position, pin_number, date, incr) %>% 
    mutate(date = as.character(date)) %>% 
    spread(key = date, value = incr)


###############################################################################
### define UI
###############################################################################

ui <- fluidPage(
   
     titlePanel("SET graphing"),
    
    sidebarLayout(
        sidebarPanel(
            
            # show the name of the file in use
            paste("File in use is:", filelist),
            br(), br(),
            
            

            # select size of points for single-panel plots
            sliderInput(inputId = "ptsize_single", 
                        label = "point size, single plots",
                        min = 0.5,
                        max = 4,
                        value = 2,
                        step = 0.5),
            
            # select size of points for multi-panel plots
            sliderInput(inputId = "ptsize_multi", 
                        label = "point size, multi-panel plots",
                        min = 0.5,
                        max = 4,
                        value = 1,
                        step = 0.5),
            
            
            # select scales for faceting
            selectInput(inputId = "scales_multi", 
                        label = strong("fixed or flexible scales in multi-panel plots"),
                        choices = c("fixed", "free", "free_y", "free_x"),
                        selected = "fixed"
            )
           
        ),
        
        
        mainPanel(
            
            # select a SET to plot
            selectInput(inputId = "SET", 
                        label = strong("Select a SET to work with"),
                        choices = unique(dat$set_id),
                        selected = unique(dat$set_id)[1]
            ),
            
            # select date range to plot
            dateRangeInput("date", 
                           label = strong("Date range"), 
                           start = min(dat$date, na.rm = TRUE), 
                           end = max(dat$date, na.rm = TRUE)
            ),
            
            tabsetPanel(type = "tabs",
                        id = "tabselected",
            
                        tabPanel("Raw Data", value = 1,
                                 br(),
                                 # choose whether to include +/- a stdev
                                 checkboxInput(inputId = "sdline", 
                                               label = strong("Include error bars (+/- 1 stdev)"),
                                               value = FALSE
                                 ),
                                 plotlyOutput(outputId = "plotly_raw_arm"),
                                 br(), br(),
                                 plotlyOutput(outputId = "plotly_raw_pin")
                        ),
                        
                        tabPanel("Incremental Calcs", value = 2,
                                 br(),
                                 sliderInput(inputId = "incr_threshold", 
                                             label = "Choose threshold of interest (mm)",
                                             min = 10,
                                             max = 100,
                                             value = 25,
                                             step = 5),
                                 br(),
                                 plotlyOutput(outputId = "plotly_incr_pin"),
                                 
                                 br(), br(),
                                 textOutput(outputId = "count_incr_pin"),
                                 
                                 checkboxInput(inputId = "incr_table",
                                               label = "Show these points in a table",
                                               value = FALSE),
                                 conditionalPanel(condition = "input.incr_table == true",
                                                  br(), 
                                                  DT::dataTableOutput(outputId = "tbl_incr_pin")
                                 ),
                                 
                                 br(),
                                 
                                 checkboxInput(inputId = "incr_tbl_wide",
                                               label = "View ALL incremental changes for this SET, with each pin on a row and each date as a column",
                                               value = FALSE),
                                 conditionalPanel(condition = "input.incr_tbl_wide == true",
                                                  br(),
                                                  DT::dataTableOutput(outputId = "tbl_incr_wide")
                                                  ),
                                 br(), br(),
                                 plotlyOutput(outputId = "plotly_incr_arm")
                                 
                                 
                        ),
                        
                        tabPanel("Cumulative Calcs", value = 3,
                                 br(),
                                 # choose whether to overlay regression or not
                                 checkboxInput(inputId = "cumu_smooth", 
                                               label = strong("Overlay Linear Regression"),
                                               value = FALSE
                                 ),
                                 # make plots
                                 plotlyOutput(outputId = "plotly_cumu_set_sub",
                                              height = 400),
                                 # choose number of columns for complete plot
                                 selectInput(inputId = "columns", 
                                             label = strong("Choose # columns for graph below"),
                                             choices = c(1, 2, 3, 4, 5),
                                             selected = 4
                                 ),
                                 plotlyOutput(outputId = "plotly_cumu_set",
                                              height = 600)
                        )
            )
        )
        
    )
)



###############################################################################
# Define server function
###############################################################################


server <- function(input, output) {
    
    
    ##############################################
    # reactive data operations
    ##############################################
    
    # subset data, reactively
    dat_sub <- reactive({
        req(input$SET)
        req(input$date)
        dat %>%
            filter(set_id == input$SET,
                   date >= as.Date(input$date[1]),
                   date <= as.Date(input$date[2]))
    })
    
    # subset cumulative data, reactively  
    cumu_out_set_sub <- reactive({
        req(input$SET)
        req(input$date)
        cumu_out_set %>% 
            filter(set_id == input$SET,
                   date >= as.Date(input$date[1]),
                   date <= as.Date(input$date[2])) %>% 
            mutate(mean_cumu = mean_cumu - mean_cumu[1])
    })
    
    # subset incremental change list, reactively
    incr_out_sub <- reactive({
        req(input$date)
        # write custom function to subset data frames
        datesub <- function(x){
            df <- x
            df[df$date >= input$date[1] & df$date <= input$date[2], ]
        }
        # apply that function to each piece of the incr_out list
        lapply(incr_out, datesub)
    })
    
    
    # subset the incr_wide data frame
    # only by SET; keep all dates included
    incr_wide_sub <- reactive({
        req(input$SET)
        incr_wide[incr_wide$set_id == input$SET, ]
    })
    
   

    ##############################################
    # Plots
    ##############################################
    
    
    # create plotly plot of avg raw reading by arm
    output$plotly_raw_arm <- renderPlotly({
        req(input$SET)
        req(input$date)
        q <- plot_raw_arm(dat_sub(), 
                          pointsize = input$ptsize_single,
                          scales = input$scales_multi,
                          sdline = input$sdline,
                          sdlinesize = 0.7) +
            ylab("mm")
        q 
    }) 
    
    # create plotly plot of raw readings by pin
    output$plotly_raw_pin <- renderPlotly({
        req(input$SET)
        req(input$date)
        z <- plot_raw_pin(dat_sub(), set = input$SET, 
                          pointsize = input$ptsize_multi,
                          scales = input$scales_multi) +
            ylab("mm")
        z
    })
    
    
    # create plotly plot of incremental change by pin
    output$plotly_incr_pin <- renderPlotly({
        req(input$SET)
        req(input$date)
        req(input$incr_threshold)
        a <- plot_incr_pin(data = incr_out_sub()$pin,
                           set = input$SET,
                           threshold = input$incr_threshold,
                           pointsize = input$ptsize_multi,
                           scales = input$scales_multi) +
            ylab("mm")
            
        a
    })
    
    # count how many pins are outside the selected threshold
    output$count_incr_pin <- renderText({
        exceed <- incr_out_sub()$pin %>% 
            filter(abs(incr) >= input$incr_threshold,
                   set_id == input$SET)
        paste0(nrow(exceed), " observations at this SET, in this time period, have incremental changes outside the selected threshold.")
    })
    
    # make a table of pins outside the selected threshold
    output$tbl_incr_pin <- DT::renderDataTable({
        tabdat <- incr_out_sub()$pin %>% 
            filter(abs(incr) >= input$incr_threshold,
                   set_id == input$SET) %>% 
            arrange(date, desc(incr)) %>% 
            select(incr, year, month, day, 
                   arm_position, pin_number, 
                   qaqc_code, arm_qaqc_code, everything())
        DT::datatable(data = tabdat, 
                      rownames = FALSE,
                      options = list(pageLength = 10,
                                     autoWidth = TRUE,
                                     columnDefs = list(list(
                                         className = 'dt-center', 
                                         targets = "_all"))
                                     )
                      )
    })
    
    
    # make the table of incremental changes that looks like the NPS spreadsheet
    output$tbl_incr_wide <- DT::renderDataTable({
        incrdat <- incr_wide_sub()
        incrdat <- janitor::remove_empty(incrdat, which = c("rows", "cols"))
        DT::datatable(data = incrdat, 
                      rownames = FALSE,
                      options = list(pageLength = 36,
                                     autoWidth = TRUE,
                                     columnDefs = list(list(
                                         className = 'dt-center', 
                                         targets = "_all"))
                      )
        )
    })
    
    
    # create plotly plot of incremental change by arm
    output$plotly_incr_arm <- renderPlotly({
        req(input$SET)
        req(input$date)
        req(input$incr_threshold)
        a <- plot_incr_arm(data = incr_out_sub()$arm,
                           set = input$SET,
                           threshold = input$incr_threshold,
                           pointsize = input$ptsize_single)+
            ylab("mm")
        a
    })
    
    # create plotly plot of cumulative change by SET
    output$plotly_cumu_set <- renderPlotly({
        req(input$SET)
        b <- plot_cumu_set(data = cumu_out$set,
                           columns = as.integer(input$columns), 
                           pointsize = input$ptsize_multi,
                           scales = input$scales_multi,
                           smooth = input$cumu_smooth,
                           lty_smooth = 1) +
            ylab("mm")
            
        b
    })
    
    
    # create plotly plot of cumulative change by INDIVIDUAL SET
    output$plotly_cumu_set_sub <- renderPlotly({
        req(input$SET)
        req(input$date)
        c <- plot_cumu_set(data = cumu_out_set_sub(),
                           columns = 1, 
                           pointsize = input$ptsize_single,
                           smooth = input$cumu_smooth,
                           lty_smooth = 1) +
            ggtitle("Cumulative change since first selected date") +
            ylab("mm")
        
        c
    })
    
    
}

###############################################################################
# Create Shiny object
###############################################################################
shinyApp(ui = ui, server = server)
