# just working with one dataset until this all works
# basically copied and adapted code from this app:
# https://shiny.rstudio.com/#code-app

# load packages and function script
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(here)
library(shiny)
library(plotly)
library(DT)
source(here::here('R_scripts', '000_functions.R'))



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


# cumulative change
cumu_out <- calc_change_cumu(dat)
# incremental change
incr_out <- calc_change_incr(dat)


### define UI
ui <- fluidPage(
    titlePanel("SET graphing"),
    sidebarLayout(
        sidebarPanel(
            
            # show the name of the file in use
            paste("File in use is:", filelist),
            br(), br(),
            
            # select a SET to plot
            selectInput(inputId = "SET", 
                        label = strong("SET ID"),
                        choices = unique(dat$set_id),
                        selected = unique(dat$set_id)[1]
            ),
            
            # select date range to plot
            dateRangeInput("date", 
                           label = strong("Date range"), 
                           start = min(dat$date, na.rm = TRUE), 
                           end = max(dat$date, na.rm = TRUE)
            ),

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
            
            # choose whether to include +/- a stdev
            checkboxInput(inputId = "sdline", 
                          label = strong("Include error bars (+/- 1 stdev)"),
                          value = FALSE
            ),
            
            # select scales for faceting
            selectInput(inputId = "scales_multi", 
                        label = strong("fixed or flexible scales in multi-panel plots"),
                        choices = c("fixed", "free", "free_y", "free_x"),
                        selected = "fixed"
            ),
            
            # set threshold for incremental change plots
            sliderInput(inputId = "incr_threshold", 
                        label = "Choose threshold of interest (mm)",
                        min = 10,
                        max = 100,
                        value = 25,
                        step = 5),
            
            
            conditionalPanel(
                condition = "input.tabselected == 3",
                sliderInput(inputId = "nothingImportant", 
                            label = "pick a number; nothing will happen",
                            min = 0.5,
                            max = 4,
                            value = 1,
                            step = 0.5),
                # select the number of columns to show
                # in the cumulative change graph
                selectInput(inputId = "columns", 
                            label = strong("Choose # columns for graph below"),
                            choices = c(1, 2, 3, 4, 5),
                            selected = 4
                ),
                # choose whether to overlay regression or not
                checkboxInput(inputId = "cumu_smooth", 
                              label = strong("Overlay Linear Regression"),
                              value = FALSE
                )
            )
            
            
        ),
        
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        id = "tabselected",
            
                        tabPanel("Raw Data", value = 1,
                                 br(),
                                 
                                 plotlyOutput(outputId = "plotly_raw_arm"),
                                 br(), br(),
                                 plotlyOutput(outputId = "plotly_raw_pin")
                        ),
                        
                        tabPanel("Incremental Calcs", value = 2,
                                 br(),
                                 
                                 br(),
                                 plotlyOutput(outputId = "plotly_incr_pin"),
                                 br(), br(),
                                 textOutput(outputId = "count_incr_pin"),
                                 br(), 
                                 DT::dataTableOutput(outputId = "tbl_incr_pin"),
                                 br(), br(),
                                 plotlyOutput(outputId = "plotly_incr_arm")
                        ),
                        
                        tabPanel("Cumulative Calcs", value = 3,
                                 br(),
                                 
                                 plotlyOutput(outputId = "plotly_cumu_set",
                                              height = 500)
                        )
            )
        )
        
    )
)



# Define server function
server <- function(input, output) {
    
    
    # subset data, reactively
    dat_sub <- reactive({
        req(input$SET)
        req(input$date)
        dat %>%
            filter(set_id == input$SET,
                   date >= as.Date(input$date[1]),
                   date <= as.Date(input$date[2]))
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
    
   

    # create plotly plot of avg raw reading by arm
    output$plotly_raw_arm <- renderPlotly({
        req(input$SET)
        req(input$date)
        q <- plot_raw_arm(dat_sub(), 
                          pointsize = input$ptsize_single,
                          scales = input$scales_multi,
                          sdline = input$sdline,
                          sdlinesize = 0.7)
        q 
    }) 
    
    # create plotly plot of raw readings by pin
    output$plotly_raw_pin <- renderPlotly({
        req(input$SET)
        req(input$date)
        z <- plot_raw_pin(dat_sub(), set = input$SET, 
                          pointsize = input$ptsize_multi,
                          scales = input$scales_multi) +
            ylab("")
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
                           scales = input$scales_multi)
            
        a
    })
    
    # count how many pins are outside the selected threshold
    output$count_incr_pin <- renderText({
        exceed <- incr_out_sub()$pin %>% 
            filter(abs(incr) >= input$incr_threshold,
                   set_id == input$SET)
        paste0(nrow(exceed), " observations at this SET, in this time period, have incremental changes outside the selected threshold. They are grouped by arm and pin below: ")
    })
    
    # make a table of pins outside the selected threshold
    output$tbl_incr_pin <- DT::renderDataTable({
        tabdat <- incr_out_sub()$pin %>% 
            filter(abs(incr) >= input$incr_threshold,
                   set_id == input$SET) %>% 
            arrange(arm_position, pin_number, date) %>% 
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
    
    
    # create plotly plot of incremental change by arm
    output$plotly_incr_arm <- renderPlotly({
        req(input$SET)
        req(input$date)
        req(input$incr_threshold)
        a <- plot_incr_arm(data = incr_out_sub()$arm,
                           set = input$SET,
                           threshold = input$incr_threshold,
                           pointsize = input$ptsize_single)
        a
    })
    
    # create plotly plot of cumulative change by SET
    output$plotly_cumu_set <- renderPlotly({
        req(input$SET)
        b <- plot_cumu_set(data = cumu_out$set,
                           columns = input$columns, 
                           pointsize = input$ptsize_multi,
                           scales = input$scales_multi,
                           smooth = input$cumu_smooth,
                           lty_smooth = 1) 
            
        b
    })
    
}

# Create Shiny object
shinyApp(ui = ui, server = server)
