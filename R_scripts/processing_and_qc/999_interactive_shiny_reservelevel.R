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
source(here::here('R_scripts', '000_functions.R'))



################################################################################
# get the data in
################################################################################

# find the folder with processed data
path <- here::here('data', 'processed')

# in that folder, find the name of the file(s) that ends with 'set_QC.csv'
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


# cumulative change
calc_change_cumu(dat)
# incremental change
calc_change_incr(dat)


### define UI
ui <- fluidPage(
    titlePanel("SET graphing"),
    sidebarLayout(
        sidebarPanel(
            
            # show the name of the file in use
            paste("File in use is:", filelist),
            br(), br(),
            
            # select a SET to plot
            selectInput(inputId = "SET", label = strong("SET ID"),
                        choices = unique(dat$set_id),
                        selected = unique(dat$set_id)[1]
            ),
            
            # select date range to plot
            dateRangeInput("date", strong("Date range"), 
                           start = min(dat$date, na.rm = TRUE), 
                           end = max(dat$date, na.rm = TRUE)
            ),

            # select size of points for single-panel plots
            sliderInput(inputId = "ptsize_single", label = "point size, single plots",
                        min = 0.5,
                        max = 4,
                        value = 2,
                        step = 0.5),
            
            # select size of points for multi-panel plots
            sliderInput(inputId = "ptsize_multi", label = "point size, multi-panel plots",
                        min = 0.5,
                        max = 4,
                        value = 1,
                        step = 0.5),
            
            # select whether to overlay a trend line
            checkboxInput(inputId = "lmsmooth", 
                          label = strong("Overlay linear regression"),
                          value = FALSE
            ),
            
            # select whether to overlay a loess line
            checkboxInput(inputId = "loesssmooth", 
                          label = strong("Overlay loess smooth"),
                          value = FALSE
            ),
            
            # select the span for loess
            # only if loess box is true
            conditionalPanel(
                condition = "input.loesssmooth == true",
                sliderInput(inputId = "loess_span",
                        label = "loess span",
                        min = 0.1,
                        max = 1.5,
                        value = 0.5,
                        step = 0.1)
            )
            
            
            
        ),
        
        
        mainPanel(
            plotlyOutput(outputId = "plotlyscatter"),
            br(), br(),
            plotlyOutput(outputId = "plotly_raw_arm"),
            br(), br(),
            plotlyOutput(outputId = "plotly_raw_pin"),
            br(), br(),
            plotlyOutput(outputId = "plotly_incr_pin")
        )
        
    )
)



# Define server function
server <- function(input, output) {
    
    
    # subset data, reactively
    dat2 <- reactive({
        req(input$SET)
        req(input$date)
        dat %>%
            filter(set_id == input$SET,
                   date >= as.Date(input$date[1]),
                   date <= as.Date(input$date[2]))
    })

    
    # create plotly object
    output$plotlyscatter <- renderPlotly({
        
        req(input$SET)
        req(input$date)
        
        # create the base plot
        p <- ggplot(dat2()) +
            geom_point(aes(x = date, y = pin_height, col = as.factor(arm_position))) +
            labs(title = paste("Raw pin measurements at", input$SET), x = "Date", y = "pin height (mm)") +
            theme_bw() +
            scale_color_discrete(name = 'Arm Position') +
            theme(legend.position = 'bottom')
        
        
        # add smoothing layers if checked
        if (input$lmsmooth == TRUE) 
            p <- p + geom_smooth(aes(x = date, y = pin_height), 
                                 method = "lm", 
                                 se = FALSE,
                                 col = "red3",
                                 alpha = 0.5)
        
        if (input$loesssmooth == TRUE) 
            p <- p + geom_smooth(aes(x = date, y = pin_height), 
                                 method = "loess", 
                                 se = FALSE, 
                                 span = input$loess_span,
                                 alpha = 0.5)
        
        # return the plot
        p
        
    })
    
    
    # create plotly plot of avg raw reading by arm
    output$plotly_raw_arm <- renderPlotly({
        req(input$SET)
        req(input$date)
        q <- plot_raw_arm(dat2(), pointsize = input$ptsize_single)
        q
    }) 
    
    # create plotly plot of raw readings by pin
    output$plotly_raw_pin <- renderPlotly({
        req(input$SET)
        req(input$date)
        z <- plot_raw_pin(dat2(), set = input$SET, pointsize = input$ptsize_multi)
        z
    })
    
    
    # create plotly plot of incremental change by pin
    output$plotly_incr_pin <- renderPlotly({
        req(input$SET)
        req(input$date)
        a <- plot_incr_pin2(input$SET, pointsize = input$ptsize_multi)
        a
    })
    
}

# Create Shiny object
shinyApp(ui = ui, server = server)
