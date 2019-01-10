# just working with one dataset until this all works
# basically copied and adapted code from this app:
# https://shiny.rstudio.com/#code-app

# load packages
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(here)
library(shiny)
library(plotly)



## load and shape data
reserve <- 'GND'
# source functions script
funs_path <- here('R', '000_functions.R')
source(funs_path)
funs_path2 <- here('R', '000_functions_extra.R')
source(funs_path2)
# read in data
path <- here('data', 'intermediate', paste0(reserve, '.csv'))
dat_full <- read_csv(path)
# make sure pin heights are in mm so flagging works properly
dat <- height_to_mm(dat_full)
###### Calculations of Change
#############
# cumulative change
calc_change_cumu(dat)
# incremental change
calc_change_incr(dat)


### define UI
ui <- fluidPage(
    titlePanel("SET graphing"),
    sidebarLayout(
        sidebarPanel(
            
            # select a SET to plot
            selectInput(inputId = "SET", label = strong("SET ID"),
                        choices = unique(dat$set_id),
                        selected = unique(dat$set_id)[1]
            ),
            
            # select date range to plot
            dateRangeInput("date", strong("Date range"), 
                           start = min(dat$date), end = max(dat$date)
            ),

            # select size of points for plots
            sliderInput(inputId = "ptsize", label = "point size",
                        min = 0.5,
                        max = 4,
                        value = 2,
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
        if (input$lmsmooth == TRUE) p <- p + geom_smooth(aes(x = date, y = pin_height), method = "lm", se = FALSE)
        
        if (input$loesssmooth == TRUE) p <- p + geom_smooth(aes(x = date, y = pin_height), method = "loess", se = FALSE, span = input$loess_span)
        
        # return the plot
        p
        
    })
    
    
    # create plotly plot of avg raw reading by arm
    output$plotly_raw_arm <- renderPlotly({
        req(input$SET)
        req(input$date)
        q <- plot_raw_arm(dat2(), pointsize = input$ptsize)
        q
    }) 
    
    # create plotly plot of raw readings by pin
    output$plotly_raw_pin <- renderPlotly({
        req(input$SET)
        req(input$date)
        z <- plot_raw_pin(dat2(), set = input$SET, pointsize = input$ptsize)
        z
    })
    
    
    # create plotly plot of incremental change by pin
    output$plotly_incr_pin <- renderPlotly({
        req(input$SET)
        req(input$date)
        a <- plot_incr_pin2(input$SET, pointsize = input$ptsize)
        a
    })
    
}

# Create Shiny object
shinyApp(ui = ui, server = server)
