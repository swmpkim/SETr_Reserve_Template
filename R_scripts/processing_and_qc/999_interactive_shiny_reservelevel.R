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
            
            # set threshold for incremental change plots
            sliderInput(inputId = "incr_threshold", label = "threshold for incremental change",
                        min = 10,
                        max = 100,
                        value = 25,
                        step = 5),
            
            # select scales for faceting
            selectInput(inputId = "scales_multi", 
                        label = strong("fixed or flexible scales? \nmulti-panel plots"),
                        choices = c("fixed", "free", "free_y", "free_x"),
                        selected = "fixed"
            ),
            
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
            ),
            
            
            # select the number of columns to show
            # in the cumulative change graph
            selectInput(inputId = "columns", 
                        label = strong("# columns in cumulative change graph"),
                        choices = c(1, 2, 3, 4, 5),
                        selected = 4
            )
            
            
            
        ),
        
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
            
                        tabPanel("Raw Data",
                                 plotlyOutput(outputId = "plotlyscatter"),
                                 br(), br(),
                                 plotlyOutput(outputId = "plotly_raw_arm"),
                                 br(), br(),
                                 plotlyOutput(outputId = "plotly_raw_pin")
                        ),
                        
                        tabPanel("Incremental Calcs",
                                 plotlyOutput(outputId = "plotly_incr_pin"),
                                 br(), br(),
                                 # HTML(paste0("The following observations are above the selected threshold.")),
                                 textOutput(outputId = "count_incr_pin"),
                                 br(), 
                                 tableOutput(outputId = "tbl_incr_pin"),
                                 br(), br(),
                                 plotlyOutput(outputId = "plotly_incr_arm")
                        ),
                        
                        tabPanel("Cumulative Calcs",
                                 plotlyOutput(outputId = "plotly_cumu_set")
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
    
    # 
    # incr_out_sub_exceed <- reactive({
    #     req(input$date)
    #     req(input$SET)
    #     req(input$threshold)
    #     pins <- incr_out$pin %>% 
    #         filter()
    # })
    
    # filter the incremental subset based on the user selected threshold
    # incr_out_sub2 <- reactive({
    #     incr_out_sub()$pin %>% 
    #         dplyr::filter(abs(incr) >= input$threshold)
    # })

    
    # create plotly object
    output$plotlyscatter <- renderPlotly({
        
        req(input$SET)
        req(input$date)
        
        # create the base plot
        p <- ggplot(dat_sub()) +
            geom_point(aes(x = date, y = pin_height, col = as.factor(arm_position))) +
            labs(title = paste("Raw pin measurements at", input$SET), 
                 x = "Date", 
                 y = "pin height (mm)",
                 color = "Arm Position") +
            theme_bw() +
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
        q <- plot_raw_arm(dat_sub(), 
                          pointsize = input$ptsize_single,
                          scales = input$scales_multi)
        q 
    }) 
    
    # create plotly plot of raw readings by pin
    output$plotly_raw_pin <- renderPlotly({
        req(input$SET)
        req(input$date)
        z <- plot_raw_pin(dat_sub(), set = input$SET, 
                          pointsize = input$ptsize_multi,
                          scales = input$scales_multi)
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
        paste0("There are ", nrow(exceed), " observations at this SET outside the selected threshold: ")
    })
    
    # make a table of pins outside the selected threshold
    output$tbl_incr_pin <- renderTable({
        incr_out_sub()$pin %>% 
            filter(abs(incr) >= input$incr_threshold,
                   set_id == input$SET)},
        striped = TRUE, spacing = "l", width = "90%",
        caption = "Pin readings outside the selected threshold."
    )
    
    
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
                           scales = input$scales_multi)
        b
    })
    
}

# Create Shiny object
shinyApp(ui = ui, server = server)
