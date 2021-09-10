
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Load ingredients (data, theme)
load("ingredients.Rdata")
qrtrs <- data.frame(q=c("2019-Q1", "2019-Q2", "2019-Q3", "2019-Q4", "2020-Q1", "2020-Q2", "2020-Q3", "2020-Q4", "2021-Q1", "2021-Q2", "2021-Q3"), n=1:11)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Race to End TB Rankings"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("indicator.i", "Indicator:",
                        unique(ba$indicatorname)),
            selectInput("location_type.i", "Location type:",
                        c("Regions"="region", 
                          "Provinces/HUCs"="province")),
            selectInput("end.i", "End:",
                        qrtrs$q, "2021-Q3"),
            sliderInput("qrts.num.i",
                        "Additional quarters:",
                        min = 1,
                        max = max(qrtrs$n)-1,
                        value = max(qrtrs$n)-1),
            p(paste0("Data as of ", as.of.date, ". Email Tom to refresh the dataset."))
            ),

        # Show a plot of the generated distribution
        mainPanel(
           uiOutput("ui_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # instead of global assignment use reactive values
    # values <- reactiveValues()
    # function where you do all the processing, e.g. read file, get data from it
    # file_processing <- function() {
    #     # imagine this coefficient comes from your file
    #     coefficient <- 10
    #     return(coefficient)
    # }
    # this function defines a height of your plot
    plot_height <- function() {
        # calculate values$facetCount
        # values$facetCount <- as.numeric(input$dimension1In) * file_processing()
        # return(values$facetCount)
        height.param <- length(unique(ba[ba$location_type==input$location_type.i, "loc.name"])) * 25
        return(height.param)
    }

    output$bumpchart <- renderPlot({
        # generate output based on input$...  from ui.R
        end.num <- qrtrs[qrtrs$q==input$end.i, "n"]
        df.rankings <- ba %>% filter(location_type==input$location_type.i, Quarter %in% qrtrs$q[(end.num-input$qrts.num.i):end.num], indicatorname==input$indicator.i) %>% 
            group_by(Quarter) %>% 
            arrange(Quarter, desc(percent), loc.name) %>% 
            mutate(ranking = row_number()) %>% 
            as.data.frame()

        # draw the bumpchart
        ggplot(df.rankings, aes(Quarter, ranking, group = loc.name)) +
            geom_line(aes(color = loc.name, alpha = 1), size = 2) +
            geom_point(aes(color = loc.name, alpha = 1), size = 4) +
            geom_point(color = "#FFFFFF", size = 1) +
            scale_y_reverse(breaks = 1:nrow(df.rankings)) + 
            theme(legend.position = "none") +
            scale_x_discrete(expand = expansion(add = 2)) + # c(.19, .05)) +
            geom_text(data = df.rankings %>% filter(Quarter == min(df.rankings$Quarter)),
                      aes(label = loc.name, x = 0.7) , hjust = 1, fontface = "bold", color = "#888888", size = 4) +
            geom_text(data = df.rankings %>% filter(Quarter == max(df.rankings$Quarter)), aes(label = loc.name, x = (input$qrts.num.i) + 1.3) , hjust = 0, fontface = "bold", color = "#888888", size = 4) +
            theme(legend.position = "none") +
            labs(x = "",
                 y = "Rank",
                 title = df.rankings$indicatorname[1],
                 subtitle = "Ranked by performance each quarter") +
            my_theme() 
    })
    
    output$ui_plot <- renderUI({
        plotOutput("bumpchart", height = plot_height(), width = "100%")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
