rm(list=ls())
cat("\014")
setwd('/Users/evelyn/Documents/2017Spring/MSAN622/assignment3')

load_package <- function(package_name){
  if(package_name %in% rownames(installed.packages()) == FALSE) {install.packages(package_name)}
}

load_package('d3heatmap'); library(d3heatmap)
load_package('shiny'); library(shiny)
load_package('pairsD3'); library(pairsD3)
load_package('parcoords'); library(parcoords)
load_package('githubinstall'); library(githubinstall)

options(unzip = 'internal')
devtools::install_github("timelyportfolio/parcoords")
library(parcoords)

fb <- read.csv('dataset_Facebook.csv', sep = ";",header=T)
names(fb) <- c("Total.likes","Type","Category","Month","Weekday","Hour","Paid","Post.Reach","Post.Imp","Users","Consumers","Consumptions","Imp.by.people","reach.by.people","liked.and.engaged","comment","like","share","Interactions")
my_min <- 1
my_max <- 4

## Only run examples in interactive R sessions
if (interactive()) {
  options(device.ask.default = FALSE)
  
  ui <- fluidPage(
    tags$style("
              body {
    -moz-transform: scale(0.65, 0.65); /* Moz-browsers */
    zoom: 0.65; /* Other non-webkit browsers */
    zoom: 65%; /* Webkit browsers */
}
              "),
    # titlePanel("MSAN 622 Facebook Dataset"),
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("SelecetedVars", "Select four factors from below:",
                           c("Page total likes" = "Total.likes",'Type' = "Type","Category" = "Category", "Post Month" = "Month", "Post Weekday" = "Weekday","Post Hour" = "Hour", "Paid" = "Paid", "Lifetime Post Total Reach" = "Post.Reach", "Lifetime Post Total Impressions" = "Post.Imp", "Lifetime Engaged Users" = "Users", "Lifetime Post Consumers" = "Consumers", "Lifetime Post Consumptions" = "Consumptions", "Lifetime Post Impressions by people who have liked your Page" = "Imp.by.people","Lifetime Post reach by people who like your Page" = "reach.by.people", "Lifetime People who have liked your Page and engaged with your post" = "liked.and.engaged", "Comment" = "comment", "Like" = "like", "Share" = "share", "Total Interactions" = "Interactions"), 
                           selected = c("Post.Reach","Weekday","Hour","Paid"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Heat Map", 
                       h3("Heat Map of Facebook dataset"),
                       selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
                       checkboxInput("cluster", "Apply clustering"),
                       d3heatmapOutput("heatmap"),
                       
                       # sliderInput("likes", "Number of Total Page likes:",
                       #             min = 80000, max = 140000, value = c(100000,140000),
                       #             width = '90%'),
                       textOutput("Selected")
          ),
    
          tabPanel("Scatterplot Matrix", 
                       h3("Scatter Matrix of Facebook dataset"),
                       h4("Select range of data to highlight."),
                       selectInput("group", "Color by", c("Type", "Category", "Weekday", "Paid")),
                       pairsD3Output("distPlot", width = "900px",height="800px")
          ),
          
          tabPanel("Parallel Coordinates", 
                   h3("Parallel Coordinates of Facebook dataset"),
                   h4("Select range of line to highlight. Click near the bottom of each column to reset selection"),
                   selectInput("groupPar", "Color by", c("Type", "Category", "Weekday", "Paid")),
                   parcoordsOutput("parcoords", width = "900px", height = "800px" ))
        )
      )
    )
  )
  # Server logic
  server <- function(input, output,session) {
    observe({
      if(length(input$SelecetedVars) > my_max)
      {
        updateCheckboxGroupInput(session, "SelecetedVars", selected= input$SelecetedVars[-3])
      }
      if(length(input$SelecetedVars) < my_min)
      {
        updateCheckboxGroupInput(session, "SelecetedVars", selected = c("Post.Reach","Weekday","Hour","Paid"))
      }
    })
    
    output$heatmap <- renderD3heatmap({
      fb_subset <- fb[(fb$Page.total.likes > input$likes[1]) & (fb$Page.total.likes < input$likes[2]), ]
      heat_df <- aggregate(. ~ Month, data = fb[,c(input$SelecetedVars,'Month')], FUN = sum)
      row.names(heat_df) <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
      heat_df <-within(heat_df, rm(Month))
      d3heatmap(
        heat_df,
        scale="column",
        colors = input$palette,
        dendrogram = if (input$cluster) "both" else "none",
        xaxis_height = 150
      )
    })
    
    output$distPlot <- renderPairsD3({
      pairsD3(fb[,input$SelecetedVars],group=fb[[input$group]], width = "900px")
    })
    
    output$parcoords = renderParcoords({
      parcoords(
        fb[,c(input$groupPar, input$SelecetedVars)]  # order columns so species first
        , rownames=F
        , brushMode="1d"
        , color = list(
          colorScale = htmlwidgets::JS(sprintf(
            'd3.scale.ordinal().range(%s).domain(%s)'
            ,jsonlite::toJSON(RColorBrewer::brewer.pal(4,'Set1'))
            ,jsonlite::toJSON(as.character(unique(fb[[input$groupPar]])))
          ))
          ,colorBy = input$groupPar
        )
      )
    })
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
}

