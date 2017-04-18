library(shiny)
library(ggvis)
library(magrittr)
# setwd('~/Desktop/MSAN/MSAN622/Homework/HW2/Git')
source('cleaning.R')

ui <- fluidPage(
  headerPanel('Population, life expectancy, and fertility rate by year'),
  sidebarPanel(
    sliderInput("year", "Year", 1960, 2015, 1960, step = 1, animate = TRUE),
    checkboxGroupInput("region", "Regions", choices = levels(df_full$Region), selected = levels(df_full$Region))
  ),
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis")
  )
)

server <- function(input, output){
  selected <- reactive({
    df_full[(df_full$Year == input$year) & (df_full$Region %in% input$region),]
    })
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    return(format(x$Country.Name))
  }
  
  selected %>%
      ggvis(~Life.Expectancy, ~Fertility.Rate, key := ~Country.Name) %>% 
      layer_points(fill = ~factor(Region, levels = levels(df_full$Region)), size = ~Pop.Group) %>% 
      hide_legend("size") %>%
      add_legend("fill", title = "Region") %>%
      scale_numeric("x", domain = c(10, 90), nice = FALSE) %>%
      scale_numeric("y", domain = c(0, 9.0), nice = FALSE) %>%
      add_tooltip(all_values, "hover") %>%
      bind_shiny('ggvis', 'ggvis_ui')
}

shinyApp(ui = ui, server = server)