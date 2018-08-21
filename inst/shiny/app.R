library(shiny)
library(shinydashboard)
# library(sparkline)
# library(jsonlite)
library(dplyr)
# library(leaflet)
library(sfi)
library(raster)
source('global.R')

header <- dashboardHeader(title="SFI aesthetics tweak-o-matic 1.0")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Main",
      tabName="main",
      icon=icon("database")),
    menuItem(
      text = 'About',
      tabName = 'about',
      icon = icon("cog", lib = "glyphicon"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(
      tabName="main",
      fluidPage(
        fluidRow(
          # h1(icon('line-chart'), align = 'center'),
          h1('SFI aesthetics tweak-o-matic 1.0', align = 'center'),
          h5("An initiative of SFI and DataBrew", align = 'center'),
          h1(icon('sitemap'), align = 'center')
          ),
        fluidRow(
          column(3,
                 sliderInput('base_size',
                             'Font base size',
                             min = 0, max = 30,
                             value = 12),
                 checkboxInput('nomargin',
                              'No margin',
                              value = FALSE),
                 selectInput('fc', 'Font color', 
                             choices = c('black', 'blue', 'darkblue', 'brown',
                                         'darkgrey', 'grey', 'red', 'orange', 'darkorange', 'green')),
                 checkboxInput('gM', 'Show major grid?',
                               value = TRUE),
                 checkboxInput('gm', 'Show minor grid?',
                               value = FALSE),
                 selectInput('gc', 'Grid color', 
                             choices = c('black', 'blue', 'darkblue', 'brown',
                                         'darkgrey', 'grey', 'red', 'orange', 'darkorange', 'green')),
                 selectInput('gl', 'Line type',
                             choices = c('Blank' = 0,
                                         'Solid' = 1,
                                         'Dashed' = 2,
                                         'Dotted' = 3,
                                         'Dot-dash' = 4,
                                         'Long dash' = 5,
                                         'Double dash' = 6)),
                 checkboxInput('boxes', 'Box around plot',
                               value = FALSE),
                 selectInput('bc', 'Background color', 
                             choices = c('white', 'black', 'blue', 'darkblue', 'brown',
                                         'darkgrey', 'grey', 'red', 'orange', 'darkorange', 'green')),
                 selectInput('pc', 'Panel background color', 
                             choices = c('transparent', 'white', 'black', 'blue', 'darkblue', 'brown',
                                         'darkgrey', 'grey', 'red', 'orange', 'darkorange', 'green')), 
                 selectInput('lp', 'Legend position',
                             choices = c('right', 'left', 'top', 'bottom')),
                 sliderInput('axis', 'Axis', min = 1, max = 4, value = 1),
                 sliderInput('point_size', 'Point size', min = 0.1,
                             max = 10,
                             step = 0.1,
                             value = 3),
                 sliderInput('point_type',
                                          'Point type',
                                          min = 0,
                                          max = 25,
                                          step = 1,
                                          value = 16),
                 sliderInput('point_opacity',
                                             'Point opacity',
                                             min = 0,
                                             max = 1,
                                             step = 0.05,
                                             value = 0.5)
                 ),
          column(3),
          column(6,
                 plotOutput('plot1'))
        )
      )
    ),
    tabItem(
      tabName = 'about',
      fluidPage(
        fluidRow(
          div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
          h4('This web application was built in partnership with ',
             a(href = 'http://databrew.cc',
               target='_blank', 'Databrew'),
             align = 'center'),
          p('Empowering research and analysis through collaborative data science.', align = 'center'),
          div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                             icon = icon("envelope", lib = "font-awesome")),
                href="mailto:info@databrew.cc",
                align = 'center')), 
          style = 'text-align:center;'
        )
      )
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    make_g1(point_size = input$point_size,
            point_type = input$point_type,
            point_opacity = input$point_opacity,
            base_size = input$base_size,
            nomargin = input$nomargin,
            fc = input$fc,
            gM = input$gM,
            gm = input$gm,
            gc = input$gc,
            gl = as.numeric(input$gl),
            boxes = input$boxes,
            bc = input$bc,
            pc = input$pc,
            lp = input$lp,
            axis = input$axis
            )
  })
  
}

shinyApp(ui, server)