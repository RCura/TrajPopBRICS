###########################
# File: ui.R
# Description: UI for Shiny Application TrajPop
# Date: 26/06/2013
# Author: Robin Cura (robin.cura@gmail.com)
###########################
library(shiny)

shinyUI(fluidPage(theme = "spacelab.bootstrap.min.css",
                  tags$head(tags$link(rel="icon", type="image/png", href="favicon.png"), 
                            tags$title("TrajPop"),
                            includeScript("www/analytics.js"),
                            includeScript("http://cdn.leafletjs.com/leaflet-0.7.3/leaflet.js"),
                            includeCSS("http://cdn.leafletjs.com/leaflet-0.7.3/leaflet.css")
                            ),
                  #absolutePanel(h1("ABC"), bottom = "10%", left = "10%", width = "150px", height = "200px", draggable = TRUE, cursor = "move"),
                  fluidRow(
                    column(4, tags$img(src="logo_UMR.jpg", height="150px")),
                    column(4, h1("TrajPop")),   
                    column(4, tags$img(src="logo_GeoDC.jpg", height="150px"))
                  ),
                  sidebarLayout(
                    sidebarPanel(
                      checkboxInput(inputId = "slowInternet", label = "Slow internet ?", value = TRUE),
                      selectInput(inputId = 'dataset', label = "Choose country",
                                  multiple = FALSE,
                                  choices = c("South Africa", "Brazil", "Russia", "India", "China", "USA", "France")),                   
  
                      selectInput("timeColumnSelected", "Temporal columns :", choices="", multiple=TRUE),
                      htmlOutput('nbCities'),
                      selectizeInput(inputId = 'brewerPalette', label="Color Palette", choices = colorPaletteList,
                                     multiple=FALSE, selected="Set1", options = list(
                                       dropdownParent = 'body',
                                       render = I(sprintf(
                                         "{
option: function(item, escape) {
return '<div><img ' +
'src=\"palettes/' + item.value + '.svg\" /> ' + item.value + ' </div>';
}
}")))),
                      br(),
                      sliderInput("nbClusters",
                                  "Number of wanted clusters:",
                                  value = 3,
                                  min = 2,
                                  max = 20),
                      br(),
                      downloadButton("downloadPDF", "Download the PDF report")
                      
                    ),
                    
                    mainPanel(
                      uiOutput(outputId='customTabsets'),
                      tabsetPanel(
                        tabPanel(title = "Clusters Plots",
                                 #uiOutput("debugTools"),
                                 plotOutput("tree"),
                                 conditionalPanel(condition = "!input.slowInternet",
                                                  ggvisOutput("afc")),
                                 conditionalPanel(condition = "input.slowInternet",
                                                  plotOutput("afc_slow")),
                                 plotOutput("clustersMean"),
                                 plotOutput("clustersWeights"),
                                 plotOutput("clustersMeanWeights")
                                 
                        ),
                        tabPanel(title="Clusters measures",
                                 tags$h5("Means"),
                                 tableOutput("clusterMeans"),
                                 tags$h5("Counts"),
                                 tableOutput("clustersCounts"),
                                 tags$h5("Variances"),
                                 tableOutput("clustersVariances"),
                                 tags$h5("Distances (χ²)"),
                                 tags$em("χ² distance between the mean profile and each cluster mean profile"),
                                 tableOutput("clustersDistances")
                        ),
                        
                        tabPanel(title="RankSize & RankClock",
                                 conditionalPanel(condition = "!input.slowInternet",
                                                  ggvisOutput("ranksize2")),
                                 conditionalPanel(condition = "input.slowInternet",
                                                  plotOutput("ranksize"))
                                 ),
                        tabPanel("Table",
                                 conditionalPanel(condition = "!input.slowInternet",
                                                  dataTableOutput('mytable')),
                                 conditionalPanel(condition = "input.slowInternet",
                                                  tableOutput('mytable_slow')),
                                 downloadButton(outputId='tableExport', label='Download Table')),
                        tabPanel("Maps",
                                 fluidRow(
                                   column(6,
                                          selectInput('sizeAttribute', 'Scales points on :',
                                                      choices="", multiple=FALSE)),
                                   column(6, sliderInput('maxSize', 'Max. point size',
                                                         value=25, min=1, max=100))
                                   ),
                                 conditionalPanel(condition = "!input.slowInternet",
                                                  htmlOutput('webmap')),
                                 conditionalPanel(condition = "input.slowInternet",
                                                  plotOutput('ggmap'))
                        ),
                        tabPanel('Correspondence',
                                 fluidRow(
                                   column(6, selectInput("correspondanceColumnSelected", "Correspondence column :",choices="", multiple=FALSE)),
                                   column(6, selectInput("correspondanceType", "Type :", choices=c("Chi2", 'ANOVA'), multiple=FALSE, selected="ANOVA"))
                                   ),
                                 conditionalPanel(
                                   condition = '(input.correspondanceType == "Chi2") && (input.correspondanceColumnSelected != "None")',
                                   h3('Contingency Table'),
                                   htmlOutput(outputId='contingencyTable'),
                                   h3('Chi² test'),
                                   htmlOutput(outputId='Chi2results'),
                                   h3('Residuals Table'),
                                   htmlOutput(outputId="Chi2residuals")
                                 ),
                                 
                                 conditionalPanel(
                                   condition = '(input.correspondanceType == "ANOVA")',
                                   checkboxInput(inputId='logBoxPlot', label='Log10', value=TRUE),
                                   plotOutput(outputId='AnovaBoxPlot'),
                                   htmlOutput(outputId='AnovaResults')
                                 )),
                        tabPanel(title="About", includeMarkdown(path="README.md"))
                      )
                    ))))
