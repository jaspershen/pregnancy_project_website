fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(

    # uiOutput(outputId = "da.group.area2"),

    selectInput(inputId = "daMAlog",
                label = "Logarithm method",
                choices = c(
                  "No log" = "no",
                  "Log 2" = "log2",
                  "Log e" = "loge",
                  "Log 10" = "log10"
                ), selected = "log10"),

    selectInput(inputId = "daMAscale",
                label = "Scale method",
                choices = c(
                  "Pareto scale" = "pareto",
                  "Auto scale" = "auto",
                  "No scale" = "no"
                ), selected = "auto"),

    checkboxInput(inputId = "daMAcenter",
                  label = "Center or not",
                  value = TRUE),

    # numericInput(inputId = "daMAvipCutoff",
    #              label = "VIP cutoff",
    #              value = 0, min = 0, max = 10),


#------------------------------------------------------------------------------

    actionButton(inputId = 'daMAsubmitButton',
                 "Submit",
                 styleclass = "info",
                 icon = icon('play-circle')),
    useShinyalert(),
    actionButton(inputId = "daMA2DM",
                 label = "Next", styleclass = "warning"),
    helpText("Click", strong("Submit"), "to do multivariate analysis")
  ),


  mainPanel = mainPanel(
    span(textOutput(outputId = "da.ma.params.message"), style = "color:red"),
    tabsetPanel(id = "daMAresult",
                type = "tabs",
                tabPanel("PCA analysis",
                         value = "daMApca",
                         icon = icon("image"),
                         shinysky::busyIndicator(text = "Processing..."),
                         span(textOutput(outputId = "daMApca.message"), style = "color:red"),
                         span(textOutput(outputId = "daMApcaPlot.message"), style = "color:red"),
                         plotly::plotlyOutput(outputId = "daMApcaPlot",
                                              height = "600px",
                                              width = "100%")
                ),

                tabPanel("PLS analysis",
                         value = "daMApls",
                         icon = icon("image"),
                         actionButton(inputId = "daMAplsShowQ2cum",
                                      label = "Q2cum",
                                      styleclass = "info"),
                         bsModal(id = "ModalExampleDAma1",
                                 title = "Q2cum",
                                 trigger = "daMAplsShowQ2cum",
                                 size = "large",
                                 # plotOutput("dc.single.peak.plot.dn"),
                                 plotly::plotlyOutput("daMAplsQ2Plot"),
                                 sliderInput(inputId = "daMAncomp",
                                             label = "Number of component",
                                             min = 1,
                                             max = 10,
                                             value = 3,
                                             step = 1,
                                             round = TRUE),
                                 # numericInput(inputId = "daMAncomp",
                                 #              label = "Number of component",
                                 #              value = 3, min = 1,
                                 #              max = 10, step = 1),
                                 actionButton(inputId = "daMAplsSubmitButton",
                                              label = "Submit",
                                              styleclass = "info")
                         ),

                         actionButton(inputId = "daMAplsShowQ2R2cum",
                                      label = "Q2cum&R2cum",
                                      styleclass = "info"),
                         bsModal(id = "ModalExampleDAma2",
                                 title = "Q2cum & R2cum",
                                 trigger = "daMAplsShowQ2R2cum",
                                 size = "large",
                                 # plotOutput("dc.single.peak.plot.dn"),
                                 plotly::plotlyOutput("daMAplsQ2R2Plot")
                         ),

                         shinysky::busyIndicator(text = "Processing..."),
                         span(textOutput(outputId = "daMApls.message"), style = "color:red"),
                         span(textOutput(outputId = "daMAplsQ2Plot.message"), style = "color:red"),
                         span(textOutput(outputId = "daMApls2.message"), style = "color:red"),
                         span(textOutput(outputId = "daMAplsQ2R2Plot.message"), style = "color:red"),
                         span(textOutput(outputId = "daMAvip.message"), style = "color:red"),
                         plotly::plotlyOutput(outputId = "daMAplsPlot",
                                              height = "600px",
                                              width = "100%"
                                              )
                         # icon = icon("table"),
                         # shinysky::busyIndicator(text = "Processing..."),
                         # plotly::plotlyOutput(outputId = "da.volcano.plot", height = "500px")
                         # DT::dataTableOutput(outputId = "dc.ms1.before.mv")
                ),

                tabPanel("HCA analysis",
                         value = "daMAhca",
                         icon = icon("image"),
                         actionButton(inputId = "daMAhcaShowParameter",
                                     label = "Parameter setting",
                                     styleclass = "info"),
                         bsModal(id = "ModalExampleDAma3",
                                 title = "Parameter setting",
                                 trigger = "daMAhcaShowParameter",
                                 size = "small",
                                 selectInput(inputId = "daMAhcaClusteringDistanceRows",
                                             label = "Distance measure used in clustering rows",
                                             choices = c(
                                               "Euclidean" = "euclidean",
                                               "Maximum" = "maximum",
                                               "Manhattan" = "manhattan",
                                               "Canberra" = "canberra",
                                               "Binary" = "binary",
                                               "Minkowski" = "minkowski"
                                             ), selected = "euclidean"),

                                 selectInput(inputId = "daMAhcaClusteringDistanceCols",
                                             label = "Distance measure used in clustering columns",
                                             choices = c(
                                               "Euclidean" = "euclidean",
                                               "Maximum" = "maximum",
                                               "Manhattan" = "manhattan",
                                               "Canberra" = "canberra",
                                               "Binary" = "binary",
                                               "Minkowski" = "minkowski"
                                             ), selected = "euclidean"),

                                 selectInput(inputId = "daMAhcaClusteringMethod",
                                             label = "Clustering method",
                                             choices = c(
                                               "Ward.D" = "ward.D",
                                               "Ward.D2" = "ward.D2",
                                               "Single" = "single",
                                               "Complete" = "complete",
                                               "Average" = "average",
                                               "Mcquitty" = "mcquitty",
                                               "Median" = "median",
                                               "Centroid" = "centroid"
                                             ),
                                             selected = "ward.D"),

                                        checkboxInput(inputId = "daMAhcaClusterRows",
                                                      label = "Cluster rows",
                                                      value = TRUE),

                                        checkboxInput(inputId = "daMAhcaClusterCols",
                                                      label = "Cluster columns",
                                                      value = TRUE),

                                                   checkboxInput(inputId = "daMAhcaShowRowNames",
                                                                 label = "Show row names",
                                                                 value = TRUE),
                                                   checkboxInput(inputId = "daMAhcaShowColNames",
                                                                 label = "Show column names",
                                                                 value = TRUE),
                                 colourpicker::colourInput(inputId = "daMAhcaGroup1Col",
                                                               label = "Control group color",
                                                               value = "dodgerblue",
                                                               returnName = TRUE,
                                                               palette = "square",
                                                               # showColour = TRUE,
                                                               allowTransparent = TRUE),

                                 colourpicker::colourInput(inputId = "daMAhcaGroup2Col",
                                                               label = "Case group color",
                                                               value = "firebrick1",
                                                               returnName = TRUE,
                                                               palette = "square",
                                                               # showColour = TRUE,
                                                               allowTransparent = TRUE),
                                            ##color for high and low
                                 colourpicker::colourInput(inputId = "daMAhcaLowCol",
                                                               label = "Low",
                                                               value = "navy",
                                                               returnName = TRUE,
                                                               palette = "square",
                                                               # showColour = TRUE,
                                                               allowTransparent = TRUE),

                                 colourpicker::colourInput(inputId = "daMAhcaMiddleCol",
                                                               label = "Middle",
                                                               value = "white",
                                                               returnName = TRUE,
                                                               palette = "square",
                                                               # showColour = TRUE,
                                                               allowTransparent = TRUE),

                                 colourpicker::colourInput(inputId = "daMAhcaHighCol",
                                                               label = "High",
                                                               value = "firebrick",
                                                               returnName = TRUE,
                                                               palette = "square",
                                                               # showColour = TRUE,
                                                               allowTransparent = TRUE),
                                 actionButton(inputId = "daMAhcaSubmitButton",
                                              label = "Submit",
                                              styleclass = "info"),
                                          br(), br(), br()
                         ),
                         span(textOutput(outputId = "daMAhca.message"), style = "color:red"),
                         plotOutput(outputId = "daMAhcaHeatmap",
                                    height = "600px",
                                    width = "100%"),
                         dropdownButton(
                           sliderInput(inputId = "daMAhcaHeatmapWidth",
                                       label = "Width (inch)", min = 6,
                                       max = 20, value = 7),

                           sliderInput(inputId = "daMAhcaHeatmapHeight",
                                       label = "Width (inch)", min = 6,
                                       max = 20, value = 7),

                           downloadButton('daMAhcaHeatmapDownload', 'Download heatmap'),

                           circle = TRUE, status = "info", icon = icon("download"),
                           width = "500px",
                           tooltip = tooltipOptions(title = "Download image"),
                           # inputId = "ma.hca.download.DB",
                           right = FALSE, up = FALSE
                         )
    ),
    tabPanel("Fold-change & P-value & VIP",
             value = "daMAvip",
             icon = icon("table"),
             shinysky::busyIndicator(text = "Processing..."),
             DT::dataTableOutput(outputId = "daMAfcPvip")
    )
  )
)
),
br(), br(), br()
)