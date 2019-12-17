fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(


    numericInput(inputId = "daDMpCutoff", label = "P-value cutoff",
                value = 0.05, min = 0, max = 1),
    numericInput(inputId = "daDMfcCutoff", label = "Fold-change cutoff",
                 value = 2, min = 1, max = 50),

    numericInput(inputId = "daDMvipCutoff",
                 label = "VIP cutoff",
                 value = 1, min = 0, max = 10),


#------------------------------------------------------------------------------

    actionButton(inputId = 'daDMsubmitButton',
                 "Submit",
                 styleclass = "info",
                 icon = icon('play-circle')),
    useShinyalert(),
    actionButton(inputId = "daDM2validation",
                 label = "Next", styleclass = "warning"),
    helpText("Click", strong("Submit"), "to select differential metabolites.")
  ),


  mainPanel = mainPanel(
    span(textOutput(outputId = "da.volcano.info"), style = "color:red"),
    span(textOutput(outputId = "da.volcano.3d.plot.message"), style = "color:red"),
    span(textOutput(outputId = "da.dm.params.message"), style = "color:red"),
    tabsetPanel(id = "daMAresult",
                type = "tabs",
                tabPanel("3D plot",
                         value = "daDMvolcanoPlot3d",
                         icon = icon("image"),
                         span(textOutput(outputId = "daDifferentialMetabolite.message"), style = "color:black"),
                         shinysky::busyIndicator(text = "Processing..."),
                         plotly::plotlyOutput(outputId = "da.volcano.3d.plot",
                                              height = "600px",
                                              width = "100%")
                ),

                tabPanel("Differential metabolites",
                         value = "daDM",
                         icon = icon("table"),
                         shinysky::busyIndicator(text = "Processing..."),
                         DT::dataTableOutput(outputId = "daDifferentialMetabolite")
                )
  )
)
),
br(), br(), br()
)