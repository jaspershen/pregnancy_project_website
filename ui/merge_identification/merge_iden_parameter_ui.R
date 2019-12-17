fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(
    # h4("Batch alignment parameters"),
    numericInput(inputId = "merge.iden.mz.tol",
                 label = "m/z tolerance (ppm)",
                 value = 25, min = 5, max = 50, step = 1),

    numericInput(inputId = "merge.iden.rt.tol",
                 label = "Retention time tolerance (second)",
                 value = 180, min = 5, max = 300, step = 1),

    actionButton(inputId = 'merge.iden.submit.button',
                 "Submit",
                 styleclass = "info",
                 icon = icon('play-circle')),
    useShinyalert(),
    actionButton(inputId = "merge.iden.parameter.2.result.download",
                 label = "Next", styleclass = "warning"),
    helpText("Click", strong("Submit"), "to merge identification.")
  ),
  mainPanel = mainPanel(
    span(textOutput(outputId = "merge.iden.params.message"), style = "color:black"),
    tabsetPanel(id = "merge.result", type = "tabs",
                tabPanel(title = "Merge identification",
                         icon = icon("table"),
                         shinysky::busyIndicator(text = "Processsing..."),
                         DT::dataTableOutput(outputId = "merge.iden.peak.identification")
                )
                ),
    br(),br(),br()
  )
  )
)