fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(
    # h4("Batch alignment parameters"),
    numericInput(inputId = "dc.ba.mz.tol",
                 label = "m/z tolerance (ppm)",
                 value = 25, min = 5, max = 50, step = 1),

    numericInput(inputId = "dc.ba.rt.tol",
                 label = "Retention time tolerance (second)",
                 value = 30, min = 5, max = 300, step = 1),

    actionButton(inputId = 'dc.ba.submit.button',
                 "Submit",
                 styleclass = "info",
                 icon = icon('play-circle')),
    useShinyalert(),
    actionButton(inputId = "dc.ba.2.dc.qa1",
                 label = "Next", styleclass = "warning"),
    helpText("Click", strong("Submit"), "to align batch.")
    # helpText("If there is only one batch, please click",
    #          strong("Next"), "to skip this step.")
  ),
  mainPanel = mainPanel(
    span(textOutput(outputId = "dc.batch.alignment.result1.info"), style = "color:red"),
    span(textOutput(outputId = "dc.batch.alignment.result2.info"), style = "color:red"),
    span(textOutput(outputId = "dc.ba.params.message"), style = "color:red"),
    tabsetPanel(id = "dc.ba.match",
                type = "tabs",
                tabPanel(title = "Parameter optimization",
                         icon = icon("image"),
                         tabsetPanel(type = "pills",
                                     tabPanel(title = "m/z vs m/z error",
                                              shinysky::busyIndicator(text = "Processing..."),
                                              plotlyOutput(outputId = "dc.ba.mz.plot",
                                                           width = "100%",
                                                           height = "600px"
                                              ),
                                              br(), br(), br()
                                              ),
                                     tabPanel(title = "RT vs RT error",
                                              shinysky::busyIndicator(text = "Processing..."),
                                              plotlyOutput(outputId = "dc.ba.rt.plot",
                                                           width = "100%",
                                                           height = "600px"
                                              ),
                                              br(), br(), br()
                                     ),
                                     tabPanel(title = "Log10intensity vs Log10intensity error",
                                              shinysky::busyIndicator(text = "Processing..."),
                                              plotlyOutput(outputId = "dc.ba.int.plot",
                                                           width = "100%",
                                                           height = "600px"
                                              ),
                                              br(), br(), br()
                                     )
                                     )
                         ),

                tabPanel(title = "MS1 peak table after batch alignment",
                         icon = icon("table"),
                         shinysky::busyIndicator(text = "Processsing..."),
                         DT::dataTableOutput(outputId = "dc.batch.alignment.result2")
                )
    )
  )
)
)