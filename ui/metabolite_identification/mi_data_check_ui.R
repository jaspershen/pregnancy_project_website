  fluidPage(
    tabsetPanel(id = "miDataCheckResult",
                type = "tabs",
                tabPanel(title = "Check Result",
                         value = "miCheckResult",
                         DT::dataTableOutput(outputId = "mi.data.check.result"),
                         actionButton(inputId = "mi.data.check.2.mi.upload",
                                      label = "Previous", styleclass = "info"),

                         actionButton(inputId = "mi.data.check.2.mz.match",
                                      label = "Next", styleclass = "warning"),

                         helpText("If there are Errors in your data, please click",
                                  strong("Previous"), "to check your data and upload again."),
                                br(), br(), br()
                         ),

                tabPanel(title = "Data Profile",
                         value = "miDataFile",
                         conditionalPanel(
                           condition = 'input.miDataCheckResult == "miDataFile"',
                           column(width = 7,
                                  shinysky::busyIndicator(text = "Processing..."),
                                  plotlyOutput(outputId = "mi.ms2.plot",
                                               # width = "800px",
                                               height = "500px"
                                  )
                           ),
                           column(width = 5,
                                  shinysky::busyIndicator(text = "Processing..."),
                                  plotlyOutput(outputId = "mi.single.ms2.plot",
                                               # width = "800px",
                                               height = "500px"
                                  )
                           ), br(), br(), br()

                         )
                        )
                )
  )
