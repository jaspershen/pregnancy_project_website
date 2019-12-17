  fluidPage(
    tabsetPanel(id = "daDataCheakResult", type = "tabs",
                tabPanel(title = "Data check result",
                         icon = icon("table"),
                         # shinysky::busyIndicator(text = "Upload data and check..."),
                         span(textOutput(outputId = "da.data.check.result.message"), style = "color:red"),
                         DT::dataTableOutput(outputId = "da.data.check.result"),

                         br(), br(),
                         actionButton(inputId = "da.data.check.2.da.upload",
                                      # inputId = "dadataCheck2daupload",
                                      label = "Previous", styleclass = "info"),
                         actionButton(inputId = "da.data.check.2.uni.analysis",
                                      # inputId = "DAdataCheck2DAdataProfile",
                                      label = "Next", styleclass = "warning"),
                         helpText("If there are Errors in your data, please click",
                                  strong("Previous"), "to check your data and upload again."),
                         br(),br(),br()
                )
    )
  )