  fluidPage(
    tabsetPanel(id = "dcDataCheakResult", type = "tabs",
      tabPanel(title = "Data check result",
               icon = icon("table"),
               # shinysky::busyIndicator(text = "Upload data and check..."),
               span(textOutput(outputId = "dc.data.check.result.info"), style = "color:red"),
               DT::dataTableOutput(outputId = "dc.data.check.result"),

               br(), br(),
               actionButton(inputId = "dc.data.check.2.dc.upload",
                            # inputId = "DCdataCheck2DCupload",
                            label = "Previous", styleclass = "info"),
               actionButton(inputId = "dc.data.check.2.dc.batch.alignment",
                            # inputId = "DCdataCheck2DCdataProfile",
                            label = "Next", styleclass = "warning"),
               helpText("If there are Errors in your data, please click",
                        strong("Previous"), "to check your data and upload again."),
               br(),br(),br()
      )
    )
  )




  # fluidPage(
  #   column(width = 10, offset = 1,
  #          # shinysky::busyIndicator(text = "Upload data and check..."),
  #          DT::dataTableOutput(outputId = "dc.data.check.result"),
  #
  #          br(), br(),
  #          actionButton(inputId = "dc.data.check.2.dc.upload",
  #                       # inputId = "DCdataCheck2DCupload",
  #                       label = "Previous", styleclass = "info"),
  #          actionButton(inputId = "dc.data.check.2.dc.batch.alignment",
  #                       # inputId = "DCdataCheck2DCdataProfile",
  #                       label = "Next", styleclass = "warning"),
  #          helpText("If there are Errors in your data, please click",
  #                   strong("Previous"), "to check your data and upload again."),
  #          br(),br(),br()
  #
  #   )
  # )