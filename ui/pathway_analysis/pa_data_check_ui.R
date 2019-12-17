fluidPage(
  tabsetPanel(id = "paDataCheakResult", type = "tabs",
              tabPanel(title = "Data check result",
                       icon = icon("table"),
                       # shinysky::busyIndicator(text = "Upload data and check..."),
                       span(textOutput(outputId = "pa.data.check.result.message"), style = "color:red"),
                       DT::dataTableOutput(outputId = "match.result"),

                       br(), br(),
                       actionButton(inputId = "pa.data.check.2.pa.enter",
                                    # inputId = "dadataCheck2daupload",
                                    label = "Previous", styleclass = "info"),
                       actionButton(inputId = "pa.data.check.2.pathway.enrichment",
                                    # inputId = "DAdataCheck2DAdataProfile",
                                    label = "Next", styleclass = "warning"),
                       helpText("If there are Errors or all metabolites are not in KEGG database, please click",
                                strong("Previous"), "to check your data and submit again."),
                       br(),br(),br()
              )
  )
)