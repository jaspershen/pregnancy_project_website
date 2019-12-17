fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(

    radioButtons(
      inputId = "daValidationUsevalidationDataset",
      label = "Dataset for validation",
      choices = list("Only discovery dataset" = "dis",
                     "Discovery and validation dataset" = "disAndVal"),
      selected = "dis"
    ),

    conditionalPanel(condition = "input.daValidationUsevalidationDataset == 'disAndVal'",
                     fileInput(
                       inputId = "DAms1PeakTableValidation",
                       label = h5(
                         "MS1 peak table (Validation)",
                         shinyBS::tipify(
                           el = icon(name = "info-circle"),
                           placement = "bottom",
                           trigger = "hover",
                           title = "The first column must be peak name"
                         )
                       ),
                       multiple = FALSE,
                       accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"),
                       buttonLabel = "Browser"
                     ),
                     #sample information
                     fileInput(
                       inputId = "DAsampleInfoValidation",
                       label = h5(
                         "Sample information (validation)",
                         shinyBS::tipify(
                           el = icon(name = "info-circle"),
                           placement = "bottom",
                           trigger = "hover",
                           title = "Column 1: sample name, column 2: group"
                         )
                       ),
                       multiple = FALSE,
                       accept = c("txt/csv", "text/comma-separated-values,text/plain", ".csv"),
                       buttonLabel = "Browser"
                     ),
                     useShinyalert(),
                     actionButton(
                       inputId = 'daValidationUploadButton',
                       label = "Upload",
                       styleclass = "info",
                       # class = "btn-primary",
                       icon = icon('play-circle')
                     ),
                     helpText("Please upload validation dataset.")
                     ),

#------------------------------------------------------------------------------
useShinyalert(),
actionButton(inputId = 'daValidationSubmitButton',
                 "Submit",
                 styleclass = "info",
                 icon = icon('play-circle')),
    actionButton(inputId = "daValidation2Download",
                 label = "Next", styleclass = "warning"),
    helpText("Click", strong("Submit"), "to do validation")
  ),


#------------------------------------------------------------------------------
mainPanel = mainPanel(
  span(textOutput(outputId = "da.validation.params.message"), style = "color:red"),
  tabsetPanel(id = "daValidationResult",
              type = "tabs",
              tabPanel(title = "Discovery dataset",
                       value = "daDiscoveryDataset",
                       icon = icon("tasks"),
              tabsetPanel(id = "daDiscoveryPanel",
                          type = "pills",
                          tabPanel(title = "PCA analysis",
                                   icon = icon("image"),
                                   shinysky::busyIndicator(text = "Processing..."),
                                   span(textOutput(outputId = "daValidationPCA1.message"), style = "color:red"),
                                   span(textOutput(outputId = "daValidationPCAplot1.message"), style = "color:red"),
                                   plotly::plotlyOutput(outputId = "daValidationPCAplot1",
                                                        height = "600px",
                                                        width = "100%")
                          ),
                          tabPanel(title = "PLS analysis",
                                   icon = icon("image"),
                                   shinysky::busyIndicator(text = "Processing..."),
                                   span(textOutput(outputId = "daValidationPLS1.message"), style = "color:red"),
                                   plotly::plotlyOutput(outputId = "daValidationPLSplot1",
                                                        height = "600px",
                                                        width = "100%")
                          ),
                          tabPanel(title = "HCA analysis",
                                   icon = icon("image"),
                                   span(textOutput(outputId = "daValidationHCA1.message"), style = "color:red"),
                                   plotOutput(outputId = "daValidationHCAheatmap1",
                                              height = "600px",
                                              width = "100%"),
                                   dropdownButton(
                                     sliderInput(inputId = "daValidationHCAheatmapWidth1",
                                                 label = "Width (inch)", min = 6,
                                                 max = 20, value = 7),

                                     sliderInput(inputId = "daValidationHCAheatmapHeight1",
                                                 label = "Width (inch)", min = 6,
                                                 max = 20, value = 7),

                                     downloadButton('daValidationHCAheatmap1Download1', 'Download heatmap'),

                                     circle = TRUE, status = "info", icon = icon("download"),
                                     width = "500px",
                                     tooltip = tooltipOptions(title = "Download image"),
                                     # inputId = "ma.hca.download.DB",
                                     right = FALSE, up = FALSE
                                   )
                          ),
                          tabPanel(title = "ROC analysis",
                                   icon = icon("image"),
                                   selectInput(inputId = "daValidationPredictionModel",
                                               label = "Prediction model",
                                               choices = c("PLS" = "pls",
                                                           "Random Forest" = "rf",
                                                           "Support Vector Machine" = "svm",
                                                           "Logistic regression" = "lr"),
                                               selected = "lr"),
                                   actionButton(inputId = "daValidationROCanalysis",
                                                label = "Submit", styleclass = "info"),
                                   helpText("Click", strong("Submit"), "to do ROC analysis."),
                                   shinysky::busyIndicator(text = "Processing..."),
                                   span(textOutput(outputId = "ROC.message"), style = "color:red"),
                                   span(textOutput(outputId = "daValidationROClplot1.message"), style = "color:red"),
                                   plotly::plotlyOutput(outputId = "daValidationROClplot1",
                                                        height = "600px",
                                                        width = "100%")
                          )

                          )
              ),
              tabPanel(title = "Validation dataset",
                       value = "daValidationDataset",
                       icon = icon("tasks"),
                       tabsetPanel(id = "daValidationPanel",
                                   type = "pills",
                                   tabPanel(title = "PCA analysis",
                                            icon = icon("image"),
                                            shinysky::busyIndicator(text = "Processing..."),
                                            span(textOutput(outputId = "daValidationPCA2.message"), style = "color:red"),
                                            span(textOutput(outputId = "daValidationPCAplot2.message"), style = "color:red"),
                                            plotly::plotlyOutput(outputId = "daValidationPCAplot2",
                                                                 height = "600px",
                                                                 width = "100%")),
                                   tabPanel(title = "PLS analysis",
                                            icon = icon("image"),
                                            shinysky::busyIndicator(text = "Processing..."),
                                            span(textOutput(outputId = "daValidationPLS2.message"), style = "color:red"),
                                            plotly::plotlyOutput(outputId = "daValidationPLSplot2",
                                                                 height = "600px",
                                                                 width = "100%")),
                                   tabPanel(title = "HCA analysis",
                                            icon = icon("image"),
                                            span(textOutput(outputId = "daValidationHCA2.message"), style = "color:red"),
                                            plotOutput(outputId = "daValidationHCAheatmap2",
                                                       height = "600px",
                                                       width = "100%"),
                                            dropdownButton(
                                              sliderInput(inputId = "daValidationHCAheatmapWidth2",
                                                          label = "Width (inch)", min = 6,
                                                          max = 20, value = 7),

                                              sliderInput(inputId = "daValidationHCAheatmapHeight2",
                                                          label = "Width (inch)", min = 6,
                                                          max = 20, value = 7),

                                              downloadButton('daValidationHCAheatmap1Download2', 'Download heatmap'),

                                              circle = TRUE, status = "info", icon = icon("download"),
                                              width = "500px",
                                              tooltip = tooltipOptions(title = "Download image"),
                                              # inputId = "ma.hca.download.DB",
                                              right = FALSE, up = FALSE
                                            )
                                   ),
                                   tabPanel(title = "ROC analysis",
                                            icon = icon("image"),
                                            shinysky::busyIndicator(text = "Processing..."),
                                            span(textOutput(outputId = "daValidationROClplot2.message"), style = "color:red"),
                                            plotly::plotlyOutput(outputId = "daValidationROClplot2",
                                                                 height = "600px", width = "100%"))
                       )
              )
  )
)


#   mainPanel = mainPanel(
#     span(textOutput(outputId = "da.validation.params.message"), style = "color:red"),
#     tabsetPanel(id = "daValidationResult",
#                 type = "tabs",
#                 tabPanel("PCA analysis",
#                          value = "daValidationPCA",
#                          icon = icon("image"),
#                          tabsetPanel(id = "daValidationPCApanel",
#                                      type = "pills",
#                                      tabPanel(title = "Discovery dataset",
#                                               shinysky::busyIndicator(text = "Processing..."),
#                                               span(textOutput(outputId = "daValidationPCA1.message"), style = "color:red"),
#                                               span(textOutput(outputId = "daValidationPCAplot1.message"), style = "color:red"),
#                                               plotly::plotlyOutput(outputId = "daValidationPCAplot1",
#                                                                    height = "600px",
#                                                                    width = "100%")
#                                               ),
#                                      tabPanel(title = "Validation dataset",
#                                               shinysky::busyIndicator(text = "Processing..."),
#                                               span(textOutput(outputId = "daValidationPCA2.message"), style = "color:red"),
#                                               span(textOutput(outputId = "daValidationPCAplot2.message"), style = "color:red"),
#                                               plotly::plotlyOutput(outputId = "daValidationPCAplot2",
#                                                                    height = "600px",
#                                                                    width = "100%"))
#                                      )
#                 ),
#
#                 tabPanel("PLS analysis",
#                          value = "daValidationPLS",
#                          icon = icon("image"),
#                          tabsetPanel(id = "daValidationPLSpanel",
#                                      type = "pills",
#                                      tabPanel(title = "Discovery dataset",
#                                               shinysky::busyIndicator(text = "Processing..."),
#                                               span(textOutput(outputId = "daValidationPLS1.message"), style = "color:red"),
#                                               plotly::plotlyOutput(outputId = "daValidationPLSplot1",
#                                                                    height = "600px",
#                                                                    width = "100%")
#                                               ),
#                                      tabPanel(title = "Validation dataset",
#                                               shinysky::busyIndicator(text = "Processing..."),
#                                               span(textOutput(outputId = "daValidationPLS2.message"), style = "color:red"),
#                                               plotly::plotlyOutput(outputId = "daValidationPLSplot2",
#                                                                    height = "600px",
#                                                                    width = "100%"))
#                          )
#                 ),
#
#                 tabPanel("HCA analysis",
#                          value = "daValidationHCA",
#                          icon = icon("image"),
#                          tabsetPanel(id = "daValidationHCApanel",
#                                      type = "pills",
#                                      tabPanel(title = "Discovery dataset",
#                                               span(textOutput(outputId = "daValidationHCA1.message"), style = "color:red"),
#                                               plotOutput(outputId = "daValidationHCAheatmap1",
#                                                          height = "600px",
#                                                          width = "100%"),
#                                               dropdownButton(
#                                                 sliderInput(inputId = "daValidationHCAheatmapWidth1",
#                                                             label = "Width (inch)", min = 6,
#                                                             max = 20, value = 7),
#
#                                                 sliderInput(inputId = "daValidationHCAheatmapHeight1",
#                                                             label = "Width (inch)", min = 6,
#                                                             max = 20, value = 7),
#
#                                                 downloadButton('daValidationHCAheatmap1Download1', 'Download heatmap'),
#
#                                                 circle = TRUE, status = "info", icon = icon("download"),
#                                                 width = "500px",
#                                                 tooltip = tooltipOptions(title = "Download image"),
#                                                 # inputId = "ma.hca.download.DB",
#                                                 right = FALSE, up = FALSE
#                                               )
#                                      ),
#                                      tabPanel(title = "Validation dataset",
#                                               span(textOutput(outputId = "daValidationHCA2.message"), style = "color:red"),
#                                               plotOutput(outputId = "daValidationHCAheatmap2",
#                                                          height = "600px",
#                                                          width = "100%"),
#                                               dropdownButton(
#                                                 sliderInput(inputId = "daValidationHCAheatmapWidth2",
#                                                             label = "Width (inch)", min = 6,
#                                                             max = 20, value = 7),
#
#                                                 sliderInput(inputId = "daValidationHCAheatmapHeight2",
#                                                             label = "Width (inch)", min = 6,
#                                                             max = 20, value = 7),
#
#                                                 downloadButton('daValidationHCAheatmap1Download2', 'Download heatmap'),
#
#                                                 circle = TRUE, status = "info", icon = icon("download"),
#                                                 width = "500px",
#                                                 tooltip = tooltipOptions(title = "Download image"),
#                                                 # inputId = "ma.hca.download.DB",
#                                                 right = FALSE, up = FALSE
#                                               )
#                                               )
#                          )
#     ),
#
#     tabPanel("ROC analysis",
#              value = "daValidationROC",
#              icon = icon("image"),
#              tabsetPanel(id = "daValidationROCpanel",
#                          type = "pills",
#                          tabPanel(title = "Discovery dataset",
#                                   selectInput(inputId = "daValidationPredictionModel",
#                                               label = "Prediction model",
#                                               choices = c("PLS" = "pls",
#                                                           "Random Forest" = "rf",
#                                                           "Support Vector Machine" = "svm",
#                                                           "Logistic regression" = "lr"),
#                                               selected = "pls"),
#                                   actionButton(inputId = "daValidationROCanalysis",
#                                                label = "Submit", styleclass = "info"),
#                                   helpText("Click", strong("Submit"), "to do ROC analysis."),
#                                   shinysky::busyIndicator(text = "Processing..."),
#                                   span(textOutput(outputId = "ROC.message"), style = "color:red"),
#                                   span(textOutput(outputId = "daValidationROClplot1.message"), style = "color:red"),
#                                   plotly::plotlyOutput(outputId = "daValidationROClplot1",
#                                                        height = "600px",
#                                                        width = "100%")
#                          ),
#                          tabPanel(title = "Validation dataset",
#                                   shinysky::busyIndicator(text = "Processing..."),
#                                   span(textOutput(outputId = "daValidationROClplot2.message"), style = "color:red"),
#                                   plotly::plotlyOutput(outputId = "daValidationROClplot2",
#                                                        height = "600px", width = "100%"))
#                          )
#     )
#   )
# )
),
br(), br(), br()
)