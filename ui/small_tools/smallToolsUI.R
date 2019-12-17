##ms2 plot UI
ui.ms2plot <- function(){
  tagList(
    fluidPage(
      sidebarLayout(sidebarPanel = sidebarPanel(
        tabsetPanel(
          id = 'ms2plotMainpan',
          type = 'pills',
          tabPanel('Input data',
                   value = "ms2plot.upload.data",
                   icon = icon("upload")),
          tabPanel('Paramter Setting',
                   value = "ms2plot.parameter.setting",
                   icon = icon(name = "cogs")
                   ),
          # tabPanel('Download', value = "ms2plot.download"),
          ##input data
          conditionalPanel(
            condition = 'input.ms2plotMainpan == "ms2plot.upload.data"',

            textInput(inputId = 'spec1.mz',
                      label = h5(
                        "m/z of MS2 spectrum 1",
                        shinyBS::tipify(
                          el = icon(name = "info-circle"),
                          placement = "bottom",
                          trigger = "hover",
                          title = "Separated by space"
                        )
                      ),
                      value = "41.03804 42.03358 43.05364 44.04896 46.06474 55.05411 56.04935 58.06517 59.06065 72.08075 74.09636 82.06505 100.07563")
            ,

            textInput(inputId = 'spec1.int',
                      label = h5(
                        "Intensity of MS2 spectrum 1",
                        shinyBS::tipify(
                          el = icon(name = "info-circle"),
                          placement = "bottom",
                          trigger = "hover",
                          title = "Separated by space"
                        )
                      ),
                      value =  "24240.48 13001 27436.41 33663.91 26778.75 43576.34 94950.15 165465.61 14507.27 24916.17 42110.55 18407.28 124344.4"),



            textInput(inputId = 'spec2.mz',
                      label = h5(
                        "m/z of MS2 spectrum 2",
                        shinyBS::tipify(
                          el = icon(name = "info-circle"),
                          placement = "bottom",
                          trigger = "hover",
                          title = "Separated by space"
                        )
                      ),
                      value = "")
            ,
            textInput(inputId = 'spec2.int',
                      label = h5(
                        "Intensity of MS2 spectrum 1",
                        shinyBS::tipify(
                          el = icon(name = "info-circle"),
                          placement = "bottom",
                          trigger = "hover",
                          title = "Separated by space"
                        )
                      ),
                      value = "")
            ),
          #parameter setting
          conditionalPanel(
            condition = 'input.ms2plotMainpan == "ms2plot.parameter.setting"',
            ##ppm for match
            sliderInput(inputId = "ms2plotPPM",
                        label = "Tolerance for fragment ion match (ppm)",
                        min = 1, max = 100, value = 30),

            sliderInput(inputId = "ms2plotRealIntCutoff",
                        label = "Relative intensity tolerance for fragment ion removal (%)",
                        min = 0, max = 99, value = 0)
          )
        )

      ), mainPanel = mainPanel(


        fluidPage(
          column(width = 1,
                 dropdownButton(
                   colourInput(inputId = "ms2plotCol1",
                               label = "Spectrum 1 color",
                               value = "lightseagreen",
                               returnName = TRUE,
                               allowTransparent = TRUE),

                   #color for spectrum2
                   colourInput(inputId = "ms2plotCol2",
                               label = "Spectrum 2 color",
                               value = "tomato",
                               returnName = TRUE,
                               allowTransparent = TRUE),

                   #color no match fragement
                   colourInput(inputId = "ms2plotCol3",
                               label = "No-matched color",
                               value = "gray",
                               returnName = TRUE,
                               # showColour = TRUE,
                               allowTransparent = TRUE),

                   #lwd
                   numericInput(inputId = "ms2plotLwd",
                                label = "The line width",
                                value = 1),

                   #cex.lab
                   # column(width = 6,
                   numericInput(inputId = "ms2plotCexLab",
                                label = "The size of x and y labels",
                                value = 1.8),
                   # ),

                   #cex.axis
                   # column(width = 6,
                   numericInput(inputId = "ms2plotCexAxis",
                                label = "The size of axis annotation",
                                value = 1.5),
                   # ),
                   #xlab
                   textInput(inputId = "ms2plotXlab",
                             label = "Text of X axis",
                             value = "Mass to charge ratio (m/z)"
                   ),
                   #ylab
                   textInput(inputId = "ms2plotYlab",
                             label = "Text of Y axis",
                             value = "Relative intensity (%)"),

                   circle = TRUE, status = "info",
                   icon = icon("cogs"),
                   width = "300px",
                   tooltip = tooltipOptions(title = "Image setting")
                 ),
                 dropdownButton(
                   sliderInput(inputId = "ms2plotImageWidth",
                               label = "Width (inch)", min = 6,
                               max = 20, value = 8),
                   sliderInput(inputId = "ms2plotImageHeight",
                               label = "Height (inch)", min = 3,
                               max = 20, value = 6),
                   downloadButton('ms2plot.download', 'Download image'),
                   circle = TRUE, status = "info",
                   icon = icon("download"),
                   width = "300px",
                   tooltip = tooltipOptions(title = "Download image")
                 )
          ),

          column(width = 8,
                 plotOutput(outputId = "ms2.match.plot",
                            width = "800px",
                            height = "600px")
          )
        )
      )
      ),
      br(), br(), br()
    )
  )
}




##worklist UI
ui.worklist <- function(){
  tagList(
    fluidPage(
      sidebarLayout(sidebarPanel = sidebarPanel(
        tabsetPanel(
          id = 'worklistMainpan',
          type = 'pills',
          tabPanel('Input data',
                   value = "worklist.upload.data",
                   icon = icon(name = "upload")
                   ),
          tabPanel('Paramter Setting',
                   value = "worklist.parameter.setting",
                   icon = icon(name = "cogs")
                   ),
          # tabPanel('Download', value = "worklist.download"),
          ##input data
          conditionalPanel(
            condition = 'input.worklistMainpan == "worklist.upload.data"',

            textInput(inputId = 'worklist.sampleName',
                      label = h5("Sample names",
                                    shinyBS::tipify(el = icon(name = "info-circle",
                                    lib = "font-awesome"),
                                    placement = "bottom",
                                    trigger = "hover",
                                    title = "Separated by spaces")
                      ),
                      value =  "a b c d e f g h i j k l m n o p q r s t u v w x y z")

            # shinyBS::bsTooltip(id = "worklist.sampleName",
            #           title = "test",
            #           placement =  "right",
            #           trigger = "hover"
            #           )

          ),
          #parameter setting
          conditionalPanel(
            condition = 'input.worklistMainpan == "worklist.parameter.setting"',

            ##ppm for match
                   selectInput(inputId = "worklistInstrument",
                               label = "Instrument platform",
                               choices = c("Agilent", "AB"),
                               selected = "Agilent", multiple = FALSE
                   ),

                   selectInput(inputId = "worklistRandommethod",
                               label = "Random method",
                               choices = c("No", "Position", "Injection"),
                               selected = "Position", multiple = FALSE),

                   numericInput(inputId = "worklistReplication",
                                label = "Replication times",
                                value = 1, min = 1, max = 5, step = 1),
                   # ),

            # column(width = 6,
                   numericInput(inputId = "worklistQCstep",
                                label = "QC step",
                                value = 8, min = 1, max = 20, step = 1),
            # ),

            # column(width = 6,
                   numericInput(inputId = "worklistConditionQCnumber",
                                label = "Condition QC number",
                                value = 10, min = 1, max = 20, step = 1),
            # ),

            # column(width = 6,
                   numericInput(inputId = "worklistValidationQCstep",
                                label = "Validation QC step",
                                value = 40, min = 1, max = 60, step = 1),
            # ),

            # column(width = 6,
                   numericInput(inputId = "worklistTestmixstep",
                                label = "Test mixture step",
                                value = 40, min = 1, max = 60, step = 1),
            # ),

            # column(width = 6,
                   numericInput(inputId = "worklistInjectionfrom",
                                label = "Injection from",
                                value = 1, min = 1, max = 100000, step = 1),
            # ),

            textInput(inputId = "worklistDir",
                      label = "Directory for data",
                      value = "project1")
          )
          # conditionalPanel(
          #   condition = 'input.worklistMainpan == "worklist.download"',
          #   downloadButton('worklist.download1', 'Download positive worklist'),
          #   downloadButton('worklist.download2', 'Download negative worklist')
          # )
        )

      ), mainPanel = mainPanel(
        # textOutput(outputId = "test"),
        tabsetPanel(id = "worklistResultMainpan", type = "tabs",
                    tabPanel("Sample names",
                             value = "worklist.result.sample.names",
                             icon = icon("table")
                             ),
                    tabPanel("Positive worklist",
                             value = "worklist.positive",
                             icon = icon("plus")
                             ),
                    tabPanel("Negative worklist",
                             value = "worklist.negative",
                             icon = icon("minus")
                             ),
                    conditionalPanel(
                      condition = 'input.worklistResultMainpan == "worklist.result.sample.names"',
                      DT::dataTableOutput("worklist.sample.names")
                      ),
                    conditionalPanel(
                      condition = 'input.worklistResultMainpan == "worklist.positive"',
                      DT::dataTableOutput("worklist.positive")
                      ),
                    conditionalPanel(
                      condition = 'input.worklistResultMainpan == "worklist.negative"',
                      DT::dataTableOutput("worklist.negative")
                      )
                    )
      )
      ),
      br(), br(), br()
    )
  )
}