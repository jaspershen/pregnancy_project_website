fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(
    selectInput(
      inputId = "dc.os.pca.log",
      label = "Logarithm method",
      choices = c(
        "No log" = "no",
        "Log 2" = "log2",
        "Log e" = "loge",
        "Log 10" = "log10"
      ),
      selected = "log10"
    ),

    selectInput(
      inputId = "dc.os.pca.scale",
      label = "Scale method",
      choices = c(
        "Pareto scale" = "pareto",
        "Auto scale" = "auto",
        "No scale" = "no"
      ),
      selected = "auto"
    ),

    checkboxInput(
      inputId = "dc.os.pca.center",
      label = "Center or not",
      value = TRUE
    ),

    sliderInput(
      inputId = "dc.os.pca.ci.tol",
      label = "Samples will be considered as outliers outside % CI",
      min = 90,
      max = 100,
      value = 95,
      step = 1,
      round = TRUE
    ),



    sliderInput(
      inputId = "dc.os.zero.tol",
      label = "Samples will be considered as outliers with zero value ratio > %",
      min = 20,
      max = 90,
      value = 50,
      step = 0.5,
      round = TRUE
    ),

    shinyalert::useShinyalert(),

    actionButton(
      inputId = 'dc.os.submit.button',
      label = "Submit",
      styleclass = "info",
      icon = icon('play-circle')
    ),

    useShinyalert(),
    actionButton(inputId = "dc.os.2.qa2",
                 label = "Next", styleclass = "warning"),
    helpText("Click", strong("Submit"), "to find outlier samples"),
    br(),
    br(),
    br()
  ),
  mainPanel = mainPanel(
    span(textOutput(outputId = "dc.os.pca.plot.message"), style = "color:red"),
    span(textOutput(outputId = "dc.os.pca.message"), style = "color:red"),
    span(textOutput(outputId = "dc.os.sample.zero.ratio.message"), style = "color:red"),
    span(textOutput(outputId = "dc.os.zero.plot.message"), style = "color:red"),
    span(textOutput(outputId = "dc.os.params.message"), style = "color:red"),
    tabsetPanel(id = "DCosResult",
                type = "tabs",
                tabPanel("Summary",
                         value = "dc.os.summary",
                         icon = icon("tasks")
                ),
                tabPanel("MS1 peak table (after outlier sample removal)",
                         value = "dc.data.after.outlier.removal",
                         icon = icon("table")
                )
    ),

    conditionalPanel(
      condition = 'input.DCosResult == "dc.data.after.outlier.removal"',
      shinysky::busyIndicator(text = "Processing..."),
      # actionButton(inputId = "DCosShowSample",
      #              label = "Show sample",
      #              styleclass = "info"),
      # bsModal(id = "ModalExampleOS",
      #         title = "Sample information",
      #         trigger = "DCosShowSample",
      #         size = "large",
      #         # plotOutput("dc.single.peak.plot.os"),
      #         plotOutput("dc.single.sample.plot.os"),
      #         column(width = 6,
      #                sliderInput(inputId = "dc.single.sample.plot.os.width",
      #                            label = "Width (inch)", min = 6,
      #                            max = 20, value = 8)),
      #         column(width = 6,
      #                sliderInput(inputId = "dc.single.sample.plot.os.height",
      #                            label = "Height (inch)", min = 3,
      #                            max = 20, value = 6)),
      #         downloadButton('dc.single.sample.plot.os.download',
      #                        'Download')
      #
      #
      # ),
      # br(), br(),
      DT::dataTableOutput(outputId = "dc.ms1.os")
    ),

    conditionalPanel(
      condition = 'input.DCosResult == "dc.os.summary"',
      shinysky::busyIndicator(text = "Processing..."),

      ####Other information of peaks
      actionButton(inputId = "DCosShowPCA",
                   label = "PCA score plot",
                   styleclass = "info"),
      bsModal(id = "ModalExampleOS2",
              title = "PCA score plot",
              trigger = "DCosShowPCA",
              size = "large",
              column(width = 1,
                     shinyWidgets::dropdownButton(
                       uiOutput(outputId = "dc.os.group.area1"),
                       column(width = 6,
                              sliderInput(inputId = "dc.os.pca.plot.width",
                                          label = "Width (inch)", min = 6,
                                          max = 20, value = 8)),
                       column(width = 6,
                              sliderInput(inputId = "dc.os.pca.plot.height",
                                          label = "Height (inch)", min = 3,
                                          max = 20, value = 6)),
                       downloadButton('dc.os.pca.plot.download',
                                      'Download'),
                       circle = TRUE, status = "info", icon = icon("cogs"),
                       width = "500px", size = "sm",
                       tooltip = tooltipOptions(title = "Image setting")
                     )
              ),
              column(width = 11,
                     plotly::plotlyOutput("dc.os.pca.plot")
              )
      ),

      actionButton(inputId = "DCosShowZero",
                   label = "Zero value ratio",
                   styleclass = "info"),
      bsModal(id = "ModalExampleOS3",
              title = "Zero ratio",
              trigger = "DCosShowZero",
              size = "large",
              shinyWidgets::dropdownButton(
                uiOutput(outputId = "dc.os.group.area2"),

                column(width = 6,
                       sliderInput(inputId = "dc.os.zero.plot.width",
                                   label = "Width (inch)", min = 6,
                                   max = 20, value = 8)),
                column(width = 6,
                       sliderInput(inputId = "dc.os.zero.plot.height",
                                   label = "Height (inch)", min = 3,
                                   max = 20, value = 6)),
                downloadButton('dc.os.zero.plot.download',
                               'Download'),
                circle = TRUE, status = "info", icon = icon("cogs"),
                width = "500px", size = "sm",
                tooltip = tooltipOptions(title = "Image setting")
              ),
              plotly::plotlyOutput("dc.os.zero.plot")

      ),

      shinysky::busyIndicator(text = "Find outlier samples..."),
      hr(),
      uiOutput(outputId = "dc.os.result.area")
    ),
    br(), br(), br()
  )
)
)