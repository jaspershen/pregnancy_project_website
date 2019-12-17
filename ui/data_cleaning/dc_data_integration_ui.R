fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(
    # h4("Data integration paramaters"),
    radioButtons(
      inputId = "DCdiHasQC",
      label = "QC sample-based methods",
      choices = list("YES" = "hasQC",
                     "NO" = "noQC"),
      selected = "hasQC"
    ),

    conditionalPanel(
      condition = "input.DCdiHasQC == 'hasQC'",
      selectInput(
        inputId = "integrationMethod1",
        label = "Integration method",
        choices = c("QC mean" = "qc.mean",
                    "QC median" = "qc.median",
                    "None" = "no"),
        selected = "qc.median"
      )
    ),

    conditionalPanel(
      condition = "input.DCdiHasQC == 'noQC'",
      selectInput(
        inputId = "integrationMethod2",
        label = "Integration method",
        choices = c("Subject mean" = "subject.mean",
                    "Subject median" = "subject.median",
                    "None" = "no"),
        selected = "subject.median"
      )
    ),
    shinyalert::useShinyalert(),
    actionButton(
      inputId = 'dc.di.submit.button',
      label = "Submit",
      styleclass = "info",
      icon = icon('play-circle')
    ),

    useShinyalert(),
    actionButton(
      inputId = "dc.di.2.os",
      label = "Next", styleclass = "warning"),
    helpText("Click", strong("Submit"), "to integrate data"),
    helpText("If your don't want to do data integration,
             please select method as None."),
    br(),
    br(),
    br()
  ),
  mainPanel = mainPanel(
    span(textOutput(outputId = "dc.ms1.di.message"), style = "color:red"),
    span(textOutput(outputId = "dc.di.params.message"), style = "color:red"),
    tabsetPanel(id = "DCdiResult",
                type = "tabs",
                tabPanel("Summary",
                         value = "dc.di.summary",
                         icon = icon("tasks")
                ),
                tabPanel("MS1 peak table (after data integration)",
                         value = "dc.data.after.data.integration",
                         icon = icon("table")
                )
    ),

    conditionalPanel(
      condition = 'input.DCdiResult == "dc.data.after.data.integration"',
      shinysky::busyIndicator(text = "Processing..."),
      actionButton(inputId = "DCdiShowPeak1",
                   label = "Before integration",
                   styleclass = "info"),
      bsModal(id = "ModalExampleDI1",
              title = "Peak information before data integration",
              trigger = "DCdiShowPeak1",
              size = "large",
              plotly::plotlyOutput("dc.single.peak.plot.di1",
                                   width = "100%",
                                   height = "600px"),
              column(width = 6,
                     sliderInput(inputId = "dc.single.peak.plot.di1.width",
                                 label = "Width (inch)", min = 6,
                                 max = 20, value = 8)),
              column(width = 6,
                     sliderInput(inputId = "dc.single.peak.plot.di1.height",
                                 label = "Height (inch)", min = 3,
                                 max = 20, value = 6)),
              downloadButton('dc.single.peak.plot.di1.download',
                             'Download')


      ),

      actionButton(inputId = "DCdiShowPeak2",
                   label = "After integration",
                   styleclass = "info"),
      bsModal(id = "ModalExampledi2",
              title = "Peak information after data integration",
              trigger = "DCdiShowPeak2",
              size = "large",
              # plotOutput("dc.single.peak.plot.di"),
              plotly::plotlyOutput("dc.single.peak.plot.di2",
                                   width = "100%",
                                   height = "600px"),
              column(width = 6,
                     sliderInput(inputId = "dc.single.peak.plot.di2.width",
                                 label = "Width (inch)", min = 6,
                                 max = 20, value = 8)),
              column(width = 6,
                     sliderInput(inputId = "dc.single.peak.plot.di2.height",
                                 label = "Height (inch)", min = 3,
                                 max = 20, value = 6)),
              downloadButton('dc.single.peak.plot.di2.download',
                             'Download')
      ),

      br(), br(),
      DT::dataTableOutput(outputId = "dc.ms1.di")
    ),

    conditionalPanel(
      condition = 'input.DCdiResult == "dc.di.summary"',
      shinysky::busyIndicator(text = "Processing..."),

      ####Other information of peaks before and after normation
      actionButton(inputId = "DCdiShowRSD",
                   label = "RSD comparison",
                   styleclass = "info"),
      bsModal(id = "ModalExampleDI3",
              title = "RSD comparison",
              trigger = "DCdiShowRSD",
              size = "large",
              column(width = 1,
                     shinyWidgets::dropdownButton(
                       uiOutput(outputId = "dc.di.group.area3"),
                       column(width = 6,
                              sliderInput(inputId = "dc.di.rsd.comparison.width",
                                          label = "Width (inch)", min = 6,
                                          max = 20, value = 8)),
                       column(width = 6,
                              sliderInput(inputId = "dc.di.rsd.comparison.height",
                                          label = "Height (inch)", min = 3,
                                          max = 20, value = 6)),
                       downloadButton('dc.di.rsd.comparison.download',
                                      'Download'),
                       circle = TRUE, status = "info", icon = icon("cogs"),
                       width = "500px", size = "sm",
                       tooltip = tooltipOptions(title = "Image setting")
                     )
              ),
              column(width = 11,
                     plotly::plotlyOutput("dc.di.rsd.comparison",
                                          width = "100%",
                                          height = "600px")
              )
      ),

      actionButton(inputId = "DCdiShowRSD2",
                   label = "RSD of peaks",
                   styleclass = "info"),
      bsModal(id = "ModalExampleDI4",
              title = "RSD of peaks",
              trigger = "DCdiShowRSD2",
              size = "large",
              shinyWidgets::dropdownButton(
                uiOutput(outputId = "dc.di.group.area4"),
                sliderInput(inputId = "dc.di.rsd.distribution.tol",
                            label = "RSD tolerance", min = 0, max = 1000,
                            value = 30, step = 1),
                # column(width = 6,
                #        sliderInput(inputId = "dc.di.rsd.distribution.width",
                #                    label = "Width (inch)", min = 6,
                #                    max = 20, value = 8)),
                # column(width = 6,
                #        sliderInput(inputId = "dc.di.rsd.distribution.height",
                #                    label = "Height (inch)", min = 3,
                #                    max = 20, value = 6)),
                downloadButton('dc.di.rsd.distribution1.download',
                               'Download'),
                downloadButton('dc.di.rsd.distribution2.download',
                               'Download'),
                circle = TRUE, status = "info", icon = icon("cogs"),
                width = "500px", size = "sm",
                tooltip = tooltipOptions(title = "Image setting")
              ),
              plotly::plotlyOutput("dc.di.rsd.distribution1"),
              plotly::plotlyOutput("dc.di.rsd.distribution2")

      ),

      hr(),
      column(width = 1,
             shinyWidgets::dropdownButton(
               uiOutput(outputId = "dc.di.group.area1"),
               sliderInput(inputId = "dc.boxplot.di1.width",
                           label = "Width", min = 4, max = 20, value = 7),
               sliderInput(inputId = "dc.boxplot.di1.height",
                           label = "Height", min = 4, max = 20, value = 7),
               downloadButton(outputId = "dc.boxplot.di1.download",
                              label = "Download image"),

               circle = TRUE, status = "info", icon = icon("cogs"),
               width = "500px", size = "sm",
               tooltip = tooltipOptions(title = "Image setting")
               # inputId = "ma.hca.setting.DB"
             )
      ),

      column(width = 11,
             plotly::plotlyOutput(outputId = "dc.boxplot.di1")
      ),
      hr(),
      column(width = 1,
             shinyWidgets::dropdownButton(
               uiOutput(outputId = "dc.di.group.area2"),
               sliderInput(inputId = "dc.boxplot.di2.width",
                           label = "Width", min = 4, max = 20, value = 7),
               sliderInput(inputId = "dc.boxplot.di2.height",
                           label = "Height", min = 4, max = 20, value = 7),
               downloadButton(outputId = "dc.boxplot.di2.download",
                              label = "Download image"),

               circle = TRUE, status = "info", icon = icon("cogs"),
               width = "500px", size = "sm",
               tooltip = tooltipOptions(title = "Image setting")
               # inputId = "ma.hca.setting.DB"
             )
      ),
      column(width = 11,
             plotly::plotlyOutput(outputId = "dc.boxplot.di2")
      )

    ),
    br(), br(), br()
  )
)
)