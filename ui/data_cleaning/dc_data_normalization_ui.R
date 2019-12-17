fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(
    # h4("Data normalization paramaters"),
    radioButtons(
      inputId = "DCdnHasQC",
      label = "QC sample-based methods",
      choices = list("YES" = "hasQC",
                     "NO" = "noQC"),
      selected = "hasQC"
    ),

    conditionalPanel(
      condition = "input.DCdnHasQC == 'hasQC'",
      selectInput(
        inputId = "normalizationMethod1",
        label = "Normalization method",
        choices = c("QC LOESS" = "loess",
                    "QC SVR (MetNormalizer)" = "svr"),
        selected = "svr"
      ),
      conditionalPanel(
        condition = "input.normalizationMethod1 == 'loess'",
        checkboxInput(
          inputId = "loess.kepp.dimension",
          label = "KEPP dimension or not?",
          value = TRUE
        ),
        checkboxInput(
          inputId = "parameter.optimization",
          label = "Optimize parameters?",
          value = TRUE
        ),
        sliderInput(
          inputId = "begin.end",
          label = "Beigin and End",
          min = 0.5,
          max = 1,
          value = c(0.5, 1),
          step = 0.1,
          round = FALSE
        ),
        sliderInput(
          inputId = "loess.step",
          label = "Step of LOESS",
          min = 0.1,
          max = 0.4,
          value = 0.2,
          step = 0.1,
          round = FALSE
        )
      ),
      conditionalPanel(
        condition = "input.normalizationMethod1 == 'svr'",
        checkboxInput(
          inputId = "svr.kepp.dimension",
          label = "KEPP dimension or not?",
          value = TRUE
        ),
        sliderInput(
          inputId = "svr.multiple",
          label = "How many peaks used?",
          min = 1,
          max = 10,
          value = 5,
          step = 1,
          round = TRUE
        )
      )
    ),

    conditionalPanel(
      condition = "input.DCdnHasQC == 'noQC'",
      selectInput(
        inputId = "normalizationMethod2",
        label = "Normalization method",
        choices = c(
          "None" = "no",
          "Mean" = "mean",
          "Median" = "median",
          "Total" = "total"
          # "Probabilistic Quotient Normalization(PQN)" = "pqn",
          # "Quantile" = "quantile"
        ),
        selected = "median"
      )
    ),
    shinyalert::useShinyalert(),
    actionButton(
      inputId = 'dc.dn.submit.button',
      label = "Submit",
      styleclass = "info",
      icon = icon('play-circle')
    ),

    useShinyalert(),
    actionButton(inputId = "dc.dn.2.di",
                 # inputId = "DCmvResult2DCdiParameter",
                 label = "Next", styleclass = "warning"),
    helpText("Click", strong("Submit"), "to normalize data"),
    helpText("If your don't want to do data normalization,
             please select method as None."),

    br(),
    br(),
    br()
  ),
  mainPanel = mainPanel(
    span(textOutput(outputId = "dc.ms1.dn.message"), style = "color:red"),
    span(textOutput(outputId = "dc.dn.params.message"), style = "color:red"),
    tabsetPanel(id = "DCdnResult",
                type = "tabs",
                tabPanel("Summary",
                         value = "dc.dn.summary",
                         icon = icon("tasks")
                ),
                tabPanel("MS1 peak table (after data normalization)",
                         value = "dc.data.after.data.normalization",
                         icon = icon("table")
                )
    ),

    conditionalPanel(
      condition = 'input.DCdnResult == "dc.data.after.data.normalization"',
      shinysky::busyIndicator(text = "Processing..."),
      actionButton(inputId = "DCdnShowPeak1",
                   label = "Before normalization",
                   styleclass = "info"),
      bsModal(id = "ModalExampleDN1",
              title = "Peak information before data normalization",
              trigger = "DCdnShowPeak1",
              size = "large",
              plotly::plotlyOutput("dc.single.peak.plot.dn1"),
              column(width = 6,
                     sliderInput(inputId = "dc.single.peak.plot.dn1.width",
                                 label = "Width (inch)", min = 6,
                                 max = 20, value = 8)),
              column(width = 6,
                     sliderInput(inputId = "dc.single.peak.plot.dn1.height",
                                 label = "Height (inch)", min = 3,
                                 max = 20, value = 6)),
              downloadButton('dc.single.peak.plot.dn1.download',
                             'Download')


      ),

      actionButton(inputId = "DCdnShowPeak2",
                   label = "After normalization",
                   styleclass = "info"),
      bsModal(id = "ModalExampleDN2",
              title = "Peak information after data normalization",
              trigger = "DCdnShowPeak2",
              size = "large",
              # plotOutput("dc.single.peak.plot.dn"),
              plotly::plotlyOutput("dc.single.peak.plot.dn2"),
              column(width = 6,
                     sliderInput(inputId = "dc.single.peak.plot.dn2.width",
                                 label = "Width (inch)", min = 6,
                                 max = 20, value = 8)),
              column(width = 6,
                     sliderInput(inputId = "dc.single.peak.plot.dn2.height",
                                 label = "Height (inch)", min = 3,
                                 max = 20, value = 6)),
              downloadButton('dc.single.peak.plot.dn2.download',
                             'Download')
      ),

      br(), br(),
      shinysky::busyIndicator(text = "Processing..."),
      DT::dataTableOutput(outputId = "dc.ms1.dn")
    ),

    conditionalPanel(
      condition = 'input.DCdnResult == "dc.dn.summary"',
      # shinysky::busyIndicator(text = "Processing...It may take a while"),

      ####Other information of peaks before and after normation
      actionButton(inputId = "DCdnShowRSD",
                   label = "RSD comparison",
                   styleclass = "info"),
      bsModal(id = "ModalExampleDN3",
              title = "RSD comparison",
              trigger = "DCdnShowRSD",
              size = "large",
              column(width = 1,
                     shinyWidgets::dropdownButton(
                       uiOutput(outputId = "dc.dn.group.area3"),
                       column(width = 6,
                              sliderInput(inputId = "dc.rsd.comparison.width",
                                          label = "Width (inch)", min = 6,
                                          max = 20, value = 8)),
                       column(width = 6,
                              sliderInput(inputId = "dc.rsd.comparison.height",
                                          label = "Height (inch)", min = 3,
                                          max = 20, value = 6)),
                       downloadButton('dc.rsd.comparison.download',
                                      'Download'),
                       circle = TRUE, status = "info", icon = icon("cogs"),
                       width = "500px", size = "sm",
                       tooltip = tooltipOptions(title = "Image setting")
                     )
              ),
              column(width = 11,
                     plotly::plotlyOutput("dc.rsd.comparison")
              )
      ),

      actionButton(inputId = "DCdnShowRSD2",
                   label = "RSD of peaks",
                   styleclass = "info"),
      bsModal(id = "ModalExampleDN4",
              title = "RSD of peaks",
              trigger = "DCdnShowRSD2",
              size = "large",
              shinyWidgets::dropdownButton(
                uiOutput(outputId = "dc.dn.group.area4"),
                sliderInput(inputId = "dc.rsd.distribution.tol",
                            label = "RSD tolerance", min = 0, max = 1000,
                            value = 30, step = 1),
                # column(width = 6,
                #        sliderInput(inputId = "dc.rsd.distribution.width",
                #                    label = "Width (inch)", min = 6,
                #                    max = 20, value = 8)),
                # column(width = 6,
                #        sliderInput(inputId = "dc.rsd.distribution.height",
                #                    label = "Height (inch)", min = 3,
                #                    max = 20, value = 6)),
                downloadButton('dc.rsd.distribution1.download',
                               'Download'),
                downloadButton('dc.rsd.distribution2.download',
                               'Download'),
                circle = TRUE, status = "info", icon = icon("cogs"),
                width = "500px", size = "sm",
                tooltip = tooltipOptions(title = "Image setting")
              ),
              plotly::plotlyOutput("dc.rsd.distribution1"),
              plotly::plotlyOutput("dc.rsd.distribution2")

      ),

      hr(),
      column(width = 1,
             shinyWidgets::dropdownButton(
               uiOutput(outputId = "dc.dn.group.area1"),
               sliderInput(inputId = "dc.boxplot.dn1.width",
                           label = "Width", min = 4, max = 20, value = 7),
               sliderInput(inputId = "dc.boxplot.dn1.height",
                           label = "Height", min = 4, max = 20, value = 7),
               downloadButton(outputId = "dc.boxplot.dn1.download",
                              label = "Download image"),

               circle = TRUE, status = "info", icon = icon("cogs"),
               width = "500px", size = "sm",
               tooltip = tooltipOptions(title = "Image setting")
               # inputId = "ma.hca.setting.DB"
             )
      ),

      column(width = 11,
             shinysky::busyIndicator(text = "Processing..."),
             plotly::plotlyOutput(outputId = "dc.boxplot.dn1")
      ),
      hr(),
      column(width = 1,
             shinyWidgets::dropdownButton(
               uiOutput(outputId = "dc.dn.group.area2"),
               sliderInput(inputId = "dc.boxplot.dn2.width",
                           label = "Width", min = 4, max = 20, value = 7),
               sliderInput(inputId = "dc.boxplot.dn2.height",
                           label = "Height", min = 4, max = 20, value = 7),
               downloadButton(outputId = "dc.boxplot.dn2.download",
                              label = "Download image"),

               circle = TRUE, status = "info", icon = icon("cogs"),
               width = "500px", size = "sm",
               tooltip = tooltipOptions(title = "Image setting")
               # inputId = "ma.hca.setting.DB"
             )
      ),
      column(width = 11,
             shinysky::busyIndicator(text = "Processing..."),
             plotly::plotlyOutput(outputId = "dc.boxplot.dn2")
      )

    ),
    br(), br(), br(), br()
  )
)
)