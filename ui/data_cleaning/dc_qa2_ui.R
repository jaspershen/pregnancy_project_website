fluidPage(
  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "Data profile",
      icon = icon("image"),
      shinysky::busyIndicator(text = "Processing..."),
      span(textOutput(outputId = "dc.data.profile.plot2.message"), style = "color:red"),
      plotlyOutput(outputId = "dc.data.profile.plot2",
                   # width = "800px",
                   height = "500px"),
      actionButton(inputId = "dc.qa2.2.download",
                   # inputId = "DCdataCheck2DCdataProfile",
                   label = "Next", styleclass = "warning"),
      br(),
      br(),
      br()
    ),

    tabPanel(
      title = "Zero value distribution",
      icon = icon("image"),
      shinysky::busyIndicator(text = "Processing..."),
      span(textOutput(outputId = "dc.peak.zero.ratio.plot2.message"), style = "color:red"),
      span(textOutput(outputId = "dc.sample.zero.ratio.plot2.message"), style = "color:red"),
      plotlyOutput(outputId = "dc.peak.zero.ratio.plot2",
                   # width = "800px",
                   height = "500px"),
      plotlyOutput(outputId = "dc.sample.zero.ratio.plot2",
                   # width = "800px",
                   height = "500px"),
      br(),
      br(),
      br()
    ),

    tabPanel(
      title = "RSD distribution",
      icon = icon("image"),
      shinysky::busyIndicator(text = "Processing..."),
      uiOutput(outputId = "dc.qa2.group.area"),
      span(textOutput(outputId = "dc.peak.rsd.plot2.message"), style = "color:red"),
      span(textOutput(outputId = "dc.peak.rsd.plot.for.report2.message"), style = "color:red"),
      plotlyOutput(outputId = "dc.peak.rsd.plot2",
                   # width = "800px",
                   height = "500px"
      ),
      br(),
      br(),
      br()
    ),

    # tabPanel(
    #   title = "PCA score plot",
    #   column(width = 3,
    #          selectInput(inputId = "dc.pca.group2",
    #                      label = "Group information",
    #                      choices = c("class", "batch", "group"),
    #                      selected = "batch", multiple = FALSE),
    #          useShinyalert(),
    #          actionButton(inputId = "dc.qa2.pca.analysis.button",
    #                       label = "Begin", styleclass = "info"),
    #          helpText("Click", strong("Begin"), "to do PCA analysis.")
    #          ),
    #   column(width = 9,
    #          shinysky::busyIndicator(text = "Processing..."),
    #          plotlyOutput(outputId = "dc.qa2.pca.score.plot",
    #                       # width = "800px",
    #                       height = "500px")
    #          ),
    #   br(),
    #   br(),
    #   br()
    # ),

    tabPanel(
      title = "PCA score plot",
      icon = icon("image"),
      shinysky::busyIndicator(text = "Processing..."),
      span(textOutput(outputId = "dc.qa2.pca.object.message"), style = "color:red"),
      span(textOutput(outputId = "dc.qa2.pca.score.plot.message"), style = "color:red"),
      span(textOutput(outputId = "dc.qa2.pca.object.for.report.message"), style = "color:red"),
      span(textOutput(outputId = "dc.qa2.pca.score.plot.for.report.message"), style = "color:red"),
      plotlyOutput(outputId = "dc.qa2.pca.score.plot",
                   # width = "800px",
                   height = "500px"),
      br(),
      br(),
      br()
    ),

    tabPanel(
      title = "QC intensity boxplot",
      icon = icon("image"),
      shinysky::busyIndicator(text = "Processing..."),
      span(textOutput(outputId = "dc.qc.int.boxplot2.message"), style = "color:red"),
      plotlyOutput(outputId = "dc.qc.int.boxplot2",
                   # width = "800px",
                   height = "500px"),
      br(),
      br(),
      br()
    ),

    tabPanel(
      title = "QC correlation",
      icon = icon("tasks"),
      tabsetPanel(type = "pills",
                  tabPanel(
                    title = "QC correlation plot",
                    icon = icon("image"),
                    shinysky::busyIndicator(text = "Processing..."),
                    span(textOutput(outputId = "dc.qc.cor.plot2.message"), style = "color:red"),
                    span(textOutput(outputId = "dc.qc.cor2.message"), style = "color:red"),
                    plotlyOutput(outputId = "dc.qc.cor.plot2",
                                 # width = "800px",
                                 height = "800px")
                  ),
                  tabPanel(
                    title = "QC correlation matrix",
                    icon = icon("table"),
                    DT::dataTableOutput(outputId = "dc.qc.cor2")
                  )
      ),
      br(),
      br(),
      br()
    )
  )
)


