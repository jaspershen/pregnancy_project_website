fluidPage(
  tabsetPanel(
    type = "tabs",
    tabPanel(
      span(textOutput(outputId = "dc.ba.info.message"), style = "color:red"),
      span(textOutput(outputId = "dc.data.profile.plot.message"), style = "color:red"),
      title = "Data profile",
      icon = icon("image"),
      shinysky::busyIndicator(text = "Processing..."),
      plotlyOutput(outputId = "dc.data.profile.plot",
                   # width = "800px",
                   height = "500px"),
      actionButton(inputId = "dc.qa1.2.dc.mv",
                   # inputId = "DCdataCheck2DCdataProfile",
                   label = "Next", styleclass = "warning"),
      br(),
      br(),
      br()
    ),
    tabPanel(
      title = "Missing value distribution",
      icon = icon("image"),
      shinysky::busyIndicator(text = "Processing..."),
      span(textOutput(outputId = "dc.peak.mv.ratio.plot.message"), style = "color:red"),
      span(textOutput(outputId = "dc.sample.mv.ratio.plot.message"), style = "color:red"),
      plotlyOutput(outputId = "dc.peak.mv.ratio.plot",
                   # width = "800px",
                   height = "500px"),
      plotlyOutput(outputId = "dc.sample.mv.ratio.plot",
                   # width = "800px",
                   height = "500px"),
      br(),
      br(),
      br()
    ),

    tabPanel(
      title = "Zero value distribution",
      icon = icon("image"),
      shinysky::busyIndicator(text = "Processing..."),
      span(textOutput(outputId = "dc.peak.zero.ratio.plot.message"), style = "color:red"),
      span(textOutput(outputId = "dc.sample.zero.ratio.plot.message"), style = "color:red"),
      plotlyOutput(outputId = "dc.peak.zero.ratio.plot",
                   # width = "800px",
                   height = "500px"),
      plotlyOutput(outputId = "dc.sample.zero.ratio.plot",
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
      span(textOutput(outputId = "dc.peak.rsd.plot.message"), style = "color:red"),
      span(textOutput(outputId = "dc.peak.rsd.plot.for.report.message"), style = "color:red"),
      uiOutput(outputId = "dc.qa1.group.area"),
      plotlyOutput(outputId = "dc.peak.rsd.plot",
                   # width = "800px",
                   height = "500px"
      ),
      br(),
      br(),
      br()
    ),

    tabPanel(
      title = "PCA score plot",
      icon = icon("image"),
                    shinysky::busyIndicator(text = "Processing..."),
      span(textOutput(outputId = "dc.qa1.pca.object.message"), style = "color:red"),
      span(textOutput(outputId = "dc.qa1.pca.score.plot.message"), style = "color:red"),
      span(textOutput(outputId = "dc.qa1.pca.score.plot.for.report.message"), style = "color:red"),
                    plotlyOutput(outputId = "dc.qa1.pca.score.plot",
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
      span(textOutput(outputId = "dc.qc.int.boxplot.message"), style = "color:red"),
      plotlyOutput(outputId = "dc.qc.int.boxplot",
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
                    span(textOutput(outputId = "dc.qc.cor.plot.message"), style = "color:red"),
                    plotlyOutput(outputId = "dc.qc.cor.plot",
                                 # width = "800px",
                                 height = "800px")
                  ),
                  tabPanel(
                    title = "QC correlation matrix",
                    icon = icon("table"),
                    span(textOutput(outputId = "dc.qc.cor.message"), style = "color:red"),
                    DT::dataTableOutput(outputId = "dc.qc.cor")
                  )
      ),
      br(),
      br(),
      br()
    )

  )


)