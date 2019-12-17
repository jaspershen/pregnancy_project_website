fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(

    # uiOutput(outputId = "dc.zero.group.area"),
    sliderInput(inputId = "dc.zero.peak.remove.tol",
                label = "Remove peaks with zero ratio>(%)",
                min = 10, max = 80, value = 50,
                step = 1, round = FALSE),

    actionButton(inputId = 'dc.zero.submit.button',
                 "Submit",
                 styleclass = "info",
                 icon = icon('play-circle')),
    useShinyalert(),
    actionButton(inputId = "dc.zero.2.dc.dn",
                 label = "Next", styleclass = "warning"),
    helpText("Click", strong("Submit"), "to filter zero values")
  ),
  mainPanel = mainPanel(
    span(textOutput(outputId = "dc.zero.params.message"), style = "color:red"),
    tabsetPanel(id = "DCzeroResult",
                type = "tabs",
                tabPanel("Summary",
                         value = "dc.zero.summary",
                         icon = icon("tasks"),
                         span(textOutput(outputId = "zero.processing.message"), style = "color:black")
                ),
                tabPanel("MS1 peak table (after zero processing)",
                         value = "dc.ms1.after.zero",
                         icon = icon("table"),
                         shinysky::busyIndicator(text = "Processing..."),
                         span(textOutput(outputId = "dc.ms1.after.zero.message"), style = "color:red"),
                         DT::dataTableOutput(outputId = "dc.ms1.after.zero")
                )
    ),
    br(), br(), br()
  )
)
)