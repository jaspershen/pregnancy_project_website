fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(

    uiOutput(outputId = "dc.mv.group.area"),
    sliderInput(inputId = "dc.mv.peak.remove.tol",
                label = "Remove peaks with MV ratio>(%)",
                min = 10, max = 80, value = 50,
                step = 1, round = FALSE),

    selectInput(inputId = "DCimputationMethod",
                label = "Imputation method",
                choices = c("Zero value", "Mean", "Median",
                            "Minimum", "KNN", "missForest",
                            "BPCA"
                            # "PPCA",
                            # "SVD"
                            ),
                selected = "KNN"),

    conditionalPanel(
      condition = "input.DCimputationMethod == 'KNN'",
      sliderInput(inputId = "k",
                  label = "Number of neighbors",
                  min = 2, max = 20, value = 10,
                  step = 1, round = TRUE),
      sliderInput(inputId = "rowmax",
                  label = "The maximum percent missing data allowed in any row",
                  min = 1, max = 70, value = 50,
                  step = 0.5, round = FALSE),
      sliderInput(inputId = "colmax",
                  label = "The maximum percent missing data allowed in any column",
                  min = 1, max = 90, value = 80,
                  step = 0.5, round = FALSE)
    ),
    conditionalPanel(
      condition = "input.DCimputationMethod == 'missForest'",
      sliderInput(inputId = "mvNtree",
                  label = "Number of trees to grow in each forest",
                  min = 1, max = 30, value = 10,
                  step = 1, round = TRUE),
      checkboxInput(inputId = "mvReplace",
                    label = "Bootstrap sampling (with replacements) is performed",
                    value = TRUE)
    ),
    conditionalPanel(
      condition = "input.DCimputationMethod == 'BPCA'",
      sliderInput(inputId = "bpca.nPcs",
                  label = "Number of principal components to calculate",
                  min = 1, max = 5, value = 2,
                  step = 1, round = TRUE)
    ),
    conditionalPanel(
      condition = "input.DCimputationMethod == 'PPCA'",
      sliderInput(inputId = "ppca.nPcs",
                  label = "Number of principal components to calculate",
                  min = 1, max = 5, value = 2,
                  step = 1, round = TRUE)
    ),
    conditionalPanel(
      condition = "input.DCimputationMethod == 'SVD'",
      sliderInput(inputId = "svd.nPcs",
                  label = "Number of principal components to calculate",
                  min = 1, max = 5, value = 2,
                  step = 1, round = TRUE)
    ),
    actionButton(inputId = 'dc.mv.submit.button',
                 "Submit",
                 styleclass = "info",
                 icon = icon('play-circle')),
    useShinyalert(),
    actionButton(inputId = "dc.mv.2.dc.zero",
                 label = "Next", styleclass = "warning"),
    helpText("Click", strong("Submit"), "to process Missing values.
             If you don't have any MVs in you dataset, you can select any method.")
  ),
  mainPanel = mainPanel(
    span(textOutput(outputId = "dc.mv.params.message"), style = "color:red"),
    tabsetPanel(id = "DCmvResult",
                type = "tabs",
                tabPanel("Summary",
                         value = "dc.mv.summary",
                         icon = icon("tasks"),
                         span(textOutput(outputId = "mv.processing.message"), style = "color:black")
                ),
                tabPanel("MS1 peak table (after MV processing)",
                         value = "dc.ms1.after.mv",
                         icon = icon("table"),
                         shinysky::busyIndicator(text = "Processing..."),
                         span(textOutput(outputId = "dc.ms1.after.mv.message"), style = "color:red"),
                         DT::dataTableOutput(outputId = "dc.ms1.after.mv")
                )
    ),
    br(), br(), br()
  )
)
)