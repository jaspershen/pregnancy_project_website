fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(

    uiOutput(outputId = "da.group.area"),

    selectInput(inputId = "daUAlog",
                label = "Logarithm method",
                choices = c(
                  "No log" = "no",
                  "Log 2" = "log2",
                  "Log e" = "loge",
                  "Log 10" = "log10"
                ), selected = "no"),

    # selectInput(inputId = "daUAscale",
    #             label = "Scale method",
    #             choices = c(
    #               "Pareto scale" = "pareto",
    #               "Auto scale" = "auto",
    #               "No scale" = "no"
    #             ), selected = "no"),

    # checkboxInput(inputId = "daUAcenter",
    #               label = "Center or not",
    #               value = FALSE),

    selectInput(inputId = "daUAfcWhich",
                label = "Use what to calcuate fold change",
                choices = c(
                  "Median" = "median",
                  "Mean" = "mean"
                ), selected = "mean"),

#------------------------------------------------------------------------------
#hypothesis testing
selectInput(inputId = "daUAhypothesisTesting",
            label = "Hypothesis testing method",
            choices = c(
              "Student's t test" = "t",
              "Wilcoxon test" = "w"
            ), selected = "t"
),


selectInput(inputId = "daUAalternative",
            label = "Alternative",
            choices = c(
              "Two sided" = "two.sided",
              "Less" = "less",
              "Greater" = "greater"
            ), selected = "two.sided"
),

checkboxInput(inputId = "daUApaired",
              label = "Paired", value = FALSE),

selectInput(inputId = "daUAadjust",
            label = "Correction method",
            choices = c(
              "False discovery ratio (FDR)" = "fdr",
              "Holm" = "holm",
              "Hochberg" = "hochberg",
              "Hommel" = "hommel",
              "Bonferroni" = "bonferroni",
              "BH" = "BH",
              "BY" = "BY",
              "No correction" = "no"
            ), selected = "fdr"),
numericInput(inputId = "daDMpCutoff1", label = "P-value cutoff",
             value = 0.05, min = 0, max = 1),
numericInput(inputId = "daDMfcCutoff1", label = "Fold-change cutoff",
             value = 2, min = 1, max = 50),

# numericInput(inputId = "daUApCutoff", label = "P-value cutoff",
#             value = 0.05, min = 0, max = 1),
# numericInput(inputId = "daUAfcCutoff", label = "Fold change cutoff",
#              value = 2, min = 1, max = 50),


    actionButton(inputId = 'daUAsubmitButton',
                 "Submit",
                 styleclass = "info",
                 icon = icon('play-circle')),
    useShinyalert(),
    actionButton(inputId = "daUA2MA",
                 label = "Next", styleclass = "warning"),
    helpText("Click", strong("Submit"), "to do univariate analysis")
  ),
  mainPanel = mainPanel(
    span(textOutput(outputId = "da.ua.params.message"), style = "color:red"),
    tabsetPanel(id = "daUAresult",
                type = "tabs",

                tabPanel("Volcano plot",
                         value = "daDMvolcanoPlot",
                         icon = icon("image"),
                         # actionButton(inputId = 'daUAgenerateVolcanoPlot',
                         #              "Generate Volcano plot",
                         #              styleclass = "info",
                         #              icon = icon('play-circle'))

                         column(width = 12,
                                shinysky::busyIndicator(text = "Processing..."),
                                plotly::plotlyOutput(outputId = "da.volcano.plot",
                                                     height = "600px",
                                                     width = "100%")
                         )
                ),

                tabPanel("Fold change and P-value",
                         value = "daUAfcP",
                         icon = icon("table"),
                         shinysky::busyIndicator(text = "Processing..."),
                         DT::dataTableOutput(outputId = "da.ms1.p.fc")
                ),
                br(), br(), br()
    ),
    br(), br(), br()
  )
)
)



















##validation

