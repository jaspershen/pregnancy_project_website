##PLS analysis UI
ui.ma.pls <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "maPLSMainpan",
                  type = "pills",
                  tabPanel("Upload data",
                           value = "ma.pls.upload.data",
                           icon = icon(name = "upload")
                           ),
                  tabPanel("Parameter setting",
                           value = "ma.pls.parameter.setting",
                           icon = icon(name = "cogs")
                           )
      ),
      conditionalPanel(
        condition = 'input.maPLSMainpan == "ma.pls.upload.data"',

        textInput(inputId = "ma.pls.project.name",
                  label = h5(
                    "Project name",
                    shinyBS::tipify(
                      el = icon(name = "info-circle"),
                      placement = "bottom",
                      trigger = "hover",
                      title = "Please set the name of your project"
                    )
                  ),
                  value = "project1",
                  placeholder = "Your project name"),

        selectInput(inputId = "maPLSDataFrom",
                    label = h5(
                      "Data from",
                      shinyBS::tipify(
                        el = icon(name = "info-circle"),
                        placement = "bottom",
                        trigger = "hover",
                        title = "You can use demo data or your local data"
                      )
                    ),
                    choices = c("Demo data" = "ma.pls.demo.data",
                                "Local data" = "ma.pls.local.data",
                                "Previous step" = "ma.pls.previous.step"),
                    selected = "ma.pls.demo.data", multiple = FALSE),

        conditionalPanel(
          condition = 'input.maPLSDataFrom == "ma.pls.demo.data"',
          actionButton(inputId = 'ma.pls.use.demo.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load demo data"),
          helpText(a("Click Here to Download Demo data",
                     href="http://oxr5qv74w.bkt.clouddn.com/Outlier_Sample.zip",
                     targeted = "_blank"))
        ),

        conditionalPanel(
          condition = 'input.maPLSDataFrom == "ma.pls.local.data"',
          fileInput(inputId = "ma.pls.ms1.peak.table",
                    label = h5(
                      "MS1 peak table",
                      shinyBS::tipify(
                        el = icon(name = "info-circle"),
                        placement = "bottom",
                        trigger = "hover",
                        title = "The first column must be peak name"
                      )
                    ),
                    multiple = FALSE,
                    accept = "csv",
                    buttonLabel = "Browser",
                    placeholder = "Only csv format is supported"),

          fileInput(inputId = "ma.pls.sample.info",
                    label = h5(
                      "Sample information",
                      shinyBS::tipify(
                        el = icon(name = "info-circle"),
                        placement = "bottom",
                        trigger = "hover",
                        title = "Column 1: sample name, column 2: group"
                      )
                    ),
                    multiple = FALSE,
                    accept = "csv",
                    buttonLabel = "Browser",
                    placeholder = "Only csv format is supported"),

          useShinyalert(),
          actionButton(inputId = 'ma.pls.use.local.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load your data")
        ),

        conditionalPanel(
          condition = 'input.maPLSDataFrom == "ma.pls.previous.step"',
          selectInput(inputId = "ma.pls.from.which.step",
                      label = h5("Use data from which step?",
                                 shinyBS::tipify(
                                   el = icon(name = "info-circle"),
                                   placement = "bottom",
                                   trigger = "hover",
                                   title = "Use data from which step?"),
                                 style = "color:black"),
                      choices = c(
                        "Missing value imputation" = "ma.pls.mv.step",
                        "Data normalization" = "ma.pls.dn.step",
                        "Outlier sample processing" = "ma.pls.os.step",
                        "Univariate analysis" = "ma.pls.ua.step"
                      ), selected = "ma.pls.ua.step"),

          h5("Previous step data information",
             shinyBS::tipify(
               el = icon(name = "info-circle"),
               placement = "bottom",
               trigger = "hover",
               title = "You can use data from previous steps"
             ),
             style = "color:black"),

          textOutput(outputId = "maPLSprojectName"),
          br(),
          useShinyalert(),
          actionButton(inputId = 'ma.pls.use.previous.step.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),

          helpText("Click", strong("Load data"),
                   "to load previous step data")
        )
      ),
      conditionalPanel(
        condition = 'input.maPLSMainpan == "ma.pls.parameter.setting"',

        selectInput(inputId = "ma.pls.log",
                    label = "Logarithm method",
                    choices = c(
                      "No log" = "ma.pls.no.log",
                      "Log 2" = "ma.pls.log2",
                      "Log e" = "ma.pls.loge",
                      "Log 10" = "ma.pls.log10"
                    ), selected = "ma.pls.no.log"),

        selectInput(inputId = "ma.pls.scale",
                    label = "Scale method",
                    choices = c(
                      "Pareto scale" = "ma.pls.pareto",
                      "Auto scale" = "ma.pls.auto",
                      "No scale" = "ma.pls.no.scale"
                    ), selected = "ma.pls.pareto"),

        checkboxInput(inputId = "ma.pls.center",
                      label = "Center or not",
                      value = TRUE),

        uiOutput("ma.pls.group.area"),

        useShinyalert(),
        actionButton(inputId = 'ma.pls.submit',
                     "Submit",
                     styleclass = "info",
                     icon = icon('play-circle')),
        helpText("Click", strong("Submit"), "to do PLS analysis")
      )
    )
  )
}





###PLS analysis result UI
ui.ma.pls.result <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "maPLSResultMainpan",
                  type = "tabs",
                  tabPanel("MS1 peak table",
                           value = "ma.pls.ms1.peak.table",
                           icon = icon("table")
                           ),
                  tabPanel("Sample information",
                           value = "ma.pls.sample.info",
                           icon = icon("info-circle")
                           ),
                  tabPanel("Data check",
                           value = "ma.pls.data.check",
                           icon = icon("check-circle")
                           ),
                  tabPanel("Summary",
                           value = "ma.pls.summary",
                           icon = icon("tasks")
                           )
      ),
      conditionalPanel(
        condition = 'input.maPLSResultMainpan == "ma.pls.ms1.peak.table"',
        uiOutput(outputId = "ma.pls.ms1.peak.table.area"),
        DT::dataTableOutput("ma.pls.peak.table")
      ),

      conditionalPanel(
        condition = 'input.maPLSResultMainpan == "ma.pls.sample.info"',
        uiOutput(outputId = "ma.pls.sample.info.area"),
        DT::dataTableOutput("ma.pls.sample.info")
      ),

      conditionalPanel(
        condition = 'input.maPLSResultMainpan == "ma.pls.data.check"',
        h5("Data from"),
        h5(textOutput(outputId = "ma.pls.use.data")),
        hr(),
        h5("MS1 peak table"),
        h5(textOutput(outputId = "ma.pls.ms1.peak.table.info")),
        hr(),
        h5("Sample information"),
        h5(tableOutput(outputId = "ma.pls.sample.info.info"))
      ),

      conditionalPanel(
        condition = 'input.maPLSResultMainpan == "ma.pls.summary"',
        fluidPage(
          uiOutput(outputId = "ma.pls.summary.area"),
          column(width = 6,
                 wellPanel(
                   fluidPage(
                     column(width = 1,
                            dropdownButton(
                              numericInput(inputId = "ma.pls.number.comp.to.see",
                                           label = "The number of component to see",
                                           value = 10, step = 1, min = 3, max = 30),
                              circle = TRUE, status = "info",
                              size = "sm",
                              icon = icon("cogs"),
                              width = "300px",
                              tooltip = tooltipOptions(title = "Component number")
                            )
                     ),

                     column(width = 11,
                            shinysky::busyIndicator(text = "PLS analysis..."),
                            plotOutput(outputId = "ma.pls.q2.barplot",
                                       height = "350px", width = "350px")),
                     numericInput(inputId = "ma.pls.number.comp",
                                  label = "The number of component",
                                  value = 10, step = 1, min = 3, max = 30),
                     actionButton(inputId = 'ma.pls.submit.final.model',
                                  "Submit",
                                  styleclass = "info",
                                  icon = icon('play-circle')),
                     helpText("Please select the number of componet and click",
                              strong("Submit"), "to construct the final PLS model")

                   )
                 )

          ),
          column(width = 6,
                 wellPanel(
                   fluidPage(
                     column(width = 1,
                            dropdownButton(
                              sliderInput(inputId = "ma.pls.q2r2.width",
                                          label = "Width (inch)", min = 3,
                                          max = 20, value = 7),
                              sliderInput(inputId = "ma.pls.q2r2.height",
                                          label = "Height (inch)", min = 3,
                                          max = 20, value = 7),
                              downloadButton('ma.pls.download.q2r2.barplot', 'Download plot'),
                              circle = TRUE, status = "info",
                              size = "sm",
                              icon = icon("download"),
                              width = "300px",
                              tooltip = tooltipOptions(title = "Download image")
                            )
                     ),
                     column(width = 11,
                            shinysky::busyIndicator(text = "PLS analysis..."),
                            plotOutput(outputId = "ma.pls.q2r2.barplot",
                                       height = "350px", width = "350px")),
                     actionButton(inputId = 'maPLSgo1',
                                  "Show PLS score plot",
                                  styleclass = "info",
                                  icon = icon('play-circle')),

                     bsModal(id = "modalExamplePLS",
                             title = "PLS score plot",
                             trigger = "maPLSgo1",
                             size = "large",
                             fluidPage(
                               column(width = 1,
                                      dropdownButton(
                                        numericInput(inputId = "ma.pls.cex",
                                                     label = "Point size",
                                                     value = 1, step = 0.1),
                                        numericInput(inputId = "ma.pls.cex.lab",
                                                     label = "The size of x and y labels",
                                                     value = 1.8, step = 0.1),
                                        numericInput(inputId = "ma.pls.cex.axis",
                                                     label = "The size of axis annotation",
                                                     value = 1.5, step = 0.1),
                                        circle = TRUE, status = "info",
                                        size = "sm",
                                        icon = icon("cogs"),
                                        width = "300px",
                                        tooltip = tooltipOptions(title = "Image setting")
                                      ),

                                      dropdownButton(
                                        sliderInput(inputId = "ma.pls.image.width",
                                                    label = "Width (inch)", min = 3,
                                                    max = 20, value = 7),
                                        sliderInput(inputId = "ma.pls.image.height",
                                                    label = "Height (inch)", min = 3,
                                                    max = 20, value = 7),
                                        downloadButton('ma.pls.download.pls.plot', 'Download plot'),
                                        circle = TRUE, status = "info",
                                        icon = icon("download"),
                                        width = "300px",
                                        size = "sm",
                                        tooltip = tooltipOptions(title = "Download image")
                                      )
                               ),
                               column(width = 11,
                                      shinysky::busyIndicator(text = "PLS analysis..."),
                                      plotOutput(outputId = "ma.pls.plot",
                                                 height = "600px", width = "600px"))

                             )
                     ),


                     actionButton(inputId = 'maPLSgo2',
                                  "Show VIP value",
                                  styleclass = "info",
                                  icon = icon('play-circle')),

                     bsModal(id = "modalExamplePLS2",
                             title = "VIP values",
                             trigger = "maPLSgo2",
                             size = "large",
                             fluidPage(
                               shinysky::busyIndicator(text = "PLS analysis..."),
                               DT::dataTableOutput(outputId = "ma.pls.vip")
                             )
                     )


                   )
                 )
          )
        )
      )
    )
  )
}


