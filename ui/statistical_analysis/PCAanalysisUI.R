##PCA analysis UI
ui.ma.pca <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "maPCAMainpan",
                  type = "pills",
                  tabPanel("Upload data",
                           value = "ma.pca.upload.data",
                           icon = icon(name = "upload")
                           ),
                  tabPanel("Parameter setting",
                           value = "ma.pca.parameter.setting",
                           icon = icon(name = "cogs")
                           )
      ),
      conditionalPanel(
        condition = 'input.maPCAMainpan == "ma.pca.upload.data"',

        textInput(inputId = "ma.pca.project.name",
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

        selectInput(inputId = "maPCADataFrom",
                    label = h5(
                      "Data from",
                      shinyBS::tipify(
                        el = icon(name = "info-circle"),
                        placement = "bottom",
                        trigger = "hover",
                        title = "You can use demo data or your local data"
                      )
                    ),
                    choices = c("Demo data" = "ma.pca.demo.data",
                                "Local data" = "ma.pca.local.data",
                                "Previous step" = "ma.pca.previous.step"),
                    selected = "ma.pca.demo.data", multiple = FALSE),

        conditionalPanel(
          condition = 'input.maPCADataFrom == "ma.pca.demo.data"',
          actionButton(inputId = 'ma.pca.use.demo.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load demo data"),
          helpText(a("Click Here to Download Demo data",
                     href="http://oxr5qv74w.bkt.clouddn.com/Outlier_Sample.zip",
                     targeted = "_blank"))
        ),

        conditionalPanel(
          condition = 'input.maPCADataFrom == "ma.pca.local.data"',
          fileInput(inputId = "ma.pca.ms1.peak.table",
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

          fileInput(inputId = "ma.pca.sample.info",
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
          actionButton(inputId = 'ma.pca.use.local.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load your data")
        ),

        conditionalPanel(
          condition = 'input.maPCADataFrom == "ma.pca.previous.step"',
          selectInput(inputId = "ma.pca.from.which.step",
                      label = h5("Use data from which step?",
                                 shinyBS::tipify(
                                   el = icon(name = "info-circle"),
                                   placement = "bottom",
                                   trigger = "hover",
                                   title = "Use data from which step?"),
                                 style = "color:black"),
                      choices = c(
                        "Missing value imputation" = "ma.pca.mv.step",
                        "Data normalization" = "ma.pca.dn.step",
                        "Outlier sample processing" = "ma.pca.os.step",
                        "Univariate analysis" = "ma.pca.ua.step"
                      ), selected = "ma.pca.ua.step"),

          h5("Previous step data information",
             shinyBS::tipify(
               el = icon(name = "info-circle"),
               placement = "bottom",
               trigger = "hover",
               title = "You can use data from previous steps"
             ),
             style = "color:black"),

          textOutput(outputId = "maPCAprojectName"),
          br(),
          useShinyalert(),
          actionButton(inputId = 'ma.pca.use.previous.step.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),

          helpText("Click", strong("Load data"),
                   "to load previous step data")
        )
      ),
      conditionalPanel(
        condition = 'input.maPCAMainpan == "ma.pca.parameter.setting"',

        selectInput(inputId = "ma.pca.log",
                    label = "Logarithm method",
                    choices = c(
                      "No log" = "ma.pca.no.log",
                      "Log 2" = "ma.pca.log2",
                      "Log e" = "ma.pca.loge",
                      "Log 10" = "ma.pca.log10"
                    ), selected = "ma.pca.no.log"),

        selectInput(inputId = "ma.pca.scale",
                    label = "Scale method",
                    choices = c(
                      "Pareto scale" = "ma.pca.pareto",
                      "Auto scale" = "ma.pca.auto",
                      "No scale" = "ma.pca.no.scale"
                    ), selected = "ma.pca.pareto"),

        checkboxInput(inputId = "ma.pca.center",
                      label = "Center or not",
                      value = TRUE),

        uiOutput("ma.pca.group.area"),

        useShinyalert(),
        actionButton(inputId = 'ma.pca.submit',
                     "Submit",
                     styleclass = "info",
                     icon = icon('play-circle')),
        helpText("Click", strong("Submit"), "to do PCA analysis")
      )
    )
  )
}





###PCA analysis result UI
ui.ma.pca.result <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "maPCAResultMainpan", type = "tabs",
                  tabPanel("MS1 peak table",
                           value = "ma.pca.ms1.peak.table",
                           icon = icon("table")
                           ),
                  tabPanel("Sample information",
                           value = "ma.pca.sample.info",
                           icon = icon("info-circle")
                           ),
                  tabPanel("Data check",
                           value = "ma.pca.data.check",
                           icon = icon("check-circle")
                           ),
                  tabPanel("Summary",
                           value = "ma.pca.summary",
                           icon = icon("tasks")
                           )
      ),
      conditionalPanel(
        condition = 'input.maPCAResultMainpan == "ma.pca.ms1.peak.table"',
        uiOutput(outputId = "ma.pca.ms1.peak.table.area"),
        DT::dataTableOutput("ma.pca.peak.table")
      ),

      conditionalPanel(
        condition = 'input.maPCAResultMainpan == "ma.pca.sample.info"',

        uiOutput(outputId = "ma.pca.sample.info.area"),

        DT::dataTableOutput("ma.pca.sample.info")
      ),

      conditionalPanel(
        condition = 'input.maPCAResultMainpan == "ma.pca.data.check"',
        h5("Data from"),
        h5(textOutput(outputId = "ma.pca.use.data")),
        hr(),
        h5("MS1 peak table"),
        h5(textOutput(outputId = "ma.pca.ms1.peak.table.info")),
        hr(),
        h5("Sample information"),
        h5(tableOutput(outputId = "ma.pca.sample.info.info"))
      ),

      conditionalPanel(
        condition = 'input.maPCAResultMainpan == "ma.pca.summary"',
        fluidPage(
          uiOutput(outputId = "ma.pca.summary.area"),

          fluidPage(
            column(width = 1,
                   dropdownButton(
                     numericInput(inputId = "ma.pca.cex",
                                  label = "Point size",
                                  value = 1, step = 0.1),
                     numericInput(inputId = "ma.pca.cex.lab",
                                  label = "The size of x and y labels",
                                  value = 1.8, step = 0.1),
                     numericInput(inputId = "ma.pca.cex.axis",
                                  label = "The size of axis annotation",
                                  value = 1.5, step = 0.1),
                     circle = TRUE, status = "info",
                     icon = icon("cogs"),
                     width = "300px",
                     tooltip = tooltipOptions(title = "Image setting")
                   ),
                   dropdownButton(
                     sliderInput(inputId = "ma.pca.image.width",
                                 label = "Width (inch)", min = 3,
                                 max = 20, value = 7),
                     sliderInput(inputId = "ma.pca.image.height",
                                 label = "Height (inch)", min = 3,
                                 max = 20, value = 7),
                     downloadButton('ma.pca.download.pca.plot', 'Download plot'),
                     circle = TRUE, status = "info", icon = icon("download"),
                     width = "300px",
                     tooltip = tooltipOptions(title = "Download image")
                   )
            ),

            column(width = 8,
                   plotOutput(outputId = "ma.pca.plot",
                              height = "600px", width = "600px")
            )

          )


        )
      )
    )
  )
}