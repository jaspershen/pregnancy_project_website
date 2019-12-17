
###T test UI
ui.ua.ttest <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "uaTtestMainpan",
                  type = "pills",
                  tabPanel("Upload data",
                           value = "ua.ttest.upload.data",
                           icon = icon(name = "upload")
                           ),
                  tabPanel("Parameter setting",
                           value = "ua.ttest.parameter.setting",
                           icon = icon(name = "cogs")
                           )
      ),
      conditionalPanel(
        condition = 'input.uaTtestMainpan == "ua.ttest.upload.data"',

        textInput(inputId = "ua.ttest.project.name",
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

        selectInput(inputId = "uaTtestDataFrom",
                    label = h5(
                      "Data from",
                      shinyBS::tipify(
                        el = icon(name = "info-circle"),
                        placement = "bottom",
                        trigger = "hover",
                        title = "You can use demo data or your local data"
                      )
                    ),
                    choices = c("Demo data" = "ua.ttest.demo.data",
                                "Local data" = "ua.ttest.local.data",
                                "Previous step" = "ua.ttest.previous.step"),
                    selected = "ua.ttest.demo.data", multiple = FALSE),

        conditionalPanel(
          condition = 'input.uaTtestDataFrom == "ua.ttest.demo.data"',
          actionButton(inputId = 'ua.ttest.use.demo.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load demo data"),
          helpText(a("Click Here to Download Demo data",
                     href="http://oxr5qv74w.bkt.clouddn.com/Outlier_Sample.zip",
                     targeted = "_blank"))
        ),

        conditionalPanel(
          condition = 'input.uaTtestDataFrom == "ua.ttest.local.data"',
          fileInput(inputId = "ua.ttest.ms1.peak.table",
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

          fileInput(inputId = "ua.ttest.sample.info",
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
          actionButton(inputId = 'ua.ttest.use.local.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load your data")
        ),

        conditionalPanel(
          condition = 'input.uaTtestDataFrom == "ua.ttest.previous.step"',
          selectInput(inputId = "ua.ttest.from.which.step",
                      label = h5("Use data from which step?",
                                 shinyBS::tipify(
                                   el = icon(name = "info-circle"),
                                   placement = "bottom",
                                   trigger = "hover",
                                   title = "Use data from which step?"),
                                 style = "color:black"),
                      choices = c(
                        "Missing value imputation" = "ua.ttest.mv.step",
                        "Data normalization" = "ua.ttest.dn.step",
                        "Outlier sample processing" = "ua.ttest.os.step"
                      ), selected = "ua.ttest.os.step"),

          h5("Previous step data information",
             shinyBS::tipify(
               el = icon(name = "info-circle"),
               placement = "bottom",
               trigger = "hover",
               title = "You can use data from previous steps"
             ),
             style = "color:black"),

          textOutput(outputId = "uaTtestprojectName"),
          br(),
          useShinyalert(),
          actionButton(inputId = 'ua.ttest.use.previous.step.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),

          helpText("Click", strong("Load data"),
                   "to load previous step data")
        )
      ),
      conditionalPanel(
        condition = 'input.uaTtestMainpan == "ua.ttest.parameter.setting"',

        selectInput(inputId = "ua.ttest.log",
                    label = "Logarithm method",
                    choices = c(
                      "No log" = "ua.ttest.no.log",
                      "Log 2" = "ua.ttest.log2",
                      "Log e" = "ua.ttest.loge",
                      "Log 10" = "ua.ttest.log10"
                    ), selected = "ua.ttest.no.log"),

        selectInput(inputId = "ua.ttest.scale",
                    label = "Scale method",
                    choices = c(
                      "Pareto scale" = "ua.ttest.pareto",
                      "Auto scale" = "ua.ttest.auto",
                      "No scale" = "ua.ttest.no.scale"
                    ), selected = "ua.ttest.pareto"),

        checkboxInput(inputId = "ua.ttest.center",
                      label = "Center or not",
                      value = TRUE),

        uiOutput("ua.ttest.group.area"),

        selectInput(inputId = "ua.ttest.alternative",
                    label = "Alternative",
                    choices = c(
                      "Two sided" = "uaTtest.two.sided",
                      "Less" = "uaTtest.less",
                      "Greater" = "uaTtest.greater"
                    ), selected = "uaTtest.two.sided"
        ),

        checkboxInput(inputId = "ua.ttest.paired",
                      label = "Paired t-test", value = FALSE),

        selectInput(inputId = "ua.ttest.adjust",
                    label = "correction method",
                    choices = c(
                      "False discovery ratio (FDR)" = "uaTtestFDR",
                      "Holm" = "uaTtestHolm",
                      "Hochberg" = "uaTtesthochberg",
                      "Hommel" = "uaTtestHommel",
                      "Bonferroni" = "uaTtestBonferroni",
                      "BH" = "uaTtestBH",
                      "BY" = "uaTtestBY",
                      "No correction" = "uaTtestNoCorrection"
                    ), selected = "uaTtestFDR"),

        actionButton(inputId = 'ua.ttest.submit',
                     "Submit",
                     styleclass = "info",
                     icon = icon('play-circle')),
        helpText("Click", strong("Submit"), "to calcaule fold change")
      )
    )
  )
}





###T test result UI
ui.ua.ttest.result <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "uaTtestResultMainpan",
                  type = "tabs",
                  tabPanel("MS1 peak table",
                           value = "ua.ttest.ms1.peak.table",
                           icon = icon("table")
                           ),
                  tabPanel("Sample information",
                           value = "ua.ttest.sample.info",
                           icon = icon("info-circle")
                           ),
                  tabPanel("Data check",
                           value = "ua.ttest.data.check",
                           icon = icon("check-circle")
                           ),
                  tabPanel("Summary",
                           value = "ua.ttest.summary",
                           icon = icon("tasks")
                           )
      ),
      conditionalPanel(
        condition = 'input.uaTtestResultMainpan == "ua.ttest.ms1.peak.table"',

        uiOutput(outputId = "ua.ttest.ms1.peak.table.area"),

        actionButton(inputId = "uaTtestGo1",
                     label = "Generate intensity distribution",
                     styleclass = "info"),

        bsModal(id = "modalExample21",
                title = "Intendity distribution",
                trigger = "uaTtestGo1",
                size = "large",
                sidebarLayout(
                  sidebarPanel = sidebarPanel(
                    textOutput(outputId = "ua.ttest.peak.info1"),
                    textOutput(outputId = "ua.ttest.peak.info2")
                  ),
                  mainPanel = mainPanel(
                    plotOutput("ua.ttest.peak.distribution"),
                    column(width = 5,
                           sliderInput(inputId = "ua.ttest.peak.distribution.width",
                                       label = "Width (inch)", min = 6,
                                       max = 20, value = 8)),
                    column(width = 5,
                           sliderInput(inputId = "ua.ttest.peak.distribution.height",
                                       label = "Height (inch)", min = 3,
                                       max = 20, value = 6)),
                    downloadButton('ua.ttest.download.peak.distribution',
                                   'Download this plot')
                  ))
        ),
        br(), br(),
        DT::dataTableOutput("ua.ttest.peak.table")
      ),

      conditionalPanel(
        condition = 'input.uaTtestResultMainpan == "ua.ttest.sample.info"',
        uiOutput(outputId = "ua.ttest.sample.info.area"),
        DT::dataTableOutput("ua.ttest.sample.info")
      ),

      conditionalPanel(
        condition = 'input.uaTtestResultMainpan == "ua.ttest.data.check"',
        h5("Data from"),
        h5(textOutput(outputId = "ua.ttest.use.data")),
        hr(),
        h5("MS1 peak table"),
        h5(textOutput(outputId = "ua.ttest.ms1.peak.table.info")),
        hr(),
        h5("Sample information"),
        h5(tableOutput(outputId = "ua.ttest.sample.info.info"))
      ),

      conditionalPanel(
        condition = 'input.uaTtestResultMainpan == "ua.ttest.summary"',
        fluidPage(
          uiOutput(outputId = "ua.ttest.summary.area"),
          actionButton(inputId = "uaTtestGo2",
                       label = "Generate box plot",
                       styleclass = "info"),
          downloadButton('ua.ttest.download.data', 'Download data with P-value'),

          bsModal(id = "modalExample22",
                  title = "Box plot",
                  trigger = "uaTtestGo2",
                  size = "large",
                  sidebarLayout(
                    sidebarPanel = sidebarPanel(
                      textOutput(outputId = "ua.ttest.info")
                    ),
                    mainPanel = mainPanel(
                      plotOutput("ua.ttest.box.plot"),
                      column(width = 5,
                             sliderInput(inputId = "ua.ttest.box.plot.width",
                                         label = "Width (inch)", min = 6,
                                         max = 20, value = 8)),
                      column(width = 5,
                             sliderInput(inputId = "ua.ttest.box.plot.height",
                                         label = "Height (inch)", min = 3,
                                         max = 20, value = 6)),
                      downloadButton('ua.ttest.download.box.plot',
                                     'Download this plot')
                    ))
          ),

          br(), br(),
          DT::dataTableOutput(outputId = "ua.ttest.p")

        )
      )
    )
  )
}