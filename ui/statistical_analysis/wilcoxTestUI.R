###Wilcox test UI
ui.ua.wilcox <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "uaWilcoxMainpan",
                  type = "pills",
                  tabPanel("Upload data",
                           value = "ua.wilcox.upload.data",
                           icon = icon(name = "upload")
                           ),
                  tabPanel("Parameter setting",
                           value = "ua.wilcox.parameter.setting",
                           icon = icon(name = "cogs")
                           )
      ),
      conditionalPanel(
        condition = 'input.uaWilcoxMainpan == "ua.wilcox.upload.data"',

        textInput(inputId = "ua.wilcox.project.name",
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

        selectInput(inputId = "uaWilcoxDataFrom",
                    label = h5(
                      "Data from",
                      shinyBS::tipify(
                        el = icon(name = "info-circle"),
                        placement = "bottom",
                        trigger = "hover",
                        title = "You can use demo data or your local data"
                      )
                    ),
                    choices = c("Demo data" = "ua.wilcox.demo.data",
                                "Local data" = "ua.wilcox.local.data",
                                "Previous step" = "ua.wilcox.previous.step"),
                    selected = "ua.wilcox.demo.data", multiple = FALSE),

        conditionalPanel(
          condition = 'input.uaWilcoxDataFrom == "ua.wilcox.demo.data"',
          actionButton(inputId = 'ua.wilcox.use.demo.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load demo data"),
          helpText(a("Click Here to Download Demo data",
                     href="http://oxr5qv74w.bkt.clouddn.com/Outlier_Sample.zip",
                     targeted = "_blank"))
        ),

        conditionalPanel(
          condition = 'input.uaWilcoxDataFrom == "ua.wilcox.local.data"',
          fileInput(inputId = "ua.wilcox.ms1.peak.table",
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

          fileInput(inputId = "ua.wilcox.sample.info",
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
          actionButton(inputId = 'ua.wilcox.use.local.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load your data")
        ),

        conditionalPanel(
          condition = 'input.uaWilcoxDataFrom == "ua.wilcox.previous.step"',
          selectInput(inputId = "ua.wilcox.from.which.step",
                      label = h5("Use data from which step?",
                                 shinyBS::tipify(
                                   el = icon(name = "info-circle"),
                                   placement = "bottom",
                                   trigger = "hover",
                                   title = "Use data from which step?"),
                                 style = "color:black"),
                      choices = c(
                        "Missing value imputation" = "ua.wilcox.mv.step",
                        "Data normalization" = "ua.wilcox.dn.step",
                        "Outlier sample processing" = "ua.wilcox.os.step"
                      ), selected = "ua.wilcox.os.step"),

          h5("Previous step data information",
             shinyBS::tipify(
               el = icon(name = "info-circle"),
               placement = "bottom",
               trigger = "hover",
               title = "You can use data from previous steps"
             ),
             style = "color:black"),

          textOutput(outputId = "uaWilcoxProjectName"),
          br(),
          useShinyalert(),
          actionButton(inputId = 'ua.wilcox.use.previous.step.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),

          helpText("Click", strong("Load data"),
                   "to load previous step data")
        )
      ),
      conditionalPanel(
        condition = 'input.uaWilcoxMainpan == "ua.wilcox.parameter.setting"',

        selectInput(inputId = "ua.wilcox.log",
                    label = "Logarithm method",
                    choices = c(
                      "No log" = "ua.wilcox.no.log",
                      "Log 2" = "ua.wilcox.log2",
                      "Log e" = "ua.wilcox.loge",
                      "Log 10" = "ua.wilcox.log10"
                    ), selected = "ua.wilcox.no.log"),

        selectInput(inputId = "ua.wilcox.scale",
                    label = "Scale method",
                    choices = c(
                      "Pareto scale" = "ua.wilcox.pareto",
                      "Auto scale" = "ua.wilcox.auto",
                      "No scale" = "ua.wilcox.no.scale"
                    ), selected = "ua.wilcox.pareto"),

        checkboxInput(inputId = "ua.wilcox.center",
                      label = "Center or not",
                      value = TRUE),

        uiOutput("ua.wilcox.group.area"),

        selectInput(inputId = "ua.wilcox.alternative",
                    label = "Alternative",
                    choices = c(
                      "Two sided" = "uaWilcox.two.sided",
                      "Less" = "uaWilcox.less",
                      "Greater" = "uaWilcox.greater"
                    ), selected = "uaWilcox.two.sided"
        ),

        checkboxInput(inputId = "ua.wilcox.paired",
                      label = "Paired t-test", value = FALSE),

        selectInput(inputId = "ua.wilcox.adjust",
                    label = "correction method",
                    choices = c(
                      "False discovery ratio (FDR)" = "uaWilcoxFDR",
                      "Holm" = "uaWilcoxHolm",
                      "Hochberg" = "uaWilcoxhochberg",
                      "Hommel" = "uaWilcoxHommel",
                      "Bonferroni" = "uaWilcoxBonferroni",
                      "BH" = "uaWilcoxBH",
                      "BY" = "uaWilcoxBY",
                      "No correction" = "uaWilcoxNoCorrection"
                    ), selected = "uaWilcoxFDR"),

        actionButton(inputId = 'ua.wilcox.submit',
                     "Submit",
                     styleclass = "info",
                     icon = icon('play-circle')),
        helpText("Click", strong("Submit"), "to calcaule fold change")
      )
    )
  )
}





###Wilcox test result UI
ui.ua.wilcox.result <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "uaWilcoxResultMainpan", type = "tabs",
                  tabPanel("MS1 peak table",
                           value = "ua.wilcox.ms1.peak.table",
                           icon = icon("table")
                           ),
                  tabPanel("Sample information",
                           value = "ua.wilcox.sample.info",
                           icon = icon("info-circle")
                           ),
                  tabPanel("Data check",
                           value = "ua.wilcox.data.check",
                           icon = icon("check-circle")
                           ),
                  tabPanel("Summary",
                           value = "ua.wilcox.summary",
                           icon = icon("tasks")
                           )
      ),
      conditionalPanel(
        condition = 'input.uaWilcoxResultMainpan == "ua.wilcox.ms1.peak.table"',

        uiOutput(outputId = "ua.wilcox.ms1.peak.table.area"),

        actionButton(inputId = "uaWilcoxGo1",
                     label = "Generate intensity distribution",
                     styleclass = "info"),

        bsModal(id = "modalExample21",
                title = "Intendity distribution",
                trigger = "uaWilcoxGo1",
                size = "large",
                sidebarLayout(
                  sidebarPanel = sidebarPanel(
                    textOutput(outputId = "ua.wilcox.peak.info1"),
                    textOutput(outputId = "ua.wilcox.peak.info2")
                  ),
                  mainPanel = mainPanel(
                    plotOutput("ua.wilcox.peak.distribution"),
                    column(width = 5,
                           sliderInput(inputId = "ua.wilcox.peak.distribution.width",
                                       label = "Width (inch)", min = 6,
                                       max = 20, value = 8)),
                    column(width = 5,
                           sliderInput(inputId = "ua.wilcox.peak.distribution.height",
                                       label = "Height (inch)", min = 3,
                                       max = 20, value = 6)),
                    downloadButton('ua.wilcox.download.peak.distribution',
                                   'Download this plot')
                  ))
        ),
        br(), br(),
        DT::dataTableOutput("ua.wilcox.peak.table")
      ),

      conditionalPanel(
        condition = 'input.uaWilcoxResultMainpan == "ua.wilcox.sample.info"',

        uiOutput(outputId = "ua.wilcox.sample.info.area"),
        DT::dataTableOutput("ua.wilcox.sample.info")
      ),

      conditionalPanel(
        condition = 'input.uaWilcoxResultMainpan == "ua.wilcox.data.check"',
        h5("Data from"),
        h5(textOutput(outputId = "ua.wilcox.use.data")),
        hr(),
        h5("MS1 peak table"),
        h5(textOutput(outputId = "ua.wilcox.ms1.peak.table.info")),
        hr(),
        h5("Sample information"),
        h5(tableOutput(outputId = "ua.wilcox.sample.info.info"))
      ),

      conditionalPanel(
        condition = 'input.uaWilcoxResultMainpan == "ua.wilcox.summary"',
        fluidPage(
          uiOutput(outputId = "ua.wilcox.summary.area"),
          actionButton(inputId = "uaWilcoxGo2",
                       label = "Generate box plot",
                       styleclass = "info"),
          downloadButton('ua.wilcox.download.data', 'Download data with P-value'),

          bsModal(id = "modalExample22",
                  title = "Box plot",
                  trigger = "uaWilcoxGo2",
                  size = "large",
                  sidebarLayout(
                    sidebarPanel = sidebarPanel(
                      textOutput(outputId = "ua.wilcox.info")
                    ),
                    mainPanel = mainPanel(
                      plotOutput("ua.wilcox.box.plot"),
                      column(width = 5,
                             sliderInput(inputId = "ua.wilcox.box.plot.width",
                                         label = "Width (inch)", min = 6,
                                         max = 20, value = 8)),
                      column(width = 5,
                             sliderInput(inputId = "ua.wilcox.box.plot.height",
                                         label = "Height (inch)", min = 3,
                                         max = 20, value = 6)),
                      downloadButton('ua.wilcox.download.box.plot',
                                     'Download this plot')
                    ))
          ),

          br(), br(),
          DT::dataTableOutput(outputId = "ua.wilcox.p")

        )
      )
    )
  )
}