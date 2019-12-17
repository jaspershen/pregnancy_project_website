
###ANOVA test UI
ui.ua.anova <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "uaANOVAMainpan",
                  type = "pills",
                  tabPanel("Upload data",
                           value = "ua.anova.upload.data",
                           icon = icon(name = "upload")
                           ),
                  tabPanel("Parameter setting",
                           value = "ua.anova.parameter.setting",
                           icon = icon(name = "cogs")
                           )
      ),
      conditionalPanel(
        condition = 'input.uaANOVAMainpan == "ua.anova.upload.data"',

        textInput(inputId = "ua.anova.project.name",
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

        selectInput(inputId = "uaANOVADataFrom",
                    label = h5(
                      "Data from",
                      shinyBS::tipify(
                        el = icon(name = "info-circle"),
                        placement = "bottom",
                        trigger = "hover",
                        title = "You can use demo data or your local data"
                      )
                    ),
                    choices = c("Demo data" = "ua.anova.demo.data",
                                "Local data" = "ua.anova.local.data",
                                "Previous step" = "ua.anova.previous.step"),
                    selected = "ua.anova.demo.data", multiple = FALSE),

        conditionalPanel(
          condition = 'input.uaANOVADataFrom == "ua.anova.demo.data"',
          actionButton(inputId = 'ua.anova.use.demo.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load demo data"),
          helpText(a("Click Here to Download Demo data",
                     href="http://oxr5qv74w.bkt.clouddn.com/Outlier_Sample.zip",
                     targeted = "_blank"))
        ),

        conditionalPanel(
          condition = 'input.uaANOVADataFrom == "ua.anova.local.data"',
          fileInput(inputId = "ua.anova.ms1.peak.table",
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

          fileInput(inputId = "ua.anova.sample.info",
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
          actionButton(inputId = 'ua.anova.use.local.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load your data")
        ),

        conditionalPanel(
          condition = 'input.uaANOVADataFrom == "ua.anova.previous.step"',
          selectInput(inputId = "ua.anova.from.which.step",
                      label = h5("Use data from which step?",
                                 shinyBS::tipify(
                                   el = icon(name = "info-circle"),
                                   placement = "bottom",
                                   trigger = "hover",
                                   title = "Use data from which step?"),
                                 style = "color:black"),
                      choices = c(
                        "Missing value imputation" = "ua.anova.mv.step",
                        "Data normalization" = "ua.anova.dn.step",
                        "Outlier sample processing" = "ua.anova.os.step"
                      ), selected = "ua.anova.os.step"),

          h5("Previous step data information",
             shinyBS::tipify(
               el = icon(name = "info-circle"),
               placement = "bottom",
               trigger = "hover",
               title = "You can use data from previous steps"
             ),
             style = "color:black"),

          textOutput(outputId = "uaANOVAProjectName"),
          br(),
          useShinyalert(),
          actionButton(inputId = 'ua.anova.use.previous.step.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),

          helpText("Click", strong("Load data"),
                   "to load previous step data")
        )
      ),
      conditionalPanel(
        condition = 'input.uaANOVAMainpan == "ua.anova.parameter.setting"',

        selectInput(inputId = "ua.anova.log",
                    label = "Logarithm method",
                    choices = c(
                      "No log" = "ua.anova.no.log",
                      "Log 2" = "ua.anova.log2",
                      "Log e" = "ua.anova.loge",
                      "Log 10" = "ua.anova.log10"
                    ), selected = "ua.anova.no.log"),

        selectInput(inputId = "ua.anova.scale",
                    label = "Scale method",
                    choices = c(
                      "Pareto scale" = "ua.anova.pareto",
                      "Auto scale" = "ua.anova.auto",
                      "No scale" = "ua.anova.no.scale"
                    ), selected = "ua.anova.pareto"),

        checkboxInput(inputId = "ua.anova.center",
                      label = "Center or not",
                      value = TRUE),

        uiOutput("ua.anova.group.area"),

        selectInput(inputId = "ua.anova.adjust",
                    label = "correction method",
                    choices = c(
                      "False discovery ratio (FDR)" = "uaANOVAFDR",
                      "Holm" = "uaANOVAHolm",
                      "Hochberg" = "uaANOVAhochberg",
                      "Hommel" = "uaANOVAHommel",
                      "Bonferroni" = "uaANOVABonferroni",
                      "BH" = "uaANOVABH",
                      "BY" = "uaANOVABY",
                      "No correction" = "uaANOVANoCorrection"
                    ), selected = "uaANOVAFDR"),

        actionButton(inputId = 'ua.anova.submit',
                     "Submit",
                     styleclass = "info",
                     icon = icon('play-circle')),
        helpText("Click", strong("Submit"), "to calcaule fold change")
      )
    )
  )
}





###ANOVA test result UI
ui.ua.anova.result <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "uaANOVAResultMainpan",
                  type = "tabs",
                  tabPanel("MS1 peak table",
                           value = "ua.anova.ms1.peak.table",
                           icon = icon("table")
                           ),
                  tabPanel("Sample information",
                           value = "ua.anova.sample.info"),
                  tabPanel("Data check",
                           value = "ua.anova.data.check",
                           icon = icon("info-circle")
                           ),
                  tabPanel("Summary",
                           value = "ua.anova.summary",
                           icon = icon("tasks")
                           )
      ),
      conditionalPanel(
        condition = 'input.uaANOVAResultMainpan == "ua.anova.ms1.peak.table"',

        uiOutput(outputId = "ua.anova.ms1.peak.table.area"),

        actionButton(inputId = "uaANOVAGo1",
                     label = "Generate intensity distribution",
                     styleclass = "info"),

        bsModal(id = "modalExample31",
                title = "Intendity distribution",
                trigger = "uaANOVAGo1",
                size = "large",
                sidebarLayout(
                  sidebarPanel = sidebarPanel(
                    textOutput(outputId = "ua.anova.peak.info1"),
                    textOutput(outputId = "ua.anova.peak.info2")
                  ),
                  mainPanel = mainPanel(
                    plotOutput("ua.anova.peak.distribution"),
                    column(width = 5,
                           sliderInput(inputId = "ua.anova.peak.distribution.width",
                                       label = "Width (inch)", min = 6,
                                       max = 20, value = 8)),
                    column(width = 5,
                           sliderInput(inputId = "ua.anova.peak.distribution.height",
                                       label = "Height (inch)", min = 3,
                                       max = 20, value = 6)),
                    downloadButton('ua.anova.download.peak.distribution',
                                   'Download this plot')
                  ))
        ),
        br(), br(),
        DT::dataTableOutput("ua.anova.peak.table")
      ),

      conditionalPanel(
        condition = 'input.uaANOVAResultMainpan == "ua.anova.sample.info"',
        uiOutput(outputId = "ua.anova.sample.info.area"),
        DT::dataTableOutput("ua.anova.sample.info")
      ),

      conditionalPanel(
        condition = 'input.uaANOVAResultMainpan == "ua.anova.data.check"',
        h5("Data from"),
        h5(textOutput(outputId = "ua.anova.use.data")),
        hr(),
        h5("MS1 peak table"),
        h5(textOutput(outputId = "ua.anova.ms1.peak.table.info")),
        hr(),
        h5("Sample information"),
        h5(tableOutput(outputId = "ua.anova.sample.info.info"))
      ),

      conditionalPanel(
        condition = 'input.uaANOVAResultMainpan == "ua.anova.summary"',
        fluidPage(

          uiOutput(outputId = "ua.anova.summary.area"),
          actionButton(inputId = "uaANOVAGo2",
                       label = "Generate box plot",
                       styleclass = "info"),
          downloadButton('ua.anova.download.data', 'Download data with P-value'),

          bsModal(id = "modalExample32",
                  title = "Box plot",
                  trigger = "uaANOVAGo2",
                  size = "large",
                  sidebarLayout(
                    sidebarPanel = sidebarPanel(
                      textOutput(outputId = "ua.anova.info")
                    ),
                    mainPanel = mainPanel(
                      plotOutput("ua.anova.box.plot"),
                      column(width = 5,
                             sliderInput(inputId = "ua.anova.box.plot.width",
                                         label = "Width (inch)", min = 6,
                                         max = 20, value = 8)),
                      column(width = 5,
                             sliderInput(inputId = "ua.anova.box.plot.height",
                                         label = "Height (inch)", min = 3,
                                         max = 20, value = 6)),
                      downloadButton('ua.anova.download.box.plot',
                                     'Download this plot')
                    ))
          ),

          br(), br(),
          DT::dataTableOutput(outputId = "ua.anova.p")

        )
      )
    )
  )
}



