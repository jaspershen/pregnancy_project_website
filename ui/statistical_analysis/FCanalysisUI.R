###Fold change UI
ui.ua.fc <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "uaFCmainpan",
                  type = "pills",
                  tabPanel("Upload data",
                           value = "ua.fc.upload.data",
                           icon = icon(name = "upload")
                           ),
                  tabPanel("Parameter setting",
                           value = "ua.fc.parameter.setting",
                           icon = icon(name = "cogs")
                           )
      ),
      conditionalPanel(
        condition = 'input.uaFCmainpan == "ua.fc.upload.data"',

        textInput(inputId = "ua.fc.project.name",
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

        selectInput(inputId = "uaFCDataFrom",
                    label = h5(
                      "Data from",
                      shinyBS::tipify(
                        el = icon(name = "info-circle"),
                        placement = "bottom",
                        trigger = "hover",
                        title = "You can use demo data or your local data"
                      )
                    ),
                    choices = c("Demo data" = "ua.fc.demo.data",
                                "Local data" = "ua.fc.local.data",
                                "Previous step" = "ua.fc.previous.step"),
                    selected = "ua.fc.demo.data", multiple = FALSE),

        conditionalPanel(
          condition = 'input.uaFCDataFrom == "ua.fc.demo.data"',
          actionButton(inputId = 'ua.fc.use.demo.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load demo data"),
          helpText(a("Click Here to Download Demo data",
                     href="http://oxr5qv74w.bkt.clouddn.com/Outlier_Sample.zip",
                     targeted = "_blank"))
        ),

        conditionalPanel(
          condition = 'input.uaFCDataFrom == "ua.fc.local.data"',
          fileInput(inputId = "ua.fc.ms1.peak.table",
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

          fileInput(inputId = "ua.fc.sample.info",
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
          actionButton(inputId = 'ua.fc.use.local.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load your data")
        ),

        conditionalPanel(
          condition = 'input.uaFCDataFrom == "ua.fc.previous.step"',
          selectInput(inputId = "ua.fc.from.which.step",
                      label = h5("Use data from which step?",
                                 shinyBS::tipify(
                                   el = icon(name = "info-circle"),
                                   placement = "bottom",
                                   trigger = "hover",
                                   title = "Use data from which step?"),
                                 style = "color:black"),
                      choices = c(
                        "Missing value imputation" = "ua.fc.mv.step",
                        "Data normalization" = "ua.fc.dn.step",
                        "Outlier sample processing" = "ua.fc.os.step"
                      ), selected = "ua.fc.os.step"),

          h5("Previous step data information",
             shinyBS::tipify(
               el = icon(name = "info-circle"),
               placement = "bottom",
               trigger = "hover",
               title = "You can use data from previous steps"
             ),
             style = "color:black"),

          textOutput(outputId = "uaFCprojectName"),
          br(),
          useShinyalert(),
          actionButton(inputId = 'ua.fc.use.previous.step.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),

          helpText("Click", strong("Load data"),
                   "to load previous step data")
        )
      ),
      conditionalPanel(
        condition = 'input.uaFCmainpan == "ua.fc.parameter.setting"',

        selectInput(inputId = "ua.fc.log",
                    label = "Logarithm method",
                    choices = c(
                      "No log" = "ua.fc.no.log",
                      "Log 2" = "ua.fc.log2",
                      "Log e" = "ua.fc.log2",
                      "Log 10" = "ua.fc.log10"
                    ), selected = "ua.fc.no.log"),

        selectInput(inputId = "ua.fc.scale",
                    label = "Scale method",
                    choices = c(
                      "Pareto scale" = "ua.fc.pareto",
                      "Auto scale" = "ua.fc.auto",
                      "No scale" = "ua.fc.no.scale"
                    ), selected = "ua.fc.no.scale"),

        checkboxInput(inputId = "ua.fc.center",
                      label = "Center or not",
                      value = FALSE),

        uiOutput("ua.fc.group.area"),

        selectInput(inputId = "ua.fc.use.what",
                    label = "Use what to calcuate FC",
                    choices = c(
                      "Median" = "ua.fc.median",
                      "Mean" = "ua.fc.mean"
                    ), selected = "ua.fc.median"),
        actionButton(inputId = 'ua.fc.submit',
                     "Submit",
                     styleclass = "info",
                     icon = icon('play-circle')),
        helpText("Click", strong("Submit"), "to calcaule fold change")
      )
    )
  )
}





###Fold change result UI
ui.ua.fc.result <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "uaFCresultMainpan",
                  type = "tabs",
                  tabPanel("MS1 peak table",
                           value = "ua.fc.ms1.peak.table",
                           icon = icon("table")
                           ),
                  tabPanel("Sample information",
                           value = "ua.fc.sample.info",
                           icon = icon("info-circle")
                           ),
                  tabPanel("Data check",
                           value = "ua.fc.data.check",
                           icon = icon("check-circle")
                           ),
                  tabPanel("Summary",
                           value = "ua.fc.summary",
                           icon = icon("tasks")
                           )
    ),
    conditionalPanel(
      condition = 'input.uaFCresultMainpan == "ua.fc.ms1.peak.table"',
      uiOutput(outputId = "ua.fc.ms1.peak.table.area"),
      actionButton(inputId = "uaFCgo1",
                   label = "Generate intensity distribution",
                   styleclass = "info"),

      bsModal(id = "modalExample11",
              title = "Intendity distribution",
              trigger = "uaFCgo1",
              size = "large",
              sidebarLayout(
                sidebarPanel = sidebarPanel(
                  textOutput(outputId = "ua.fc.peak.info1"),
                  textOutput(outputId = "ua.fc.peak.info2")
                ),
                mainPanel = mainPanel(
                  plotOutput("ua.fc.peak.distribution"),
                  column(width = 5,
                         sliderInput(inputId = "ua.fc.peak.distribution.width",
                                     label = "Width (inch)", min = 6,
                                     max = 20, value = 8)),
                  column(width = 5,
                         sliderInput(inputId = "ua.fc.peak.distribution.height",
                                     label = "Height (inch)", min = 3,
                                     max = 20, value = 6)),
                  downloadButton('ua.fc.download.peak.distribution',
                                 'Download this plot')
                ))
      ),
      br(), br(),
      DT::dataTableOutput("ua.fc.peak.table")
    ),

    conditionalPanel(
      condition = 'input.uaFCresultMainpan == "ua.fc.sample.info"',
      uiOutput(outputId = "ua.fc.sample.info.area"),
      DT::dataTableOutput("ua.fc.sample.info")
    ),

    conditionalPanel(
      condition = 'input.uaFCresultMainpan == "ua.fc.data.check"',
      h5("Data from"),
      h5(textOutput(outputId = "ua.fc.use.data")),
      hr(),
      h5("MS1 peak table"),
      h5(textOutput(outputId = "ua.fc.ms1.peak.table.info")),
      hr(),
      h5("Sample information"),
      h5(tableOutput(outputId = "ua.fc.sample.info.info"))
    ),

    conditionalPanel(
      condition = 'input.uaFCresultMainpan == "ua.fc.summary"',
      fluidPage(
        uiOutput(outputId = "ua.fc.summary.area"),
        actionButton(inputId = "uaFCgo2",
                     label = "Generate box plot",
                     styleclass = "info"),
        downloadButton('ua.fc.download.data', 'Download data with fold change'),
        bsModal(id = "modalExample12",
                title = "Box plot",
                trigger = "uaFCgo2",
                size = "large",
                plotOutput("ua.fc.box.plot"),
                column(width = 5,
                       sliderInput(inputId = "ua.fc.box.plot.width",
                                   label = "Width (inch)", min = 6,
                                   max = 20, value = 8)),
                column(width = 5,
                       sliderInput(inputId = "ua.fc.box.plot.height",
                                   label = "Height (inch)", min = 3,
                                   max = 20, value = 6)),
                downloadButton('ua.fc.download.box.plot',
                               'Download this plot')
        ),
        br(), br(),
        DT::dataTableOutput(outputId = "ua.fc")

      )
    )
    )
  )
}
