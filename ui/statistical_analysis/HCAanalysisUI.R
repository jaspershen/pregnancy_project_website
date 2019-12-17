##HCA analysis UI
ui.ma.hca <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "maHCAMainpan",
                  type = "pills",
                  tabPanel("Upload data",
                           value = "ma.hca.upload.data",
                           icon = icon(name = "upload")
                           ),
                  tabPanel("Parameter setting",
                           value = "ma.hca.parameter.setting",
                           icon = icon(name = "cogs")
                           )
      ),
      conditionalPanel(
        condition = 'input.maHCAMainpan == "ma.hca.upload.data"',

        textInput(inputId = "ma.hca.project.name",
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

        selectInput(inputId = "maHCADataFrom",
                    label = h5(
                      "Data from",
                      shinyBS::tipify(
                        el = icon(name = "info-circle"),
                        placement = "bottom",
                        trigger = "hover",
                        title = "Please selct"
                      )
                    ),
                    choices = c("Demo data" = "ma.hca.demo.data",
                                "Local data" = "ma.hca.local.data",
                                "Previous step" = "ma.hca.previous.step"),
                    selected = "ma.hca.demo.data", multiple = FALSE),

        conditionalPanel(
          condition = 'input.maHCADataFrom == "ma.hca.demo.data"',
          actionButton(inputId = 'ma.hca.use.demo.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load demo data"),
          helpText(a("Click Here to Download Demo data",
                     href="http://oxr5qv74w.bkt.clouddn.com/Outlier_Sample.zip",
                     targeted = "_blank"))
        ),

        conditionalPanel(
          condition = 'input.maHCADataFrom == "ma.hca.local.data"',
          fileInput(inputId = "ma.hca.ms1.peak.table",
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

          fileInput(inputId = "ma.hca.sample.info",
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
          actionButton(inputId = 'ma.hca.use.local.data.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),
          helpText("Click", strong("Load data"), "to load your data")
        ),

        conditionalPanel(
          condition = 'input.maHCADataFrom == "ma.hca.previous.step"',
          selectInput(inputId = "ma.hca.from.which.step",
                      label = h5("Use data from which step?",
                                 shinyBS::tipify(
                                   el = icon(name = "info-circle"),
                                   placement = "bottom",
                                   trigger = "hover",
                                   title = "Use data from which step?"),
                                 style = "color:black"),
                      choices = c(
                        "Missing value imputation" = "ma.hca.mv.step",
                        "Data normalization" = "ma.hca.dn.step",
                        "Outlier sample processing" = "ma.hca.os.step",
                        "Univariate analysis" = "ma.hca.ua.step"
                      ), selected = "ma.hca.ua.step"),

          h5("Previous step data information",
             shinyBS::tipify(
               el = icon(name = "info-circle"),
               placement = "bottom",
               trigger = "hover",
               title = "You can use data from previous steps"
             ),
             style = "color:black"),

          textOutput(outputId = "maHCAprojectName"),
          br(),
          useShinyalert(),
          actionButton(inputId = 'ma.hca.use.previous.step.AB',
                       "Load data",
                       styleclass = "info",
                       icon = icon('play-circle')),

          helpText("Click", strong("Load data"),
                   "to load previous step data")
        )
      ),
      conditionalPanel(
        condition = 'input.maHCAMainpan == "ma.hca.parameter.setting"',

        selectInput(inputId = "ma.hca.log",
                    label = "Logarithm method",
                    choices = c(
                      "No log" = "ma.hca.no.log",
                      "Log 2" = "ma.hca.log2",
                      "Log e" = "ma.hca.loge",
                      "Log 10" = "ma.hca.log10"
                    ), selected = "ma.hca.no.log"),

        selectInput(inputId = "ma.hca.scale",
                    label = "Scale method",
                    choices = c(
                      "Pareto scale" = "ma.hca.pareto",
                      "Auto scale" = "ma.hca.auto",
                      "No scale" = "ma.hca.no.scale"
                    ), selected = "ma.hca.pareto"),

        checkboxInput(inputId = "ma.hca.center",
                      label = "Center or not",
                      value = TRUE),

        uiOutput("ma.hca.group.area"),

        selectInput(inputId = "ma.hca.clustering.distance.rows",
                    label = "Distance measure used in clustering rows",
                    choices = c(
                      "Euclidean" = "maHCARowEuclidean",
                      "Maximum" = "maHCARowMaximum",
                      "Manhattan" = "maHCARowManhattan",
                      "Canberra" = "maHCARowCanberra",
                      "Binary" = "maHCARowBinary",
                      "Minkowski" = "maHCARowMinkowski"
                    ), selected = "maHCARowEuclidean"),

        selectInput(inputId = "ma.hca.clustering.distance.cols",
                    label = "Distance measure used in clustering columns",
                    choices = c(
                      "Euclidean" = "maHCAColEuclidean",
                      "Maximum" = "maHCAColMaximum",
                      "Manhattan" = "maHCAColManhattan",
                      "Canberra" = "maHCAColCanberra",
                      "Binary" = "maHCAColBinary",
                      "Minkowski" = "maHCAColMinkowski"
                    ), selected = "maHCAColEuclidean"),

        selectInput(inputId = "ma.hca.clustering.method",
                    label = "Clustering method",
                    choices = c(
                      "Ward.D" = "maHCAward.D",
                      "Ward.D2" = "maHCAward.D2",
                      "Single" = "maHCAsingle",
                      "Complete" = "maHCAcomplete",
                      "Average" = "maHCAaverage",
                      "Mcquitty" = "maHCAmcquitty",
                      "Median" = "maHCAmedian",
                      "Centroid" = "maHCAcentroid"
                    ),
                    selected = "maHCAward.D"),

        column(width = 6,
               checkboxInput(inputId = "ma.hca.cluster.rows",
                             label = "Cluster rows",
                             value = TRUE)),
        column(width = 6,
               checkboxInput(inputId = "ma.hca.cluster.cols",
                             label = "Cluster columns",
                             value = TRUE)
        ),

        useShinyalert(),
        actionButton(inputId = 'ma.hca.submit',
                     "Submit",
                     styleclass = "info",
                     icon = icon('play-circle')),
        helpText("Click", strong("Submit"), "to do HCA analysis")
      )
    )
  )
}





###HCA analysis result UI
ui.ma.hca.result <- function(){
  tagList(
    fluidPage(
      tabsetPanel(id = "maHCAResultMainpan",
                  type = "tabs",
                  tabPanel("MS1 peak table",
                           value = "ma.hca.ms1.peak.table",
                           icon = icon("table")
                           ),
                  tabPanel("Sample information",
                           value = "ma.hca.sample.info",
                           icon = icon("info-circle")
                           ),
                  tabPanel("Data check",
                           value = "ma.hca.data.check",
                           icon = icon("check-circle")
                           ),
                  tabPanel("Summary",
                           value = "ma.hca.summary",
                           icon = icon("tasks")
                           )
      ),

      conditionalPanel(
        condition = 'input.maHCAResultMainpan == "ma.hca.ms1.peak.table"',
        uiOutput(outputId = "ma.hca.ms1.peak.table.area"),
        DT::dataTableOutput("ma.hca.peak.table")
      ),

      conditionalPanel(
        condition = 'input.maHCAResultMainpan == "ma.hca.sample.info"',
        uiOutput(outputId = "ma.hca.sample.info.area"),
        DT::dataTableOutput("ma.hca.sample.info")
      ),

      conditionalPanel(
        condition = 'input.maHCAResultMainpan == "ma.hca.data.check"',
        h5("Data from"),
        h5(textOutput(outputId = "ma.hca.use.data")),
        hr(),
        h5("MS1 peak table"),
        h5(textOutput(outputId = "ma.hca.ms1.peak.table.info")),
        hr(),
        h5("Sample information"),
        h5(tableOutput(outputId = "ma.hca.sample.info.info"))
      ),

      conditionalPanel(
        condition = "input.maHCAResultMainpan == 'ma.hca.summary'",
        uiOutput(outputId = "ma.hca.summary.area"),
        fluidPage(
          column(width = 1,
                 dropdownButton(
                   column(width = 6,
                          checkboxInput(inputId = "ma.hca.show.row.names",
                                        label = "Show row names",
                                        value = TRUE)),
                   column(width = 6,
                          checkboxInput(inputId = "ma.hca.show.col.names",
                                        label = "Show column names",
                                        value = TRUE)),

                   h5("Font size"),
                   column(width = 4,
                          numericInput(inputId = "ma.hca.font.size",
                                       label = "Legend", value = 10,
                                       min = 1, max = 50, step = 0.5)
                   ),

                   column(width = 4,
                          numericInput(inputId = "ma.hca.row.font.size",
                                       label = "Row", value = 10,
                                       min = 1, max = 50, step = 0.5)),

                   column(width = 4,
                          numericInput(inputId = "ma.hca.col.font.size",
                                       label = "Column", value = 10,
                                       min = 1, max = 50, step = 0.5)
                   ),

                   column(width = 6,
                          checkboxInput(inputId = "ma.hca.display.number",
                                        label = "Display numbers",
                                        value = FALSE)),

                   column(width = 6,
                          checkboxInput(inputId = "ma.hca.legend",
                                        label = "Show legend",
                                        value = TRUE)),

                   circle = TRUE, status = "info", icon = icon("cogs"),
                   width = "500px",
                   tooltip = tooltipOptions(title = "Image setting")
                   # inputId = "ma.hca.setting.DB"
                 ),

                 dropdownButton(
                   uiOutput(outputId = "ma.hca.cor.area"),
                   checkboxInput(inputId = "maHCABorderCol1",
                                 label = "Show border color",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.maHCABorderCol1",
                     colourInput(inputId = "maHCABorderCol2",
                                 label = "Border color",
                                 value = "white",
                                 returnName = TRUE,
                                 palette = "limited",
                                 # showColour = TRUE,
                                 allowTransparent = TRUE)
                   ),

                   ##six group color
                   h5("Color for groups"),
                   column(width = 4,
                          colourInput(inputId = "ma.hca.group1.col",
                                      label = "Group 1",
                                      value = "dodgerblue",
                                      returnName = TRUE,
                                      palette = "square",
                                      # showColour = TRUE,
                                      allowTransparent = TRUE)
                   ),

                   column(width = 4,
                          colourInput(inputId = "ma.hca.group2.col",
                                      label = "Group 2",
                                      value = "firebrick1",
                                      returnName = TRUE,
                                      palette = "square",
                                      # showColour = TRUE,
                                      allowTransparent = TRUE)
                   ),

                   column(width = 4,
                          colourInput(inputId = "ma.hca.group3.col",
                                      label = "Group 3",
                                      value = "orange",
                                      returnName = TRUE,
                                      palette = "square",
                                      # showColour = TRUE,
                                      allowTransparent = TRUE)
                   ),

                   column(width = 4,
                          colourInput(inputId = "ma.hca.group4.col",
                                      label = "Group 4",
                                      value = "springgreen2",
                                      returnName = TRUE,
                                      palette = "square",
                                      # showColour = TRUE,
                                      allowTransparent = TRUE)
                   ),

                   column(width = 4,
                          colourInput(inputId = "ma.hca.group5.col",
                                      label = "Group 5",
                                      value = "orchid",
                                      returnName = TRUE,
                                      palette = "square",
                                      # showColour = TRUE,
                                      allowTransparent = TRUE)
                   ),

                   column(width = 4,
                          colourInput(inputId = "ma.hca.group6.col",
                                      label = "Group 6",
                                      value = "blue",
                                      returnName = TRUE,
                                      palette = "square",
                                      # showColour = TRUE,
                                      allowTransparent = TRUE)
                   ),

                   ##color for high and low
                   h5("Color for heatmap"),
                   column(width = 4,
                          colourInput(inputId = "ma.hca.low.col",
                                      label = "Low",
                                      value = "navy",
                                      returnName = TRUE,
                                      palette = "square",
                                      # showColour = TRUE,
                                      allowTransparent = TRUE)
                   ),

                   column(width = 4,
                          colourInput(inputId = "ma.hca.middle.col",
                                      label = "Middle",
                                      value = "white",
                                      returnName = TRUE,
                                      palette = "square",
                                      # showColour = TRUE,
                                      allowTransparent = TRUE)
                   ),

                   column(width = 4,
                          colourInput(inputId = "ma.hca.high.col",
                                      label = "High",
                                      value = "firebrick",
                                      returnName = TRUE,
                                      palette = "square",
                                      # showColour = TRUE,
                                      allowTransparent = TRUE)
                   ),


                   circle = TRUE, status = "info", icon = icon("cog"),
                   width = "500px",
                   tooltip = tooltipOptions(title = "Change color"),
                   # inputId = "ma.hca.color.DB",
                   right = FALSE, up = FALSE
                 ),

                 dropdownButton(
                   sliderInput(inputId = "ma.hca.image.width",
                               label = "Width (inch)", min = 6,
                               max = 20, value = 7),

                   sliderInput(inputId = "ma.hca.image.height",
                               label = "Width (inch)", min = 6,
                               max = 20, value = 7),

                   downloadButton('ma.download.heatmap', 'Download heatmap'),

                   circle = TRUE, status = "info", icon = icon("download"),
                   width = "500px",
                   tooltip = tooltipOptions(title = "Download image"),
                   # inputId = "ma.hca.download.DB",
                   right = FALSE, up = FALSE
                 ),
                 br(), br(),
                 dropdownButton(textOutput(outputId = "ma.hca.col.name"),
                                circle = TRUE, status = "warning", icon = icon("info"),
                                width = "500px",
                                tooltip = tooltipOptions(title = "Sample order"),
                                # inputId = "ma.hca.sample.order.DB",
                                right = FALSE, up = FALSE),

                 dropdownButton(textOutput(outputId = "ma.hca.row.name"),
                                circle = TRUE, status = "warning", icon = icon("info"),
                                width = "500px",
                                tooltip = tooltipOptions(title = "Variate order"),
                                # inputId = "ma.hca.variate.order.DB",
                                right = FALSE, up = FALSE)
          ),

          column(width = 10,
                 plotOutput(outputId = "ma.hca.heatmap",
                            height = "600px", width = "800px")
          )

        )



      )
    )
  )
}
