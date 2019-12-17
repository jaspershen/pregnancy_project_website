ui.metaboliteidentification <- function(){
  tagList(
           sidebarLayout(
             sidebarPanel = sidebarPanel(
             uiOutput(outputId = "met.iden.area")
           ),
           mainPanel = mainPanel(
             uiOutput(outputId = "met.iden.result.area")
           )
           )
)
}



ui.met.iden <- function() {
  tagList(fluidPage(
    tabsetPanel(
      id = "miMainpan",
      type = "pills",
      tabPanel(
        "Upload data",
        value = "mi.upload.data"
        # icon = icon(name = "upload")
      ),
      tabPanel(
        "Parameter setting",
        value = "mi.parameter.setting"
        # icon = icon(name = "cogs")
      )
    ),

    conditionalPanel(
      condition = 'input.miMainpan == "mi.upload.data"',
      textInput(
        inputId = "mi.project.name",
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
        placeholder = "Your project name"
      ),
      selectInput(
        inputId = "miDataFrom",
        label = h5(
          "Data from",
          shinyBS::tipify(
            el = icon(name = "info-circle"),
            placement = "bottom",
            trigger = "hover",
            title = "You can use demo data or your local data"
          )
        ),
        choices = c(
          "Demo data" = "mi.demo.data",
          "Local data" = "mi.local.data"
          # "Previous step" = "miPreviousStep"
        ),
        selected = "mi.demo.data",
        multiple = FALSE
      ),

      conditionalPanel(
        condition = 'input.miDataFrom == "mi.demo.data"',

        h5("Download demo data", style = "color:black"),

        actionButton(
          inputId = 'mi.use.demo.data.AB',
          label = "Load data",
          styleclass = "info",
          icon = icon('play-circle')
        ),
        helpText("Click", strong("Load data"), "to load demo data"),
        helpText(
          a(
            "Click Here to Download Demo data",
            href = "http://oxr5qv74w.bkt.cloudmi.com/Data_normalization.zip",
            target = "_blank"
          )
        )
      ),

      conditionalPanel(
        condition = 'input.miDataFrom == "mi.local.data"',
        fileInput(
          inputId = "miMS1PeakTable",
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
          placeholder = "Only csv format is supported"
        ),
        fileInput(
          inputId = "miSampleInfo",
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
          placeholder = "Only csv format is supported"
        ),

        fileInput(
          inputId = "miMS2data",
          label = h5(
            "MS/MS data",
            shinyBS::tipify(
              el = icon(name = "info-circle"),
              placement = "bottom",
              trigger = "hover",
              title = "MS/MS data, mgf or msp"
            )
          ),
          multiple = TRUE,
          accept = "msp/mgf",
          buttonLabel = "Browser",
          placeholder = "Only msp and mgf format are supported"
        ),

        useShinyalert(),
        actionButton(
          inputId = 'mi.use.local.data.AB',
          label = "Load data",
          styleclass = "info",
          icon = icon('play-circle')
        ),
        helpText("Click", strong("Load data"), "to load your data")
      ),

      conditionalPanel(
        condition = 'input.miDataFrom == "miPreviousStep"',
        h5("Previous step data information",
           shinyBS::tipify(
             el = icon(name = "info-circle"),
             placement = "bottom",
             trigger = "hover",
             title = "You can use data from previous steps"
           ),
           style = "color:black"),
        textOutput(outputId = "miProjectName"),
        br(),
        actionButton(
          inputId = 'mi.use.previous.step.AB',
          label = "Load data",
          styleclass = "info",
          icon = icon('play-circle')
        ),
        helpText("Click", strong("Load data"),
                 "to load previous step data")
      )
    ),

    conditionalPanel(
      condition = 'input.miMainpan == "mi.parameter.setting"',

      selectInput(inputId = "miPolarity",
                  label = "Ionization polarity",
                  choices = c(
                    "Positive" = "mi.positive",
                    "Negative" = "mi.negative"
                  ),
                  selected = "mi.positive"),

      conditionalPanel(condition = "input.miPolarity == 'mi.positive'",
                       selectInput(inputId = "mi.pos.adduct",
                                   label = "Adduct type",
                                   choices = c(
                                     "[M+H]+" = "mi.pos.m.plus.h",
                                     "[M+K]+" = "mi.pos.m.plus.k",
                                     "[M+Na]+" = "mi.pos.m.plus.na",
                                     "[M+NH4]+" = "mi.pos.m.plus.nh4"
                                   ), selected = c("mi.pos.m.plus.h",
                                                   "mi.pos.m.plus.k",
                                                   "mi.pos.m.plus.na",
                                                   "mi.pos.m.plus.nh4"),
                                   multiple = TRUE
                       )
                       ),

      conditionalPanel(condition = "input.miPolarity == 'mi.negative'",
                       selectInput(inputId = "mi.neg.adduct",
                                   label = "Adduct type",
                                   choices = c(
                                     "[M-H]-" = "mi.pos.m.minus.h",
                                     "[M+Cl]+" = "mi.pos.m.plus.cl",
                                     "[M+CH3COO]+" = "mi.pos.m.plus.ch3coo"
                                   ), selected = c("mi.pos.m.minus.h",
                                                   "mi.pos.m.plus.cl",
                                                   "mi.pos.m.plus.ch3coo"),
                                   multiple = TRUE
                       )
      ),

      sliderInput(inputId = "mi.ms1.match.mz.tol",
                  label = "m/z macth tolerance (ppm)",
                  min = 1, max = 200, value = 25),


      selectInput(inputId = "mi.ms1.match.library",
                  label = "MS1 library",
                  choices = c(
                    # "HMDB" = "mi.hmdb",
                    "KEGG" = "mi.kegg"
                  ), selected = c("mi.kegg"),
                  multiple = FALSE
                  ),


      hr(),
      radioButtons(inputId = "miHasMS2",
                   label = "Data with MS2 spectrum?",
                   choices = list(
                     "Yes" = "mi.has.ms2",
                     "NO" = "mi.no.ms2"
                   ),
                   selected = "mi.no.ms2"
                   ),

      conditionalPanel(condition = "input.miHasMS2 == 'mi.has.ms2'",
                       h5(
                         "MS1 peak and MS/MS spectrum match",
                         shinyBS::tipify(
                           el = icon(name = "info-circle"),
                           placement = "bottom",
                           trigger = "hover",
                           title = "MS1 peak and MS/MS spectrum match"
                         )
                       ),

                       selectInput(inputId = "mi.ms2.type", label = "MS/MS data type",
                                   choices = c(
                                     "MGF" = "mi.mgf",
                                     "MSP" = "mi.msp"
                                   ),
                                   selected = "mi.mgf", multiple = FALSE,
                       ),

                       sliderInput(inputId = "mi.ms1.ms2.match.mz.tol",
                                   label = "m/z tolerance (ppm)",
                                   min = 1, max = 200, value = 25),

                       sliderInput(inputId = "mi.ms1.ms2.match.rt.tol",
                                   label = "RT tolerance (second)",
                                   min = 1, max = 300, value = 10),

                       ##二级谱图匹配参数
                       actionButton(inputId = "miGo1",
                                    label = "Advanced",
                                    styleclass = "info"),

                       bsModal(id = "miExample11",
                               title = "Parameter setting for MS/MS match",
                               trigger = "miGo1",
                               size = "small",

                               selectInput(inputId = "mi.instrument",
                                           label = "Instrument",
                                           choices = c(
                                             "Agilent QTOF" = "mi.agilent.tof",
                                             "Sciex Triple TOF" = "mi.sciex.tof",
                                             "Other QTOF" = "mi.other.tof",
                                             "Thermo Orbitrap" = "mi.thermo.orbi"
                                           ),
                                           selected = "mi.sciex.tof"),

                               selectInput(inputId = "mi.ms2.library",
                                           label = "MS/MS library",
                                           choices = c(
                                             "In-house" = "mi.in.house",
                                             "METLIN" = "mi.metlin",
                                             "NIST" = "mi.nist"
                                           ), selected = c("mi.in.house", "mi.metlin", "mi.nist"),
                                           multiple = TRUE
                                           ),

                               selectInput(inputId = "mi.ce",
                                           label = "Collision energy",
                                           choices = c(
                                           "30" = "mi.30",
                                           "10" = "mi.10",
                                           "20" = "mi.20",
                                           "40" = "mi.40",
                                           "50" = "mi.50"
                                           ), selected = "mi.30",
                                           multiple = FALSE
                                           ),

                               selectInput(inputId = "mi.dp.method",
                                           label = "Dot product method",
                                           choices = c(
                                             "Forward" = "mi.forward",
                                             "Reverse" = "mi.reverse",
                                             "Forward or Reverse" = "mi.forward.or.reverse"
                                           ), selected = "mi.forward.or.reverse",
                                           multiple = FALSE),
                       sliderInput(inputId = "mi.dp.tol",
                                   label = "Dot product tolerance (0-1)",
                                   min = 0.3, max = 1,
                                   value = 0.8, step = 0.1)
                       )

                       ),

      br(),
      actionButton(
        inputId = 'miSubmit',
        label = "Submit",
        styleclass = "info",
        icon = icon('play-circle')
      ),
      helpText("Click", strong("Submit"), "to Identified data")
    ),
    br(),
    br(),
    br()
  ))
}




ui.miResult <- function() {
  tagList(fluidPage(
    tabsetPanel(
      id = "miResultMainpan",
      type = "tabs",
      tabPanel("MS1 peak table",
               value = "mi.ms1.peak.table",
               icon = icon("table")
      ),

      tabPanel("Sample information",
               value = "mi.sample.info",
               icon = icon("info-circle")
      ),

      tabPanel("MS/MS data",
               value = "mi.ms2.data",
               icon = icon("glyphicon glyphicon-signal", lib = "glyphicon")
      ),

      tabPanel("Data check",
               value = "mi.data.check",
               icon = icon("check-circle")
      ),

      tabPanel("Summary",
               value = "mi.summary",
               icon = icon("tasks")
      )
    ),
    conditionalPanel(
      condition = 'input.miResultMainpan == "mi.ms1.peak.table"',
      uiOutput(outputId = "mi.ms1.peak.table.area"),
      DT::dataTableOutput("mi.peak.table")
    ),

    conditionalPanel(condition = 'input.miResultMainpan == "mi.sample.info"',
                     uiOutput(outputId = "mi.sample.info.area"),
                     DT::dataTableOutput("mi.sample.info")
    ),

    conditionalPanel(condition = 'input.miResultMainpan == "mi.ms2.data"',
                     uiOutput(outputId = "mi.ms2.data.area"),
                     shinysky::busyIndicator(text = "Loading MS/MS data..."),
                     h5(
                       "MS2 peak plot",
                       shinyBS::tipify(
                         el = icon(name = "info-circle"),
                         placement = "bottom",
                         trigger = "hover",
                         title = "Brush and double-click to zoom"
                       )
                     ),
                            plotOutput("mi.ms2.plot",
                                       height = 500,
                                       # width = 500,
                                       # Equivalent to: click = clickOpts(id = "plot_click")
                                       click = "mi.ms2.plot_click",
                                       dblclick = "mi.ms2.plot_dblclick",
                                       brush = brushOpts(
                                         id = "mi.ms2.plot_brush",
                                         resetOnNew = TRUE
                                       )
                            ),
                            actionButton(inputId = "miShowGo1",
                                         label = "Show MS2 information",
                                         styleclass = "info"),
                            bsModal(id = "mimodalExample3",
                                    title = "MS/MS spectrum",
                                    trigger = "miShowGo1",
                                    size = "large",
                                    DT::dataTableOutput(outputId = "mi.ms2.info")
                            ),
                     h5(
                       "MS/MS spectrum",
                       shinyBS::tipify(
                         el = icon(name = "info-circle"),
                         placement = "bottom",
                         trigger = "hover",
                         title = "Click in bottom to see single MS/MS spectrrum"
                       )
                     ),
                            plotOutput(outputId = "mi.ms2.single.plot",
                                       height = 500
                                       # width = 500
                                       )
    ),

    conditionalPanel(
      condition = 'input.miResultMainpan == "mi.data.check"',
      h4("Data from"),
      h5(textOutput(outputId = "mi.use.data")),
      hr(),
      h4("MS1 peak table"),
      h5(textOutput(outputId = "mi.ms1.peak.table.info")),
      hr(),
      h4("Sample information"),
      h5(tableOutput(outputId = "mi.sample.info.info")),
      hr(),
      h4("MS/MS data"),
      h5(textOutput(outputId = "mi.ms2.data.info"))
    ),

    conditionalPanel(
      condition = 'input.miResultMainpan == "mi.summary"',
      uiOutput(outputId = "mi.summary.area"),

      tabsetPanel(id = "miSummaryMainpan",
                  type = "tabs",
                  tabPanel("MS1 match result",
                           value = "mi.ms1.macth.result",
                           icon = icon("table")
                  ),
                  tabPanel("MS/MS match result",
                           value = "mi.ms2.macth.result",
                           icon = icon("table")
                  )
                  ),
      conditionalPanel(
        condition = "input.miSummaryMainpan == 'mi.ms1.macth.result'",

        actionButton(inputId = "miGo1",
                     label = "Identification information",
                     styleclass = "info"),
        br(), br(),

        bsModal(id = "miExample1",
                title = "Match information",
                trigger = "miGo1",
                size = "large",
                textOutput(outputId = "mi.peak.ms1.match.info1"),
                DT::dataTableOutput(outputId = "mi.peak.ms1.match.info2")
        ),

        shinysky::busyIndicator(text = "Identify metabolite..."),
        DT::dataTableOutput("mi.ms1.match.result")
        ),

      conditionalPanel(
        condition = "input.miSummaryMainpan == 'mi.ms2.macth.result'",


        actionButton(inputId = "miGo2",
                     label = "Identification information",
                     styleclass = "info"),
        br(), br(),

        bsModal(id = "miExample2",
                title = "Match information",
                trigger = "miGo2",
                size = "large",
                textOutput(outputId = "mi.peak.ms2.match.info1"),
                actionButton(inputId = "miGo3",
                             label = "Show MS/MS match spetra",
                             styleclass = "info"),
                br(), br(),
                DT::dataTableOutput(outputId = "mi.peak.ms2.match.info2")
        ),

        bsModal(id = "miExample3",
                title = "MS/MS match spetra",
                trigger = "miGo3",
                size = "large",
                plotOutput("mi.single.ms2.match.plot")
        ),


        shinysky::busyIndicator(text = "Identify metabolite..."),
        DT::dataTableOutput("mi.ms2.match.result")
      )
    ),
    br(),
    br(),
    br()
  )
  )
}

