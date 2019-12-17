ui.ms2.match <- function(){
  tagList(
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        uiOutput(outputId = "ms2.match.left.area")
      ),
      mainPanel = mainPanel(
        uiOutput(outputId = "ms2.match.right.area")
      )
    )
  )
}



ui.ms2.match.left <- function() {
  tagList(
    fluidPage(
    tabsetPanel(
      id = "ms2Mainpan",
      type = "pills",
      tabPanel(
        "Upload data",
        value = "ms2.upload.data",
        icon = icon(name = "upload")
      ),
      tabPanel(
        "Parameter setting",
        value = "ms2.parameter.setting",
        icon = icon(name = "cogs")
      )
    ),

    conditionalPanel(
      condition = 'input.ms2Mainpan == "ms2.upload.data"',
      textAreaInput(inputId = "ms2Spec",
                    label = h5(
                      "MS2 spectrum",
                      shinyBS::tipify(
                        el = icon(name = "info-circle"),
                        placement = "bottom",
                        trigger = "hover",
                        title = "Column 1, m/z, column 2, intensity"
                      )
                    ),
                    value = "39.02672235\t0.014018692\n41.04098525\t0.049188594\n56.05155683\t0.129887678\n83.78670651\t0.013067371\n84.04511379\t1\n85.02824595\t0.030459356\n102.0543983\t0.040965608\n",
                    width = "300px", height = "300px"),

      br(),
      actionButton(
        inputId = 'ms2LoadData',
        label = "Load data",
        styleclass = "info",
        icon = icon('play-circle')
      ),
      helpText("Click", strong("Load data"), "to upload data")

    ),

    conditionalPanel(
      condition = 'input.ms2Mainpan == "ms2.parameter.setting"',

      selectInput(inputId = "ms2Polarity",
                  label = "Ionization polarity",
                  choices = c(
                    "Positive" = "ms2.positive",
                    "Negative" = "ms2.negative"
                  ),
                  selected = "ms2.positive"),

      selectInput(inputId = "ms2.instrument",
                  label = "Instrument",
                  choices = c(
                    "Agilent QTOF" = "ms2.agilent.tof",
                    "Sciex Triple TOF" = "ms2.sciex.tof",
                    "Other QTOF" = "ms2.other.tof"
                    # "Thermo Orbitrap" = "ms2.thermo.orbi"
                  ),
                  selected = "ms2.sciex.tof"),

      selectInput(inputId = "ms2.ms2.library",
                  label = "MS/MS library",
                  choices = c(
                    "In-house" = "ms2.in.house",
                    "METLIN" = "ms2.metlin",
                    "NIST" = "ms2.nist"
                  ), selected = c("ms2.in.house", "ms2.metlin", "ms2.nist"),
                  multiple = TRUE
      ),

      selectInput(inputId = "ms2.ce",
                  label = "Collision energy",
                  choices = c(
                    "30" = "ms2.30",
                    "10" = "ms2.10",
                    "20" = "ms2.20",
                    "40" = "ms2.40",
                    "50" = "ms2.50"
                  ), selected = "ms2.30",
                  multiple = FALSE
      ),

      selectInput(inputId = "ms2.dp.method",
                  label = "Dot product method",
                  choices = c(
                    "Forward" = "ms2.forward",
                    "Reverse" = "ms2.reverse",
                    "Forward or Reverse" = "ms2.forward.or.reverse"
                  ), selected = "ms2.forward.or.reverse",
                  multiple = FALSE),
      sliderInput(inputId = "ms2.dp.tol",
                  label = "Dot product tolerance (0-1)",
                  min = 0.3, max = 1,
                  value = 0.8, step = 0.1),



      radioButtons(inputId = "ms2UseMZ",
                   label = "Use precursor m/z?",
                   choices = list(
                     "Yes" = "ms2.use.mz",
                     "NO" = "ms2.no.mz"
                   ),
                   selected = "ms2.no.mz"
      ),

      conditionalPanel(condition = "input.ms2UseMZ == 'ms2.use.mz'",

                       numericInput(inputId = "ms2PrecursorMZ",
                                    label = "Precursor m/z",
                                    value = 148.061, min = 0, max = 200000, step = 0.0000001),

                       conditionalPanel(condition = "input.ms2Polarity == 'ms2.positive'",
                                        selectInput(inputId = "ms2.pos.adduct",
                                                    label = "Adduct type",
                                                    choices = c(
                                                      "[M+H]+" = "ms2.pos.m.plus.h",
                                                      "[M+K]+" = "ms2.pos.m.plus.k",
                                                      "[M+Na]+" = "ms2.pos.m.plus.na",
                                                      "[M+NH4]+" = "ms2.pos.m.plus.nh4"
                                                    ), selected = c("ms2.pos.m.plus.h",
                                                                    "ms2.pos.m.plus.k",
                                                                    "ms2.pos.m.plus.na",
                                                                    "ms2.pos.m.plus.nh4"),
                                                    multiple = TRUE
                                        )
                       ),

                       conditionalPanel(condition = "input.ms2Polarity == 'ms2.negative'",
                                        selectInput(inputId = "ms2.neg.adduct",
                                                    label = "Adduct type",
                                                    choices = c(
                                                      "[M-H]-" = "ms2.pos.m.ms2nus.h",
                                                      "[M+Cl]+" = "ms2.pos.m.plus.cl",
                                                      "[M+CH3COO]+" = "ms2.pos.m.plus.ch3coo"
                                                    ), selected = c("ms2.pos.m.ms2nus.h",
                                                                    "ms2.pos.m.plus.cl",
                                                                    "ms2.pos.m.plus.ch3coo"),
                                                    multiple = TRUE
                                        )
                       ),

                       sliderInput(inputId = "ms2.ms1.match.mz.tol",
                                   label = "m/z macth tolerance (ppm)",
                                   min = 1, max = 200, value = 25)

      ),

      br(),
      actionButton(
        inputId = 'ms2Submit',
        label = "Submit",
        styleclass = "info",
        icon = icon('play-circle')
      ),
      helpText("Click", strong("Submsit"), "to Identify data")
    ),
    br(),
    br(),
    br()
  ))
}




ui.ms2.match.right <- function() {
  tagList(fluidPage(
           h5("MS2 information"),
           checkboxInput(inputId = "ms2SpecShowMZ",
                         label = "Show m/z value",
                         value = FALSE),
           plotOutput("ms2.query.spec",
                      height = 400,
                      # width = 500,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      # click = "ms2.query.spec_click",
                      dblclick = "ms2.query.spec_dblclick",
                      brush = brushOpts(
                        id = "ms2.query.spec_brush",
                        resetOnNew = TRUE
                      )
           ),
hr(),
h5("Match result"),
shinysky::busyIndicator(text = "Searching in library..."),

actionButton(inputId = "ms2ShowGo1",
             label = ,
             h5("Show MS/MS match",
               shinyBS::tipify(
                 el = icon(name = "info-circle"),
                 placement = "bottom",
                 trigger = "hover",
                 title = "Click in bottom to see MS/MS spectrum"
               )
             ),
             styleclass = "info", size = "small"),
br(),br(),
bsModal(id = "ms2modalExample1",
        title = "MS/MS spectrum",
        trigger = "ms2ShowGo1",
        size = "large",
        plotOutput(outputId = "ms2.spec.match.plot")
),

DT::dataTableOutput(outputId = "ms2.match.result")
  )
  )
}

