
fluidPage(
  # headerPanel(title = "Upload data"),
  column(width = 4,
         wellPanel(
           tabPanel(
            "Parameter Setting",
             value = "mi.ms2.parameter.setting",
             selectInput(inputId = "miMS2polarity",
                         label = "Ionization polarity",
                         choices = c(
                           "Positive" = "positive",
                           "Negative" = "negative"
                         ),
                         selected = "positive"),

             conditionalPanel(condition = "input.miMS2polarity == 'mi.ms2.positive'",
                              selectInput(inputId = "mi.ms2.pos.adduct",
                                          label = "Adduct type",
                                          choices = c(
                                            "[M+H]+" = "mi.plus.h",
                                            "[M+K]+" = "mi.plus.k",
                                            "[M+Na]+" = "mi.plus.na",
                                            "[M+NH4]+" = "mi.plus.nh4"
                                          ),
                                          selected = c("mi.plus.h",
                                                          "mi.plus.k",
                                                          "mi.plus.na",
                                                          "mi.plus.nh4"),
                                          multiple = TRUE)
                              ),

             conditionalPanel(condition = "input.miMS2polarity == 'mi.ms2.negative'",
                              selectInput(inputId = "mi.ms2.neg.adduct",
                                          label = "Adduct type",
                                          choices = c(
                                            "[M-H]-" = "mi.minus.h",
                                            "[M+Cl]+" = "mi.plus.cl",
                                            "[M+CH3COO]+" = "mi.plus.ch3coo"
                                          ),
                                          selected = c("mi.minus.h",
                                                          "mi.plus.cl",
                                                          "mi.plus.ch3coo"),
                                          multiple = TRUE)
                              ),


            selectInput(inputId = "mi.ms2.instrument",
                        label = "Instrument",
                        choices = c(
                          "Agilent QTOF" = "mi.ms2.agilent.tof",
                          "Sciex Triple TOF" = "mi.ms2.sciex.tof",
                          "Other QTOF" = "mi.ms2.other.tof",
                          "Thermo Orbitrap" = "mi.ms.thermo.orbi"
                        ),
                        selected = "mi.ms2.sciex.tof"),

            # selectInput(inputId = "mi.ms2.library",
            #             label = "MS/MS library",
            #             choices = c(
            #               "In-house" = "in.house",
            #               "METLIN" = "massbank"
            #             ), selected = c("in.house", "massbank"),
            #             multiple = TRUE
            #             ),

            selectInput(inputId = "mi.ms2.ce",
                        label = "Collision energy (CE, ev)",
                        choices = c(
                          "30" = "30",
                          "10" = "10",
                          "20" = "20",
                          "40" = "40",
                          "50" = "50"
                        ), selected = "30",
                        multiple = FALSE
            ),

            selectInput(inputId = "mi.ms2.dp.method",
                        label = "Dot product method",
                        choices = c(
                          "Forward" = "mi.ms2.forward",
                          "Reverse" = "mi.ms2.reverse",
                          "Forward or Reverse" = "mi.ms2.forward.or.reverse"
                        ), selected = "mi.ms2.forward.or.reverse",
                        multiple = FALSE),

            sliderInput(inputId = "mi.ms2.dp.tol",
                        label = "Dot product tolerance (0-1)",
                        min = 0.3, max = 1,
                        value = 0.8, step = 0.1),

             actionButton(
               inputId = 'mi.ms2.match.button',
               label = "Submit",
               styleclass = "info",
               icon = icon('play-circle')
             ),
             # actionButton(inputId = "mi.ms2.match.2.result.download",
             #             label = "Next", styleclass = "warning"),
             helpText("Click", strong("Submit"), "to identified metabolite."),
             br(),
             br(),
             br()
             )
           )
  ),

  column(width = 7,
         actionButton(inputId = "miMS2go2",
                      label = "Identification information",
                      styleclass = "info"),
         br(), br(),
         bsModal(id = "miExample2",
                 title = "Match information",
                 trigger = "miMS2go2",
                 size = "large",
                 textOutput(outputId = "mi.peak.ms2.match.info1"),
                 actionButton(inputId = "miMS2go3",
                              label = "Show MS/MS match spetra",
                              styleclass = "info"),
                 br(), br(),
                 DT::dataTableOutput(outputId = "mi.peak.ms2.match.info2")
         ),
         bsModal(id = "miExample3",
                 title = "MS/MS match spetra",
                 trigger = "miMS2go3",
                 size = "large",
                 plotOutput("mi.single.ms2.match.plot")
         ),
         DT::dataTableOutput(outputId = "mi.ms2.match.result")
  )
)
