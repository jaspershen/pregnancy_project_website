fluidPage(
  sidebarLayout(sidebarPanel = sidebarPanel(
    selectInput(inputId = "pa.pathway.library",
                label = "Pathway library",
                choices = list(
                  "Mammals" = list(
                    "Homo sapiens (human)" = "hsa",
                    "Mus musculus (mouse)" = "mmu",
                    "Rattus norvegicus (rat)" = "rat",
                    "Bos taurus (cow)" = "bta"
                  ),
                  "Birds" = list(
                    "Gallus gallus (chicken)" = "gga"
                  ),
                  "Fish" = list(
                    "Danio rerio (zebrafish)" = "dre"
                  ),
                  "Insects" = list(
                    "Drosophila melanogaster (fruit fly)" = "dme"
                  ),

                  "Nematodes" = list(
                    "Caenorhabditis elegans (nematode)" = "cel"
                  ),

                  "Fungi" = list(
                    "Saccharomyces cerevisiae (yeast)" = "sce"
                  ),
                  "Others" = list(
                    "Arabidopsis thaliana (thale cress)" = "ath",
                    "Schistosoma mansoni" = "smm",
                    "Plasmodum falciparum 3D7 (Malaria)" = "pfa",
                    "Trypanosoma brucei" = "tbr",
                    "Escherichia coli K-12 MG1655" = "eco",
                    "Pseudomonas putida KT2440" = "ppu",
                    "Synechococcus elongatus" = "syf"
                  )
                ),
                selected = "hsa", multiple = FALSE),

    selectInput(inputId = "pa.algorithm",
                label = "Pathway analysis algorithm",
                choices = c(
                  "Hypergeometric Test" = "hypergeometric",
                  "Fisher's Exact Test" = "fisher"
                ), selected = "hypergeometric"),

    numericInput(inputId = "pa.p.cutoff", label = "P-value cutoff",
                 value = 0.05, min = 0, max = 0.1),
    numericInput(inputId = "pa.overlap.cutoff", label = "Overlap cutoff (%)",
                 value = 0, min = 0, max = 100),

    useShinyalert(),
    actionButton(
      inputId = 'paSubmit',
      label = "Submit",
      styleclass = "info",
      icon = icon('play-circle')
    ),

    actionButton(inputId = "pa.pathway.enrichment.2.result.download",
                 label = "Next", styleclass = "warning"),

    helpText("Click", strong("Submit"), "to do pathway analysis")

  ),
  mainPanel = mainPanel(
    span(textOutput(outputId = "pa.pathway.enrichment.message"), style = "color:red"),
    tabsetPanel(id = "paPathwayEnrichmentPlot",
                type = "tabs",
                tabPanel("Pathway enrichment plot",
                         value = "paPathwayPlot",
                         icon = icon("image"),
                                shinysky::busyIndicator(text = "Processing..."),
                                plotly::plotlyOutput(outputId = "pa.pathway.plot",
                                                     height = "600px",
                                                     width = "100%")

                ),

                tabPanel("Pathway enrichment table",
                         value = "paPathwayTable",
                         icon = icon("table"),
                         shinysky::busyIndicator(text = "Processing..."),
                         DT::dataTableOutput(outputId = "pa.pathway.table")
                ),
                br(), br(), br()
    ),
    br(), br(), br()
  )
  )
)



