ui.pathwayanalysis <- function(){
  tagList(
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        uiOutput(outputId = "path.ana.area")
      ),
      mainPanel = mainPanel(
        uiOutput(outputId = "path.ana.result.area")
      )
    )
  )
}



ui.path.ana <- function() {
  tagList(
    fluidPage(
    tabsetPanel(
      id = "paMainpan",
      type = "pills",
      tabPanel(
        "Upload data",
        value = "pa.upload.data",
        icon = icon(name = "upload")
      ),
      tabPanel(
        "Parameter setting",
        value = "pa.parameter.setting",
        icon = icon(name = "cogs")
      )
      ),
      conditionalPanel(condition = "input.paMainpan == 'pa.upload.data'",
                       textInput(
                         inputId = "pa.project.name",
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

                       selectInput(inputId = "pa.metabolite.type",
                                   label = "Metabolite type",
                                   choices = c(
                                     # "Metabolite name" = "pa.met.name",
                                     "KEGG ID" = "kegg.id",
                                     "HMDB ID" = "hmdb.id"
                                   ),
                                   selected = "pa.met.name"),


                       selectInput(inputId = "paDataFrom",
                                   label = h5(
                                     "How to provide dysregulated metabolites?",
                                     shinyBS::tipify(
                                       el = icon(name = "info-circle"),
                                       placement = "bottom",
                                       trigger = "hover",
                                       title = "You can paste or upload dysregulated metabolite list."
                                     )
                                   ),
                                   choices = c("Paste" = "paste",
                                               "Upload data" = "upload",
                                               "Previous step" = "previous"),
                                   selected = "paste", multiple = FALSE),

                       conditionalPanel(condition = "input.paDataFrom == 'paste'",
                                        textAreaInput(inputId = "pa.met.name.list",
                                                      label = h5(
                                                        "Metabolite name list",
                                                        shinyBS::tipify(
                                                          el = icon(name = "info-circle"),
                                                          placement = "bottom",
                                                          trigger = "hover",
                                                          title = "One column, no header like example shows"
                                                        )
                                                      ),
                                                      value = example.compound.list,
                                                      width = "400px", height = "200px"),

                                        br(),
                                        useShinyalert(),
                                        actionButton(
                                          inputId = 'pa.upload.list.AB',
                                          label = "Load data",
                                          styleclass = "info",
                                          icon = icon('play-circle')
                                        ),
                                        helpText("Click", strong("Load data"), "to load your data")
                                        ),

                       conditionalPanel(
                         condition = "input.paDataFrom == 'upload'",
                         fileInput(inputId = "pa.met.name.list.file",
                                   label = h5(
                                     "Metabolite name list table",
                                     shinyBS::tipify(
                                       el = icon(name = "info-circle"),
                                       placement = "bottom",
                                       trigger = "hover",
                                       title = "One column, no header and .csv format."
                                     )
                                   ),
                                   multiple = FALSE, accept = "csv/txt"),
                         useShinyalert(),
                         actionButton(
                           inputId = 'pa.upload.table.AB',
                           label = "Load data",
                           styleclass = "info",
                           icon = icon('play-circle')
                         ),
                         helpText("Click", strong("Load data"), "to load your data")
                       ),

                       conditionalPanel(
                         condition = "input.paDataFrom == 'previous'",
                         selectInput(inputId = "pa.from.which.step",
                                     label = h5("Use data from which step?",
                                                shinyBS::tipify(
                                                  el = icon(name = "info-circle"),
                                                  placement = "bottom",
                                                  trigger = "hover",
                                                  title = "Use data from which step?"),
                                                style = "color:black"),
                                     choices = c(
                                       "Univariate analysis" = "ua.step",
                                       "Multivariate analysis" = "ma.step",
                                       "Biomarker discovery" = "bd.step"
                                     ), selected = "ua.step"),
                         textOutput(outputId = "paProjectName"),
                         br(),
                         useShinyalert(),
                         actionButton(
                           inputId = 'pa.use.previous.step.AB',
                           label = "Load data",
                           styleclass = "info",
                           icon = icon('play-circle')
                         ),
                         helpText("Click", strong("Load data"),
                                  "to load previous step data")
                       )
                      ),


      conditionalPanel(
        condition = "input.paMainpan == 'pa.parameter.setting'",
        selectInput(inputId = "pa.pathway.library",
                    label = "Pathway library",
                    choices = list(
                      "Mammals" = list(
                        "Homo sapiens (human)" = "hsa",
                        "Mus musculus (mouse)" = "mmu",
                        "Rattus norvegicus (rat)" = "rat",
                        "Bos taurus (cow) " = "bta"
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

        # selectInput(inputId = "pa.pathway.library",
        #             label = "Pathway library",
        #             choices = c(
        #               "Homo sapiens (human)" = "hsa",
        #               "Mus musculus (mouse)" = "mmu",
        #               "Rattus norvegicus (rat)" = "rat",
        #               "Bos taurus (cow) " = "bta",
        #               "Drosophila melanogaster (fruit fly)" = "dme",
        #               "Gallus gallus (chicken)" = "gga",
        #               "Danio rerio (zebrafish)" = "dre",
        #               "Caenorhabditis elegans (nematode)" = "cel",
        #               "Saccharomyces cerevisiae (yeast)" = "sce",
        #               "Arabidopsis thaliana (thale cress)" = "ath",
        #               "Schistosoma mansoni" = "smm",
        #               "Plasmodum falciparum 3D7 (Malaria)" = "pfa",
        #               "Trypanosoma brucei" = "tbr",
        #               "Escherichia coli K-12 MG1655" = "eco",
        #               "Pseudomonas putida KT2440" = "ppu",
        #               "Synechococcus elongatus" = "syf"
        #
        #             ), selected = "hsa", multiple = FALSE),

        selectInput(inputId = "pa.algorithm",
                    label = "Pathway analysis algorithms",
                    choices = c(
                    "Hypergeometric Test" = "hypergeometric",
                    "Fisher's Exact Test" = "fisher"
                    ), selected = "hypergeometric"),

        actionButton(
          inputId = 'paSubmit',
          label = "Submit",
          styleclass = "info",
          icon = icon('play-circle')
        ),
        helpText("Click", strong("Submit"), "to do pathway analysis")

      )

    )
  )
}




ui.path.ana.result <- function() {
  tagList(fluidPage(
    tabsetPanel(
      id = "paResultMainpan",
      type = "tabs",
      tabPanel("Metabolite name/ID matching",
               value = "pa.met.id.match",
               icon = icon("table")
      ),

      tabPanel("Pathway overview",
               value = "pa.pathway.overview",
               icon = icon("share-alt")
      ),

      tabPanel("Detail information",
               value = "pa.detail.info",
               icon = icon("info-circle")
      )
      ),
    conditionalPanel(
      condition = 'input.paResultMainpan == "pa.met.id.match"',
      uiOutput(outputId = "pa.met.name.id.match.area"),
      actionButton(inputId = "paShowDetial1",
                   label = "Show match detail",
                   styleclass = "info", size = "small"),
      br(),br(),
      bsModal(id = "pamodalExample1",
              title = "Match detail information",
              trigger = "paShowDetial1",
              size = "large",
              DT::dataTableOutput(outputId = "pa.single.match.info" ),
                         actionButton(inputId = "paConfir",
                                      label = "Use this match",
                                      styleclass = "info",
                                      size = "small")

      ),
      shinysky::busyIndicator(text = "Uploading data..."),
      DT::dataTableOutput(outputId = "pa.trans.table")
    ),
    conditionalPanel(
      condition = 'input.paResultMainpan == "pa.pathway.overview"',
      uiOutput(outputId = "pa.pathway.overview.area"),

             plotlyOutput(outputId = "pa.pathway.overview", width = "600px"),

             uiOutput(outputId = "single.path.img")

    ),
    conditionalPanel(
      condition = 'input.paResultMainpan == "pa.detail.info"',
      uiOutput(outputId = "pa.detail.info.area"),
      shinysky::busyIndicator(text = "Processing..."),
      DT::dataTableOutput(outputId = "pa.enrichmeent.result")
    )
  )
  )
}

