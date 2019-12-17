ui.link <- function(){
  tagList(
    fluidPage(
      column(width = 10, offset = 1,
             tags$a(href="http://www.zhulab.cn/", title="Zhu lab: Laboratory for Mass Spectrometry and Metabolomics",
                    tags$img(src="zhulab.png",
                             height = 100,
                             # width = 270,
                             style = "margin: 40px 20px 30px 30px;"),
                    target="_blank",
                    align = "center"),

             tags$a(href="http://metdna.zhulab.cn/index", title="MetDNA: Metabolite identification and Dysregulated Network Analysis",
                    tags$img(src="MetDNA.png",
                             height = 100,
                             # width = 476,
                             style = "margin: 30px 30px 30px 30px;"),
                    target="_blank"),

             tags$a(href="http://www.metabolomics-shanghai.org/MetCCS/",
                    title="MetCCS Predictor",
                    tags$img(src="metccs.png",
                             height = 100,
                             # width = 287,
                             style = "margin: 15px 30px 30px 20px;"),
                    target="_blank"),

             tags$a(href="http://www.metabolomics-shanghai.org/LipidCCS/",
                    title="LipidCCS Predictor",
                    tags$img(src="lipidccs.png",
                             height = 100,
                             # width = 287,
                             style = "margin: 40px 30px 30px 30px;"),
                    target="_blank"),

             tags$a(href="http://www.hmdb.ca/",
                    title="The Human Metabolome Database (HMDB)",
                    tags$img(src="hmdb.png",
                             height = 100,
                             # width = 222
                             style = "margin: 40px 30px 30px 30px;"),
                    target="_blank"),

             tags$a(href="http://www.genome.jp/kegg/",
                    title="KEGG: Kyoto Encyclopedia of Genes and Genomes",
                    tags$img(src="kegg.png",
                             height = 100,
                             # width = 210,
                             style = "margin: 30px 10px 30px 30px;"),
                    target="_blank"),

             tags$a(href="https://xcmsonline.scripps.edu/landing_page.php?pgcontent=mainPage",
                    title="The original and most widely used metabolomic and lipidomic platform",
                    tags$img(src="xcms.png",
                             height = 100,
                             width = 354,
                             style = "margin: 15px 30px 30px 10px;"),
                    target="_blank")

             # tags$a(href="https://omicschina.github.io/",
             #        title="A webset for omics study",
             #        tags$img(src="omicschina.png",
             #                 height = 100,
             #                 # width = 193,
             #                 style = "margin: 30px 30px 30px 50px;"),
             #        target="_blank")
             ),
      br(), br(), br()
      )
  )
}