ui.introduction <- function(){
  tagList(
    fluidPage(
      sidebarLayout(sidebarPanel = sidebarPanel(
        # tags$style(".well {background-color: white;}"),
        textOutput(outputId = "time"),
        br(),
        textOutput(outputId = "user.num"),
        br(),
        # textOutput(outputId = "job.num"),
        # hr(),
        h4("News", align = "left", style = "color:lightseagreen"),

        ##version 1.2
        h5("Version 1.2 (2018.12.13)", align = "left"
           # style="font-weight: normal;line-height:20px;BACKGROUND-COLOR:yellow"
        ),
        # h5("The second release verson.", align = "justify"
        #    # style="font-weight: normal;line-height:20px;BACKGROUND-COLOR:yellow"
        # ),
        actionButton(inputId = "version12Go", styleclass = "info",
                     size = "mini",
                     icon = icon("angle-double-right"),
                     label = "More"
        ),

        bsModal(id = "modalExampleV12",
                title = "Information of Version 1.2",
                trigger = "version12Go",
                size = "large",
                fluidPage(
                  includeMarkdown(path = "./data/markdown/info.v1.2.Rmd")
                )
        ),

        hr(),
        ##version 1.1
        h5("Version 1.1 (2018.10.22)", align = "left"
           # style="font-weight: normal;line-height:20px;BACKGROUND-COLOR:yellow"
           ),
        h5("The first release verson.", align = "justify"
           # style="font-weight: normal;line-height:20px;BACKGROUND-COLOR:yellow"
           ),
        actionButton(inputId = "version11Go", styleclass = "info",
                     size = "mini",
                     icon = icon("angle-double-right"),
                     label = "More"
        ),

        bsModal(id = "modalExampleV4",
                title = "Information of Version 1.1",
                trigger = "version11Go",
                size = "large",
                fluidPage(
                  includeMarkdown(path = "./data/markdown/info.v1.1.Rmd")
                )
        )

      ),
      mainPanel = mainPanel(
        includeMarkdown(path = "./data/markdown/home/home_introduction.Rmd"),
        br(), br(), br()
                            ),
      position = "right"
      )
    )
  )
}