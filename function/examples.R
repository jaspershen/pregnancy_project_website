jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
background-color: white !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;
}"

library(shiny)
library(shinyjs)
runApp(list(
  ui = fluidPage(
    useShinyjs(),
    extendShinyjs(text = jscode),
    inlineCSS(css),
    navlistPanel(id = "nav",
                 tabPanel(title = "tab1", id = "tab1",
                          br(),
                          actionButton("btn", label = "View tab2 panel")),
                 tabPanel(title = "tab2", id = "tab2",
                          "test2"),
                 tabPanel(title = "tab3", id = "tab3",
                          "test3"),
                 tabPanel(title = "tab4", id = "tab4",
                          "test4"),
                 tabPanel(title = "tab5", id = "tab5",
                          "test5")
                 )
    # tabsetPanel(
    #   id = "nav",
    #   tabPanel(title = "tab1", id = "tab1",
    #            br(),
    #            actionButton("btn", label = "View tab2 panel")),
    #   tabPanel(title = "tab2", id = "tab2",
    #            "test2")
    # )
  ),
  server = function(input, output, session) {

    # disable tab2 on page load
    js$disableTab("tab1")
    js$disableTab("tab2")
    js$disableTab("tab3")
    js$disableTab("tab4")
    js$disableTab("tab5")

    observeEvent(input$btn, {
      # enable tab2 when clicking the button
      # js$enableTab("tab2")
      # switch to tab2
      updateTabsetPanel(session, "nav", "tab2")
    })
  }
))








library(shiny)
runApp(list(
  ui = navbarPage(
    title="My App",
    tabPanel("tab1 title "),
    tabPanel("tab2 title"),
    tabPanel("User: Madzia")),
  server = function(input, output) { }
))







library(shiny)
library(shinyjs)

jscode <- '
shinyjs.init = function() {
$(".nav").on("click", ".disabled", function (e) {
e.preventDefault();
return false;
});
}
'

css <- '
.disabled {
background: #eee !important;
cursor: default !important;
color: black !important;
}
'

shinyApp(
  ui = fluidPage(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = "init"),
    tags$style(css),
    checkboxInput("foo", "Disable tab2", FALSE),
    tabsetPanel(
      id = "navbar",
      tabPanel(title = "tab1",
               value = "tab1",
               h1("Tab 1")
      ),
      tabPanel(title = "tab2",
               value = "tab2",
               h1("Tab 2")
      ),
      tabPanel(title = "tab3",
               value = "tab3",
               h1("Tab 3")
      )
    )
  ),
  server = function(input, output) {
    observe({
      toggleClass(condition = input$foo,
                  class = "disabled",
                  selector = "#navbar li a[data-value=tab2]")
    })
  }
)








library(shiny)
library(shinyjs)

jscode <- '
shinyjs.init = function() {
$(".nav").on("click", ".disabled", function (e) {
e.preventDefault();
return false;
});
}'

css <- '
.disabled {
background: #eee !important;
cursor: default !important;
color: black !important;
}'

shinyApp(
  ui = fluidPage(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = "init"),
    tags$style(css),


    selectizeInput("foo", "Show tab2",selected = NULL,choices=c("a","b","c","d"), multiple = TRUE),


    tabsetPanel(


      id = "navbar",
      tabPanel(title = "tab1",
               value = "tab1",
               h1("Tab 1")
      ),
      tabPanel(title = "tab2",
               value = "tab2",
               h1("Tab 2")
      ),
      tabPanel(title = "tab3",
               id="pqr",
               value = "tab3",
               h1("Tab 3")
      )
    )


  ),
  server = function(input, output) {
    observe(print(length(input$foo)==1))
    observe({
      toggleClass(selector = "#navbar li a[data-value=tab3]", class = "disabled",
                  condition = length(input$foo)==1)
    })})






library(shiny)
library(shinyjs)


ui <- fluidPage(
  shinyjs::useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      actionButton("Button1", "Run"),
      shinyjs::hidden(p(id = "text1", "Processing..."))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {

  plotReady <- reactiveValues(ok = FALSE)

  observeEvent(input$Button1, {
    shinyjs::disable("Button1")
    shinyjs::show("text1")
    plotReady$ok <- FALSE
    # do some cool and complex stuff
    Sys.sleep(2)
    plotReady$ok <- TRUE
  })

  output$plot <-renderPlot({
    if (plotReady$ok) {
      shinyjs::enable("Button1")
      shinyjs::hide("text1")
      hist(rnorm(100, 4, 1),breaks = 50)
    }
  })
}

shinyApp(ui, server)











library(shiny)
library(shinyjs)

runApp(shinyApp(
  ui = fluidPage(
    # need to make a call to useShinyjs() in order to use its functions in server
    shinyjs::useShinyjs(),
    actionButton("start_proc", "Click to start processing data"),
    downloadButton("data_file")
  ),
  server = function(input, output) {
    observe({
      if (input$start_proc > 0) {
        Sys.sleep(1)
        # enable the download button
        shinyjs::enable("data_file")
        # change the text of the download button
        # shinyjs::text("data_file",
        #               sprintf("<i class='fa fa-download'></i>
        #                       Download (file size: %s)",
        #                       round(runif(1, 1, 10000))
        #               )
        # )
      }
    })

    output$data_file <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        write.csv(data.frame(x=runif(5), y=rnorm(5)), file)
      }
    )

    # disable the downdload button on page load
    shinyjs::disable("data_file")
  }
))
