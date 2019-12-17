# Logged = FALSE
# Signuped = FALSE
# load("users")
# users <- users
# my_password <- users$username
# my_username <- users$passwd
# credentials <- readRDS("credentials/credentials.rds")
#
# credentials <- data.frame(user = c("test", "shenxt", "jaspershen"),
#                           pw = c("06195aa345d5f8889bc7de7dfef92e30",
#                                  "4692a48ba6b76a03baac39753fe45f49",
#                                  "7cb4615c56d621b8771739352b15270b"
#                                  ),
#                           locked_out = c(FALSE,
#                                         FALSE,
#                                         FALSE),
#                           country = c("China", "China", "China"),
#                           org = c("CAS", "CAS", "CAS"),
#                           emial = c("shenxt@sioc.ac.cn",
#                                     "shenxt@sioc.ac.cn",
#                                     "shenxt@sioc.ac.cn"),
#                           stringsAsFactors = FALSE)
#
# saveRDS(credentials, "credentials/credentials.rds")


load("data/country", envir = environment())


##number wrong
num_fails_to_lockout <- 100



share <- list(
  title = "omicTools - An Integrative Platform for Omics Data Analysis",
  url = "http://daattali.com/shiny/timevis-demo/",
  image = "http://daattali.com/shiny/img/timevis-demo.png",
  description = "Create rich and fully interactive timeline visualizations. Timelines can be included in Shiny apps and R markdown documents, or viewed from the R console and RStudio Viewer.",
  twitter_user = "daattali"
)

codeConsole <-
  'library(timevis)

data <- data.frame(
id      = 1:4,
content = c("Item one", "Item two",
"Ranged item", "Item four"),
start   = c("2016-01-10", "2016-01-11",
"2016-01-20", "2016-02-14 15:00:00"),
end     = c(NA, NA, "2016-02-04", NA)
)

timevis(data)'

codeShiny <-
  'library(shiny)
library(timevis)

data <- data.frame(
id      = 1:4,
content = c("Item one", "Item two",
"Ranged item", "Item four"),
start   = c("2016-01-10", "2016-01-11",
"2016-01-20", "2016-02-14 15:00:00"),
end     = c(NA, NA, "2016-02-04", NA)
)

ui <- fluidPage(
timevisOutput("timeline")
)

server <- function(input, output, session) {
output$timeline <- renderTimevis({
timevis(data)
})
}

shinyApp(ui = ui, server = server)
'




#example of compound list
example.compound.list <- "C00164\nC00099\nC00300\nC01026\nC00122\nC00037\nC00155\nC00097\nC00079\nC00065\nC00188\nC00082\nC00183\nC00166\nC00163\nC00022\nC00213"
example.compound.list2 <- "377.2328311\n379.2485071\n399.2459926\n401.261456\n425.3300447\n432.279734\n476.3059521\n520.3321067\n560.3269599\n564.3582824\n606.3690372\n608.3842407\n628.3821581\n648.379374\n650.3953008\n652.4102222\n672.4084578\n694.4216328\n696.4368192\n714.5356497\n716.4348125\n830.6185439\n888.6610249\n946.7024672"



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
background-color: #aaa !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;
}"






my.theme <- ggplot2::theme_bw()+
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13),
                 axis.title.y = ggplot2::element_text(size = 13)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
                 axis.text.y = ggplot2::element_text(size = 10)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 13)) +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10))