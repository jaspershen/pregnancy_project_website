#####################################upload data###############################
#####merge identification
output$mergeidentification.area <- renderUI({
  if (user_input$authenticated == TRUE){
    ui.mergeidentification()
  }else{
    not_logged_in7()
  }
})


##warning if no data selected or project name is null
observeEvent(eventExpr = input$merge.iden.upload.button, {
  if(is.null(input$mergeIdenMS1PeakTable) | is.null(input$mergeIdenMS2PeakTable) | input$mergeIdenProjectID == ""){
    shinyalert::shinyalert(title = "Error",
                           text = "Project name, MS1 peak table and MS2 identification results are required.",
                           type = "error")
  }
})


##create folder
observeEvent(eventExpr = input$merge.iden.upload.button,{
  if(!is.null(input$mergeIdenProjectID)){
    if(input$mergeIdenProjectID != ""){
      dir.create(file.path("./user_data", user_input$username))
      dir.create(file.path("./user_data", user_input$username, input$mergeIdenProjectID))
      dir.create(file.path("./user_data", user_input$username, input$mergeIdenProjectID, "merge_identification"))
    }
  }
})

##merge.iden.user.path
merge.iden.user.path <- reactive({
  req(user_input$username)
  req(input$mergeIdenProjectID)
  file.path("user_data", user_input$username, input$mergeIdenProjectID, "merge_identification")
})

##warning if no data selected or project name is null
observeEvent(eventExpr = input$merge.iden.upload.button, {
  if(is.null(input$mergeIdenMS1PeakTable) | is.null(input$mergeIdenMS2PeakTable) | input$mergeIdenProjectID == ""){
    shinyalert::shinyalert(title = "Error",
                           text = "Upload data first",
                           type = "error")
  }
})


###record job number
observeEvent(input$merge.iden.upload.button,{
  if(!is.null(input$mergeIdenProjectID)){
    if(input$mergeIdenProjectID != ""){
      credentials <- readRDS("credentials/credentials.rds")
      credentials$job.number[match(user_input$username, credentials$user)] <- as.numeric(credentials$job.number[match(user_input$username, credentials$user)])+1
      saveRDS(credentials, "credentials/credentials.rds")
    }
  }
})















###read MS1 peak table
merge.iden.ms1.raw <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$merge.iden.upload.button, {
  withProgress(message = 'Upload MS1 peak table...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {#upload data
                 if (!is.null(input$mergeIdenMS1PeakTable) & input$mergeIdenProjectID != "") {
                   temp.file <- input$mergeIdenMS1PeakTable$datapath
                   merge.iden.ms1.raw$data <- lapply(1:length(temp.file), function(idx){
                     incProgress(1/length(temp.file),
                                 detail = paste("Data", idx))
                     as.data.frame(readr::read_csv(temp.file[idx],
                                                   col_types = readr::cols()))
                   })
                 }
               })
})


merge.iden.ms2.raw <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$merge.iden.upload.button, {
  withProgress(message = 'Upload MS2 identification result table...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {#upload data
                 if (!is.null(input$mergeIdenMS2PeakTable) & input$mergeIdenProjectID != "") {
                   temp.file <- input$mergeIdenMS2PeakTable$datapath
                   merge.iden.ms2.raw$data <- lapply(1:length(temp.file), function(idx){
                     incProgress(1/length(temp.file),
                                 detail = paste("Data", idx))
                     as.data.frame(readr::read_csv(temp.file[idx],
                                                   col_types = readr::cols()))
                   })
                 }
               })
})





















#------------------------------------------------------------------------------
##Check data
merge.iden.data.check.result <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$merge.iden.upload.button,{
  if(!is.null(merge.iden.ms1.raw$data) & input$mergeIdenProjectID != ""){
    check.result <- try({
      checkDataMerge(peak.table1 = merge.iden.ms1.raw$data,
                     peak.table2 = merge.iden.ms2.raw$data)
      })
    if(class(check.result)[1] == "try-error"){
      output$merge.iden.data.check.result.info <- renderText({paste("Data check: ",check.result[[1]])})
      check.result <- data.frame("Data.File" = "Data",
                                 "Information" = "There are errors in your data",
                                 "Result" = "Error",
                                 stringsAsFactors = FALSE)
    }else{
      output$merge.iden.data.check.result.info <- renderText({""})
    }
  }else{
    check.result <- data.frame("Data.File" = "Data",
                               "Information" = "You don't provide MS1 peak table or sample information",
                               "Result" = "Error",
                               stringsAsFactors = FALSE)
  }
  merge.iden.data.check.result$data <- check.result
})


output$merge.iden.data.check.result <- DT::renderDataTable(
  DT::datatable(merge.iden.data.check.result$data,
                class = 'cell-border stripe',
                editable = FALSE,
                selection = "single",
                # caption = 'Raw data containing MVs',
                escape = FALSE,
                # filter = "top",
                filter = "none",
                rownames = FALSE,
                extensions = list("ColReorder" = NULL,
                                  "Buttons" = NULL,
                                  "FixedColumns" = list(leftColumns=2)),
                options = list(
                  dom = 'BRrltpi',##remove serach options
                  # autoWidth = TRUE,
                  lengthMenu = list(c(15, 50, -1), c('15', '50', 'All')),
                  ColReorder = TRUE,
                  buttons =
                    list(
                      list(
                        extend = 'collection',
                        buttons = list(list(extend="csv",filename = "Data.check.result"),
                                       list(extend='excel',filename = "Data.check.result")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ) %>% DT::formatStyle(
    'Result',
    target = 'row',
    backgroundColor = DT::styleEqual(c("Error: No name",
                                       "Error: No mz",
                                       "Error: No rt",
                                       "Error: No mzmed",
                                       "Error: No rtmed",
                                       "Error: No hits.forward",
                                       "Error: No hits.reverse"),
                                     c(rep('#FC4B4E', 7)))
  ),
  server = FALSE
)



##jump to parameter setting page from data check
observeEvent(input$merge.iden.data.check.2.parameter, {
  if(length(grep(pattern = "Error",merge.iden.data.check.result$data[,3])) == 0){
    if(!is.null(merge.iden.ms1.raw$data) & !is.null(merge.iden.ms2.raw$data)){
      updateTabsetPanel(session, "mergeIdenTab",
                        selected = "mergeIdenParameter")
    }
  }
})

##error if there is error in data
observeEvent(eventExpr = input$merge.iden.data.check.2.parameter, {
  if(length(grep(pattern = "Error",merge.iden.data.check.result$data[,3])) != 0){
    shinyalert::shinyalert(title = "Error",
                           text = "There are errors in you files, please check and
                           upload again.",
                           type = "error")
  }

})



















###identification

merge.iden.peak.identification <- reactiveValues(data = NULL)

observeEvent(eventExpr = input$merge.iden.submit.button, {
if(!is.null(merge.iden.data.check.result$data)){
  if(length(grep(pattern = "Error",merge.iden.data.check.result$data[,3])) == 0){

    withProgress(message = 'Merge identification...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   temp <- try({lapply(1:1, function(idx){
                     incProgress(1/length(idx),
                                 detail = "This may take a while")
                     mergeIden(peak.table1 = merge.iden.ms1.raw$data,
                               peak.table2 = merge.iden.ms2.raw$data,
                               mz.tolerance = input$merge.iden.mz.tol,
                               rt.tolerance = input$merge.iden.rt.tol)
                   })[[1]]
                   })

                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$merge.iden.params.message <- renderText({paste("Merge identification: ",info)})
                     rm(list = c("temp"))
                   }else{
                     merge.iden.peak.identification$data <- temp
                     info <- paste("There are",sum(!is.na(temp$identification)), "peaks are identified.")
                     output$merge.iden.params.message <- renderText({info})
                     rm(list = c("temp"))
                   }
                 })
  }
}
})


output$merge.iden.peak.identification <- DT::renderDataTable(
  DT::datatable(merge.iden.peak.identification$data,
                class = 'cell-border stripe',
                editable = FALSE,
                # selection = "single",
                selection = list(mode = 'single', target = 'row'),
                # caption = 'Raw data containing MVs',
                escape = FALSE,
                # filter = "top",
                filter = "none",
                rownames = FALSE,
                extensions = list("ColReorder" = NULL,
                                  "Buttons" = NULL,
                                  "FixedColumns" = list(leftColumns=2)),
                options = list(
                  dom = 'BRrltpi',
                  # autoWidth = TRUE,
                  lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                  ColReorder = TRUE,
                  buttons =
                    list(
                      list(
                        extend = 'collection',
                        buttons = list(list(extend="csv",filename = "Merge.result"),
                                       list(extend='excel',filename = "Merge.result")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)

##jump to result download
observeEvent(input$merge.iden.parameter.2.result.download, {
    if(!is.null(merge.iden.peak.identification$data)){
      updateTabsetPanel(session, "mergeIdenTab",
                        selected = "mergeIdenDownload")
    }

})

##error if there is error in data
observeEvent(eventExpr = input$merge.iden.parameter.2.result.download, {
  if(is.null(merge.iden.peak.identification$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Click Submit first!",
                           type = "error")
  }
})

















###download result
merge.result <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$merge.iden.generate.analysis.result, {
  if(!is.null(merge.iden.peak.identification)){
    ##copy data
    withProgress(message = 'Generate analysis result...',
                 detail = "This may tale a while",
                 value = 0,{
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may tale a while")
                     merge.result$data <- data.frame(merge.iden.peak.identification$data,
                                                     merge.iden.ms1.raw$data[[1]][,-c(1:3)])
                   })
                 })
  }
})



####download analysis result
output$merge.iden.result.download <- downloadHandler(
  filename = "Identification.result.csv",
  content = function(file){
    readr::write_csv(merge.result$data, file)
  }
)