
#####################################upload data###############################
#####data cleaning
###data cleaning.area
output$datacleaning.area <- renderUI({
  if (user_input$authenticated == TRUE){
    ui.datacleaning()
  }else{
    not_logged_in3()
  }
})


##warning if no data selected or project name is null
observeEvent(eventExpr = input$dc.upload.button, {
  #if don't use demo data and don't upload data or give project name, error
  if(input$dc.use.demo.data == FALSE){
    if(is.null(input$DCms1PeakTable) | is.null(input$DCsampleInfo) | input$DCprojectID == ""){
      shinyalert::shinyalert(title = "Error",
                             text = "Project name, MS1 peak table and Sample information are required.",
                             type = "error")
    }
  }
})


observeEvent(eventExpr = input$dc.upload.button, {
  #if don't use demo data and don't upload data or give project name, error
  if(input$dc.use.demo.data == TRUE){
    if(input$DCprojectID == ""){
      shinyalert::shinyalert(title = "Error",
                             text = "Project name is required.",
                             type = "error")
    }
  }
})


##create folder
observeEvent(input$dc.upload.button,{
  if(!is.null(input$DCprojectID)){
    if(input$DCprojectID != ""){
      dir.create(file.path("./user_data", user_input$username))
      dir.create(file.path("./user_data", user_input$username, input$DCprojectID))
      dir.create(file.path("./user_data", user_input$username, input$DCprojectID, "data_cleaning"))
    }
  }
})


###record job number
# observeEvent(input$dc.upload.button,{
#   if(!is.null(input$DCprojectID)){
#     if(input$DCprojectID != ""){
#     load("credentials/jobNumber")
#       jobNumber <- jobNumber + 1
#       save(jobNumber, file = "credentials/jobNumber")
#     }
#   }
# })


# ###record job number
observeEvent(input$dc.upload.button,{
  if(!is.null(input$DCprojectID)){
    if(input$DCprojectID != ""){
      credentials <- readRDS("credentials/credentials.rds")
      credentials$job.number[match(user_input$username, credentials$user)] <- as.numeric(credentials$job.number[match(user_input$username, credentials$user)])+1
      saveRDS(credentials, "credentials/credentials.rds")
    }
  }
})



















# -------------------------------------------------------------------------------
###read MS1 peak table
dc.ms1.raw <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.upload.button, {
  withProgress(message = 'Upload MS1 peak table...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 if(input$dc.use.demo.data == TRUE){##use demo data
                   temp.file <- dir("./data/demo_data/Data_Cleaning")
                   temp.file <- grep("batch", x = temp.file, value = TRUE)
                   dc.ms1.raw$data <- lapply(1:length(temp.file), function(idx){
                     x <- temp.file[idx]
                     incProgress(1/length(temp.file),
                                 detail = paste("Data", idx))
                     as.data.frame(readr::read_csv(file.path("./data/demo_data/Data_Cleaning",x),
                                                   col_types = readr::cols()))
                   })
                 }else{#upload data
                   if (!is.null(input$DCms1PeakTable) & input$DCprojectID != "") {
                     temp.file <- input$DCms1PeakTable$datapath
                     dc.ms1.raw$data <- lapply(1:length(temp.file), function(idx){
                       incProgress(1/length(temp.file),
                                   detail = paste("Data", idx))
                       as.data.frame(readr::read_csv(temp.file[idx],
                                                     col_types = readr::cols()))
                     })
                   }
                 }
               })
})


## read sample.info
dc.sample.info.raw <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.upload.button, {
  if(input$dc.use.demo.data == TRUE){
    dc.sample.info.raw$data <- as.data.frame(readr::read_csv("./data/demo_data/Data_Cleaning/sample.information.csv",
                                                             col_types = readr::cols()))
    dc.sample.info.raw$data[,"group"] <- as.character(dc.sample.info.raw$data[,"group"])

  }else{
    if (!is.null(input$DCsampleInfo) & input$DCprojectID != "") {
      dc.sample.info.raw$data <-
        as.data.frame(readr::read_csv(input$DCsampleInfo$datapath,col_types = readr::cols()))
    }

  }
})



##save data user upload
observeEvent(eventExpr = input$dc.upload.button, {
  if(input$DCprojectID != ""){
    withProgress(message = 'Save data...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     if(!is.null(dc.ms1.raw$data)){
                       dc.ms1.raw <- dc.ms1.raw$data
                       save(dc.ms1.raw, file = file.path("user_data", user_input$username,
                                                         input$DCprojectID,
                                                         "data_cleaning",
                                                         "dc.ms1.raw"))
                       rm(list= c("dc.ms1.raw"))
                     }

                     if(!is.null(dc.sample.info.raw$data)){
                       dc.sample.info.raw <- dc.sample.info.raw$data
                       save(dc.sample.info.raw, file = file.path("user_data", user_input$username,
                                                                 input$DCprojectID,
                                                                 "data_cleaning",
                                                                 "dc.sample.info.raw"))
                       rm(list= c("dc.sample.info.raw"))
                     }
                   })
                 })
  }
})


##jump to data check page from upload data page
observeEvent(input$dc.upload.button, {
  if(!is.null(dc.ms1.raw$data) & !is.null(dc.sample.info.raw$data) & input$DCprojectID != ""){
    updateTabsetPanel(session, inputId = "DCtab",
                      selected = "DCdataCheck")
  }

})






















#------------------------------------------------------------------------------
##Check data
dc.data.check.result <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.upload.button,{
  if(!is.null(dc.ms1.raw$data) & !is.null(dc.sample.info.raw$data) & input$DCprojectID != ""){
    check.result <- try({checkData(peak.table = dc.ms1.raw$data,
                                                         sample.info = dc.sample.info.raw$data,
                                                         step = "cleaning")})
    if(class(check.result)[1] == "try-error"){
      output$dc.data.check.result.info <- renderText({paste("Data check: ",check.result[[1]])})
      check.result <- data.frame("Data.File" = "Data",
                                 "Information" = "There are errors in your data",
                                 "Result" = "Error",
                                 stringsAsFactors = FALSE)
    }else{
      output$dc.data.check.result.info <- renderText({""})
    }
  }else{
    check.result <- data.frame("Data.File" = "Data",
                               "Information" = "You don't provide MS1 peak table or sample information",
                               "Result" = "Error",
                               stringsAsFactors = FALSE)
  }
  dc.data.check.result$data <- check.result
})


output$dc.data.check.result <- DT::renderDataTable(
  DT::datatable(dc.data.check.result$data,
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
    backgroundColor = DT::styleEqual(c("Error: NA in sample.information",
                                       "Error: space in sample.information",
                                       "Error: No sample.name",
                                       "Error: No injection.order",
                                       "Error: No class",
                                       "Error: No batch",
                                       "Error: No group",
                                       "Error: No Subject",
                                       "Error: The sample names in sample.inforamtion and data are not same",
                                       "Error: No name",
                                       "Error: No mz",
                                       "Error: No rt"),
                                     c(rep('#FC4B4E', 12)))
  ),
  server = FALSE
)



##jump to upload data page from data check page
observeEvent(input$dc.data.check.2.dc.upload, {
  updateTabsetPanel(session, "DCtab",
                    selected = "DCupload")

})

##jump to batch alignment page from data check
observeEvent(input$dc.data.check.2.dc.batch.alignment, {
  if(!is.null(dc.data.check.result$data)){
    if(length(grep(pattern = "Error",dc.data.check.result$data[,3])) == 0){
      updateTabsetPanel(session, "DCtab",
                        selected = "DCbatchAlignment")
    }
  }
})

##error if there is an error in data
observeEvent(eventExpr = input$dc.data.check.2.dc.batch.alignment, {
  if(!is.null(dc.data.check.result$data)){
    if(length(grep(pattern = "Error",dc.data.check.result$data[,3])) != 0){
      shinyalert::shinyalert(title = "Error",
                             text = "There are errors in you files, please click Previous to check and
                             upload again.",
                             type = "error")
    }
  }
})


##dc.data.info is the information of dataset.
dc.data.info <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.data.check.2.dc.batch.alignment,{
  if(!is.null(dc.ms1.raw$data) & !is.null(dc.sample.info.raw$data)){
    dc.data.info$data <- paste("There are", length(dc.ms1.raw$data), "batches.",
                               "And there are",
                               ifelse(any(dc.sample.info.raw$data$class == "QC"),
                                      "QC sample", "no QC samples."), "in your data.")
  }
})





















#------------------------------------------------------------------------------
#batch alignment
###rough match
dc.batch.alignment.result1 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.submit.button,{
  if(!is.null(dc.ms1.raw$data) & !is.null(dc.sample.info.raw$data) & input$DCprojectID != ""){
    withProgress(message = 'Rough alignment...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   temp <- try({lapply(1:1, function(idx){
                     incProgress(1/length(idx),
                                 detail = "This may take a while")
                     roughAlign(peak.table = dc.ms1.raw$data,
                                combine.mz.tol = input$dc.ba.mz.tol,
                                combine.rt.tol = input$dc.ba.rt.tol)
                   })[[1]]
                   })

                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$dc.batch.alignment.result1.info <- renderText({paste("Roungh alignment: ",info)})
                     rm(list = c("temp"))
                   }else{
                     dc.batch.alignment.result1$data <- temp
                     output$dc.batch.alignment.result1.info <- renderText({""})
                     rm(list = c("temp"))
                   }
                 })
  }
})



######save parameters of batch alignment
dc.ba.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.submit.button, {
  temp <- try({
    dcBAparams <- DCbaParam(
      input$dc.ba.mz.tol,
      input$dc.ba.rt.tol)
    dc.ba.params$data <- dcBAparams
    save(dcBAparams, file = file.path("user_data", user_input$username,
                                      input$DCprojectID,
                                      "data_cleaning",
                                      "dcBAparams"))
  })
  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$dc.ba.params.message <- renderText({paste("Batch alignment parameters: ",info)})
    rm(list="temp")
  }else{
    output$dc.ba.params.message <- renderText({""})
    rm(list="temp")
  }
})





output$dc.ba.mz.plot <- renderPlotly({
  if(is.null(dc.batch.alignment.result1$data)) return(NULL)
  if(length(dc.ms1.raw$data) == 1) return(NULL)
  dc.ba.mz.plot <- baMZplot(dc.batch.alignment.result1$data)
  ggplotly(dc.ba.mz.plot,
           tooltip = c("mz", "mz.error"),
           source = "dc.ba.mz.plot")
})


output$dc.ba.rt.plot <- renderPlotly({
  if(is.null(dc.batch.alignment.result1$data)) return(NULL)
  if(length(dc.ms1.raw$data) == 1) return(NULL)
  dc.ba.rt.plot <- baRTplot(dc.batch.alignment.result1$data)
  ggplotly(dc.ba.rt.plot,
           tooltip = c("rt", "rt.error"),
           source = "dc.ba.rt.plot")
})


output$dc.ba.int.plot <- renderPlotly({
  if(is.null(dc.batch.alignment.result1$data)) return(NULL)
  if(length(dc.ms1.raw$data) == 1) return(NULL)
  dc.ba.int.plot <- baINTplot(dc.batch.alignment.result1$data)
  ggplotly(dc.ba.int.plot, tooltip = c(
    "int",
    "int.error"),
    source = "dc.ba.int.plot")
})


###accurate match
dc.batch.alignment.result2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.submit.button,{
  if(!is.null(dc.batch.alignment.result1$data)){
    withProgress(message = 'Accurate alignment...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   temp <- try({
                     lapply(1:1, function(idx){
                     incProgress(1/length(idx),
                                 detail = "This may take a while")
                     accurateAlign(peak.table = dc.ms1.raw$data,
                                   simple.data = dc.batch.alignment.result1$data)
                   })[[1]]
                   })
                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$dc.batch.alignment.result2.info <- renderText({paste("Accurate alignment: ",info)})
                     dc.batch.alignment.result2$data <- NULL
                     rm(list = c("temp"))
                   }else{
                     dc.batch.alignment.result2$data <- temp
                     output$dc.batch.alignment.result2.info <- renderText({""})
                     rm(list = c("temp"))
                   }
                 })
  }
})


# dc.batch.alignment.result2.tags <- reactiveValues(data = NULL)
# observe({
#   if(!is.null(dc.batch.alignment.result2$data)){
#     dc.batch.alignment.result2.tags$data <-
#       dc.batch.alignment.result2$data[,-match(dc.sample.info.raw$data$sample.name, colnames(dc.batch.alignment.result2$data))]
#   }
# })


output$dc.batch.alignment.result2 <- DT::renderDataTable(
  DT::datatable(dc.batch.alignment.result2$data,
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
                        buttons = list(list(extend="csv",filename = "MS1.peak.table.after.batch.alignment"),
                                       list(extend='excel',filename = "MS1.peak.table.after.batch.alignment")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)


###Warning if there is only one batch, warning user and tell them to click next
observeEvent(eventExpr = input$dc.ba.submit.button, {
  if(!is.null(dc.ms1.raw$data) & !is.null(dc.sample.info.raw$data) & input$DCprojectID != ""){
    if(length(dc.ms1.raw$data) == 1){
      shinyalert::shinyalert(title = "Note",
                             text = "There is only one batch data.",
                             type = "info")
    }
  }
})



###Error if there are more than one batch, warning if user click Next beforeclick submit
observeEvent(eventExpr = input$dc.ba.2.dc.qa1, {
  if(!is.null(dc.ms1.raw$data) & !is.null(dc.sample.info.raw$data) & input$DCprojectID != ""){
    if(is.null(dc.batch.alignment.result2$data)){
      shinyalert::shinyalert(title = "Error",
                             text = "Please click Submit first.",
                             type = "error")
    }
  }
})


##jump to data quality page page from batch alignment page
observeEvent(input$dc.ba.2.dc.qa1, {
  if(!is.null(dc.batch.alignment.result2$data)){
    updateTabsetPanel(session, "DCtab",
                      selected = "DCdataQualityBefore")
  }
})



##save information of batch alginment
dc.ba.info <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1,{
  if(!is.null(dc.batch.alignment.result2$data)){

    temp <- try({getBatchAlignmentInfo(dc.ms1.raw$data,
                                        dc.batch.alignment.result1$data,
                                        dc.batch.alignment.result2$data)})

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.ba.info.message <- renderText({paste("Batch alignment info: ", info)})
      rm(list = c("temp"))
    }else{
      dc.ba.info$data <- temp
      output$dc.ba.info.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})





















#-------------------------------------------------------------------------------
#data quality assessment, before data cleaning
## peak profile
dc.data.profile.plot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1 ,{
  if(!is.null(dc.batch.alignment.result2$data)){
    temp <- try({dataInformation(object = dc.batch.alignment.result2$data)})
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.data.profile.plot.message <- renderText({paste("Data profile: ",info)})
      rm(list = c("temp"))
    }else{
      dc.data.profile.plot$data <- temp
      output$dc.data.profile.plot.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})


output$dc.data.profile.plot <- renderPlotly({
  if(is.null(dc.data.profile.plot$data)) return(NULL)
  ggplotly(dc.data.profile.plot$data, tooltip = c(
    "rt",
    "mz","int.log"),
    source = "dc.data.profile.plot")
})


## MV ratio in peaks
dc.peak.mv.ratio.plot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1 ,{
  if(!is.null(dc.batch.alignment.result2$data)){
    temp <- try({mvPeakPlot(data = dc.batch.alignment.result2$data)})
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.peak.mv.ratio.plot.message <- renderText({paste("MV ratio plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.peak.mv.ratio.plot$data <- temp
      output$dc.peak.mv.ratio.plot.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})

output$dc.peak.mv.ratio.plot <- renderPlotly({
  if(is.null(dc.peak.mv.ratio.plot$data)) return(NULL)
  ggplotly(dc.peak.mv.ratio.plot$data, tooltip = c(
    "Index",
    "MV.ratio","Peak"),
    source = "dc.peak.mv.ratio.plot")
})


## MV ratio in Samples
dc.sample.mv.ratio.plot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1 ,{
  if(!is.null(dc.batch.alignment.result2$data)){
    temp <- try({mvSamplePlot(data = dc.batch.alignment.result2$data)})
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.sample.mv.ratio.plot.message <- renderText({paste("MV ratio plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.sample.mv.ratio.plot$data <- temp
      output$dc.sample.mv.ratio.plot.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})

output$dc.sample.mv.ratio.plot <- renderPlotly({
  if(is.null(dc.sample.mv.ratio.plot$data)) return(NULL)
  ggplotly(dc.sample.mv.ratio.plot$data,
           tooltip = c(
             "Index",
             "MV.ratio","Sample"),
           source = "dc.sample.mv.ratio.plot")
})



## zero ratio in peaks
dc.peak.zero.ratio.plot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1 ,{
  if(!is.null(dc.batch.alignment.result2$data)){
    temp <- try({zeroPeakPlot(data = dc.batch.alignment.result2$data,
                                                 text = FALSE)})
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.peak.zero.ratio.plot.message <- renderText({paste("Zero ratio plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.peak.zero.ratio.plot$data <- temp
      output$dc.peak.zero.ratio.plot.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})

output$dc.peak.zero.ratio.plot <- renderPlotly({
  if(is.null(dc.peak.zero.ratio.plot$data)) return(NULL)
  ggplotly(dc.peak.zero.ratio.plot$data,
           tooltip = c(
             "Index",
             "zero.ratio","Peak"),
           source = "dc.peak.zero.ratio.plot")
})


## zero ratio in Samples
dc.sample.zero.ratio.plot <- reactiveValues(data =NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1 ,{
  if(!is.null(dc.batch.alignment.result2$data)){
    temp <- try({zeroSamplePlot(data = dc.batch.alignment.result2$data,
                                                     text = FALSE)})
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.sample.zero.ratio.plot.message <- renderText({paste("Zero ratio plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.sample.zero.ratio.plot$data <- temp
      output$dc.sample.zero.ratio.plot.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})

output$dc.sample.zero.ratio.plot <- renderPlotly({
  if(is.null(dc.sample.zero.ratio.plot$data)) return(NULL)
  ggplotly(dc.sample.zero.ratio.plot$data,
           tooltip = c(
             "Index",
             "zero.ratio","Sample"),
           source = "dc.sample.zero.ratio.plot")
})


###group information in da_qa1_ui
dc.group.name <- reactive({
  if(is.null(dc.sample.info.raw$data)) return(NULL)
  unique(dc.sample.info.raw$data[,"group"])
})


output$dc.qa1.group.area <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.qa1.group",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No",
                                            selected_header = "Selected")
    )
  )
})



###RSD distribution
dc.peak.rsd.plot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1 ,{
  if(!is.null(dc.batch.alignment.result2$data) & !is.null(dc.sample.info.raw$data)){
    if(is.null(input$dc.qa1.group)){
      group <-  unique(dc.sample.info.raw$data[,"group"])[1]
    }else{
      group <- input$dc.qa1.group
    }
    temp <- try({rsdDistribution(data = dc.batch.alignment.result2$data,
                                             sample.info = dc.sample.info.raw$data,
                                             group = group,
                                             rsd.tol = 30)})
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.peak.rsd.plot.message <- renderText({paste("RSD plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.peak.rsd.plot$data <- temp
      output$dc.peak.rsd.plot.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})

observeEvent(eventExpr = input$dc.qa1.group, {
  if(!is.null(dc.batch.alignment.result2$data) & !is.null(dc.sample.info.raw$data)){
    temp <- try({rsdDistribution(data = dc.batch.alignment.result2$data,
                                             sample.info = dc.sample.info.raw$data,
                                             group = input$dc.qa1.group,
                                             rsd.tol = 30)})
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.peak.rsd.plot.message <- renderText({paste("RSD plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.peak.rsd.plot$data <- temp
      output$dc.peak.rsd.plot.message <- renderText({""})
      rm(list = c("temp"))
    }
  }

})


dc.peak.rsd.plot.for.report <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1 ,{
  if(!is.null(dc.batch.alignment.result2$data)){
    if(any(unique(dc.sample.info.raw$data[,"group"]) == "QC")){
      temp.group <- "QC"
    }else{
      temp.group <- unique(dc.sample.info.raw$data[,"group"])[1]
    }
    temp <- try({rsdDistribution(data = dc.batch.alignment.result2$data,
                                 sample.info = dc.sample.info.raw$data,
                                 group = temp.group,
                                 rsd.tol = 30)})
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.peak.rsd.plot.for.report.message <- renderText({paste("RSD plot for report: ",info)})
      rm(list = c("temp"))
    }else{
      dc.peak.rsd.plot.for.report$data <- temp
      output$dc.peak.rsd.plot.for.report.message <- renderText({""})
      rm(list = c("temp"))
    }
    rm(list = c("temp.group"))
  }
})


output$dc.peak.rsd.plot <- renderPlotly({
  if(is.null(dc.peak.rsd.plot$data)) return(NULL)
  ggplotly(dc.peak.rsd.plot$data,
           #        tooltip = c(
           # "Index",
           # "zero.ratio","Sample"),
           source = "dc.peak.rsd.plot")
})



#-------------------------------------------------------------------------------
#PCA score plot, should be fixed
dc.qa1.pca.object <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1,{
  if(is.null(dc.batch.alignment.result2$data) | is.null(dc.sample.info.raw$data)) return(NULL)
  dc.peak.table <- dc.batch.alignment.result2$data
  dc.sample.info <- dc.sample.info.raw$data

  temp1 <- try({
    dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"],
                                      colnames(dc.peak.table)), drop = FALSE]
    dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"],
                                     colnames(dc.peak.table)), drop = FALSE]
    ##log
    ##if there are NA is dc.sample, impute it
    if(sum(is.na(dc.sample)) != 0){
      mv.ratio.peak <- apply(dc.sample, 1, function(x) sum(is.na(x))/ncol(dc.sample))
      idx1 <- which(mv.ratio.peak > 0.5)
      if(length(idx1) > 0){
        dc.sample <- dc.sample[-idx1,]
        dc.tags <- dc.tags[-idx1,]
      }
      mv.ratio.sample <- apply(dc.sample, 2, function(x) sum(is.na(x))/nrow(dc.sample))
      idx2 <- which(mv.ratio.sample > 0.5)
      if(length(idx2) > 0){
        dc.sample <- dc.sample[,-idx2]
        dc.sample.info <- dc.sample.info[-idx2,]
      }

      temp <- impute::impute.knn(data = as.matrix(dc.sample), k = 10)
      dc.sample <- as.data.frame(temp$data)
      rm(list = "temp")
    }
    ##remove some peaks with large zero value ratio
    zero.ratio.peak <- apply(dc.sample, 1, function(x) sum(x==0)/ncol(dc.sample))
    idx1 <- which(zero.ratio.peak > 0.5)
    if(length(idx1) > 0){
      dc.sample <- dc.sample[-idx1,]
      dc.tags <- dc.tags[-idx1,]
    }

    dc.sample <- sxtLog(sample = dc.sample, method = "log10")
    ##scale
    dc.sample <- sxtScale(sample = dc.sample,
                          method = "auto",
                          center = TRUE)
    pca.object <- prcomp(data.frame(t(dc.sample)),
                                retx = TRUE,
                                center = FALSE,
                                scale = FALSE)
  })

  if(class(temp1)[1] == "try-error"){
    info <- temp1[[1]]
    output$dc.qa1.pca.object.message <- renderText({paste("PCA analysis: ",info)})
    rm(list = c("temp1"))
  }else{
    dc.qa1.pca.object$data <- pca.object
    output$dc.qa1.pca.object.message <- renderText({""})
    rm(list = c("temp1"))
  }
  rm(list = c("dc.peak.table", "dc.sample", "dc.tags", "pca.object"))

  })


dc.qa1.pca.score.plot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1,{
if(!is.null(dc.qa1.pca.object$data)){
  temp <- try({
    pcaScorePlot(pca.object = dc.qa1.pca.object$data,
                 batch.info = dc.sample.info.raw$data[,c(1,4)],
                 class.info = dc.sample.info.raw$data[,c(1,3)])
  })
  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$dc.qa1.pca.score.plot.message <- renderText({paste("PCA plot: ",info)})
    rm(list = c("temp"))
  }else{
    dc.qa1.pca.score.plot$data <- temp
    output$dc.qa1.pca.score.plot.message <- renderText({""})
    rm(list = c("temp"))
  }

}
})

output$dc.qa1.pca.score.plot <- renderPlotly({
  if(!is.null(dc.qa1.pca.score.plot$data)){
    plotly::ggplotly(dc.qa1.pca.score.plot$data, source = "temp")
  }
})


dc.qa1.pca.score.plot.for.report <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1 ,{
  if(!is.null(dc.sample.info.raw$data)){
    temp <- try({pcaScorePlot(pca.object = dc.qa1.pca.object$data,
                                                          batch.info = dc.sample.info.raw$data[,c(1,4)],
                                                          class.info = dc.sample.info.raw$data[,c(1,3)])})
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.qa1.pca.score.plot.for.report.message <- renderText({paste("PCA plot for report: ",info)})
      rm(list = c("temp"))
    }else{
      dc.qa1.pca.score.plot.for.report$data <- temp
      output$dc.qa1.pca.score.plot.for.report.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})

###Boxplots for QC samples
dc.qc.int.boxplot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1 ,{
  if(!is.null(dc.batch.alignment.result2$data)){
    temp <- try({
      qcIntBoxplot(data = dc.batch.alignment.result2$data,
                    sample.info = dc.sample.info.raw$data)
      })
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.qc.int.boxplot.message <- renderText({paste("QC intensity boxplot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.qc.int.boxplot$data <- temp
      output$dc.qc.int.boxplot.message <- renderText({""})
      rm(list = c("temp"))
    }

  }
})


output$dc.qc.int.boxplot <- renderPlotly({
  if(is.null(dc.qc.int.boxplot$data)) return(NULL)
  ggplotly(dc.qc.int.boxplot$data,
           source = "dc.qc.int.boxplot")
})


###QC correlation
dc.qc.cor.plot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1,{
  if(!is.null(dc.batch.alignment.result2$data) & !is.null(dc.sample.info.raw$data)){
    temp <- try({qcCorPlot(data = dc.batch.alignment.result2$data,
                           sample.info = dc.sample.info.raw$data)})
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.qc.cor.plot.message <- renderText({paste("QC cor plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.qc.cor.plot$data <- temp
      output$dc.qc.cor.plot.message <- renderText({""})
      rm(list = c("temp"))
    }

  }
})

output$dc.qc.cor.plot <- renderPlotly({
  if(!is.null(dc.qc.cor.plot$data)){
    ggplotly(dc.qc.cor.plot$data)
  }
})

dc.qc.cor <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.ba.2.dc.qa1,{
  if(!is.null(dc.batch.alignment.result2$data) & !is.null(dc.sample.info.raw$data)){
    temp <- try({qcCor(data = dc.batch.alignment.result2$data,
                            sample.info = dc.sample.info.raw$data)})
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.qc.cor.message <- renderText({paste("QC cor: ",info)})
      rm(list = c("temp"))
    }else{
      dc.qc.cor$data <- temp
      output$dc.qc.cor.message <- renderText({""})
      rm(list = c("temp"))
    }

  }
})



output$dc.qc.cor <- DT::renderDataTable(
  DT::datatable(dc.qc.cor$data,
                class = 'cell-border stripe',
                editable = FALSE,
                # selection = "single",
                selection = list(mode = 'single', target = 'row'),
                # caption = 'Raw data containing MVs',
                escape = FALSE,
                # filter = "top",
                filter = "none",
                rownames = TRUE,
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
                        buttons = list(list(extend="csv",filename = "QC.sample.correlation"),
                                       list(extend='excel',filename = "QC.sample.correlation")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)





##jump to MV processing page from data quality assessment page before cleaning
observeEvent(input$dc.qa1.2.dc.mv, {
  updateTabsetPanel(session, "DCtab",
                    selected = "DCmvProcessing")

})





















#-------------------------------------------------------------------------------
#MV processing

dc.ms1.after.mv <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.mv.submit.button,{
  if(sum(is.na(dc.batch.alignment.result2$data)) != 0){
    withProgress(message = 'Missing value processing...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   temp <- try({
                     lapply(1:1, function(idx){
                     incProgress(1/length(idx),
                                 detail = "This may take a while")
                       dc.sample <-
                         dc.batch.alignment.result2$data[,match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.batch.alignment.result2$data))]
                       dc.tags <-
                         dc.batch.alignment.result2$data[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.batch.alignment.result2$data))]
                     mvImputation(sample = dc.sample,
                                  tags = dc.tags,
                                  sample.info = dc.sample.info.raw$data,
                                  mv.peak.remove.tol = input$dc.mv.peak.remove.tol,
                                  method = input$DCimputationMethod,
                                  rowmax = input$rowmax/100,
                                  colmax = input$colmax/100,
                                  k = input$k,
                                  ntree = input$mvNtree, replace = input$mvReplace,
                                  bpca.npcs = input$bpca.nPcs,
                                  ppca.npcs = input$ppca.npcs,
                                  svd.npcs = input$svd.npcs)
                   })[[1]]
                   })
                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$dc.ms1.after.mv.message <- renderText({paste("MV processing: ",info)})
                     rm(list = c("temp"))
                   }else{
                     dc.ms1.after.mv$data <- temp
                     output$dc.ms1.after.mv.message <- renderText({""})
                     rm(list = c("temp"))
                   }

                   rm(list = c("dc.sample", "dc.tags"))
                 })
  }else{
    dc.ms1.after.mv$data <- dc.batch.alignment.result2$data
  }
})


observeEvent(eventExpr = input$dc.mv.submit.button,{
if(!is.null(dc.ms1.after.mv$data) & !is.null(dc.batch.alignment.result2$data)){
temp <- try({
  name1 <- dc.batch.alignment.result2$data[,'name']
  name2 <- dc.ms1.after.mv$data[,'name']
  delete.name <- setdiff(name1, name2)
  if(length(delete.name) > 0){
    info <- paste("There are ", length(delete.name), " peaks are removed from the dataset: ",
                  paste(delete.name, collapse = ";"), sep = "")
    output$mv.processing.message <- renderText({info})
  }else{
    output$mv.processing.message <- renderText({"No peaks are removed from the dataset."})
  }
})

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$mv.processing.message <- renderText({paste("MV processing: ",info)})
    rm(list = c("temp"))
  }
  rm(list = c("name1", "name2", "delete.name"))
}
})


######save parameters of Missing value processing
dc.mv.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.mv.submit.button, {
  temp <- try({
    dcMVparams <- DCmvParam(
      input$dc.mv.peak.remove.tol,
      input$DCimputationMethod,
      input$k,
      input$mvNtree,
      input$mvReplace,
      input$bpca.nPcs,
      input$ppca.nPcs,
      input$svd.nPcs,
      input$rowmax,
      input$colmax)
    dc.mv.params$data <- dcMVparams
    save(dcMVparams, file = file.path("user_data", user_input$username,
                                      input$DCprojectID,
                                      "data_cleaning",
                                      "dcMVparams"))
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$dc.mv.params.message <- renderText({paste("MV processing parameters: ",info)})
    rm(list="temp")
  }else{
    output$dc.mv.params.message <- renderText({""})
    rm(list="temp")
  }

})


#MS1 peak table after MV processing
output$dc.ms1.after.mv <- DT::renderDataTable(
  DT::datatable(dc.ms1.after.mv$data,
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
                        buttons = list(list(extend="csv",filename = "MS1.peak.table.after.mv.processing"),
                                       list(extend='excel',filename = "MS1.peak.table.after.mv.processing")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)


###Error if there are no data after MV processing, warninguser click Next beforeclick submit
observeEvent(eventExpr = input$dc.mv.2.dc.zero, {
  if(is.null(dc.ms1.after.mv$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click Submit first.",
                           type = "error")
  }

})

##jump to Data normalization page from missing value processing page
observeEvent(input$dc.mv.2.dc.zero, {
  if(!is.null(dc.ms1.after.mv$data)){
    updateTabsetPanel(session, "DCtab",
                      selected = "DCzeroProcessing")
  }
})



















#-------------------------------------------------------------------------------
#zero value processing
dc.ms1.after.zero <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.zero.submit.button,{
  withProgress(message = 'Zero value processing...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 temp <- try({
                   lapply(1:1, function(idx){
                   incProgress(1/length(idx),
                               detail = "This may take a while")
                   dc.sample <-
                     dc.ms1.after.mv$data[,match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.ms1.after.mv$data))]
                   dc.tags <-
                     dc.ms1.after.mv$data[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.ms1.after.mv$data))]

                   zeroFilter(sample = dc.sample, tags = dc.tags,
                              sample.info = dc.sample.info.raw$data,
                              zero.peak.remove.tol = input$dc.zero.peak.remove.tol)
                 })[[1]]
                 })

                 if(class(temp)[1] == "try-error"){
                   info <- temp[[1]]
                   output$dc.ms1.after.zero.message <- renderText({paste("Zero processing: ",info)})
                   rm(list = c("temp"))
                 }else{
                   dc.ms1.after.zero$data <- temp
                   output$dc.ms1.after.zero.message <- renderText({""})
                   rm(list = c("temp"))
                 }
                 rm(list = c("dc.sample", "dc.tags"))
               })
})


######save parameters of zero value processing
dc.zero.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.zero.submit.button, {
  temp <- try({
    dcZeroParams <- data.frame("Parameter" = "Peaks will be removed if zero ratio more than %",
                               "Setting" = input$dc.zero.peak.remove.tol,
                               stringsAsFactors = FALSE)
    dc.zero.params$data <- dcZeroParams
    save(dcZeroParams, file = file.path("user_data", user_input$username,
                                        input$DCprojectID,
                                        "data_cleaning",
                                        "dcZeroParams"))
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$dc.zero.params.message <- renderText({paste("Zero processing parameters: ",info)})
    rm(list="temp")
  }else{
    output$dc.zero.params.message <- renderText({""})
    rm(list="temp")
  }


})



observeEvent(eventExpr = input$dc.zero.submit.button,{
  if(!is.null(dc.ms1.after.mv$data) & !is.null(dc.ms1.after.zero$data)){
    temp <- try({
      name1 <- dc.ms1.after.mv$data[,'name']
      name2 <- dc.ms1.after.zero$data[,'name']
      delete.name <- setdiff(name1, name2)
      if(length(delete.name) > 0){
        info <- paste("There are ", length(delete.name), " peaks are removed from the dataset: ",
                      paste(delete.name, collapse = ";"), sep = "")
        output$zero.processing.message <- renderText({info})
      }else{
        output$zero.processing.message <- renderText({"No peaks are removed from the dataset."})
      }
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$zero.processing.message <- renderText({paste("MV processing: ",info)})
      rm(list = c("temp"))
    }
    rm(list = c("name1", "name2", "delete.name"))
  }
})



#MS1 peak table after zero processing
output$dc.ms1.after.zero <- DT::renderDataTable(
  DT::datatable(dc.ms1.after.zero$data,
                class = 'cell-border stripe',
                editable = FALSE,
                # selection = "single",
                selection = list(mode = 'single', target = 'row'),
                # caption = 'Raw data containing zeros',
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
                        buttons = list(list(extend="csv",filename = "MS1.peak.table.after.zero.processing"),
                                       list(extend='excel',filename = "MS1.peak.table.after.zero.processing")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)



###Error if there are no data after zero processing, warning user click Next beforeclick submit
observeEvent(eventExpr = input$dc.zero.2.dc.dn, {
  if(is.null(dc.ms1.after.zero$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click Submit first.",
                           type = "error")
  }

})

##jump to Data normalization page from zero value processing page
observeEvent(input$dc.zero.2.dc.dn, {
  if(!is.null(dc.ms1.after.zero$data)){
    updateTabsetPanel(session, "DCtab",
                      selected = "DCdn")
  }
})


















#-------------------------------------------------------------------------------
#data normalization
######save parameters of data normalization
dc.dn.params <- reactiveValues(data = NULL)
observeEvent(input$dc.dn.submit.button, {
  temp <- try({
    dcDNparams <- DCdnParam(
      input$DCdnHasQC,
      input$normalizationMethod1,
      input$normalizationMethod2,
      input$loess.kepp.dimension,
      input$parameter.optimization,
      input$begin.end,
      input$loess.step,
      input$svr.kepp.dimension,
      input$svr.multiple)

    dc.dn.params$data <- dcDNparams
    save(dcDNparams, file = file.path("user_data", user_input$username,
                                      input$DCprojectID,
                                      "data_cleaning",
                                      "dcDNparams"))
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$dc.dn.params.message <- renderText({paste("Data normalization parameters: ",info)})
    rm(list="temp")
  }else{
    output$dc.dn.params.message <- renderText({""})
    rm(list="temp")
  }

})

DCwithQC <- reactive({
  dc.sample.info <- dc.sample.info.raw$data
  group <- unique(dc.sample.info[,"group"])
  if(all(group != "QC")) return(0)
  return(1)
})

###error information if there are no QC samples in your data
observeEvent(eventExpr = input$dc.dn.submit.button, {
  if(input$DCdnHasQC == "hasQC" & DCwithQC() == 0){
    shinyalert::shinyalert("Error", "There are no QC samples in your data.",
                           type = "error")
  }
})



dc.ms1.dn <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.dn.submit.button,{
  if(is.null(dc.ms1.after.zero$data)) return(NULL)
  dc.peak.table <- dc.ms1.after.zero$data
  dc.sample.info <- dc.sample.info.raw$data

  dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"], colnames(dc.peak.table)), drop = FALSE]
  dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"], colnames(dc.peak.table)), drop = FALSE]
  # cat("test1\n")
  temp <- try({
    if(input$DCdnHasQC == "noQC"){
      # cat("test2\n")
      dataNormalization(sample = dc.sample,
                        tags = dc.tags,
                        sample.info = dc.sample.info,
                        method = input$normalizationMethod2)
                        # cat("test3\n")
    }else{
      if(input$DCdnHasQC == "hasQC"){
        if(DCwithQC() == 0) return(NULL)
        dataNormalization(sample = dc.sample,
                          tags = dc.tags,
                          sample.info = dc.sample.info,
                          method = input$normalizationMethod1,
                          optimization = input$parameter.optimization,
                          begin = input$begin.end[1],
                          end = input$begin.end[2],
                          step = input$loess.step,
                          dimension1 = input$loess.kepp.dimension,
                          dimension2 = input$svr.kepp.dimension,
                          multiple = input$svr.multiple)
      }
    }
  })
  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$dc.ms1.dn.message <- renderText({paste("Data normalization: ",info)})
    rm(list = c("temp"))
  }else{
    dc.ms1.dn$data <- temp
    output$dc.ms1.dn.message <- renderText({""})
    rm(list = c("temp"))
  }
  rm(list = c("dc.peak.table", "dc.sample.info", "dc.sample", "dc.tags"))

})



output$dc.ms1.dn <- DT::renderDataTable(
  DT::datatable(dc.ms1.dn$data,
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
                  dom = 'BRrltpi',
                  # autoWidth = TRUE,
                  lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                  ColReorder = TRUE,
                  buttons =
                    list(
                      list(
                        extend = 'collection',
                        buttons = list(list(extend="csv",filename = "Data.after.normalization"),
                                       list(extend='excel',filename = "Data.after.normalization")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)


###information for single peak before normalization
observeEvent(eventExpr = input$DCdnShowPeak1, {
  peak.table <- dc.ms1.after.zero$data
  sample.info <- dc.sample.info.raw$data
  sample <- peak.table[,match(sample.info[,"sample.name"], colnames(peak.table)), drop = FALSE]
  tags <- peak.table[,-match(sample.info[,"sample.name"], colnames(peak.table)), drop = FALSE]

  if (is.null(input$dc.ms1.dn_rows_selected)) {
    dc.single.peak.plot.dn1 <- function(){
      par( mar = c(5,5,4,2))
      ggplot2::ggplot(data = data.frame("x" = 1, "y" = 1, stringsAsFactors = FALSE), ggplot2::aes(x = x, y = y)) +
        ggplot2::theme_bw() +
        ggplot2::annotate(geom = "text", x = 1, y = 1, label = "Plese select a peak")
    }

  }else{
    peak.idx <- as.numeric(input$dc.ms1.dn_rows_selected)
    dc.single.peak.plot.dn1 <- function(){
      par(mar = c(5,5,4,2))
      plot <- dnSinglePeak(sample = sample, tags = tags,
                           sample.info = sample.info, peak.idx = peak.idx,
                           alpha = 0.8, size = 2, text = FALSE,
                           title = paste(tags[peak.idx, "name"])
      )
      plot
    }

    # rm(list = c("peak.table", "sample.info", "sample", "tags"))
  }


  output$dc.single.peak.plot.dn1 <- renderPlotly({
    ggplotly(dc.single.peak.plot.dn1(),
             tooltip = c("Sample", "Injection.order", "Intensity"),
             source = "dc.single.peak.plot.dn")
  })

  output$dc.single.peak.plot.dn1.download <- downloadHandler(
    filename = function() { "Peak.information.pdf" },
    content = function(file) {
      device <- function(..., width , height) {
        grDevices::pdf(..., width = input$dc.single.peak.plot.dn1.width,
                       height = input$dc.single.peak.plot.dn1.height)
      }
      ggsave(file, plot = dc.single.peak.plot.dn1(), device = device)
    }, contentType = "pdf")

})




###information for single peak after normalization
observeEvent(eventExpr = input$DCdnShowPeak2, {
  peak.table <- dc.ms1.dn$data
  sample.info <- dc.sample.info.raw$data
  sample <- peak.table[,match(sample.info[,"sample.name"], colnames(peak.table)), drop = FALSE]
  tags <- peak.table[,-match(sample.info[,"sample.name"], colnames(peak.table)), drop = FALSE]

  if (is.null(input$dc.ms1.dn_rows_selected)) {
    dc.single.peak.plot.dn2 <- function(){
      par( mar = c(5,5,4,2))
      ggplot2::ggplot(data = data.frame("x" = 1, "y" = 1, stringsAsFactors = FALSE), ggplot2::aes(x = x, y = y)) +
        ggplot2::theme_bw() +
        ggplot2::annotate(geom = "text", x = 1, y = 1, label = "Plese select a peak")
    }

  }else{
    peak.idx <- as.numeric(input$dc.ms1.dn_rows_selected)
    dc.single.peak.plot.dn2 <- function(){
      par(mar = c(5,5,4,2))
      plot <- dnSinglePeak(sample = sample, tags = tags,
                           sample.info = sample.info, peak.idx = peak.idx,
                           alpha = 0.8, size = 2, text = FALSE,
                           title = paste(tags[peak.idx, "name"])
      )
      plot
    }

    # rm(list = c("peak.table", "sample.info", "sample", "tags"))
  }


  output$dc.single.peak.plot.dn2 <- renderPlotly({
    ggplotly(dc.single.peak.plot.dn2(),
             tooltip = c("Sample", "Injection.order", "Intensity"),
             source = "dc.single.peak.plot.dn2")
  })

  output$dc.single.peak.plot.dn2.download <- downloadHandler(
    filename = function() { "Peak.information.after.normalization.pdf" },
    content = function(file) {
      device <- function(..., width , height) {
        grDevices::pdf(..., width = input$dc.single.peak.plot.dn2.width,
                       height = input$dc.single.peak.plot.dn2.height)
      }
      ggsave(file, plot = dc.single.peak.plot.dn2(), device = device)
    }, contentType = "pdf")

})



##group information for selection
dc.group.name <- reactive({
  dc.group.name <- unique(dc.sample.info.raw$data[,"group"])
  if(any(dc.group.name == "QC")){
    dc.group.name <- c("QC", dc.group.name[-which(dc.group.name == "QC")])
  }
  dc.group.name
})

output$dc.dn.group.area1 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.dn.group1",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.dn.group.area2 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.dn.group2",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.dn.group.area3 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.dn.group3",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.dn.group.area4 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.dn.group4",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


###Box plot beforedata normalization
# observeEvent(eventExpr = input$dc.dn.submit.button, {
dc.boxplot.dn1 <- function(){
  par(mar = c(5,5,4,2))
  if(is.null(dc.ms1.after.zero$data)) return(NULL)
  if(is.null(input$dc.dn.group1)){
    group <- unique(dc.sample.info.raw$data[,"group"])
    if(any(group == "QC")){
      group <- "QC"
    }
  }else{
    group <- input$dc.dn.group1
  }
  dc.boxplot.dn1 <- dnSampleBoxplot(sample = dc.ms1.after.zero$data,
                                    sample.info = dc.sample.info.raw$data,
                                    # group = input$dc.dn.group1,
                                    group = group,
                                    title = "before normalization"
  )
  dc.boxplot.dn1

}

output$dc.boxplot.dn1 <- renderPlotly({
  ggplotly(dc.boxplot.dn1(), tooltip = c("Sample"),
           source = "dc.boxplot.dn1")
})

# })


###Box plot after data normalization
# observeEvent(eventExpr = input$dc.dn.submit.button, {
dc.boxplot.dn2 <- function(){
  par(mar = c(5,5,4,2))
  if(is.null(dc.ms1.dn$data)) return(NULL)

  if(is.null(input$dc.dn.group2)){
    group <- unique(dc.sample.info.raw$data[,"group"])
    if(any(group == "QC")){
      group <- "QC"
    }
  }else{
    group <- input$dc.dn.group2
  }
  dc.boxplot.dn2 <- dnSampleBoxplot(sample = dc.ms1.dn$data,
                                    sample.info = dc.sample.info.raw$data,
                                    # group = input$dc.dn.group1,
                                    group = group,
                                    title = "After normalization"
  )
  dc.boxplot.dn2

}

output$dc.boxplot.dn2 <- renderPlotly({
  ggplotly(dc.boxplot.dn2(), tooltip = c("Sample"),
           source = "dc.boxplot.dn2")
})

# })


# download plot
output$dc.boxplot.dn1.download <- downloadHandler(
  filename = function() {"Boxplot.befor.data.normalization.pdf"},
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.boxplot.dn1.width,
                     height = input$dc.boxplot.dn1.height)
    }
    ggsave(file, plot = dc.boxplot.dn1(), device = device)
  }, contentType = "pdf")


output$dc.boxplot.dn2.download <- downloadHandler(
  filename = function() {"Boxplot.after.data.normalization.pdf"},
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.boxplot.dn2.width,
                     height = input$dc.boxplot.dn2.height)
    }
    ggsave(file, plot = dc.boxplot.dn2(), device = device)
  }, contentType = "pdf")





###RSD before and after
# observeEvent(eventExpr = input$DCdnShowRSD, {
dc.rsd.comparison <- function(){
  par(mar = c(5,5,4,2))
  if(is.null(dc.ms1.dn$data)) return(NULL)
  if(is.null(input$dc.dn.group3)){
    group <- unique(dc.sample.info.raw$data[,"group"])
    if(any(group == "QC")){
      group <- "QC"
    }
  }else{
    group <- input$dc.dn.group3
  }

  tags <- dc.ms1.dn$data[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.ms1.dn$data)), drop = FALSE]

  dc.rsd.comparison <- rsdBeforeAfter(sample1 = dc.ms1.after.zero$data,
                                       sample2 = dc.ms1.dn$data,
                                       tags = tags,
                                       sample.info = dc.sample.info.raw$data,
                                       alpha = 0.5, size = 2,
                                       group = group,
                                       title = "RSD comparison")
  rm(list = c("tags"))
  dc.rsd.comparison

}

output$dc.rsd.comparison <- renderPlotly({
  ggplotly(dc.rsd.comparison(),
           tooltip = c("Peak", "Before", "After", "Change"),
           source = "dc.rsd.comparison")
})

# })


# download plot
output$dc.rsd.comparison.download <- downloadHandler(
  filename = function() {"RSD.comparison.pdf"},
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.rsd.comparison.width,
                     height = input$dc.rsd.comparison.height)
    }
    ggsave(file, plot = dc.rsd.comparison(), device = device)
  }, contentType = "pdf")






###RSD distribution before and after normalization
observeEvent(eventExpr = input$DCdnShowRSD2, {
  dc.rsd.distribution1 <- function(){
    par(mar = c(5,5,4,2))
    if(is.null(dc.ms1.after.zero$data)) return(NULL)
    if(is.null(input$dc.dn.group4)){
      group <- unique(dc.sample.info.raw$data[,"group"])
      if(any(group == "QC")){
        group <- "QC"
      }
    }else{
      group <- input$dc.dn.group4
    }

    tags <- dc.ms1.after.zero$data[,-match(dc.sample.info.raw$data[,"sample.name"],
                                           colnames(dc.ms1.after.zero$data)), drop = FALSE]

    dc.rsd.distribution1 <- rsdDistribution2(sample = dc.ms1.after.zero$data,
                                             rsd.tol = input$dc.rsd.distribution.tol,
                                             tags = tags,
                                             sample.info = dc.sample.info.raw$data,
                                             alpha = 0.5, size = 2,
                                             group = group,
                                             title = "Before normalization")
    rm(list = c("tags"))
    dc.rsd.distribution1

  }

  output$dc.rsd.distribution1 <- renderPlotly({
    ggplotly(dc.rsd.distribution1(),
             tooltip = c("Peak", "RSD", "Group"),
             source = "dc.rsd.distribution1")
  })
})


# download plot
output$dc.rsd.distribution1.download <- downloadHandler(
  filename = function() {"RSD.distributation.before.normalization.pdf"},
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = 7,
                     height = 7)
    }
    ggsave(file, plot = dc.rsd.distribution1(), device = device)
  }, contentType = "pdf")



observeEvent(eventExpr = input$DCdnShowRSD2, {
  dc.rsd.distribution2 <- function(){
    par(mar = c(5,5,4,2))
    if(is.null(dc.ms1.dn$data)) return(NULL)
    if(is.null(input$dc.dn.group4)){
      group <- unique(dc.sample.info.raw$data[,"group"])
      if(any(group == "QC")){
        group <- "QC"
      }
    }else{
      group <- input$dc.dn.group4
    }

    tags <- dc.ms1.dn$data[,-match(dc.sample.info.raw$data[,"sample.name"],
                                   colnames(dc.ms1.dn$data)), drop = FALSE]

    dc.rsd.distribution2 <- rsdDistribution2(sample = dc.ms1.dn$data,
                                             rsd.tol = input$dc.rsd.distribution.tol,
                                             tags = tags,
                                             sample.info = dc.sample.info.raw$data,
                                             alpha = 0.5, size = 2,
                                             group = group,
                                             title = "After normalization")
    rm(list = c("tags"))
    dc.rsd.distribution2

  }

  output$dc.rsd.distribution2 <- renderPlotly({
    ggplotly(dc.rsd.distribution2(),
             tooltip = c("Peak", "RSD", "Group"),
             source = "dc.rsd.distribution2")
  })

})


# download plot
output$dc.rsd.distribution2.download <- downloadHandler(
  filename = function() {"RSD.distributation.after.normalization.pdf"},
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = 7,
                     height = 7)
    }
    ggsave(file, plot = dc.rsd.distribution2(), device = device)
  }, contentType = "pdf")


###Error if there are no data normalization, warninguser click Next before click submit
observeEvent(eventExpr = input$dc.dn.2.di, {
  if(is.null(dc.ms1.dn$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click Submit first.",
                           type = "error")
  }

})

##jump to Data normalization page from zero value processing page
observeEvent(input$dc.dn.2.di, {
  if(!is.null(dc.ms1.dn$data)){
    updateTabsetPanel(session, "DCtab",
                      selected = "DCdi")
  }
})



















#-------------------------------------------------------------------------------
#data integration
###error information if there are no QC samples in your data
observeEvent(eventExpr = input$dc.di.submit.button, {
  if(input$DCdiHasQC == "hasQC" & DCwithQC() == 0){
    shinyalert::shinyalert("Error", "There are no QC samples in your data.",
                           type = "error")
  }
})

##save parameter
dc.di.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.di.submit.button,{
  temp <- try({
    dcDIparams <- DCdiParam(DCdiHasQC = input$DCdiHasQC,
                            integrationMethod1 = input$integrationMethod1,
                            integrationMethod2 = input$integrationMethod2)

    dc.di.params$data <- dcDIparams

    save(dcDIparams, file = file.path("user_data", user_input$username,
                                      input$DCprojectID,
                                      "data_cleaning",
                                      "dcDIparams"))
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$dc.di.params.message <- renderText({paste("Data integration parameters: ",info)})
    rm(list = "temp")
  }else{
    output$dc.di.params.message <- renderText({""})
    rm(list = "temp")
  }


})

##data integration
dc.ms1.di <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.di.submit.button,{
  withProgress(message = 'Data integration...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 temp <- try({
                   lapply(1:1, function(idx){
                     incProgress(1/length(idx),
                                 detail = "This may take a while")
                     dc.peak.table <- dc.ms1.dn$data
                     dc.sample.info <- dc.sample.info.raw$data
                     dc.sample <- dc.peak.table[,match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.peak.table)), drop = FALSE]
                     dc.tags <- dc.peak.table[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.peak.table)), drop = FALSE]
                     if(input$DCdiHasQC == "hasQC"){
                       method <- input$integrationMethod1
                     }else{
                       method <- input$integrationMethod2
                     }

                     if(method != "no"){
                       temp.data <- dataIntegration(sample = dc.sample,
                                                         sample.info = dc.sample.info,
                                                         method = method)
                       data.frame(dc.tags, temp.data, stringsAsFactors = FALSE)
                     }else{
                       dc.ms1.dn$data
                     }
                   })[[1]]
                 })

                 if(class(temp)[1] == "try-error"){
                   info <- temp[[1]]
                   output$dc.ms1.di.message <- renderText({paste("Data integration: ",info)})
                   rm(list = c("temp"))
                 }else{
                   dc.ms1.di$data <- temp
                   output$dc.ms1.di.message <- renderText({""})
                   rm(list = c("temp"))
                 }

                 rm(list = c("dc.peak.table", "dc.sample.info", "dc.sample", "dc.tags"))
               })
})


output$dc.ms1.di <- DT::renderDataTable(
  DT::datatable(dc.ms1.di$data,
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
                  dom = 'BRrltpi',
                  # autoWidth = TRUE,
                  lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                  ColReorder = TRUE,
                  buttons =
                    list(
                      list(
                        extend = 'collection',
                        buttons = list(list(extend="csv",filename = "Data.after.integration"),
                                       list(extend='excel',filename = "Data.after.integration")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)



###information for single peak before integration
observeEvent(eventExpr = input$DCdiShowPeak1, {
  peak.table <- dc.ms1.dn$data
  sample.info <- dc.sample.info.raw$data
  sample <- peak.table[,match(sample.info[,1], colnames(peak.table)), drop = FALSE]
  tags <- peak.table[,-match(sample.info[,1], colnames(peak.table)), drop = FALSE]

  if (is.null(input$dc.ms1.di_rows_selected)) {
    dc.single.peak.plot.di1 <- function(){
      par( mar = c(5,5,4,2))
      ggplot2::ggplot(data = data.frame("x" = 1, "y" = 1, stringsAsFactors = FALSE), ggplot2::aes(x = x, y = y)) +
        ggplot2::theme_bw() +
        ggplot2::annotate(geom = "text", x = 1, y = 1, label = "Plese select a peak")
    }

  }else{
    peak.idx <- as.numeric(input$dc.ms1.di_rows_selected)
    dc.single.peak.plot.di1 <- function(){
      par(mar = c(5,5,4,2))
      plot <- dnSinglePeak(sample = sample, tags = tags,
                           sample.info = sample.info, peak.idx = peak.idx,
                           alpha = 0.8, size = 2, text = FALSE,
                           title = paste(tags[peak.idx, "name"])
      )
      plot
    }

    # rm(list = c("peak.table", "sample.info", "sample", "tags"))
  }

  output$dc.single.peak.plot.di1 <- renderPlotly({
    ggplotly(dc.single.peak.plot.di1(),
             tooltip = c("Sample", "Injection.order", "Batch"),
             source = "dc.single.peak.plot.di")
  })

  output$dc.single.peak.plot.di1.download <- downloadHandler(
    filename = function() { "Peak.information.pdf" },
    content = function(file) {
      device <- function(..., width , height) {
        grDevices::pdf(..., width = input$dc.single.peak.plot.di1.width,
                       height = input$dc.single.peak.plot.di1.height)
      }
      ggsave(file, plot = dc.single.peak.plot.di1(), device = device)
    }, contentType = "pdf")

})


###information for single peak after integration
observeEvent(eventExpr = input$DCdiShowPeak2, {
  peak.table <- dc.ms1.di$data
  sample.info <- dc.sample.info.raw$data
  sample <- peak.table[,match(sample.info[,1], colnames(peak.table)), drop = FALSE]
  tags <- peak.table[,-match(sample.info[,1], colnames(peak.table)), drop = FALSE]

  if (is.null(input$dc.ms1.di_rows_selected)) {
    dc.single.peak.plot.di2 <- function(){
      par( mar = c(5,5,4,2))
      ggplot2::ggplot(data = data.frame("x" = 1, "y" = 1, stringsAsFactors = FALSE), ggplot2::aes(x = x, y = y)) +
        ggplot2::theme_bw() +
        ggplot2::annotate(geom = "text", x = 1, y = 1, label = "Plese select a peak")
    }

  }else{
    peak.idx <- as.numeric(input$dc.ms1.di_rows_selected)
    dc.single.peak.plot.di2 <- function(){
      par(mar = c(5,5,4,2))
      plot <- dnSinglePeak(sample = sample, tags = tags,
                           sample.info = sample.info, peak.idx = peak.idx,
                           alpha = 0.8, size = 2, text = FALSE,
                           title = paste(tags[peak.idx, "name"])
      )
      plot
    }

    # rm(list = c("peak.table", "sample.info", "sample", "tags"))
  }


  output$dc.single.peak.plot.di2 <- renderPlotly({
    ggplotly(dc.single.peak.plot.di2(),
             tooltip = c("Sample", "Injection.order", "Intensity"),
             source = "dc.single.peak.plot.di2")
  })

  output$dc.single.peak.plot.di2.download <- downloadHandler(
    filename = function() { "Peak.information.after.integration.pdf" },
    content = function(file) {
      device <- function(..., width , height) {
        grDevices::pdf(..., width = input$dc.single.peak.plot.di2.width,
                       height = input$dc.single.peak.plot.di2.height)
      }
      ggsave(file, plot = dc.single.peak.plot.di2(), device = device)
    }, contentType = "pdf")

})



##group information for selection
dc.group.name <- reactive({
  dc.group.name <- unique(dc.sample.info.raw$data[,"group"])
  if(any(dc.group.name == "QC")){
    dc.group.name <- c("QC", dc.group.name[-which(dc.group.name == "QC")])
  }
  dc.group.name
})

output$dc.di.group.area1 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.di.group1",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.di.group.area2 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.di.group2",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.di.group.area3 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.di.group3",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.di.group.area4 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.di.group4",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No selected:",
                                            selected_header = "Selected:")
    )
  )
})


###Box plot before data integration
# observeEvent(eventExpr = input$dc.di.submit.button, {
dc.boxplot.di1 <- function(){
  par(mar = c(5,5,4,2))
  if(is.null(dc.ms1.dn$data)) return(NULL)
  if(is.null(input$dc.di.group1)){
    group <- unique(dc.sample.info.raw$data[,"group"])
    if(any(group == "QC")){
      group <- "QC"
    }
  }else{
    group <- input$dc.di.group1
  }
  dc.boxplot.di1 <- diSampleBoxplot(sample = dc.ms1.dn$data,
                                    sample.info = dc.sample.info.raw$data,
                                    # group = input$dc.di.group1,
                                    group = group,
                                    title = "before integration"
  )
  dc.boxplot.di1

}

output$dc.boxplot.di1 <- renderPlotly({
  ggplotly(dc.boxplot.di1(), tooltip = c("Sample"),
           source = "dc.boxplot.di1")
})

# })


###Box plot after data integration
# observeEvent(eventExpr = input$dc.di.submit.button, {
dc.boxplot.di2 <- function(){
  par(mar = c(5,5,4,2))
  if(is.null(dc.ms1.di$data)) return(NULL)

  if(is.null(input$dc.di.group2)){
    group <- unique(dc.sample.info.raw$data[,"group"])
    if(any(group == "QC")){
      group <- "QC"
    }
  }else{
    group <- input$dc.di.group2
  }
  dc.boxplot.di2 <- diSampleBoxplot(sample = dc.ms1.di$data,
                                    sample.info = dc.sample.info.raw$data,
                                    # group = input$dc.di.group1,
                                    group = group,
                                    title = "After integration"
  )
  dc.boxplot.di2

}

output$dc.boxplot.di2 <- renderPlotly({
  ggplotly(dc.boxplot.di2(), tooltip = c("Sample"),
           source = "dc.boxplot.di2")
})

# })


# download plot
output$dc.boxplot.di1.download <- downloadHandler(
  filename = function() { "Boxplot.befor.data.integration.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.boxplot.di1.width,
                     height = input$dc.boxplot.di1.height)
    }
    ggsave(file, plot = dc.boxplot.di1(), device = device)
  }, contentType = "pdf")


output$dc.boxplot.di2.download <- downloadHandler(
  filename = function() { "Boxplot.after.data.integration.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.boxplot.di2.width,
                     height = input$dc.boxplot.di2.height)
    }
    ggsave(file, plot = dc.boxplot.di2(), device = device)
  }, contentType = "pdf")



###RSD before and after
# observeEvent(eventExpr = input$DCdiShowRSD, {
dc.di.rsd.comparison <- function(){
  par(mar = c(5,5,4,2))
  if(is.null(dc.ms1.di$data)) return(NULL)
  if(is.null(input$dc.di.group3)){
    group <- unique(dc.sample.info.raw$data[,"group"])
    if(any(group == "QC")){
      group <- "QC"
    }
  }else{
    group <- input$dc.di.group3
  }

  tags <- dc.ms1.di$data[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.ms1.di$data)), drop = FALSE]

  dc.di.rsd.comparison <- rsdBeforeAfter(sample1 = dc.ms1.dn$data,
                                          sample2 = dc.ms1.di$data,
                                          tags = tags,
                                          sample.info = dc.sample.info.raw$data,
                                          alpha = 0.5, size = 2,
                                          group = group,
                                          title = "RSD comparison"
  )
  dc.di.rsd.comparison

}

output$dc.di.rsd.comparison <- renderPlotly({
  ggplotly(dc.di.rsd.comparison(),
           tooltip = c("Peak", "Before", "After", "Change"),
           source = "dc.di.rsd.comparison")
})

# download plot
output$dc.di.rsd.comparison.download <- downloadHandler(
  filename = function() { "RSD.comparison.pdf" },
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.di.rsd.comparison.width,
                     height = input$dc.di.rsd.comparison.height)
    }
    ggsave(file, plot = dc.di.rsd.comparison(), device = device)
  }, contentType = "pdf")



###RSD distribution before and after integration
observeEvent(eventExpr = input$DCdiShowRSD2, {
  dc.di.rsd.distribution1 <- function(){
    par(mar = c(5,5,4,2))
    if(is.null(dc.ms1.dn$data)) return(NULL)
    if(is.null(input$dc.di.group4)){
      group <- unique(dc.sample.info.raw$data[,"group"])
      if(any(group == "QC")){
        group <- "QC"
      }
    }else{
      group <- input$dc.di.group4
    }

    tags <- dc.ms1.dn$data[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.ms1.dn$data)), drop = FALSE]

    dc.di.rsd.distribution1 <- rsdDistribution2(sample = dc.ms1.dn$data,
                                                rsd.tol = input$dc.di.rsd.distribution.tol,
                                                tags = tags,
                                                sample.info = dc.sample.info.raw$data,
                                                alpha = 0.5, size = 2,
                                                group = group,
                                                title = "Before integration")
    rm(list = "tags")
    dc.di.rsd.distribution1
  }

  output$dc.di.rsd.distribution1 <- renderPlotly({
    ggplotly(dc.di.rsd.distribution1(),
             tooltip = c("Peak", "RSD", "Group"),
             source = "dc.di.rsd.distribution1")
  })

})


# download plot
output$dc.di.rsd.distribution1.download <- downloadHandler(
  filename = function() {"RSD.distributation.before.integration.pdf"},
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = 7,
                     height = 7)
    }
    ggsave(file, plot = dc.di.rsd.distribution1(), device = device)
  }, contentType = "pdf")



observeEvent(eventExpr = input$DCdiShowRSD2, {
  dc.di.rsd.distribution2 <- function(){
    par(mar = c(5,5,4,2))
    if(is.null(dc.ms1.di$data)) return(NULL)
    if(is.null(input$dc.di.group4)){
      group <- unique(dc.sample.info.raw$data[,"group"])
      if(any(group == "QC")){
        group <- "QC"
      }
    }else{
      group <- input$dc.di.group4
    }

    tags <- dc.ms1.di$data[,-match(dc.sample.info.raw$data[,"sample.name"], colnames(dc.ms1.di$data)), drop = FALSE]

    dc.di.rsd.distribution2 <- rsdDistribution2(sample = dc.ms1.di$data,
                                                rsd.tol = input$dc.di.rsd.distribution.tol,
                                                tags = tags,
                                                sample.info = dc.sample.info.raw$data,
                                                alpha = 0.5, size = 2,
                                                group = group,
                                                title = "After integration")
    rm(list = "tags")
    dc.di.rsd.distribution2

  }

  output$dc.di.rsd.distribution2 <- renderPlotly({
    ggplotly(dc.di.rsd.distribution2(),
             tooltip = c("Peak", "RSD", "Group"),
             source = "dc.di.rsd.distribution2")
  })

})


# download plot
output$dc.di.rsd.distribution2.download <- downloadHandler(
  filename = function() { "RSD.distributation.after.integration.pdf"},
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = 7,
                     height = 7)
    }
    ggsave(file, plot = dc.di.rsd.distribution2(), device = device)
  }, contentType = "pdf")




###Error if there are no data integration, warning user click Next before click submit
observeEvent(eventExpr = input$dc.di.2.os, {
  if(is.null(dc.ms1.di$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click Submit first.",
                           type = "error")
  }

})

##jump to Data normalization page from zero value processing page
observeEvent(input$dc.di.2.os, {
  if(!is.null(dc.ms1.di$data)){
    updateTabsetPanel(session, "DCtab",
                      selected = "DCos")
  }
})






















#-------------------------------------------------------------------------------
#outlier sample filtering
##save parameter
dc.os.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.submit.button, {
  temp <- try({
    dcOSparams <- DCosParam(
      input$dc.os.pca.log,
      input$dc.os.pca.scale,
      input$dc.os.pca.center,
      input$dc.os.pca.ci.tol,
      input$dc.os.zero.tol)
    dc.os.params$data <- dcOSparams

    save(dcOSparams, file = file.path("user_data", user_input$username,
                                      input$DCprojectID,
                                      "data_cleaning",
                                      "dcOSparams"))
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$dc.os.params.message <- renderText({paste("Outlier removal parameters: ",info)})
    rm(list="temp")
  }else{
    output$dc.os.params.message <- renderText({""})
    rm(list="temp")
  }

})


##PCA to find the outlier samples
##group information for selection
dc.group.name <- reactive({
  dc.group.name <- unique(dc.sample.info.raw$data[,"group"])
  if(any(dc.group.name == "QC")){
    dc.group.name <- c("QC", dc.group.name[-which(dc.group.name == "QC")])
  }
  dc.group.name
})

output$dc.os.group.area1 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.os.group1",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No",
                                            selected_header = "Selected")
    )
  )
})


output$dc.os.group.area2 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.os.group2",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.os.group.area3 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.os.group3",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No",
                                            selected_header = "Selected:")
    )
  )
})


output$dc.os.group.area4 <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.os.group4",
                             label = "Show which group",
                             choices = dc.group.name(),
                             selected = dc.group.name()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No",
                                            selected_header = "Selected:")
    )
  )
})




dc.os.pca <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.submit.button,{
  if(is.null(dc.ms1.di$data) | is.null(dc.sample.info.raw$data)) return(NULL)
  dc.peak.table <- dc.ms1.di$data
  dc.sample.info <- dc.sample.info.raw$data

  temp <- try({
    dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"],
                                      colnames(dc.peak.table)), drop = FALSE]
    dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"],
                                     colnames(dc.peak.table)), drop = FALSE]

    ##log
    dc.sample <- sxtLog(sample = dc.sample, method = input$dc.os.pca.log)
    ##scale
    dc.sample <- sxtScale(sample = dc.sample,
                          method = input$dc.os.pca.scale,
                          center = input$dc.os.pca.center)

    prcomp(data.frame(t(dc.sample)),
                        retx = TRUE,
                        center = FALSE,
                        scale = FALSE)
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$dc.os.pca.message <- renderText({paste("PCA plot: ",info)})
    rm(list = c("temp"))
  }else{
    dc.os.pca$data <- temp
    output$dc.os.pca.message <- renderText({""})
    rm(list = c("temp"))
  }

})


dc.os.outlier.index1 <- eventReactive(eventExpr = input$dc.os.submit.button, {
  dc.os.outlier.index1 <- try(pcaFindOutlier(dc.os.pca$data))
  if(class(dc.os.outlier.index1) == "try-error"){
    dc.os.outlier.index1 <- which(1 > 5)
  }
  dc.os.outlier.index1
})

output$dc.os.outlier.name.pca <- renderText({
  if(length(dc.os.outlier.index1()) > 0){
    return(
      paste(paste(names(dc.os.outlier.index1()), collapse = "&"),
            " are outlier samples.", sep = "")
    )
  }else{
    return("No outlier samples.")
  }
})


dc.os.pca.plot <- reactiveValues(data = NULL)
observe({
  if(!is.null(dc.os.pca$data)){
    temp <- try({
      pcaOutlierPlot(pca.object = dc.os.pca$data,
                     outlier.idx = dc.os.outlier.index1(),
                     alpha = 0.8, size = 2, text = "",
                     title = "PCA score plot")
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.os.pca.plot.message <- renderText({paste("PCA plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.os.pca.plot$data <- temp
      output$dc.os.pca.plot.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})



output$dc.os.pca.plot <- renderPlotly({
  if(is.null(dc.os.pca$data)) return(NULL)
  ggplotly(dc.os.pca.plot$data,
           tooltip = c("Sample", "PC1", "PC2", "Group"),
           source = "dc.os.pca.plot")
})

##download plot

output$dc.os.pca.plot.download <- downloadHandler(
  filename = function() { "PCA.score.plot.for.outlier.samples.pdf"},
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.os.pca.plot.width,
                     height = input$dc.os.pca.plot.height)
    }
    ggsave(file, plot = dc.os.pca.plot$data, device = device)
  }, contentType = "pdf")



##find outlier samples using zero ratio
dc.os.sample.zero.ratio <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.submit.button, {
  if(is.null(dc.ms1.di$data) | is.null(dc.sample.info.raw$data)) return(NULL)
  dc.peak.table <- dc.ms1.di$data
  dc.sample.info <- dc.sample.info.raw$data


  temp <- try({
    dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"],
                                      colnames(dc.peak.table)), drop = FALSE]
    dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"],
                                     colnames(dc.peak.table)), drop = FALSE]
    apply(dc.sample, 2, function(x){
      sum(x == 0)*100/nrow(dc.sample)
    })
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    dc.os.sample.zero.ratio.message <- renderText({paste("Zero ratio: ",info)})
    rm(list = c("temp"))
  }else{
    dc.os.sample.zero.ratio$data <- temp
    output$dc.os.sample.zero.ratio.message <- renderText({""})
    rm(list = c("temp"))
  }
})

dc.os.outlier.index2 <- eventReactive(eventExpr = input$dc.os.submit.button, {
  which(dc.os.sample.zero.ratio$data > input$dc.os.zero.tol)
})

dc.os.zero.plot <- reactiveValues(data = NULL)
observe({
  if(!is.null(dc.os.sample.zero.ratio$data)){
    temp <- try({
      DCosZeroRatioPlot(object = dc.os.sample.zero.ratio$data,
                        outlier.idx = dc.os.outlier.index2(),
                        zero.ratio.tol = input$dc.os.zero.tol,
                        sample.info = dc.sample.info.raw$data,
                        alpha = 0.8, size = 2, title = "Zero value ratio")
    })
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.os.zero.plot.message <- renderText({paste("Zero ratio plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.os.zero.plot$data <- temp
      output$dc.os.zero.plot.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})


output$dc.os.zero.plot <- renderPlotly({
  if(is.null(dc.os.zero.plot$data)) return(NULL)
  ggplotly(dc.os.zero.plot$data,
           tooltip = c("Sample", "Injection.order", "Zero.ratio", "Group"),
           source = "dc.os.zero.plot")
})

##download plot
output$dc.os.zero.plot.download <- downloadHandler(
  filename = function() { "Zero.value.ratio.plot.pdf"},
  content = function(file) {
    device <- function(..., width , height) {
      grDevices::pdf(..., width = input$dc.os.zero.plot.width,
                     height = input$dc.os.zero.plot.height)
    }
    ggsave(file, plot = dc.os.zero.plot$data, device = device)
  }, contentType = "pdf")






###outlier sample summary
dc.os.outlier.index <- eventReactive(input$dc.os.submit.button,{
  union(x = dc.os.outlier.index1(), y = dc.os.outlier.index2())
})


dcOStable <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.submit.button, {
if(length(dc.os.outlier.index()) != 0){
  sample.name <- dc.sample.info.raw$data[,"sample.name"]
  note1 <- note2 <- rep("No", length(sample.name))
  if(length(dc.os.outlier.index1()) > 0) note1[dc.os.outlier.index1()] <- "Yes"
  if(length(dc.os.outlier.index2()) > 0) note2[dc.os.outlier.index2()] <- "Yes"
  temp <- data.frame(sample.name, note1, note2, stringsAsFactors = FALSE)
  colnames(temp) <- c("Sample", "PCA", "Zero value")
  temp <- temp[dc.os.outlier.index(),]
  dcOStable$data <- temp
}
})


observeEvent(eventExpr = input$dc.os.submit.button,{
  if(length(dc.os.outlier.index()) == 0){
    output$dc.os.result.area <- renderUI({
      fluidPage(
        "No outliers"
      )
    })
  }else{
    sample.name <- dc.sample.info.raw$data[,"sample.name"]
    note1 <- note2 <- rep("No", length(sample.name))
    if(length(dc.os.outlier.index1()) > 0) note1[dc.os.outlier.index1()] <- "Yes"
    if(length(dc.os.outlier.index2()) > 0) note2[dc.os.outlier.index2()] <- "Yes"
    temp <- data.frame(sample.name, note1, note2, stringsAsFactors = FALSE)
    colnames(temp) <- c("Sample", "PCA", "Zero value")
    temp <- temp[dc.os.outlier.index(),]

    output$dc.os.outliers <- DT::renderDataTable(
      DT::datatable(temp,
                    selection = "single",
                    escape = FALSE,
                    filter = "none",
                    rownames = FALSE,
                    extensions = list("ColReorder" = NULL,
                                      "Buttons" = NULL,
                                      "FixedColumns" = list(leftColumns=2)
                    ),
                    options = list(
                      pageLength = 10,
                      dom = 'Bfrtip',
                      # autoWidth = TRUE,
                      lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                      ColReorder = TRUE,
                      buttons =
                        list(
                          list(
                            extend = 'collection',
                            # buttons = c('csv', 'excel'),
                            buttons = list(list(extend="csv",filename = "Outlier.samples"),
                                           list(extend='excel',filename = "Outlier.samples")
                            ),
                            text = 'Download'
                          ),
                          I('colvis')
                        ),
                      scrollX = TRUE
                    )
      ),
      server = FALSE
    )


    output$dc.os.result.area <- renderUI({
      fluidPage(
        column(width = 6,
               shinyWidgets::multiInput(inputId = "osDeleteSampleName",
                                        label = "Outlier samples",
                                        choices = as.character(temp[,1]),
                                        # selected = dc.group.name()[1],
                                        options = list(enable_search = FALSE,
                                                       non_selected_header = "No selected:",
                                                       selected_header = "Selected:")
               ),
               actionButton(inputId = 'osDeleteSample',
                            "Delete",
                            styleclass = "info",
                            icon = icon('play-circle')),
               helpText("Please select the sample names and click",
                        strong("Delete"), ", and then click Submit again to delete outlier samples.")
        ),
        column(width = 6,
               h5("Outlier sample information"),
               DT::dataTableOutput(outputId = "dc.os.outliers"),
               textOutput(outputId = "os.delete.sample.info")
        )
        # )
      )
    })
  }
})


observeEvent(eventExpr = input$osDeleteSample,{
  output$os.delete.sample.info <- renderText({
    paste(paste(input$osDeleteSampleName, collapse = ", "),
          " will be removed from peak table.", sep = "")
  })
})



dc.ms1.os <- reactiveValues(data = NULL)
dc.sample.info.os <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.submit.button, {
  if(is.null(dc.ms1.di$data) | is.null(dc.sample.info.raw$data)) return(NULL)
  dc.ms1.os$data <- dc.ms1.di$data
  dc.sample.info.os$data <- dc.sample.info.raw$data
  if(!is.null(input$osDeleteSampleName)) {
    dc.ms1.os$data <-
      dc.ms1.os$data[,-match(input$osDeleteSampleName, colnames(dc.ms1.os$data)), drop = FALSE]

    dc.sample.info.os$data <-
      dc.sample.info.raw$data[-match(input$osDeleteSampleName,
                                     dc.sample.info.raw$data[,"sample.name"]), ,drop = FALSE]

  }
})


##
output$dc.ms1.os <- DT::renderDataTable(
  DT::datatable(dc.ms1.os$data,
                selection = list(mode = 'single', target = 'column'),
                # selection = "single",
                escape = FALSE,
                filter = "none",
                rownames= FALSE,
                extensions = list("ColReorder" = NULL,
                                  "Buttons" = NULL,
                                  "FixedColumns" = list(leftColumns=2)
                ),
                options = list(
                  pageLength = 10,
                  dom = 'Bfrtip',
                  # autoWidth = TRUE,
                  lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                  ColReorder = TRUE,
                  buttons =
                    list(
                      list(
                        extend = 'collection',
                        # buttons = c('csv', 'excel'),
                        buttons = list(list(extend="csv",filename = "Data.after.outlier.removal"),
                                       list(extend='excel',filename = "Data.after.outlier.removal")
                        ),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)



###box plot for each sample
# observeEvent(eventExpr = input$DCosShowSample, {
#   if(is.null(dc.ms1.os$data)) return(NULL)
#   if(is.null(dc.sample.info$data)) return(NULL)
#   peak.table <- dc.ms1.os$data
#   sample.info <- dc.sample.info.os$data
#   sample <- peak.table[,match(sample.info[,1], colnames(peak.table)), drop = FALSE]
#   tags <- peak.table[,-match(sample.info[,1], colnames(peak.table)), drop = FALSE]
#
#   if (is.null(input$dc.ms1.os_columns_selected) |
#       input$dc.ms1.os_columns_selected <= ncol(tags)) {
#     dc.single.sample.plot.os <- function(){
#       par( mar = c(5,5,4,2))
#       ggplot2::ggplot(data = data.frame("x" = 1, "y" = 1, stringsAsFactors = FALSE), ggplot2::aes(x = x, y = y)) +
#         ggplot2::theme_bw() +
#         ggplot2::annotate(geom = "text", x = 1, y = 1, label = "Plese select a sample.")
#     }
#
#   }else{
#     sample.idx <- as.numeric(input$dc.ms1.os_columns_selected)
#     dc.single.sample.plot.os <- function(){
#       par(mar = c(5,5,4,2))
#       plot <- osSingleSample(sample = sample,
#                              sample.info = sample.info,
#                              sample.idx = sample.idx,
#                              alpha = 0.8, size = 2,
#                              title = "")
#
#       plot
#     }
#
#     # rm(list = c("peak.table", "sample.info", "sample", "tags"))
#   }
#
#
#   output$dc.single.sample.plot.os <- renderPlot({
#     # ggplotly(dc.single.sample.plot.os(),
#     #          tooltip = c("Sample", "Group"),
#     #          source = "dc.single.sample.plot.os")
#     dc.single.sample.plot.os()
#   })
#
#   output$dc.single.sample.plot.os.download <- downloadHandler(
#     filename = function() { "Sample.boxplot.pdf" },
#     content = function(file) {
#       device <- function(..., width , height) {
#         grDevices::pdf(..., width = input$dc.single.sample.plot.os.width,
#                        height = input$dc.single.sample.plot.os.height)
#       }
#       ggsave(file, plot = dc.single.sample.plot.os(), device = device)
#     }, contentType = "pdf")
#
# })



###Error if there are no outlier sample processing, warning user click Next before click submit
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(is.null(dc.ms1.os$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click Submit first.",
                           type = "error")
  }

})

##jump to data quality outlier sample processing page from Data integration
observeEvent(input$dc.os.2.qa2, {
  if(!is.null(dc.ms1.os$data)){
    updateTabsetPanel(session, "DCtab",
                      selected = "DCdataQualityAfter")
  }
})





















#-------------------------------------------------------------------------------
#data quality assessment, after data cleaning
## peak profile
dc.data.profile.plot2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(!is.null(dc.ms1.os$data)){
    temp <- try({
      dataInformation(object = dc.ms1.os$data)
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.data.profile.plot2.message <- renderText({paste("Data profile: ",info)})
      rm(list = c("temp"))
    }else{
      dc.data.profile.plot2$data <- temp
      output$dc.data.profile.plot2.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})



output$dc.data.profile.plot2 <- renderPlotly({
  if(!is.null(dc.data.profile.plot2$data)){
    ggplotly(dc.data.profile.plot2$data, tooltip = c(
      "rt",
      "mz","int.log"),
      source = "dc.data.profile.plot2")
  }
})


## zero ratio in peaks
dc.peak.zero.ratio.plot2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(!is.null(dc.ms1.os$data)){
    temp <- try({
      zeroPeakPlot(data = dc.ms1.os$data,
                   text = FALSE)
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.peak.zero.ratio.plot2.message <- renderText({paste("Zero ratio: ",info)})
      rm(list = c("temp"))
    }else{
      dc.peak.zero.ratio.plot2$data <- temp
      output$dc.peak.zero.ratio.plot2.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})


output$dc.peak.zero.ratio.plot2 <- renderPlotly({
  if(!is.null(dc.peak.zero.ratio.plot2$data)){
    ggplotly(dc.peak.zero.ratio.plot2$data, tooltip = c(
      "Index",
      "zero.ratio","Peak"),
      source = "dc.peak.zero.ratio.plot2")
  }
})


## zero ratio in Samples
dc.sample.zero.ratio.plot2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(!is.null(dc.ms1.os$data)){
    temp <- try({
      zeroSamplePlot(data = dc.ms1.os$data,
                     text = FALSE)
    })
    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.sample.zero.ratio.plot2.message <- renderText({paste("Sample zero ratio: ",info)})
      rm(list = c("temp"))
    }else{
      dc.sample.zero.ratio.plot2$data <- temp
      output$dc.sample.zero.ratio.plot2.message <- renderText({""})
      rm(list = c("temp"))
    }
    }

})



output$dc.sample.zero.ratio.plot2 <- renderPlotly({
  if(!is.null(dc.sample.zero.ratio.plot2$data)){
    ggplotly(dc.sample.zero.ratio.plot2$data, tooltip = c(
      "Index",
      "zero.ratio","Sample"),
      source = "dc.sample.zero.ratio.plot2")
  }
})


###group information in da_qa2_ui
dc.group.name2 <- reactive({
  if(is.null(dc.sample.info.os$data)) return(NULL)
  dc.group.name2 <- unique(dc.sample.info.os$data[,"group"])
  dc.group.name2
})


output$dc.qa2.group.area <- renderUI({
  fluidPage(
    shinyWidgets::multiInput(inputId = "dc.qa2.group",
                             label = "Show which group",
                             choices = dc.group.name2(),
                             selected = dc.group.name2()[1],
                             options = list(enable_search = FALSE,
                                            non_selected_header = "No",
                                            selected_header = "Selected")
    )
  )
})



###RSD distribution
dc.peak.rsd.plot2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(!is.null(dc.ms1.os$data)){
    if(is.null(input$dc.qa2.group)){
      group <-  unique(dc.sample.info.os$data[,"group"])[1]
    }else{
      group <- input$dc.qa2.group
    }

    temp <- try({
      rsdDistribution(data = dc.ms1.os$data,
                      sample.info = dc.sample.info.os$data,
                      group = group,
                      rsd.tol = 30)
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.peak.rsd.plot2.message <- renderText({paste("RSD plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.peak.rsd.plot2$data <- temp
      output$dc.peak.rsd.plot2.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})


observeEvent(eventExpr = input$dc.qa2.group, {
  if(!is.null(dc.ms1.os$data)){
    temp <- try({
      rsdDistribution(data = dc.ms1.os$data,
                      sample.info = dc.sample.info.os$data,
                      group = input$dc.qa2.group,
                      rsd.tol = 30)
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.peak.rsd.plot2.message <- renderText({paste("RSD plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.peak.rsd.plot2$data <- temp
      output$dc.peak.rsd.plot2.message <- renderText({""})
      rm(list = c("temp"))
    }

  }
})


dc.peak.rsd.plot.for.report2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(!is.null(dc.ms1.os$data)){
    if(any(unique(dc.sample.info.os$data[,"group"]) == "QC")){
      group <- "QC"
    }else{
      group <- unique(dc.sample.info.os$data[,"group"])[1]
    }

    temp <- try({
      rsdDistribution(data = dc.ms1.os$data,
                      sample.info = dc.sample.info.os$data,
                      group = group,
                      rsd.tol = 30)
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.peak.rsd.plot.for.report2.message <- renderText({paste("Peak RSD for report: ",info)})
      rm(list = c("temp"))
    }else{
      dc.peak.rsd.plot.for.report2$data <- temp
      output$dc.peak.rsd.plot.for.report2.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})



output$dc.peak.rsd.plot2 <- renderPlotly({
  if(!is.null(dc.peak.rsd.plot2$data)){
    ggplotly(dc.peak.rsd.plot2$data,
             #        tooltip = c(
             # "Index",
             # "zero.ratio","Sample"),
             source = "dc.peak.rsd.plot2")
  }
})




#-------------------------------------------------------------------------------
#PCA score plot, should be fixed
dc.qa2.pca.object <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2,{
  if(!is.null(dc.ms1.os$data) & !is.null(dc.sample.info.os$data)){

    temp <- try({
      dc.peak.table <- dc.ms1.os$data
      dc.sample.info <- dc.sample.info.os$data
      dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"],
                                        colnames(dc.peak.table)), drop = FALSE]
      dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"],
                                       colnames(dc.peak.table)), drop = FALSE]
      ##log
      dc.sample <- sxtLog(sample = dc.sample, method = "log10")
      ##scale
      dc.sample <- sxtScale(sample = dc.sample,
                            method = "pareto",
                            center = TRUE)

      prcomp(data.frame(t(dc.sample)),
             retx = TRUE,
             center = FALSE,
             scale = FALSE)
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.qa2.pca.object.message <- renderText({paste("PCA: ",info)})
      rm(list = c("temp"))
    }else{
      dc.qa2.pca.object$data <- temp
      output$dc.qa2.pca.object.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})

dc.qa2.pca.score.plot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2,{
  if(!is.null(dc.sample.info.os$data)){
    temp <- try({
      batch.info <- dc.sample.info.os$data[,c(1,4)]
      class.info <- dc.sample.info.os$data[,c(1,3)]
      pcaScorePlot(pca.object = dc.qa2.pca.object$data,
                                            batch.info = batch.info,
                                            class.info = class.info)
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.qa2.pca.score.plot.message <- renderText({paste("PCA plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.qa2.pca.score.plot$data <- temp
      output$dc.qa2.pca.score.plot.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})


output$dc.qa2.pca.score.plot <- renderPlotly({
  if(!is.null(dc.qa2.pca.score.plot$data)){
    plotly::ggplotly(dc.qa2.pca.score.plot$data,
                     source = "dc.qa2.pca.score.plot")
  }
})

###PCA score plot for analysis report
dc.qa2.pca.object.for.report <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2,{
  if(!is.null(dc.ms1.os$data) & !is.null(dc.sample.info.os$data)){
    temp <- try({
      dc.peak.table <- dc.ms1.os$data
      dc.sample.info <- dc.sample.info.os$data
      dc.sample <- dc.peak.table[,match(dc.sample.info[,"sample.name"],
                                        colnames(dc.peak.table)), drop = FALSE]
      dc.tags <- dc.peak.table[,-match(dc.sample.info[,"sample.name"],
                                       colnames(dc.peak.table)), drop = FALSE]
      ##log
      dc.sample <- sxtLog(sample = dc.sample, method = "log10")
      ##scale
      dc.sample <- sxtScale(sample = dc.sample,
                            method = "pareto",
                            center = TRUE)
      prcomp(data.frame(t(dc.sample)), retx = TRUE, center = FALSE, scale = FALSE)
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.qa2.pca.object.for.report.message <- renderText({paste("PCA for report: ",info)})
      rm(list = c("temp"))
    }else{
      dc.qa2.pca.object.for.report$data <- temp
      output$dc.qa2.pca.object.for.report.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})


dc.qa2.pca.score.plot.for.report <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(!is.null(dc.qa2.pca.object.for.report$data)){
  temp <- try({
    batch.info <- dc.sample.info.os$data[,c(1,4)]
    class.info <- dc.sample.info.os$data[,c(1,3)]
    pcaScorePlot(pca.object = dc.qa2.pca.object.for.report$data,
                                                          batch.info <- dc.sample.info.os$data[,c(1,4)],
                                                          class.info <- dc.sample.info.os$data[,c(1,3)])
  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$dc.qa2.pca.score.plot.for.report.message <- renderText({paste("PCA plot: ",info)})
    rm(list = c("temp"))
  }else{
    dc.qa2.pca.score.plot.for.report$data <- temp
    output$dc.qa2.pca.score.plot.for.report.message <- renderText({""})
    rm(list = c("temp"))
  }
  }
})


###Boxplots for QC samples
dc.qc.int.boxplot2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(!is.null(dc.ms1.os$data)){
    temp <- try({
      qcIntBoxplot(data = dc.ms1.os$data,
                   sample.info = dc.sample.info.os$data)
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.qc.int.boxplot2.message <- renderText({paste("QC intensity boxplot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.qc.int.boxplot2$data <- temp
      output$dc.qc.int.boxplot2.message <- renderText({""})
      rm(list = c("temp"))
    }
  }

})


output$dc.qc.int.boxplot2 <- renderPlotly({
  if(!is.null(dc.qc.int.boxplot2$data)){
    ggplotly(dc.qc.int.boxplot2$data,
             source = "dc.qc.int.boxplot2")
  }
})


###QC correlation
dc.qc.cor.plot2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2, {
  if(!is.null(dc.ms1.os$data)){
    temp <- try({
      qcCorPlot(data = dc.ms1.os$data,
                sample.info = dc.sample.info.os$data)
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.qc.cor.plot2.message <- renderText({paste("QC cor plot: ",info)})
      rm(list = c("temp"))
    }else{
      dc.qc.cor.plot2$data <- temp
      output$dc.qc.cor.plot2.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})


output$dc.qc.cor.plot2 <- renderPlotly({
  if(!is.null(dc.qc.cor.plot2$data)){
  ggplotly(dc.qc.cor.plot2$data)
  }
})


dc.qc.cor2 <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$dc.os.2.qa2,{
  if(!is.null(dc.ms1.os$data)){
    temp <- try({
      qcCor(data = dc.ms1.os$data,
            sample.info = dc.sample.info.os$data)
    })

    if(class(temp)[1] == "try-error"){
      info <- temp[[1]]
      output$dc.qc.cor2.message <- renderText({paste("QC cor: ",info)})
      rm(list = c("temp"))
    }else{
      dc.qc.cor2$data <- temp
      output$dc.qc.cor2.message <- renderText({""})
      rm(list = c("temp"))
    }
  }
})


output$dc.qc.cor2 <- DT::renderDataTable(
  DT::datatable(dc.qc.cor2$data,
                class = 'cell-border stripe',
                editable = FALSE,
                # selection = "single",
                selection = list(mode = 'single', target = 'row'),
                # caption = 'Raw data containing MVs',
                escape = FALSE,
                # filter = "top",
                filter = "none",
                rownames = TRUE,
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
                        buttons = list(list(extend="csv",filename = "QC.sample.correlation"),
                                       list(extend='excel',filename = "QC.sample.correlation")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)



##jump to result download from data quality assessment page after cleaning
observeEvent(input$dc.qa2.2.download, {
  updateTabsetPanel(session, "DCtab",
                    selected = "DCdownload")

})





















#-------------------------------------------------------------------------------
#####download result
###generate analysis report
#
#
# observe({
#   shinyjs::disable(id = "dc.report.download")
#   if (any(dir(user.path()) == "Analysis.Report.of.Data.Cleaning.html")) {
#     shinyjs::enable("dc.report.download")
#   }
# })



#
####generate analysis report
observeEvent(eventExpr = input$dc.generate.analysis.report, {
  if(is.null(dc.ms1.os$data)) return(NULL)
  now.path <- getwd()
  user.path <- file.path("user_data", user_input$username, input$DCprojectID, "data_cleaning")
  tempReport <- file.path(now.path, user.path, "DCreport.temp.Rmd")
  file.copy("data/markdown/DCreport.Rmd", tempReport, overwrite = TRUE)

  params <- list(  dc.ba.info = dc.ba.info$data,
                   dc.data.info = dc.data.info$data,
                   dc.data.profile.plot = dc.data.profile.plot$data,
                   dc.data.profile.plot2 = dc.data.profile.plot2$data,
                   dc.peak.mv.ratio.plot = dc.peak.mv.ratio.plot$data,
                   dc.sample.mv.ratio.plot = dc.sample.mv.ratio.plot$data,
                   dc.peak.zero.ratio.plot = dc.peak.zero.ratio.plot$data,
                   dc.peak.zero.ratio.plot2 = dc.peak.zero.ratio.plot2$data,
                   dc.sample.zero.ratio.plot = dc.sample.zero.ratio.plot$data,
                   dc.sample.zero.ratio.plot2 = dc.sample.zero.ratio.plot2$data,
                   dc.qa1.pca.score.plot.for.report = dc.qa1.pca.score.plot.for.report$data,
                   dc.qa2.pca.score.plot.for.report = dc.qa2.pca.score.plot.for.report$data,
                   dc.qc.int.boxplot = dc.qc.int.boxplot$data,
                   dc.qc.int.boxplot2 = dc.qc.int.boxplot2$data,
                   dc.peak.rsd.plot.for.report = dc.peak.rsd.plot.for.report$data,
                   dc.peak.rsd.plot.for.report2 = dc.peak.rsd.plot.for.report2$data,
                   dc.ba.params = dc.ba.params$data,
                   dc.mv.params = dc.mv.params$data,
                   dc.zero.params = dc.zero.params$data,
                   dc.dn.params = dc.dn.params$data,
                   dc.di.params = dc.di.params$data,
                   dc.os.params = dc.os.params$data,
                   dc.rsd.comparison = dc.rsd.comparison(),
                   dc.di.rsd.comparison = dc.di.rsd.comparison(),
                   dc.os.pca.plot = dc.os.pca.plot$data,
                   dc.os.zero.plot = dc.os.zero.plot$data
  )
  # save(params, file = "params")
  withProgress(message = 'Generate analysis report...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 lapply(1:length(tempReport), function(idx){
                   incProgress(1/length(tempReport),
                               detail = "This may take a while")
                   temp <- try({
                     rmarkdown::render(tempReport,
                                       output_file = file.path(now.path, user.path,
                                                               "Analysis.Report.of.Data.Cleaning.html"),
                                       params = params,
                                       envir = new.env(parent = globalenv())
                     )
                   })

                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$Analysis.Report.of.Data.Cleaning.message <- renderText({paste("Generate report: ",info)})
                     rm(list = c("temp"))
                   }else{
                     output$Analysis.Report.of.Data.Cleaning.message <- renderText({""})
                     rm(list = c("temp"))
                   }

                 })
               })
})


####download analysis report
output$dc.report.download <- downloadHandler(
  filename = "Analysis.Report.of.Data.Cleaning.html",
  content = function(file){
    file.copy(file.path("user_data", user_input$username, input$DCprojectID, "data_cleaning", "Analysis.Report.of.Data.Cleaning.html"), file)
  }
)


# shinyjs::disable("dc.report.download")
# ##disable untill there is a report in you files
# observeEvent(eventExpr = input$dc.generate.analysis.report, {
#   if(!is.null(user_input$username) & input$DCprojectID != ""){
#     if(any(dir(file.path("user_data", user_input$username,
#                          input$DCprojectID, "data_cleaning")) == "Analysis.Report.of.Data.Cleaning.html")){
#       shinyjs::enable("dc.report.download")
#     }
#   }
# })


# observe({
#   # shinyjs::hide(id = "dc.result.download")
#   useShinyjs()
#   shinyjs::disable(id = "dc.result.download")
#   if (any(dir(user.path()) == "Data_Cleaning_Result.zip")) {
#     # shinyjs::show("dc.result.download")
#     useShinyjs()
#     shinyjs::enable(id = "dc.result.download")
#   }
# })
#
####generate analysis result
observeEvent(eventExpr = input$dc.generate.analysis.result, {

  if(!is.null(dc.ms1.os$data)){
    #ZIP data
    withProgress(message = 'Generate analysis result...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     temp <- try({
                       now.path <- getwd()
                       temp.path <- file.path(now.path, "user_data",
                                              user_input$username, input$DCprojectID,
                                              "data_cleaning","Data_Cleaning_Result")
                       dir.create(temp.path)
                       temp.path1 <- file.path(temp.path, "Figures")
                       dir.create(temp.path1)
                       temp.path2 <- file.path(temp.path, "Intermediate data")
                       dir.create(temp.path2)

                       ##output final data
                       readr::write_csv(dc.ms1.os$data,
                                        file.path(temp.path, "MS1.peak.table.final.csv"))

                       readr::write_csv(dc.sample.info.os$data,
                                        file.path(temp.path, "Sample.information.final.csv"))

                       ##output figures
                       ggsave(dc.data.profile.plot$data,
                              file = file.path(temp.path1, "Data.profile.before.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.peak.mv.ratio.plot$data,
                              file = file.path(temp.path1, "MV.ratio.in.peaks.before.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.sample.mv.ratio.plot$data,
                              file = file.path(temp.path1, "MV.ratio.in.samples.before.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.peak.zero.ratio.plot$data,
                              file = file.path(temp.path1, "Zero.ratio.in.peaks.before.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.sample.zero.ratio.plot$data,
                              file = file.path(temp.path1, "Zero.ratio.in.samples.before.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.peak.rsd.plot.for.report$data,
                              file = file.path(temp.path1, "RSD.distributation.before.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.qa1.pca.score.plot.for.report$data,
                              file = file.path(temp.path1, "PCA.score.plot.before.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.qc.int.boxplot$data,
                              file = file.path(temp.path1, "QC.intensity.boxplot.before.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.qc.cor.plot$data,
                              file = file.path(temp.path1, "QC.correlation.plot.before.data.cleaning.pdf"), width = 7, height = 7)

                       ggsave(dc.data.profile.plot2$data,
                              file = file.path(temp.path1, "Data.profile.after.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.peak.zero.ratio.plot2$data,
                              file = file.path(temp.path1, "Zero.ratio.in.peaks.after.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.sample.zero.ratio.plot2$data,
                              file = file.path(temp.path1, "Zero.ratio.in.samples.after.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.peak.rsd.plot.for.report2$data,
                              file = file.path(temp.path1, "RSD.distributation.after.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.qa2.pca.score.plot.for.report$data,
                              file = file.path(temp.path1, "PCA.score.plot.after.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.qc.int.boxplot2$data,
                              file = file.path(temp.path1, "QC.intensity.boxplot.after.data.cleaning.pdf"), width = 7, height = 7)
                       ggsave(dc.qc.cor.plot2$data,
                              file = file.path(temp.path1, "QC.correlation.plot.after.data.cleaning.pdf"), width = 7, height = 7)

                       ##output intermediate data
                       readr::write_csv(dc.ms1.after.mv$data,
                                        file.path(temp.path2, "MS1.peak.table.after.MV.processing.csv"))

                       readr::write_csv(dc.ms1.after.zero$data,
                                        file.path(temp.path2, "MS1.peak.table.after.zero.processing.csv"))

                       readr::write_csv(dc.ms1.dn$data,
                                        file.path(temp.path2, "MS1.peak.table.after.data.normalization.csv"))

                       readr::write_csv(dc.ms1.di$data,
                                        file.path(temp.path2, "MS1.peak.table.after.data.integration.csv"))

                       readr::write_csv(dc.batch.alignment.result2$data,
                                        file.path(temp.path2, "MS1.peak.table.after.batch.alignment.csv"))

                       readr::write_csv(dcOStable$data,
                                        file.path(temp.path2, "Outlier.information.csv"))

                       setwd(file.path("user_data", user_input$username, input$DCprojectID, "data_cleaning"))
                       zip::zip(zipfile = file.path(now.path, "user_data", user_input$username, input$DCprojectID, "data_cleaning",
                                                                    "Data_Cleaning_Result.zip"),
                                                files = "Data_Cleaning_Result",
                                                recurse = TRUE)
                     })

                     if(class(temp)[1] == "try-error"){
                       info <- temp[[1]]
                       setwd(now.path)
                       output$Data.Cleaning.Result.message <- renderText({paste("Generate result: ",info)})
                       rm(list = c("temp"))
                     }else{
                       setwd(now.path)
                       output$Data.Cleaning.Result.message <- renderText({""})
                       rm(list = c("temp"))
                     }
                   })
                 })
  }
})


####download analysis result
output$dc.result.download <- downloadHandler(
  filename = "Data_Cleaning_Result.zip",
  content = function(file){
    file.copy(file.path("user_data", user_input$username,
                        input$DCprojectID,
                        "data_cleaning",
                        "Data_Cleaning_Result.zip"), file)
  }
)
