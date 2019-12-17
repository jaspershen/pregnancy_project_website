#####################################upload data###############################
#####metabolite identification
###data cleaning.area
output$metaboliteidentification.area <- renderUI({
  if (user_input$authenticated == TRUE){
    ui.metaboliteidentification()
  }else{
    not_logged_in4()
  }
})



##warning if no data selected or project name is null
observeEvent(eventExpr = input$mi.upload.button, {
  #if don't use demo data and don't upload data or give project name, error
  if(input$mi.use.demo.data == FALSE){
    if(is.null(input$MIms1PeakTable)| input$MIprojectID == ""){
      shinyalert::shinyalert(title = "Error",
                             text = "Project name, MS1 peak table are required.",
                             type = "error")
    }
  }
})


observeEvent(eventExpr = input$mi.upload.button, {
  #if don't use demo data and don't upload data or give project name, error
  if(input$mi.use.demo.data == TRUE){
    if(input$MIprojectID == ""){
      shinyalert::shinyalert(title = "Error",
                             text = "Project name is required.",
                             type = "error")
    }
  }
})


observeEvent(eventExpr = input$mi.upload.button, {
  #if choose there are MS2 data, but you don't uopload it, error
  if(input$MIhasMS2 == "Yes"){
    if(is.null(input$MIms2Data)){
      shinyalert::shinyalert(title = "Error",
                             text = "MS/MS data is required.",
                             type = "error")
    }
  }
})




##create folder
observeEvent(eventExpr = input$mi.upload.button, {
  if(!is.null(input$MIprojectID)){
    if(input$MIprojectID != ""){
      dir.create(file.path("./user_data", user_input$username, input$MIprojectID))
      dir.create(file.path("./user_data", user_input$username, input$MIprojectID, "metabolite_identification"))
    }
  }
})

##user.path
user.path <- reactive({
  req(user_input$username)
  req(input$MIprojectID)
  file.path("user_data", user_input$username, input$MIprojectID, "metabolite_identification")
})

###record job number
observeEvent(input$mi.upload.button,{
  if(!is.null(input$MIprojectID)){
    if(input$MIprojectID != ""){
      load("credentials/jobNumber")
      jobNumber <- jobNumber + 1
      save(jobNumber, file = "credentials/jobNumber")
    }
  }
})


# ###record job number
observeEvent(input$mi.upload.button,{
  if(!is.null(input$MIprojectID)){
    if(input$MIprojectID != ""){
      credentials <- readRDS("credentials/credentials.rds")
      credentials$job.number[match(user_input$username, credentials$user)] <- as.numeric(credentials$job.number[match(user_input$username, credentials$user)])+1
      saveRDS(credentials, "credentials/credentials.rds")
    }
  }
})


















###read MS1 peak table
mi.ms1.raw <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$mi.upload.button, {
  withProgress(message = 'Upload MS1 peak table...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 if(input$mi.use.demo.data == TRUE){##use demo data
                   if(input$MIprojectID != ""){
                     temp.file <- "data.csv"
                     mi.ms1.raw$data <- lapply(1:length(temp.file), function(idx){
                       x <- temp.file[idx]
                       incProgress(1/length(temp.file),
                                   detail = paste("Data", idx))
                       as.data.frame(readr::read_csv(file.path("./data/demo_data/Metabolite_Identification",x),
                                                     col_types = readr::cols()))
                     })[[1]]
                   }
                 }else{#upload data
                   if (!is.null(input$MIms1PeakTable) & input$MIprojectID != "") {
                     temp.file <- input$MIms1PeakTable$datapath
                     mi.ms1.raw$data <- lapply(1:length(temp.file), function(idx){
                       incProgress(1/length(temp.file),
                                   detail = paste("Data", idx))
                       as.data.frame(readr::read_csv(temp.file[idx],
                                                     col_types = readr::cols()))
                     })[[1]]
                   }
                 }
               })
})


##MS/MS data
mi.ms2.raw <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$mi.upload.button, {
  if(input$mi.use.demo.data == TRUE){
    if(input$MIprojectID != ""){
      withProgress(message = 'Upload MS/MS data...',
                   detail = 'This may take a while',
                   # style= "old",
                   value = 0, {
                     temp.file <-
                       dir("./data/demo_data/Metabolite_Identification")[grep("mgf",dir("./data/demo_data/Metabolite_Identification"))]
                     mi.ms2.raw$data <- lapply(1:length(temp.file), function(idx){
                       x <- temp.file[idx]
                       incProgress(1/length(temp.file),
                                   detail = paste("Data", idx))
                       readMGF(file = file.path("./data/demo_data/Metabolite_Identification",x))
                     })

                   })
    }
  }

  if(input$mi.use.demo.data == FALSE){
    if(!is.null(input$MIms2Data)){
      withProgress(message = 'Upload MS/MS data...',
                   detail = 'This may take a while',
                   # style= "old",
                   value = 0, {
                     temp.file <- input$MIms2Data$datapath
                       mi.ms2.raw$data <- lapply(1:length(temp.file), function(idx){
                         incProgress(1/length(temp.file),
                                     detail = paste("Data", idx))
                         if(input$mi.ms2.type == "mgf") {
                           readMGF(file = temp.file[idx])
                         }else{
                           readMSP(file = temp.file[idx])
                         }
                       })
                   })
    }
  }
})


###combine MS1 peak and MS2 spectra
mi.ms2.data <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$mi.upload.button,{
  if(!is.null(mi.ms2.raw$data)){
    withProgress(message = 'Combine MS1 peaks and MS/MS spectra...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   temp.ms2.data <- do.call(c, mi.ms2.raw$data)
                   names(temp.ms2.data) <- paste("peak", 1:length(temp.ms2.data), sep = "")
                   mi.ms2.data$data <- lapply(1, function(x){
                     incProgress(1/1,
                                 detail = "This may take a while")
                     mi.ms2.data <- combineMS1MS2(ms1.peak.table = mi.ms1.raw$data,
                                                  ms2.data = temp.ms2.data,
                                                  mz.tol = input$mi.ms1.ms2.match.mz.tol,
                                                  rt.tol = input$mi.ms1.ms2.match.rt.tol,
                                                  ms2.type = input$mi.ms2.type)
                   })[[1]]

                   mi.ms2.data
                 })
  }
})


###data profiles with MS information
output$mi.ms2.plot <- renderPlotly({
  if(is.null(mi.ms2.data$data)) return(NULL)
  mi.ms2.plot <- ms2MzRtPlot(object = mi.ms2.data$data)

  ggplotly(mi.ms2.plot,
           tooltip = c("name", "mz","rt"),
           source = "mi.ms2.plot")
})


##jump to data check page from upload data page
observeEvent(input$mi.upload.button, {
  if(!is.null(mi.ms1.raw$data) & input$MIprojectID != ""){
    updateTabsetPanel(session, inputId = "MItab",
                      selected = "MIdataCheck")
  }
})



















#------------------------------------------------------------------------------
##Check data
#check MS1 peak table
mi.ms1.check.result <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$mi.upload.button,{
  if(!is.null(mi.ms1.raw$data) & input$MIprojectID != ""){
    check.result1 <- miCheckMS1(peak.table = mi.ms1.raw$data)
  }else{
    check.result1 <- data.frame("Data.File" = "Data",
                               "Information" = "You don't provide MS1 peak table",
                               "Result" = "Error",
                               stringsAsFactors = FALSE)
  }

  check.result1 <- as.data.frame(check.result1)
  mi.ms1.check.result$data <- check.result1
})


##check MS/MS data
mi.ms2.check.result <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$mi.upload.button,{
  if(!is.null(mi.ms2.data$data)& input$MIprojectID != ""){
    check.result2 <- miCheckMS2(ms2.data = mi.ms2.data$data)
  }else{
    check.result2 <- data.frame("Data.File" = "Data",
                                "Information" = "You don't provide MS/MS data",
                                "Result" = "Warning",
                                stringsAsFactors = FALSE)
  }

  check.result2 <- as.data.frame(check.result2)
  mi.ms2.check.result$data <- check.result2
})


output$mi.data.check.result <- DT::renderDataTable(
  DT::datatable(rbind(mi.ms1.check.result$data, mi.ms2.check.result$data),
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
                                       "Error",
                                       "Warning"),
                                     c(rep('#FC4B4E', 4), "#FFCC00"))
  ),
  server = FALSE
)



##MS2 spectrum for one peak
mi.single.ms2 <- reactive({
  if(is.null(mi.ms2.data$data)) return(NULL)
  d <- event_data("plotly_click",
                  source = "mi.ms2.plot")
  if(!is.null(d)){
    # save(d, file = "d")
    mi.single.ms2 <- mi.ms2.data$data[[d[1,2]+1]]
    # save(mi.single.ms2, file = "mi.single.ms2")
    mi.single.ms2
  }else{
    return(NULL)
  }
})


output$mi.single.ms2.plot <- renderPlotly({
  if(is.null(mi.single.ms2())) return(NULL)
  mi.single.ms2.plot <- ms2Plot(object = mi.single.ms2())
  ggplotly(mi.single.ms2.plot,
           tooltip = c("mz","intensity"),
           source = "mi.single.ms2.plot")

})




##jump to upload data page from data check page
observeEvent(input$mi.data.check.2.mi.upload, {
  updateTabsetPanel(session, "MItab",
                    selected = "MIupload")

})

##jump to m/z match page from data check
observeEvent(input$mi.data.check.2.mz.match, {
  if(length(grep(pattern = "Error",mi.ms1.check.result$data[,3])) == 0 &
     length(grep(pattern = "Error",mi.ms2.check.result$data[,3])) == 0){
    updateTabsetPanel(session, "MItab",
                      selected = "MImzMatch")
  }
})

##error if there is error in data
observeEvent(eventExpr = input$mi.data.check.2.mz.match, {
  if(length(grep(pattern = "Error",mi.ms1.check.result$data[,3])) != 0 |
     length(grep(pattern = "Error",mi.ms2.check.result$data[,3])) != 0){
    shinyalert::shinyalert(title = "Error",
                           text = "There are errors in you files, please click Previous to check and
                           upload again.",
                           type = "error")
  }
})



















##mz identification
mi.mz.match.result <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$mi.mz.match.button,{
  adduct <- ifelse(input$miMZpolarity == "positive",
                   input$mi.mz.pos.adduct, input$mi.mz.neg.adduct)
  adduct <- unname(sapply(adduct, paraTrans))
  withProgress(message = 'Processing...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 mi.mz.match.result$data <- lapply(1:1, function(idx){
                       incProgress(1/1,
                                   detail = "This may take a while")
                       mzIdentify(peak.table = mi.ms1.raw$data[,c(1:3)],
                                  polarity = input$miMZpolarity,
                                  adduct = adduct,
                                  database = input$mi.mz.match.library,
                                  mz.tol = input$mi.mz.match.mz.tol)
                     })[[1]]
               })

})



output$mi.mz.match.result <- DT::renderDataTable(
  DT::datatable(mi.mz.match.result$data,
                selection = "single",
                escape = FALSE,
                filter = "none",
                rownames= FALSE,
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
                        buttons = list(list(extend="csv",filename="MS1.identification.result"),
                                       list(extend='excel',filename="MS1.identification.result")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)


###MS1 match information for each peak
observeEvent(eventExpr = input$miMZgo1, {
  if (is.null(input$mi.mz.match.result_rows_selected)) {

    output$mi.peak.ms1.match.info1 <- renderPrint({"No peak selected"})
    output$mi.peak.ms1.match.info2 <- DT::renderDataTable(NULL)

  }else{
    compound.name <- mi.mz.match.result$data[as.numeric(input$mi.mz.match.result_rows_selected),"Compound.name"]
    ID <- mi.mz.match.result$data[as.numeric(input$mi.mz.match.result_rows_selected),"ID"]
    Adduct <- mi.mz.match.result$data[as.numeric(input$mi.mz.match.result_rows_selected),"Adduct"]
    mz.error <- mi.mz.match.result$data[as.numeric(input$mi.mz.match.result_rows_selected),"mz.error"]

    if(is.na(compound.name)) {
      output$mi.peak.ms1.match.info1 <- renderPrint({"No identification"})
      output$mi.peak.ms1.match.info2 <- DT::renderDataTable(NULL)
    }else{
      compound.name <- strsplit(compound.name, split = ";")[[1]]
      ID <- strsplit(ID, split = ";")[[1]]
      ID1 <- paste("http://www.kegg.jp/dbget-bin/www_bget?cpd:", ID, sep = "")
      ID <- paste0("<a href='",ID1,"'>",ID,"</a>")
      Adduct <- strsplit(Adduct, split = ";")[[1]]
      mz.error <- strsplit(mz.error, split = ";")[[1]]

      mi.peak.ms1.match.info2 <- data.frame(compound.name, ID, Adduct, mz.error, stringsAsFactors = FALSE)

      output$mi.peak.ms1.match.info1 <- renderPrint({
        mi.mz.match.result$data[as.numeric(input$mi.mz.match.result_rows_selected),"name"]
      })

      output$mi.peak.ms1.match.info2 <- DT::renderDataTable(
        DT::datatable(mi.peak.ms1.match.info2,
                      selection = "single",
                      escape = FALSE,
                      filter = "none",
                      rownames= FALSE,
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
                              buttons = list(list(extend="csv",filename="Sample name"),
                                             list(extend='excel',filename="Sample name")),
                              text = 'Download'
                            ),
                            I('colvis')
                          ),
                        scrollX = TRUE
                      )
        ),
        server = FALSE
      )
    }
  }
})



##jump to MS/MS match page from m/z match
observeEvent(input$mi.mz.match.2.ms2.match, {
  if(!is.null(mi.mz.match.result$data)){
    updateTabsetPanel(session, "MItab",
                      selected = "MIms2Match")
  }
})

##error if you don't submit first
observeEvent(eventExpr = input$mi.mz.match.2.ms2.match, {
  if(is.null(mi.mz.match.result$data)){
    shinyalert::shinyalert(title = "Error",
                           text = "Please click submit first",
                           type = "error")
  }
})



















###-----------------------------------------------------------------------------
###MS/MS match identification
mi.ms2.match.result <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$mi.ms2.match.button,{
  # if(is.null(mi.ms2.data$data)) return(NULL)
  withProgress(message = 'Processing...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 #preparae MS2 spectra
                 mi.ms2.tags <- mi.ms1.raw$data[,c(1:3)]
                 colnames(mi.ms2.tags)[2] <- "mzmed"
                 ms2.name <- unname(unlist(lapply(mi.ms2.data$data, function(x) x[[1]][1,])))
                 ms1.name <- mi.ms2.tags$name
                 temp.idx <- match(ms2.name , ms1.name)
                 spec <- vector(mode = "list", length = length(ms1.name))
                 names(spec) <- ms1.name
                 spec[temp.idx] <- mi.ms2.data$data
                 spec <- lapply(spec, function(x){
                   if(is.null(x)) return(x)
                   x[[2]]
                 })

                 #MS2 library
                 if(input$mi.ms2.instrument == "mi.ms2.agilent.tof"){
                   load("data/database/zhuMetlib.rda", envir = environment())
                   library = zhuMetlib
                 }

                 if(input$mi.ms2.instrument == "mi.ms2.sciex.tof"){
                   load("data/database/zhuMetlib.rda", envir = environment())
                   library = zhuMetlib
                 }

                 if(input$mi.ms2.instrument == "mi.ms2.other.tof"){
                   load("data/database/zhuMetlib.rda", envir = environment())
                   library = zhuMetlib
                 }

                 if(input$mi.ms2.instrument == "mi.ms2.thermo.orbi"){
                   load("data/database/orbitrapMetlib.rda", envir = environment())
                   library = orbitrapMetlib
                 }

                 ###CE value
                 if(input$mi.ms2.instrument == "mi.ms2.agilent.tof"){
                   ce <- as.character(as.numeric(input$mi.ms2.ce) + 10)
                 }

                 if(input$mi.ms2.instrument == "mi.ms2.sciex.tof"){
                   ce <- as.character(as.numeric(input$mi.ms2.ce))
                 }

                 if(input$mi.ms2.instrument == "mi.ms2.other.tof"){
                   ce <- as.character(as.numeric(input$mi.ms2.ce) + 10)
                 }

                 if(input$mi.ms2.instrument == "mi.ms2.thermo.orbi"){
                   ce <- as.character(input$mi.ms2.ce)
                 }

                 if(input$miMS2polarity == "positive") pol <- "pos"
                 if(input$miMS2polarity == "negative") pol <- "neg"

                 #adduct
                 adduct.pos <- input$mi.ms2.pos.adduct
                 adduct.neg <- input$mi.ms2.neg.adduct
                 if(input$miMS2polarity == "positive") adduct <- adduct.pos
                 if(input$miMS2polarity == "negative") adduct <- adduct.neg
                 adduct <- unname(sapply(adduct, paraTrans))
                 adduct <- unname(sapply(adduct, paraTrans))

                 ##adduct table
                 if(pol == "pos"){
                   load("data/database/hilic.pos.rda", envir = environment())
                   adduct.table <- hilic.pos
                   rm(list = c("hilic.pos"))
                   adduct.table <- adduct.table[adduct.table$adduct %in% adduct,]
                 }else{
                   load("data/database/hilic.neg.rda", envir = environment())
                   adduct.table <- hilic.neg
                   rm(list = c("hilic.neg"))
                   adduct.table <- adduct.table[adduct.table$adduct %in% adduct,]
                 }
                 result <- lapply(1:1, function(idx){
                   incProgress(1/1,
                               detail = "This may take a while")
                   MetID(info = mi.ms2.tags,
                         spec = spec,
                         dp.cutoff = input$mi.ms2.dp.tol,
                         lib = library,
                         pol = pol,
                         ce = as.character(ce),
                         adduct = adduct.table,
                         d.out = ".",
                         lc = 'HILIC',
                         ms2.match.plot = FALSE)
                 })[[1]]

                 colnames(result)[1] <- "name"
                 colnames(result)[2] <- "mz"
                 colnames(mi.ms2.tags)[2] <- "mz"
                 load("data/database/inHouse.compound.rda", envir = environment())

                 result$hits.reverse[result$hits.reverse == ""] <- NA
                 result$hits.forward[result$hits.forward == ""] <- NA

                 result <- readAnnotation(data = result,
                                                       rt.filter = FALSE,
                                                       inHouse.compound = inHouse.compound)

                 colnames(result)[1] <- "name"

                 result <- result[,c(colnames(mi.ms2.tags),
                             c("Metabolite.name","KEGG.ID", "adduct", "mz.error", "ms2.sim", "labid"))]
                 colnames(result)[which(colnames(result) == "Metabolite.name")] <- "Compound.name"
                 colnames(result)[which(colnames(result) == "adduct")] <- "Adduct"
                 colnames(result)[which(colnames(result) == "KEGG.ID")] <- "ID"
                 colnames(result)[which(colnames(result) == "ms2.sim")] <- "Dot product"
                 colnames(result)[which(colnames(result) == "labid")] <- "Lab.ID"

                 rm(list = c("mi.ms2.tags", "spec",
                             "adduct.table", "inHouse.compound", "library"))
                 cat("MS2 match is done.\n")
                 mi.ms2.match.result$data <- result
                 # temp <- mi.ms2.match.result$data
                 # save(temp, file = "temp")
               })
})



output$mi.ms2.match.result <- DT::renderDataTable(
  DT::datatable(mi.ms2.match.result$data,
                selection = "single",
                escape = FALSE,
                filter = "none",
                rownames= FALSE,
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
                        buttons = list(list(extend="csv",filename = "MS2.identification.result"),
                                       list(extend='excel',filename = "MS2.identification.result name")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)


##MS2 spec
mi.ms2.spec <- eventReactive(eventExpr = input$mi.ms2.match.button, {
  mi.tags <- mi.ms1.raw$data[,c(1:3)]
  colnames(mi.tags)[2] <- "mzmed"
  ms2.name <- unname(unlist(lapply(mi.ms2.data$data, function(x) x[[1]][1,])))
  ms1.name <- mi.tags$name
  temp.idx <- match(ms2.name , ms1.name)
  spec <- vector(mode = "list", length = length(ms1.name))
  names(spec) <- ms1.name
  spec[temp.idx] <- mi.ms2.data$data
  spec <- lapply(spec, function(x){
    if(is.null(x)) return(x)
    x[[2]]
  })
  spec
})


###MS2 match information for each peak
observeEvent(eventExpr = input$miMS2go2, {
  polarity <- input$miMS2polarity
  if (is.null(input$mi.ms2.match.result_rows_selected)) {
    output$mi.peak.ms2.match.info1 <- renderPrint({"No peak selected"})
    output$mi.peak.ms2.match.info2 <- DT::renderDataTable(NULL)

  }else{
    name <- mi.ms2.match.result$data[as.numeric(input$mi.ms2.match.result_rows_selected),"name"]
    compound.name <- mi.ms2.match.result$data[as.numeric(input$mi.ms2.match.result_rows_selected),"Compound.name"]
    ID <- mi.ms2.match.result$data[as.numeric(input$mi.ms2.match.result_rows_selected),"ID"]
    Adduct <- mi.ms2.match.result$data[as.numeric(input$mi.ms2.match.result_rows_selected),"Adduct"]
    mz.error <- mi.ms2.match.result$data[as.numeric(input$mi.ms2.match.result_rows_selected),"mz.error"]
    dot.product <- mi.ms2.match.result$data[as.numeric(input$mi.ms2.match.result_rows_selected),"Dot product"]
    lab.id <- mi.ms2.match.result$data[as.numeric(input$mi.ms2.match.result_rows_selected),"Lab.ID"]

    if(is.na(compound.name)) {
      output$mi.peak.ms2.match.info1 <- renderPrint({"No identification"})
      output$mi.peak.ms2.match.info2 <- DT::renderDataTable(NULL)
    }else{
      name <- strsplit(name, split = ";")[[1]]
      compound.name <- strsplit(compound.name, split = ";")[[1]]
      ID <- strsplit(ID, split = ";")[[1]]
      ID1 <- paste("http://www.kegg.jp/dbget-bin/www_bget?cpd:", ID, sep = "")
      ID <- paste0("<a href='",ID1,"'>",ID,"</a>")
      Adduct <- strsplit(Adduct, split = ";")[[1]]
      mz.error <- strsplit(mz.error, split = ";")[[1]]
      dot.product <- strsplit(dot.product, split = ";")[[1]]
      lab.id <- strsplit(lab.id, split = ";")[[1]]

      mi.peak.ms2.match.info2 <- data.frame(name, compound.name, ID, Adduct,
                                            mz.error,dot.product, "Lab.ID" = lab.id,
                                            stringsAsFactors = FALSE)


      output$mi.peak.ms2.match.info1 <- renderPrint({
        mi.ms2.match.result$data[as.numeric(input$mi.ms2.match.result_rows_selected),"name"]
      })

      output$mi.peak.ms2.match.info2 <- DT::renderDataTable(
        DT::datatable(mi.peak.ms2.match.info2,
                      selection = "single",
                      escape = FALSE,
                      filter = "none",
                      rownames= FALSE,
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
                              buttons = list(list(extend="csv",filename="Sample name"),
                                             list(extend='excel',filename="Sample name")),
                              text = 'Download'
                            ),
                            I('colvis')
                          ),
                        scrollX = TRUE
                      )
        ),
        server = FALSE
      )


      #-----------------------------------------------------
      observeEvent(eventExpr = input$miMS2go3, {
        if (is.null(input$mi.peak.ms2.match.info2_rows_selected)){
          output$mi.single.ms2.match.plot <- renderPlot({
            par(mar = c(5,5,4,2))
            plot(1, col = "white", xaxt = "n", yaxt = "n",
                 xlab = "", ylab = "")
            legend("top", legend = "No compound is selected!")
          })
        }else{
          peak.name <- mi.peak.ms2.match.info2[as.numeric(input$mi.peak.ms2.match.info2_rows_selected),"name"]
          lab.id <- mi.peak.ms2.match.info2[as.numeric(input$mi.peak.ms2.match.info2_rows_selected),"Lab.ID"]

          if(input$mi.ms2.instrument == "mi.ms2.agilent.tof"){
            load("data/database/zhuMetlib.rda", envir = environment())
            library <- zhuMetlib
            rm(list = c("zhuMetlib"))
          }

          if(input$mi.ms2.instrument == "mi.ms2.sciex.tof"){
            load("data/database/zhuMetlib.rda", envir = environment())
            library <- zhuMetlib
            rm(list = c("zhuMetlib"))
          }

          if(input$mi.ms2.instrument == "mi.ms2.other.tof"){
            load("data/database/zhuMetlib.rda", envir = environment())
            library <- zhuMetlib
            rm(list = c("zhuMetlib"))
          }

          if(input$mi.ms2.instrument == "mi.ms2.thermo.orbi"){
            load("data/database/orbitrapMetlib.rda", envir = environment())
            library <- orbitrapMetlib
            rm(list = c("orbitrapMetlib"))
          }

          ce <- input$mi.ms2.ce
          peak.spec <- mi.ms2.spec()[match(peak.name, names(mi.ms2.spec()))]
          # save(lab.id, file = "lab.id")
          # save(polarity, file = "polarity")
          # save(ce, file = "ce")
          # save(library, file = "library")
          lab.spec <- getMS2(libID = lab.id, polarity = polarity,
                             ce = ce, lib = library)

          output$mi.single.ms2.match.plot <- renderPlot({
            par(mar = c(5,5,4,2))
            ms2Plot2(spectrum1 = peak.spec, spectrum2 = lab.spec)
          })

        }
      })

      #-----------------------------------------------------
    }
  }
})