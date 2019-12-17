output$pathwayAnalysis.area <- renderUI({
  if (user_input$authenticated == TRUE){
    ui.pathwayAnalysis()
  }else{
    not_logged_in6()
  }
})


observeEvent(eventExpr = input$paMetaboliteType,{
  if(input$paMetaboliteType == "peak.mz"){
    updateTextAreaInput(session, inputId = "pa.met.name.list",
                        value = example.compound.list2)
  }

  if(input$paMetaboliteType == "kegg.id"){
    updateTextAreaInput(session, inputId = "pa.met.name.list",
                        value = example.compound.list)
  }

})


##warning if no metabolite selected or project name is null
observeEvent(eventExpr = input$pa.enter.button, {
  #if don't use demo data and don't upload data or give project name, error
    if(is.null(input$pa.met.name.list) | input$PAprojectID == ""){
      shinyalert::shinyalert(title = "Error",
                             text = "Project name, Metabolite list are required.",
                             type = "error")
    }

})



##create folder
observeEvent(eventExpr = input$pa.enter.button, {
  if(!is.null(input$PAprojectID)){
    if(input$PAprojectID != ""){
      dir.create(file.path("./user_data", user_input$username))
      dir.create(file.path("./user_data", user_input$username, input$PAprojectID))
      dir.create(file.path("./user_data", user_input$username, input$PAprojectID, "pathway_analysis"))
    }
  }
})

##user.path
user.path <- reactive({
  req(user_input$username)
  req(input$PAprojectID)
  file.path("user_data", user_input$username, input$PAprojectID, "pathway_analysis")
})

# ###record job number
observeEvent(input$pa.enter.button,{
  if(!is.null(input$PAprojectID)){
    if(input$PAprojectID != ""){
      credentials <- readRDS("credentials/credentials.rds")
      credentials$job.number[match(user_input$username, credentials$user)] <- as.numeric(credentials$job.number[match(user_input$username, credentials$user)])+1
      saveRDS(credentials, "credentials/credentials.rds")
    }
  }
})









##jump to data check page from upload data page
observeEvent(input$pa.enter.button, {
  if(!is.null(input$pa.met.name.list) & input$PAprojectID != ""){
    if(length(input$pa.met.name.list) != 0){
      updateTabsetPanel(session, inputId = "PAtab",
                        selected = "PAdataCheck")
    }
  }

})


##save data user upload
observeEvent(eventExpr = input$pa.enter.button, {
  if(!is.null(input$PAprojectID)){
    if(input$PAprojectID != ""){
      withProgress(message = 'Save data...',
                   detail = 'This may take a while',
                   # style= "old",
                   value = 0, {
                     lapply(1:length(1), function(idx){
                       incProgress(1/length(1),
                                   detail = "This may take a while")
                       if(!is.null(input$pa.met.name.list)){
                         pa.met.name.list <- input$pa.met.name.list
                         save(pa.met.name.list, file = file.path("user_data", user_input$username,
                                                                 input$PAprojectID,
                                                                 "pathway_analysis",
                                                                 "pa.met.name.list"))
                         rm(list= c("pa.met.name.list"))
                       }
                     })
                   })
    }
  }
})


###get dysregulated metabolite ID list and check data
match.result <- reactiveValues(data = NULL)
# pa.metabolite.id <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$pa.enter.button,{
  if(!is.null(input$pa.met.name.list) & input$PAprojectID != ""){

    withProgress(message = 'Check data...',
                 detail = 'This may take a while',
                 # style= "old",
                 value = 0, {
                   check.result <- lapply(1:length(1), function(idx){
                     incProgress(1/length(1),
                                 detail = "This may take a while")
                     try({paCheckData(paMetaboliteType = input$paMetaboliteType,
                                                      pa.met.name.list = input$pa.met.name.list,
                                                      paMZpolarity = input$paMZpolarity,
                                                      pa.mz.pos.adduct = input$pa.mz.pos.adduct,
                                                      pa.mz.neg.adduct = input$pa.mz.neg.adduct,
                                                      pa.mz.match.mz.tol = input$pa.mz.match.mz.tol,
                                                      pa.mz.match.library = input$pa.mz.match.library
                     )})
                   })[[1]]
                 })

    if(class(check.result)[1] == "try-error"){
      output$pa.data.check.result.message <- renderText({paste("Data check: ",check.result[[1]])})
      match.result$data <- data.frame("ID" = NA,
                                 "Name" = NA,
                                 "CAS.ID" = NA,
                                 "HMDB.ID" = NA,
                                 "Result" = "NO",
                                 stringsAsFactors = FALSE)
    }else{
      info <- check.result$info
      output$pa.data.check.result.message <- renderText({info})
      match.result$data <- check.result$table
      # save(check.result, file = "check.result")

    }
  }else{
    match.result$data <- data.frame("ID" = NA,
                               "Name" = NA,
                               "CAS.ID" = NA,
                               "HMDB.ID" = NA,
                               "Result" = "NO",
                               stringsAsFactors = FALSE)
  }
})



output$match.result <- DT::renderDataTable(
  DT::datatable(match.result$data,
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
                        buttons = list(list(extend="csv",filename = "Match.result"),
                                       list(extend='excel',filename = "Match.result")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ) %>% DT::formatStyle(
    'Result',
    target = 'row',
    backgroundColor = DT::styleEqual(c("NO"),
                                     c(rep('#FC4B4E', 1)))
  ),
  server = FALSE
)


##jump to upload data page from data check page
observeEvent(input$pa.data.check.2.pa.enter, {
  updateTabsetPanel(session, "PAtab",
                    selected = "PAenter")

})


##jump to pathway enrichment page from data check
observeEvent(input$pa.data.check.2.pathway.enrichment, {
  if(!is.null(match.result$data)){
    if(length(grep(pattern = "YES",match.result$data[,"Result"])) > 2){
      updateTabsetPanel(session, "PAtab",
                        selected = "PApathwayEnrichment")
    }
  }
})


##error if all metabolite have no KEGG ID
observeEvent(eventExpr = input$pa.data.check.2.pathway.enrichment, {
  if(!is.null(match.result$data)){
    if(length(grep(pattern = "YES",match.result$data[,"Result"])) == 0){
      shinyalert::shinyalert(title = "Error",
                             text = "All metabolites or peaks have NO KEGG ID.",
                             type = "error")
    }
  }
  })


















####pathway enrichment analysis
##save parameter
pa.params <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$paSubmit,{
  temp <- try({
    paParams <- paParam(paMetaboliteType = input$paMetaboliteType,
                         paMZpolarity = input$paMZpolarity,
                         pa.mz.pos.adduct = input$pa.mz.pos.adduct,
                         pa.mz.neg.adduct = input$pa.mz.neg.adduct,
                         pa.mz.match.mz.tol = input$pa.mz.match.mz.tol,
                         pa.mz.match.library = input$pa.mz.match.library,
                         pa.pathway.library = input$pa.pathway.library,
                         pa.algorithm = input$pa.algorithm)
    pa.params$data <- paParams
    save(paParams, file = file.path("user_data", user_input$username,
                                      input$PAprojectID, "pathway_analysis/paParams"))
    pa.met.name.list <- input$pa.met.name.list
    save(pa.met.name.list, file = file.path("user_data", user_input$username,
                                    input$PAprojectID, "pathway_analysis/pa.met.name.list"))

  })

  if(class(temp)[1] == "try-error"){
    info <- temp[[1]]
    output$pa.pathway.enrichment.message <- renderText({paste("Pathway enrichment parameters: ",info)})
    rm(list="temp")
  }else{
    output$pa.pathway.enrichment.message <- renderText({""})
    rm(list="temp")
  }

})


###begin pathway enrichment
mse.result <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$paSubmit,{
if(!is.null(match.result$data)){
if(length(grep("YES",match.result$data[,"Result"])) > 2){

       withProgress(message = 'Pathway enrichment...',
               detail = 'This may take a while',
               # style= "old",
               value = 0, {
                 temp <- try({
                   lapply(1:length(1), function(idx){
                   incProgress(1/length(1),
                   detail = "This may take a while")
                     metabolite.id <- match.result$data[,"ID"]
                     metabolite.id <- metabolite.id[!is.na(metabolite.id)]
                     metabolite.id <- unique(metabolite.id)

                     M <- getKeggLibrary(species = input$pa.pathway.library)

                     mseAnalysis(metabolite.id = metabolite.id,
                                 M = M, test.method = input$pa.algorithm)

                   })[[1]]
                 })


                 if(class(temp)[1] == "try-error"){
                   info <- temp[[1]]
                   output$pa.pathway.enrichment.message <- renderText({paste("Roungh alignment: ",info)})
                   rm(list = c("temp"))
                 }else{
                   mse.result$data <- temp
                   info <- paste(nrow(temp), "pathways are mapped with at least 1 metabolite.")
                   output$pa.pathway.enrichment.message <- renderText({info})
                   rm(list = c("temp"))
                 }
               })

}
}
}
)

####pathway overview plot
pa.pathway.plot <- reactiveValues(data = NULL)
observeEvent(eventExpr = input$paSubmit, {
  if(!is.null(mse.result$data)){
    pa.pathway.plot$data <- pathwayOverview(object = mse.result$data,
                                            p.cutoff = input$pa.p.cutoff,
                                            overlap.cutoff = input$pa.overlap.cutoff)
  }
})



output$pa.pathway.plot <- renderPlotly({
  if(is.null(pa.pathway.plot$data)) return(NULL)
  ggplotly(pa.pathway.plot$data,
           # tooltip = c("mz", "mz.error"),
           source = "pa.pathway.plot")
})

####pathway overview table
output$pa.pathway.table <- DT::renderDataTable(
  DT::datatable(mse.result$data,
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
                        buttons = list(list(extend="csv",filename = "Pathway.enrichment.analysis.result"),
                                       list(extend='excel',filename = "Pathway.enrichment.analysis.result")),
                        text = 'Download'
                      ),
                      I('colvis')
                    ),
                  scrollX = TRUE
                )
  ),
  server = FALSE
)



##jump to result download page from pathway enrichment
observeEvent(input$pa.pathway.enrichment.2.result.download, {
  if(!is.null(mse.result$data)){
      updateTabsetPanel(session, "PAtab",
                        selected = "PAdownload")
  }
})







##download
####generate analysis report
observeEvent(eventExpr = input$pa.generate.analysis.report, {
  if(is.null(mse.result$data)) return(NULL)
  now.path <- getwd()
  user.path <- file.path("user_data", user_input$username, input$PAprojectID, "pathway_analysis")
  tempReport <- file.path(now.path, user.path, "PAreport.temp.Rmd")
  file.copy("data/markdown/PAreport.Rmd", tempReport, overwrite = TRUE)

  params <- list(##parameters
    parameter = pa.params$data,
    match.result = match.result$data,
    pa.pathway.plot = pa.pathway.plot$data,
    pa.pathway.table = mse.result$data
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
                                                               "Analysis.Report.of.Pathway.Enrichment.Analysis.html"),
                                       params = params,
                                       envir = new.env(parent = globalenv())
                     )
                   })

                   if(class(temp)[1] == "try-error"){
                     info <- temp[[1]]
                     output$paGenerateReport.message <- renderText({paste("Generate report: ",info)})
                     rm(list = c("temp"))
                   }else{
                     output$paGenerateReport.message <- renderText({""})
                     rm(list = c("temp"))
                   }

                 })
               })
})


####download analysis report
output$pa.report.download <- downloadHandler(
  filename = "Analysis.Report.of.Pathway.Enrichment.Analysis.html",
  content = function(file){
    file.copy(file.path("user_data", user_input$username,
                        input$PAprojectID, "pathway_analysis",
                        "Analysis.Report.of.Pathway.Enrichment.Analysis.html"), file)
  }
)

####generate analysis result
observeEvent(eventExpr = input$pa.generate.analysis.result, {

  if(!is.null(mse.result$data)){
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
                       temp.path <- file.path(now.path, "user_data", user_input$username,
                                              input$PAprojectID, "pathway_analysis",
                                              "Pathway_Enrichment_Analysis_Result")
                       dir.create(temp.path)
                       temp.path1 <- file.path(temp.path, "Figures")
                       dir.create(temp.path1)

                       readr::write_csv(match.result$data,
                                        file.path(temp.path, "Input.metabolites.csv"))

                       readr::write_csv(mse.result$data,
                                        file.path(temp.path, "Pathway.Enrichment.Result.csv"))

                       ##pathway overview
                       ggsave(pa.pathway.plot$data,
                              file = file.path(temp.path1, "Pathway.Enrichment.Overview.pdf"), width = 7, height = 7)



                       setwd(file.path("user_data", user_input$username, input$PAprojectID, "pathway_analysis"))
                       zip::zip(zipfile = file.path(now.path, "user_data", user_input$username, input$PAprojectID,
                                                    "pathway_analysis",
                                                    "Pathway_Enrichment_Analysis_Result.zip"),
                                files = "Pathway_Enrichment_Analysis_Result",
                                recurse = TRUE
                       )
                     })

                     if(class(temp) == "try-error"){
                       info <- temp[[1]]
                       setwd(now.path)
                       output$paGenerateResult.message <- renderText({paste("Generate result: ",info)})
                       rm(list = c("temp"))
                     }else{
                       setwd(now.path)
                       output$paGenerateResult.message <- renderText({""})
                       rm(list = c("temp"))
                     }
                   })
                 })
  }
})


####download analysis result
output$pa.result.download <- downloadHandler(
  filename = "Pathway_Enrichment_Analysis_Result.zip",
  content = function(file){
    file.copy(file.path("user_data", user_input$username,
                        input$PAprojectID, "pathway_analysis",
                        "Pathway_Enrichment_Analysis_Result.zip"), file)
  }
)

















