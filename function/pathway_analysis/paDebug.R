
                         pa.met.name.list <- example.compound.list2


###get dysregulated metabolite ID list and check data
match.result <- list(data = NULL)


    check.result <- try({paCheckData(paMetaboliteType = "peak.mz",
                                     pa.met.name.list = pa.met.name.list,
                                     paMZpolarity = "positive",
                                     pa.mz.pos.adduct = c("M+H",
                                                          "M+K",
                                                          "M+Na",
                                                          "M+NH4"),
                                     pa.mz.neg.adduct = c("M-H",
                                                          "M+Cl",
                                                          "M+CH3COO"),
                                     pa.mz.match.mz.tol = 25,
                                     pa.mz.match.library = "kegg"
    )})
    if(class(check.result)[1] == "try-error"){
      pa.data.check.result.message <- renderText({paste("Data check: ",check.result[[1]])})
      match.result$data <- data.frame("ID" = NA,
                                      "Name" = NA,
                                      "CAS.ID" = NA,
                                      "HMDB.ID" = NA,
                                      stringsAsFactors = FALSE)
    }else{
      info <- check.result$info
      pa.data.check.result.message <- renderText({info})
      match.result$data <- check.result$table

    }


    paMetaboliteType <- "peak.mz"
    paMZpolarity <- "positive"
    pa.mz.pos.adduct <- c(
      "M+H",
      "M+K",
      "M+Na",
      "M+NH4"
    )

    pa.mz.neg.adduct <- c(
      "M-H",
      "M+Cl",
      "M+CH3COO"
    )

    pa.mz.match.mz.tol <- 25
    pa.mz.match.library <- "kegg"
    pa.pathway.library <- "hsa"

    pa.algorithm <- "hypergeometric"

    temp <- try({
      paParams <- paParam(paMetaboliteType = paMetaboliteType,
                          paMZpolarity = paMZpolarity,
                          pa.mz.pos.adduct = pa.mz.pos.adduct,
                          pa.mz.neg.adduct = pa.mz.neg.adduct,
                          pa.mz.match.mz.tol = pa.mz.match.mz.tol,
                          pa.mz.match.library = pa.mz.match.library,
                          pa.pathway.library = pa.pathway.library,
                          pa.algorithm = pa.algorithm)
    })




    mse.result <- list(data = NULL)
        if(length(grep("YES",match.result$data[,"Result"])) > 2){

                         temp <- try({
                           lapply(1:length(1), function(idx){
                             metabolite.id <- match.result$data[,"ID"]
                             metabolite.id <- metabolite.id[!is.na(metabolite.id)]
                             metabolite.id <- unique(metabolite.id)

                             M <- getKeggLibrary(species = pa.pathway.library)

                             mseAnalysis(metabolite.id = metabolite.id,
                                         M = M, test.method = pa.algorithm)

                           })[[1]]
                         })

                         if(class(temp)[1] == "try-error"){
                           info <- temp[[1]]
                           output$pa.pathway.enrichment.message <- renderText({paste("Roungh alignment: ",info)})
                           rm(list = c("temp"))
                         }else{
                           mse.result$data <- temp
                           output$pa.pathway.enrichment.message <- renderText({""})
                           rm(list = c("temp"))
                         }
        }
      }

