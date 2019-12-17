paCheckData <- function(paMetaboliteType,
                        pa.met.name.list,
                        # kegg.compound,
                        paMZpolarity = c("positive", "negative"),
                        pa.mz.pos.adduct = c("M+H",
                                             "M+K",
                                             "M+Na",
                                             "M+NH4"),
                        pa.mz.neg.adduct = c("M-H",
                                             "M+Cl",
                                             "M+CH3COO"),
                        pa.mz.match.mz.tol = 25,
                        pa.mz.match.library = c("kegg")
                        ){

  paMZpolarity <- match.arg(paMZpolarity)
  pa.mz.match.library <- match.arg(pa.mz.match.library)

  if(length(pa.mz.pos.adduct) == 0) {
    pa.mz.pos.adduct <- "pa.plus.h"
  }

  if(length(pa.mz.neg.adduct) == 0) {
    pa.mz.neg.adduct <- "pa.minus.h"
  }


if(is.null(pa.met.name.list)) {
  table <- NULL
  info <- "You don't provide metabolites"
  return(list("table" = table, "info" = info))
}

if(pa.met.name.list == "") {
  table <- NULL
  info <- "You don't provide metabolites"
  return(list("table" = table, "info" = info))
}

pa.met.name.list <- strsplit(pa.met.name.list, "\n")[[1]]

if(paMetaboliteType == "kegg.id"){
  pa.met.name.list <- as.character(pa.met.name.list)
  pa.met.name.list <- pa.met.name.list[!is.na(pa.met.name.list)]
  if(length(pa.met.name.list) == 0){
    table <- NULL
    info <- "The metabolites may not have KEGG ID"
    return(list("table" = table, "info" = info))
  }

  load("data/database/kegg.compound.rda")

  idx <- match(pa.met.name.list, kegg.compound$ID)
  # idx <- idx[!is.na(idx)]
  if(all(is.na(idx))){
    table <- NULL
    info <- "The metabolites may not have KEGG ID"
    return(list("table" = table, "info" = info))
  }

  if(all(!is.na(idx))){
    info <- "All metabolites are in KEGG database"
  }else{
    info <- paste(sum(is.na(idx)), "metabolite are not in KEGG database.")
  }

  table <- kegg.compound[idx, c(1,2,4,5)]
  rm(list = c("kegg.compound"))
  table$Name <- unlist(lapply(strsplit(table$Name, split = ";"), function(x) x[1]))
  Result <- rep(NA, nrow(table))
  Result[!is.na(table$Name)] <- "YES"
  Result[is.na(Result)] <- "NO"
  table <- data.frame(table, Result, stringsAsFactors = FALSE)
  return(list("table" = table, "info" = info))
}else{
  pa.met.name.list <- as.numeric(pa.met.name.list)
  ##begin metabolite identification
  peak.table <- data.frame("name" = paste("peak", 1:length(pa.met.name.list), sep = ""),
                           "mz" = pa.met.name.list,
                           stringsAsFactors = FALSE)

  if(paMZpolarity == "positive"){
    adduct <- pa.mz.pos.adduct
  }else{
    adduct <- pa.mz.neg.adduct
  }
  identification.result <- mzIdentify(peak.table = peak.table,
                                      polarity = paMZpolarity,
                                      adduct = adduct,
                                      database = "kegg",
                                      mz.tol = pa.mz.match.mz.tol,
                                      candidate.num = 1)

  table <- identification.result
  rm(list = c("identification.result"))

  if(all(!is.na(table$ID))){
    info <- "All peaks are identified in KEGG database."
  }else{
    info <- paste(sum(!is.na(table$ID)), "peaks are identified in KEGG database.")
  }

  Result <- rep(NA, nrow(table))
  Result[!is.na(table$ID)] <- "YES"
  Result[is.na(Result)] <- "NO"
  table <- data.frame(table, Result, stringsAsFactors = FALSE)
  return(list("table" = table, "info" = info))
}

}