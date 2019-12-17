####get metabolite name and ID transformation table
setGeneric(name = "metTrans",
           def = function(
             met.name,
             type = c("kegg.id", "hmdb.id"),
             database = kegg.compound
           ){
             type <- match.arg(type)

             if(type == "kegg.id"){
               idx <- lapply(met.name, function(x) {
                 which(x == database$ID)
               })
               names(idx) <- met.name

               temp.table <- lapply(idx, function(x){
                 if(length(x) == 0){
                   temp <- data.frame(matrix("No match", ncol = 4), stringsAsFactors = FALSE)
                 }else{
                   temp <-  database[x,c(1,2,4,5),drop = FALSE]
                 }

                 colnames(temp) <- c("KEGG.ID", "Metabolite.name", "CAS.ID", "HMDB.ID")
                 temp[,2] <- unlist(lapply(temp[,2], function(x){
                   strsplit(x, split = ";")[[1]][1]
                 }))

                 if(length(x) != 0){
                   kegg.id <- temp[1,1]
                   metabolite.name <- paste(temp[,2], collapse = ";")

                   cas.id <- temp[,3]
                   hmdb.id <- temp[,4]

                   cas.id1 <- paste("http://commonchemistry.org/ChemicalDetail.aspx?ref=", cas.id, sep = "")
                   cas.id1 <- paste0("<a href='",cas.id1,"' target='_blank'>",cas.id,"</a>")

                   hmdb.id1 <- paste("http://www.hmdb.ca/metabolites/", hmdb.id, sep = "")
                   hmdb.id1 <- paste0("<a href='",hmdb.id1,"' target='_blank'>",hmdb.id,"</a>")

                   temp <- data.frame(kegg.id, metabolite.name, hmdb.id1, cas.id1, stringsAsFactors = FALSE)
                 }
                 temp

               })

               temp.table <- mapply(function(x, y){
                 kegg.id <- y
                 kegg.id1 <- paste("http://www.kegg.jp/dbget-bin/www_bget?cpd:", kegg.id, sep = "")
                 kegg.id1 <- paste0("<a href='",kegg.id1,"' target='_blank'>",kegg.id,"</a>")
                 x[1,1] <- kegg.id1
                 colnames(x) <- c("KEGG.ID", "Metabolite.name", "CAS.ID", "HMDB.ID")
                 list(x)
               },
               x = temp.table,
               y = met.name)
             }

             if(type == "hmdb.id"){
               idx <- lapply(met.name, function(x) {
                 which(x == database$HMDB.ID)
               })
               names(idx) <- met.name

               temp.table <- lapply(idx, function(x){
                 if(length(x) == 0){
                   temp <- data.frame(matrix("No match", ncol = 4), stringsAsFactors = FALSE)
                 }else{
                   temp <-  database[x,c(5, 1, 2,4),drop = FALSE]
                 }

                 colnames(temp) <- c("HMDB.ID", "KEGG.ID", "Metabolite.name", "CAS.ID")
                 temp[,3] <- unlist(lapply(temp[,3], function(x){
                   strsplit(x, split = ";")[[1]][1]
                 }))

                 if(length(x) != 0){
                   hmdb.id <- temp[1,1]
                   metabolite.name <- paste(temp[,3], collapse = ";")

                   cas.id <- temp[,4]
                   kegg.id <- temp[,2]

                   cas.id1 <- paste("http://commonchemistry.org/ChemicalDetail.aspx?ref=", cas.id, sep = "")
                   cas.id1 <- paste0("<a href='",cas.id1,"' target='_blank'>",cas.id,"</a>")

                   kegg.id1 <- paste("http://www.kegg.jp/dbget-bin/www_bget?cpd:", kegg.id, sep = "")
                   kegg.id1 <- paste0("<a href='",kegg.id1,"' target='_blank'>",kegg.id,"</a>")

                   temp <- data.frame(hmdb.id, kegg.id1, metabolite.name, cas.id1, stringsAsFactors = FALSE)
                 }
                 temp

               })

               temp.table <- mapply(function(x, y){
                 hmdb.id <- y
                 hmdb.id1 <- paste("http://www.hmdb.ca/metabolites/", hmdb.id, sep = "")
                 hmdb.id1 <- paste0("<a href='",hmdb.id1,"' target='_blank'>",hmdb.id,"</a>")
                 x[1,1] <- hmdb.id1
                 colnames(x) <- c("HMDB.ID", "KEGG.ID", "Metabolite.name", "CAS.ID")
                 list(x)
               },
               x = temp.table,
               y = met.name)
             }

             temp.table <- do.call(rbind, temp.table)
             temp.table


           })



#-------------------------------------------------------------------------------
mseAnalysis <- function(metabolite.id,
                        M,##M is the pathway library
                        test.method = c("hypergeometric", "fisher")){
  #
  metabolite.id <- unique(metabolite.id)
  test.method <- match.arg(test.method)

  metabolite.id <- metabolite.id[which(metabolite.id %in% unique(unlist(M)))]
  if(length(metabolite.id) == 0) return(NULL)

  ALL <- unname(unique(unlist(M)))
  ALL <- as.character(as.matrix(ALL))
  SIG <- as.character(as.matrix(metabolite.id))
  num_all <- length(ALL)
  num_sig <- length(SIG)

  Lall0 <- unlist(lapply(M, function(x) {
    length(intersect(x, ALL))
  }))

  Lall <- Lall0

  Lsig <- unlist(lapply(M, function(x) {
    length(intersect(x, SIG))
  }))

  remove.idx <- which(unname(Lall0, length) == 0)

  if(length(remove.idx) > 0){
    Lall <- Lall0[-remove.idx]
    Lsig <- Lsig[-remove.idx]
  }

  # error handling
  if (length(Lall) < 2){
    # stop function
    return(NULL)
  }

  P<-NaN
  system.time(for (i in 1:length(Lall)){
    # ------------------------------------
    #Generating 2?~2 table
    # -------------------------------------
    a1 <- Lsig[i]# significant and including pathway
    a2 <- Lall[i]-Lsig[i]# not significant and including pathway
    a3 <- length(SIG)-a1# significant and not including pathway
    a4 <- (length(ALL)-length(SIG))-a2# not significant and not including pathway

    if(test.method == "hypergeometric"){
      P[i] <- phyper(q = a1 - 1, m = Lall[i], n = num_all - Lall[i],
                     k = num_sig, lower.tail = FALSE)
    }else{
      tab <- t(matrix(c(a1,a2,a3,a4),2))
      # ----------------------------------
      # Fisher's exact test
      # ----------------------------------
      check <- tryCatch({
        resfish <- fisher.test(tab, alternative="greater")
      }, error = function(e){
        NA
      })

      if(class(check) != "htest"){
        P[i] <- 1
      }else{
        resfish <- fisher.test(tab, alternative="greater")
        P[i] <- resfish$p.value
      }
    }


  })



  # -----------------------
  #q-value
  # -----------------------
  Q <- p.adjust(P, method="BH")
  FDR <- p.adjust(P, method="fdr")
  # --------------------------------------------------------
  #significant metabolites for metabolite set
  # --------------------------------------------------------
  # LES <- NaN
  # for (i in 1:ncol(Lsig)){
  #   les <- SIG[Lsig[,i]==1]
  #   LES[i] <- list(les)
  #
  # }
  # names(LES) <- colnames(Lsig)

  # ----------------------
  #Result
  # ----------------------
  PQ <- cbind(P,Q, FDR)
  rownames(PQ) <- colnames(Lsig)
  colnames(PQ) <- c("p.value","q.value", "FDR")

  ##calculate the impact of pathway
  info <- lapply(M, function(module) {
    overlap.number <- length(intersect(module, metabolite.id))
    pathway.number <- length(module)
    c(pathway.number, overlap.number)
  })

  info <- do.call(rbind, info)
  colnames(info) <- c("Pathway.length", "Overlap")

  info <- data.frame(PQ, info, stringsAsFactors = FALSE)

  info <- info[order(info[,1]),]
  #     RES <- list(PQ, LES)
  #     names(RES) <- c("Result of MSEA(ORA)","significant metabolites")

  # -------------------
  #Return
  # -------------------
  path.name.id <- rownames(info)
  path.name <- unlist(lapply(path.name.id, function(x){
    strsplit(x = x, split = ";")[[1]][1]
  }))
  path.id <- unlist(lapply(path.name.id, function(x){
    strsplit(x = x, split = ";")[[1]][2]
  }))


  info <- data.frame(path.name, path.id, info, stringsAsFactors = FALSE)
  info <- info[,c(1,2,3,4,6,7)]
  rownames(info) <- NULL
  colnames(info) <- c("Pathway.name", "Pathway.ID", "p", "q", "Pathway.size", "Overlap")
  info <- info[info$Overlap > 0,]
  if(nrow(info) == 0) return(NULL)
  info
}





pathwayOverview <- function(object,
                            p.cutoff = 0.05,
                            overlap.cutoff = 0){
  if(is.null(object)) return(object)
  object <- as.data.frame(object)
  object$Pathway.size <- as.numeric(object$Pathway.size)
  my.theme <- ggplot2::theme_bw()+
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                   axis.title.y = ggplot2::element_text(size = 18)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15)) +
    ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

  object$Overlap <- object$Overlap*100/object$Pathway.size

  enriched <- rep(NA, nrow(object))
  enriched[which(object$p < p.cutoff & object$Overlap > overlap.cutoff)] <- "YES"
  enriched[is.na(enriched)] <- "NO"

  object$p <- -log(object$p, 10)

  object <- data.frame(object, enriched, stringsAsFactors = FALSE)

  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(object, ggplot2::aes(x = Overlap, y = p,
                                                label = Pathway.name,
                                                label2 = Pathway.ID)) +
    ggplot2::geom_point(
                        ggplot2::aes(colour = enriched,size = Pathway.size)) +
    ggplot2::labs(x = "Overlap (%)",
                  y = "-log10(P-value)")+
    # ggplot2::ggtitle("Peak profile")+
    my.theme +
    ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1),
                   legend.background = element_rect(fill='white')
                   )+
    ggplot2::geom_hline(ggplot2::aes(yintercept = -log(p.cutoff, 10)))+
    ggplot2::geom_vline(ggplot2::aes(xintercept = overlap.cutoff))

   return(plot)

}
